from result import Err, Ok, Result

from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Token, TokenType
from src.parser import expr_sub_parsers, stmt_sub_parsers
from src.parser.errors import ParsingError
from src.parser.interfaces import IParser
from src.parser.types import InfixParserType, Precedence, PrefixParserType


class Parser(IParser):
    def __init__(self, lexer: ILexer) -> None:
        """
        Args:
            lexer (ILexer): Lexer object providing tokens to the parser.
        """
        self.lexer = lexer
        self._current_token = self.lexer.next_token()
        self._peek_token = self.lexer.next_token()
        self._prefix_parsers: dict[TokenType, PrefixParserType] = {}
        self._infix_parsers: dict[TokenType, InfixParserType] = {}
        self._precedences: dict[TokenType, Precedence] = {}
        self._set_precedences()
        self._register_prefix()
        self._register_infix()

    @property
    def current_token(self) -> Token:
        return self._current_token

    @property
    def peek_token(self) -> Token:
        return self._peek_token

    @property
    def prefix_parsers(self) -> dict[TokenType, PrefixParserType]:
        return self._prefix_parsers

    @property
    def infix_parsers(self) -> dict[TokenType, InfixParserType]:
        return self._infix_parsers

    @property
    def cur_token_precedence(self) -> Precedence:
        return self._precedences.get(self.current_token.type, Precedence.LOWEST)

    @property
    def peek_token_precedence(self) -> Precedence:
        return self._precedences.get(self.peek_token.type, Precedence.LOWEST)

    def next_token(self) -> None:
        """Moves parser to the next token."""
        self._current_token = self._peek_token
        self._peek_token = self.lexer.next_token()

    def parse_program(self) -> Result[ast_nodes.Program, ParsingError]:
        """Parses the whole program."""
        program = ast_nodes.Program()

        while self.current_token.type != TokenType.EOF:
            match self.parse_statement():
                case Ok(stmt):
                    if stmt is not None:
                        program.statements.append(stmt)
                case Err() as err:
                    return err
            if (
                self.current_token.type == TokenType.EOF
            ):  # pyright: ignore[reportUnnecessaryComparison]
                break
            self.next_token()

        return Ok(program)

    def parse_statement(self) -> Result[Statement | None, ParsingError]:
        """Parses one statement from current token if such is valid."""
        match self.current_token.type:
            case TokenType.LET:
                return stmt_sub_parsers.parse_let_statement(self)
            case TokenType.RETURN:
                return stmt_sub_parsers.parse_return_statement(self)
            case TokenType.ENDL:
                return Ok(None)
            case _:
                return stmt_sub_parsers.parse_expression_statement(self)

    def register_prefix_parser(self, tt: TokenType, p: PrefixParserType) -> None:
        """Registers a prefix parser."""
        self._prefix_parsers[tt] = p

    def register_infix_parser(self, tt: TokenType, p: InfixParserType) -> None:
        """Registers an infix parser."""
        self._infix_parsers[tt] = p

    def set_precedence(self, tt: TokenType, p: Precedence) -> None:
        """Sets given precedence for a given token type."""
        self._precedences[tt] = p

    def _register_prefix(self) -> None:
        """Registers all prefix parsers."""
        self.register_prefix_parser(TokenType.IDENT, expr_sub_parsers.parse_identifier)
        self.register_prefix_parser(TokenType.TRUE, expr_sub_parsers.parse_boolean_literal)
        self.register_prefix_parser(TokenType.FALSE, expr_sub_parsers.parse_boolean_literal)
        self.register_prefix_parser(TokenType.INT, expr_sub_parsers.parse_integer_literal)
        self.register_prefix_parser(TokenType.FLOAT, expr_sub_parsers.parse_float_literal)
        self.register_prefix_parser(TokenType.MINUS, expr_sub_parsers.parse_prefix_operation)
        self.register_prefix_parser(TokenType.PLUS, expr_sub_parsers.parse_prefix_operation)

    def _register_infix(self) -> None:
        """Registers all infix parsers."""
        self.register_infix_parser(TokenType.PLUS, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.MINUS, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.SLASH, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.ASTERIX, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.EQ, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.NOT_EQ, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.GT, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.GTEQ, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.LT, expr_sub_parsers.parse_inifix_operation)
        self.register_infix_parser(TokenType.LTEQ, expr_sub_parsers.parse_inifix_operation)

    def _set_precedences(self) -> None:
        """Sets all precedences."""
        self.set_precedence(TokenType.EQ, Precedence.EQUALS)
        self.set_precedence(TokenType.NOT_EQ, Precedence.EQUALS)
        self.set_precedence(TokenType.LT, Precedence.LESSGREATER)
        self.set_precedence(TokenType.LTEQ, Precedence.LESSGREATER)
        self.set_precedence(TokenType.GT, Precedence.LESSGREATER)
        self.set_precedence(TokenType.GTEQ, Precedence.LESSGREATER)
        self.set_precedence(TokenType.PLUS, Precedence.SUM)
        self.set_precedence(TokenType.MINUS, Precedence.SUM)
        self.set_precedence(TokenType.ASTERIX, Precedence.PRODUCT)
        self.set_precedence(TokenType.SLASH, Precedence.PRODUCT)
