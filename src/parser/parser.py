from result import Err, Ok, Result

from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Token, TokenType
from src.parser import expr_sub_parsers, stmt_sub_parsers
from src.parser.errors import ParsingError
from src.parser.interfaces import IParser
from src.parser.types import InfixParserType, PrefixParserType


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

        self._register_prefix()

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

    def _register_prefix(self) -> None:
        """Registers all prefix parsers."""
        self.register_prefix_parser(TokenType.IDENT, expr_sub_parsers.parse_identifier)
        self.register_prefix_parser(TokenType.INT, expr_sub_parsers.parser_integer_literal)
