"""Parser class."""
from result import Err, Ok, Result

from src.ast import ast_nodes
from src.errors.parser_errors import ParsingError
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Token, TokenType
from src.parser import expr_sub_parsers, stmt_sub_parsers
from src.parser.parser_base import BaseParser
from src.parser.types import InfixParserType, Precedence, PrefixParserType


class Parser(BaseParser):
    """Parser implementing BaseParser."""

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

        while not self.cur_token_is(TokenType.EOF):
            match stmt_sub_parsers.parse_statement(self):
                case Ok(stmt):
                    if stmt is not None:
                        program.statements.append(stmt)
                case Err() as err:
                    return err
            if self.cur_token_is(TokenType.EOF):
                break
            self.next_token()

        return Ok(program)

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
        tts_and_parsers: list[tuple[TokenType, PrefixParserType]] = [
            (TokenType.IDENT, expr_sub_parsers.parse_identifier),
            (TokenType.INT, expr_sub_parsers.parse_integer_literal),
            (TokenType.FLOAT, expr_sub_parsers.parse_float_literal),
            (TokenType.TRUE, expr_sub_parsers.parse_boolean_literal),
            (TokenType.FALSE, expr_sub_parsers.parse_boolean_literal),
            (TokenType.NOT, expr_sub_parsers.parse_prefix_operation),
            (TokenType.MINUS, expr_sub_parsers.parse_prefix_operation),
            (TokenType.PLUS, expr_sub_parsers.parse_prefix_operation),
            (TokenType.LPAREN, expr_sub_parsers.parse_grouped_expression),
        ]
        for tt, parser in tts_and_parsers:
            self.register_prefix_parser(tt, parser)

    def _register_infix(self) -> None:
        """Registers all infix parsers."""
        operation_tts: list[TokenType] = [
            TokenType.PLUS,
            TokenType.MINUS,
            TokenType.SLASH,
            TokenType.ASTERIX,
            TokenType.EQ,
            TokenType.NOT_EQ,
            TokenType.GT,
            TokenType.GTEQ,
            TokenType.LT,
            TokenType.LTEQ,
            TokenType.AND,
            TokenType.OR,
        ]
        for tt in operation_tts:
            self.register_infix_parser(tt, expr_sub_parsers.parse_inifix_operation)

        tts_and_parsers: list[tuple[TokenType, InfixParserType]] = [
            (TokenType.ASSIGN, expr_sub_parsers.parse_assignment),
            (TokenType.LPAREN, expr_sub_parsers.parse_call_expression),
        ]
        for tt, parser in tts_and_parsers:
            self.register_infix_parser(tt, parser)

    def _set_precedences(self) -> None:
        """Sets all precedences."""
        tt_and_prec: list[tuple[TokenType, Precedence]] = [
            (TokenType.ASSIGN, Precedence.ASSIGN),
            (TokenType.OR, Precedence.OR),
            (TokenType.AND, Precedence.AND),
            (TokenType.NOT, Precedence.NOT),
            (TokenType.EQ, Precedence.COMPARE),
            (TokenType.NOT_EQ, Precedence.COMPARE),
            (TokenType.LT, Precedence.COMPARE),
            (TokenType.LTEQ, Precedence.COMPARE),
            (TokenType.GT, Precedence.COMPARE),
            (TokenType.GTEQ, Precedence.COMPARE),
            (TokenType.PLUS, Precedence.SUM),
            (TokenType.MINUS, Precedence.SUM),
            (TokenType.ASTERIX, Precedence.PRODUCT),
            (TokenType.SLASH, Precedence.PRODUCT),
            (TokenType.LPAREN, Precedence.CALL),
        ]
        for tt, prec in tt_and_prec:
            self.set_precedence(tt, prec)
