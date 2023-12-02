from result import Result

from src.ast_ import ast_nodes
from src.lexer.interfaces import ILexer
from src.lexer.tokens import TokenType
from src.parser.errors import ParsingError


class Parser:
    def __init__(self, lexer: ILexer) -> None:
        """
        Args:
            lexer (ILexer): Lexer object providing tokens to the parser.
        """
        self.lexer = lexer
        self.current_token = self.lexer.next_token()
        self.peek_token = self.lexer.next_token()

    def next_token(self) -> None:
        """Moves parser to the next token."""
        self.current_token = self.peek_token
        self.peek_token = self.lexer.next_token()

    def cur_token_is(self, token_t: TokenType) -> bool:
        """True if parser's current token equals to token_t else False."""
        return self.current_token.type == token_t

    def peek_token_is(self, token_t: TokenType) -> bool:
        """True if parser's peek token equals to token_t else False."""
        return self.peek_token.type == token_t

    def move_to_next_if_peek_is_expected(self, expected_t: TokenType) -> bool:
        """If parser's peek token equals to token_t, moves to next token and returns True.
        Otherwise just returns False.
        """
        if not self.peek_token_is(expected_t):
            return False
        self.next_token()
        return True

    def parse_program(self) -> Result[ast_nodes.Program, ParsingError]:
        """Parses the whole program."""
        # TODO
        ...
