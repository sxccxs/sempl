from result import Result

from src.ast_ import ast_nodes
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Token
from src.parser.errors import ParsingError
from src.parser.interfaces import IParser


class Parser(IParser):
    def __init__(self, lexer: ILexer) -> None:
        """
        Args:
            lexer (ILexer): Lexer object providing tokens to the parser.
        """
        self.lexer = lexer
        self._current_token = self.lexer.next_token()
        self._peek_token = self.lexer.next_token()

    @property
    def current_token(self) -> Token:
        return self._current_token

    @property
    def peek_token(self) -> Token:
        return self._peek_token

    def next_token(self) -> None:
        """Moves parser to the next token."""
        self._current_token = self._peek_token
        self._peek_token = self.lexer.next_token()

    def parse_program(self) -> Result[ast_nodes.Program, ParsingError]:
        """Parses the whole program."""
        # TODO
        ...
