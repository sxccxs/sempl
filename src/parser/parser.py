from result import Result

from src.ast_ import ast_nodes
from src.lexer.interfaces import ILexer
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

    def parse_program(self) -> Result[ast_nodes.Program, ParsingError]:
        """Parses the whole program."""
        # TODO
        ...
