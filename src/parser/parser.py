from result import Err, Ok, Result

import src.parser.errors as parser_errors
from src.ast_ import ast_nodes
from src.ast_.abstract import Statement
from src.lexer.interfaces import ILexer
from src.lexer.tokens import Token, TokenType
from src.parser import stmt_sub_parsers
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
            case tt:
                return Err(parser_errors.UnsupportedStatementError(tt))
