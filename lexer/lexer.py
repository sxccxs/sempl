from typing import TextIO

from lexer.tokens import Token, TokenType


class Lexer:
    def __init__(self, input_io: TextIO) -> None:
        self.input = input_io
        self.current_char = self.input.read(1)
        self.after_char = self.input.read(1)

    def _read_char(self) -> None:
        """Moves lexer to the next character."""
        if self.after_char == "":
            self.current_char = "\0"
            return

        self.current_char = self.after_char
        self.after_char = self.input.read(1)

    def _token_from_current_char(self, token_type: TokenType) -> Token:
        return Token(token_type, self.current_char)

    def next_token(self) -> Token:
        """Reads a token."""
        token: Token

        match self.current_char:
            case " ":
                token = self._token_from_current_char(TokenType.SPACE)
            case "(":
                token = self._token_from_current_char(TokenType.LPAREN)
            case ")":
                token = self._token_from_current_char(TokenType.RPAREN)
            case ":":
                token = self._token_from_current_char(TokenType.COLON)
            case "+":
                token = self._token_from_current_char(TokenType.PLUS)
            case "-":
                token = self._token_from_current_char(TokenType.MINUS)
            case "*":
                token = self._token_from_current_char(TokenType.ASTERIX)
            case "/":
                token = self._token_from_current_char(TokenType.SLASH)
            case "'":
                token = self._token_from_current_char(TokenType.APOSTROPHE)
            case "\n":
                token = self._token_from_current_char(TokenType.ENDL)
            case "\0":
                token = self._token_from_current_char(TokenType.EOF)
            case "=":
                token = self._token_from_current_char(TokenType.ASSIGN)
            case "<":
                token = self._token_from_current_char(TokenType.LT)
            case ">":
                token = self._token_from_current_char(TokenType.GT)
            case _:
                token = self._token_from_current_char(TokenType.ILLEGAL)

        self._read_char()

        return token
