from typing import TextIO

from lexer.tokens import Token, TokenType


class Lexer:
    def __init__(self, input_io: TextIO) -> None:
        self.input = input_io
        self.current_char = self.input.read(1)
        self.after_char = self.input.read(1)

    def read_char(self) -> None:
        """Moves lexer to the next character."""
        if self.after_char == "":
            self.current_char = "\0"
            return

        self.current_char = self.after_char
        self.after_char = self.input.read(1)

    def next_token(self) -> Token:
        """Reads a token."""
        token_type: TokenType = TokenType.ILLEGAL
        token_value: str | None = None

        match self.current_char:
            case " ":
                token_type = TokenType.SPACE
            case "(":
                token_type = TokenType.LPAREN
            case ")":
                token_type = TokenType.RPAREN
            case ":":
                token_type = TokenType.COLON
            case "+":
                token_type = TokenType.PLUS
            case "-":
                token_type = TokenType.MINUS
            case "*":
                token_type = TokenType.ASTERIX
            case "/":
                token_type = TokenType.SLASH
            case "^":
                token_type = TokenType.CIRCUMFLEX
            case "'":
                token_type = TokenType.APOSTROPHE
            case "\n":
                token_type = TokenType.ENDL
            case "\0":
                token_type = TokenType.EOF
            case _:
                pass

        return (
            Token(token_type, token_value)
            if token_value is not None
            else Token(token_type, self.current_char)
        )
