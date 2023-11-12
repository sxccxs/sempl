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

    def _peek_char(self) -> str:
        """Returns the next character, but does not move the lexer.
        If there is no characters left, returns zero-terminator ("\\0").
        """
        return self.after_char if self.after_char != "" else "\0"

    def _create_2_symbol_token(
        self,
        *,
        expected_second_char: str,
        two_symbol_token_type: TokenType,
        one_symbol_token_type: TokenType,
    ) -> Token:
        """Creates token with a 2-symbol or a 1-symbol literal depending on the next chararcter.

        Args:
            expected_second_char (str): Value next character must equal for a 2-symbol token.
            two_symbol_token_type (TokenType): Type of 2-symbol token.
            one_symbol_token_type (TokenType): Type of 1-symbol token.
        """
        if self._peek_char() == expected_second_char:
            first_char = self.current_char
            self._read_char()
            return Token(two_symbol_token_type, f"{first_char}{self.current_char}")

        return Token(one_symbol_token_type, self.current_char)

    def next_token(self) -> Token:
        """Reads a token."""
        token: Token

        match self.current_char:
            case " ":
                token = self._token_from_current_char(TokenType.SPACE)
            case "\n":
                token = self._token_from_current_char(TokenType.ENDL)
            case "\0":
                token = self._token_from_current_char(TokenType.EOF)
            case "(":
                token = self._token_from_current_char(TokenType.LPAREN)
            case ")":
                token = self._token_from_current_char(TokenType.RPAREN)
            case ":":
                token = self._token_from_current_char(TokenType.COLON)
            case "+":
                token = self._token_from_current_char(TokenType.PLUS)
            case "*":
                token = self._token_from_current_char(TokenType.ASTERIX)
            case "/":
                token = self._token_from_current_char(TokenType.SLASH)
            case "'":
                token = self._token_from_current_char(TokenType.APOSTROPHE)
            case "-":
                token = self._create_2_symbol_token(
                    expected_second_char=">",
                    two_symbol_token_type=TokenType.ARROW,
                    one_symbol_token_type=TokenType.MINUS,
                )
            case "!":
                token = self._create_2_symbol_token(
                    expected_second_char="=",
                    two_symbol_token_type=TokenType.NOT_EQ,
                    one_symbol_token_type=TokenType.ILLEGAL,
                )
            case "=":
                token = self._create_2_symbol_token(
                    expected_second_char="=",
                    two_symbol_token_type=TokenType.EQ,
                    one_symbol_token_type=TokenType.ASSIGN,
                )
            case "<":
                token = self._create_2_symbol_token(
                    expected_second_char="=",
                    two_symbol_token_type=TokenType.LTEQ,
                    one_symbol_token_type=TokenType.LT,
                )
            case ">":
                token = self._create_2_symbol_token(
                    expected_second_char="=",
                    two_symbol_token_type=TokenType.GTEQ,
                    one_symbol_token_type=TokenType.GT,
                )
            case _:
                token = self._token_from_current_char(TokenType.ILLEGAL)

        self._read_char()

        return token
