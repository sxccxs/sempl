from io import StringIO
from typing import TextIO

from src.lexer import matchers
from src.lexer.tokens import KEYWORDS, Token, TokenType


class Lexer:
    """Lexer class working with text stream."""

    def __init__(self, input_stream: TextIO) -> None:
        """
        Args:
            input_stream (TextIO): Input text stream for lexer.
        """
        self.in_stream = input_stream
        self.current_char = self.in_stream.read(1)
        self.after_char = self.in_stream.read(1)

    def _read_char(self) -> None:
        """Moves lexer to the next character."""
        if self.after_char == "":
            self.current_char = "\0"
            return

        self.current_char = self.after_char
        self.after_char = self.in_stream.read(1)

    def _peek_char(self) -> str:
        """
        Returns the next character, but does not move the lexer.
        If there is no characters left, returns zero-terminator ("\\0").
        """
        return self.after_char if self.after_char != "" else "\0"

    def _read_identifier(self) -> str:
        """
        Reads identifier from input stream.
        After reading lexer points to the last symbol of the identifier.
        """
        if not matchers.is_identifier_char(self.current_char):
            return ""
        sio = StringIO()
        sio.write(self.current_char)
        while matchers.is_identifier_char(self._peek_char()):
            sio.write(self._peek_char())
            self._read_char()
        return sio.getvalue()

    def _read_number_literal(self) -> str:
        """
        Reads number from input stream.
        After reading lexer points to the last symbol of the number.
        """
        if not self.current_char.isdigit():
            return ""
        sio = StringIO()
        sio.write(self.current_char)
        while matchers.is_number_char(self._peek_char()):
            sio.write(self._peek_char())
            self._read_char()
        return sio.getvalue()

    def _skip_whitespaces(self) -> None:
        while matchers.is_whitespace(self.current_char):
            self._read_char()

    def _token_from_current_char(self, token_type: TokenType) -> Token:
        """Creates a token with given type and current lexer character."""
        return Token(token_type, self.current_char)

    def _create_2_symbol_token(
        self,
        *,
        expected_second_char: str,
        two_symbol_token_type: TokenType,
        one_symbol_token_type: TokenType,
    ) -> Token:
        """
        Creates token with a 2-symbol or a 1-symbol literal depending on the next chararcter.

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

    @staticmethod
    def _create_identifier_token(literal: str) -> Token:
        """Creates a keyword token or an identifier token."""
        return Token(KEYWORDS.get(literal, TokenType.IDENT), literal)

    @staticmethod
    def _create_number_token(literal: str) -> Token:
        """Creates an integer, float or an illegal token."""
        if literal.isdigit():
            return Token(TokenType.INT, literal)
        if matchers.is_valid_unsigned_float(literal):
            return Token(TokenType.FLOAT, literal)
        return Token(TokenType.ILLEGAL, literal)

    def next_token(self) -> Token:
        """Reads a token."""
        token: Token

        self._skip_whitespaces()

        match self.current_char:
            case "\n":
                token = self._token_from_current_char(TokenType.ENDL)
            case "\0":
                token = self._token_from_current_char(TokenType.EOF)
            case "{":
                token = self._token_from_current_char(TokenType.LBRACE)
            case "}":
                token = self._token_from_current_char(TokenType.RBRACE)
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
                if matchers.is_identifier_char(self.current_char):
                    token = self._create_identifier_token(self._read_identifier())
                elif self.current_char.isdigit():
                    token = self._create_number_token(self._read_number_literal())
                else:
                    token = self._token_from_current_char(TokenType.ILLEGAL)

        self._read_char()

        return token
