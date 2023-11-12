from io import StringIO

import pytest

from lexer.lexer import Lexer
from lexer.tokens import Token, TokenType

input_and_expected: dict[str, list[Token]] = {
    "+-/*=\n)(: '><\n": [
        Token(TokenType.PLUS, "+"),
        Token(TokenType.MINUS, "-"),
        Token(TokenType.SLASH, "/"),
        Token(TokenType.ASTERIX, "*"),
        Token(TokenType.ASSIGN, "="),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.COLON, ":"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.APOSTROPHE, "'"),
        Token(TokenType.GT, ">"),
        Token(TokenType.LT, "<"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.EOF, "\0"),
    ],
    "5 sqrt == != <= >= -> 10000. \n if num == 2: \n\n": [
        Token(TokenType.INT, "5"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.IDENT, "sqrt"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.EQ, "=="),
        Token(TokenType.SPACE, " "),
        Token(TokenType.NOT_EQ, "!="),
        Token(TokenType.SPACE, " "),
        Token(TokenType.LTEQ, "<="),
        Token(TokenType.SPACE, " "),
        Token(TokenType.GTEQ, ">="),
        Token(TokenType.SPACE, " "),
        Token(TokenType.ARROW, "->"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.FLOAT, "10000."),
        Token(TokenType.SPACE, " "),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.IF, "if"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.IDENT, "num"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.EQ, "=="),
        Token(TokenType.SPACE, " "),
        Token(TokenType.INT, "2"),
        Token(TokenType.COLON, ":"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.EOF, "\0"),
    ],
}


class TestLexer:
    @pytest.mark.parametrize("input_,expected", list(input_and_expected.items()))
    def test_lexer(self, input_: str, expected: list[Token]) -> None:
        """
        Tests lexer reading.

        Arrange: Put input string to a StringIO.
        Arrange: Create a lexer with the StringIO object.

        Cycle: For each expected token
            Act: Read a token from lexer.
            Assert: Read token equals to expected token.
        """
        sio = StringIO(input_)
        lexer = Lexer(sio)
        for i, expected_token in enumerate(expected):
            token = lexer.next_token()
            assert token == expected_token, f"Token #{i} is wrong. {token} != {expected_token}"
