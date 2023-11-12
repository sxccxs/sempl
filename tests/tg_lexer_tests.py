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
    """5+1 - 4 * (sqrt(23.5)/ x' )
        x = 10
        y == 5
        25 >= 1000. ->

        """: [
        Token(TokenType.INT, "5"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.INT, "1"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.MINUS, "-"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.INT, "4"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.ASTERIX, "*"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.IDENT, "sqrt"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.FLOAT, "23.5"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.SLASH, "/"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.IDENT, "x"),
        Token(TokenType.APOSTROPHE, "'"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.IDENT, "x"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.ASSIGN, "="),
        Token(TokenType.SPACE, " "),
        Token(TokenType.INT, "10"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.IDENT, "y"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.EQ, "=="),
        Token(TokenType.SPACE, " "),
        Token(TokenType.INT, "5"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.INT, "25"),
        Token(TokenType.SPACE, " "),
        Token(TokenType.GTEQ, ">="),
        Token(TokenType.SPACE, " "),
        Token(TokenType.FLOAT, "1000."),
        Token(TokenType.SPACE, " "),
        Token(TokenType.ARROW, "->"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.EOF, ""),
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
