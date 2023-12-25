from io import StringIO

import pytest

from src.lexer.lexer import Lexer
from src.lexer.tokens import Token, TokenType

LITERAL_TOKEN_MAP: dict[str, Token] = {
    tt.value: Token(tt, tt.value)
    for tt in [
        TokenType.LCURLY,
        TokenType.RCURLY,
        TokenType.LPAREN,
        TokenType.RPAREN,
        TokenType.COLON,
        TokenType.ARROW,
        TokenType.COMA,
        TokenType.ASSIGN,
        TokenType.PLUS,
        TokenType.MINUS,
        TokenType.ASTERIX,
        TokenType.SLASH,
        TokenType.APOSTROPHE,
        TokenType.EQ,
        TokenType.NOT_EQ,
        TokenType.GT,
        TokenType.LT,
        TokenType.GTEQ,
        TokenType.LTEQ,
    ]
} | {
    "\n": Token(TokenType.ENDL, "\n"),
    "\0": Token(TokenType.EOF, "\0"),
    "`": Token(TokenType.ILLEGAL, "`"),
    "name": Token(TokenType.IDENT, "name"),
    "10": Token(TokenType.INT, "10"),
    "1.5": Token(TokenType.FLOAT, "1.5"),
    "fn": Token(TokenType.FN, "fn"),
    "if": Token(TokenType.IF, "if"),
    "else": Token(TokenType.ELSE, "else"),
    "return": Token(TokenType.RETURN, "return"),
    "mut": Token(TokenType.MUT, "mut"),
    "let": Token(TokenType.LET, "let"),
    "True": Token(TokenType.TRUE, "True"),
    "False": Token(TokenType.FALSE, "False"),
}

INPUT_AND_EXPECTED: dict[str, list[Token]] = {
    "+-/*=\n)(: '><\n\t": [
        Token(TokenType.PLUS, "+"),
        Token(TokenType.MINUS, "-"),
        Token(TokenType.SLASH, "/"),
        Token(TokenType.ASTERIX, "*"),
        Token(TokenType.ASSIGN, "="),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.COLON, ":"),
        Token(TokenType.APOSTROPHE, "'"),
        Token(TokenType.GT, ">"),
        Token(TokenType.LT, "<"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.EOF, "\0"),
    ],
    "5 sqrt == != <= >= -> 10000. \n if num == 2: \n\n": [
        Token(TokenType.INT, "5"),
        Token(TokenType.IDENT, "sqrt"),
        Token(TokenType.EQ, "=="),
        Token(TokenType.NOT_EQ, "!="),
        Token(TokenType.LTEQ, "<="),
        Token(TokenType.GTEQ, ">="),
        Token(TokenType.ARROW, "->"),
        Token(TokenType.FLOAT, "10000."),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.IF, "if"),
        Token(TokenType.IDENT, "num"),
        Token(TokenType.EQ, "=="),
        Token(TokenType.INT, "2"),
        Token(TokenType.COLON, ":"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.ENDL, "\n"),
        Token(TokenType.EOF, "\0"),
    ],
}


class TestLexerTg:
    @pytest.mark.parametrize(("input_", "expected"), list(INPUT_AND_EXPECTED.items()))
    def test_multiple_literals(self, input_: str, expected: list[Token]) -> None:
        """
        Tests lexer reading multiple literals correctly.

        Arrange: Put string of literals to a StringIO.
        Arrange: Create a lexer with the StringIO object.

        Cycle: For each expected token
            Act: Read a token from lexer.
            Assert: Token from lexer equals to expected token.
        """
        sio = StringIO(input_)
        lexer = Lexer(sio)
        for i, expected_token in enumerate(expected):
            token = lexer.next_token()
            assert token == expected_token, f"Token #{i} is wrong. {token} != {expected_token}"

    @pytest.mark.parametrize(("literal", "expected_token"), LITERAL_TOKEN_MAP.items())
    def test_one_token(self, literal: str, expected_token: Token) -> None:
        """
        Tests lexer reading one literal correctly.

        Arrange: Put literal to a StringIO.
        Arrange: Create a lexer with the StringIO object.

        Act: Read a token from lexer.
        Assert: Token from lexer equals to expected token.
        """
        sio = StringIO(literal)
        lexer = Lexer(sio)
        token = lexer.next_token()
        assert token == expected_token, f"For {literal=} expected {expected_token}, got {token}."
