from src.lexer.tokens import Token, TokenType

INFIX_OPERATIONS_PRECEDENCE_AND_EXPECTED: list[tuple[list[Token], str]] = [
    (
        [
            Token(TokenType.MINUS, literal="-"),
            Token(TokenType.MINUS, literal="-"),
            Token(TokenType.IDENT, literal="a"),
        ],
        "(-(-a))",
    ),
    (
        [
            Token(TokenType.IDENT, literal="a"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="b"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="c"),
        ],
        "((a + b) + c)",
    ),
    (
        [
            Token(TokenType.IDENT, literal="a"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="b"),
            Token(TokenType.MINUS, literal="-"),
            Token(TokenType.IDENT, literal="c"),
        ],
        "((a + b) - c)",
    ),
    (
        [
            Token(TokenType.IDENT, literal="a"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.IDENT, literal="b"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.IDENT, literal="c"),
        ],
        "((a * b) * c)",
    ),
    (
        [
            Token(TokenType.IDENT, literal="a"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.IDENT, literal="b"),
            Token(TokenType.SLASH, literal="/"),
            Token(TokenType.IDENT, literal="c"),
        ],
        "((a * b) / c)",
    ),
    (
        [
            Token(TokenType.IDENT, literal="a"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="b"),
            Token(TokenType.SLASH, literal="/"),
            Token(TokenType.IDENT, literal="c"),
        ],
        "(a + (b / c))",
    ),
    (
        [
            Token(TokenType.IDENT, literal="a"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="b"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.IDENT, literal="c"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="d"),
            Token(TokenType.SLASH, literal="/"),
            Token(TokenType.IDENT, literal="e"),
            Token(TokenType.MINUS, literal="-"),
            Token(TokenType.IDENT, literal="f"),
        ],
        "(((a + (b * c)) + (d / e)) - f)",
    ),
    (
        [
            Token(TokenType.INT, literal="5"),
            Token(TokenType.GT, literal=">"),
            Token(TokenType.INT, literal="4"),
            Token(TokenType.EQ, literal="=="),
            Token(TokenType.INT, literal="3"),
            Token(TokenType.LT, literal="<"),
            Token(TokenType.INT, literal="4"),
        ],
        "((5 > 4) == (3 < 4))",
    ),
    (
        [
            Token(TokenType.INT, literal="5"),
            Token(TokenType.LT, literal="<"),
            Token(TokenType.INT, literal="4"),
            Token(TokenType.NOT_EQ, literal="!="),
            Token(TokenType.INT, literal="3"),
            Token(TokenType.GT, literal=">"),
            Token(TokenType.INT, literal="4"),
        ],
        "((5 < 4) != (3 > 4))",
    ),
    (
        [
            Token(TokenType.INT, literal="3"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.INT, literal="4"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.INT, literal="5"),
            Token(TokenType.EQ, literal="=="),
            Token(TokenType.INT, literal="3"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.INT, literal="1"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.INT, literal="4"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.INT, literal="5"),
        ],
        "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
    ),
]
