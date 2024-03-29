"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Test data for complex parser tests.
"""
from src.lexer.tokens import Token, TokenType

INFIX_OPERATIONS_PRECEDENCE_AND_EXPECTED: list[tuple[list[Token], str]] = [
    (
        [
            Token(TokenType.MINUS, literal="-"),
            Token(TokenType.MINUS, literal="-"),
            Token(TokenType.IDENT, literal="a"),
        ],
        "(- (- a))",
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
        "(((5 > 4) == 3) < 4)",
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
        "(((5 < 4) != 3) > 4)",
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
    (
        [
            Token(TokenType.IDENT, "a"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.INT, "1"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.INT, "2"),
            Token(TokenType.COMA, ","),
            Token(TokenType.IDENT, "a"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.IDENT, "b"),
            Token(TokenType.RPAREN, ")"),
        ],
        "a((1 + 2), (a + b))",
    ),
    (
        [
            Token(TokenType.IDENT, "add"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.IDENT, "a"),
            Token(TokenType.COMA, ","),
            Token(TokenType.IDENT, "b"),
            Token(TokenType.COMA, ","),
            Token(TokenType.IDENT, "c"),
            Token(TokenType.COMA, ","),
            Token(TokenType.IDENT, "d"),
            Token(TokenType.COMA, ","),
            Token(TokenType.INT, "1"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.INT, "2"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.INT, "3"),
            Token(TokenType.COMA, ","),
            Token(TokenType.IDENT, "pow"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.INT, "2"),
            Token(TokenType.COMA, ","),
            Token(TokenType.INT, "2"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
        ],
        "add(a, b, c, d, ((1 + 2) + 3), pow(2, 2))",
    ),
    (
        [
            Token(TokenType.IDENT, "a"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.INT, "2"),
            Token(TokenType.ASTERIX, "*"),
            Token(TokenType.IDENT, "b"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.INT, "10"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.INT, "11"),
            Token(TokenType.ASTERIX, "*"),
            Token(TokenType.IDENT, "x"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.ENDL, "\n"),
        ],
        "a((2 * b((10 + (11 * x)))))",
    ),
    (
        [
            Token(TokenType.IDENT, "xxxx"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.INT, "1"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.INT, "10"),
            Token(TokenType.ASTERIX, "*"),
            Token(TokenType.INT, "2"),
            Token(TokenType.SLASH, "/"),
            Token(TokenType.INT, "3"),
            Token(TokenType.MINUS, "-"),
            Token(TokenType.INT, "4"),
            Token(TokenType.ASTERIX, "*"),
            Token(TokenType.INT, "2"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.ENDL, "\n"),
        ],
        "xxxx(((1 + ((10 * 2) / 3)) - (4 * 2)))",
    ),
]
GROUPED_EXPRESSION_AND_EXPECTED: list[tuple[list[Token], str]] = [
    (
        [
            Token(type=TokenType.INT, literal="1"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.LPAREN, literal="("),
            Token(type=TokenType.INT, literal="2"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.INT, literal="3"),
            Token(type=TokenType.RPAREN, literal=")"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.INT, literal="4"),
        ],
        "((1 + (2 + 3)) + 4)",
    ),
    (
        [
            Token(type=TokenType.LPAREN, literal="("),
            Token(type=TokenType.INT, literal="5"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.INT, literal="5"),
            Token(type=TokenType.RPAREN, literal=")"),
            Token(type=TokenType.ASTERIX, literal="*"),
            Token(type=TokenType.INT, literal="2"),
        ],
        "((5 + 5) * 2)",
    ),
    (
        [
            Token(type=TokenType.INT, literal="2"),
            Token(type=TokenType.SLASH, literal="/"),
            Token(type=TokenType.LPAREN, literal="("),
            Token(type=TokenType.INT, literal="5"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.INT, literal="5"),
            Token(type=TokenType.RPAREN, literal=")"),
        ],
        "(2 / (5 + 5))",
    ),
    (
        [
            Token(type=TokenType.MINUS, literal="-"),
            Token(type=TokenType.LPAREN, literal="("),
            Token(type=TokenType.INT, literal="5"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.INT, literal="5"),
            Token(type=TokenType.RPAREN, literal=")"),
        ],
        "(- (5 + 5))",
    ),
    (
        [
            Token(type=TokenType.LPAREN, literal="("),
            Token(type=TokenType.INT, literal="1"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.LPAREN, literal="("),
            Token(type=TokenType.INT, literal="2"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.LPAREN, literal="("),
            Token(type=TokenType.INT, literal="3"),
            Token(type=TokenType.PLUS, literal="+"),
            Token(type=TokenType.INT, literal="4"),
            Token(type=TokenType.RPAREN, literal=")"),
            Token(type=TokenType.RPAREN, literal=")"),
            Token(type=TokenType.RPAREN, literal=")"),
        ],
        "(1 + (2 + (3 + 4)))",
    ),
    (
        [
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.ASSIGN, literal="="),
            Token(TokenType.INT, literal="10"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.INT, literal="10"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.INT, literal="2"),
        ],
        "(x = (10 + (10 * 2)))",
    ),
    (
        [
            Token(TokenType.LPAREN, literal="("),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.ASSIGN, literal="="),
            Token(TokenType.INT, literal="10"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.INT, literal="10"),
            Token(TokenType.ASTERIX, literal="*"),
            Token(TokenType.INT, literal="2"),
            Token(TokenType.RPAREN, literal=")"),
            Token(TokenType.EQ, literal="=="),
            Token(TokenType.INT, literal="5"),
        ],
        "((x = (10 + (10 * 2))) == 5)",
    ),
    (
        [
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.OR, literal="or"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.AND, literal="and"),
            Token(TokenType.NOT, literal="not"),
            Token(TokenType.IDENT, literal="z"),
        ],
        "(x or (y and (not z)))",
    ),
]
