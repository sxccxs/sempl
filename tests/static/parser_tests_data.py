from src.ast import ast_nodes
from src.lexer.tokens import Token, TokenType
from tests.utils.payloads import ExpectedInfixOperation, ExpectedLetStatement

VALID_LET_STATEMENT_TOKENS_AND_EXPECTED: list[tuple[list[Token], ExpectedLetStatement]] = [
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.IDENT, "x"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "10"),
            Token(TokenType.ENDL, "\n"),
        ],
        ExpectedLetStatement(False, "int", "x"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "str"),
            Token(TokenType.IDENT, "_abcdef11_"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.ILLEGAL, '"'),
            Token(TokenType.ILLEGAL, '"'),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
        ],
        ExpectedLetStatement(False, "str", "_abcdef11_"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.MUT, "mut"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.IDENT, "y"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.FLOAT, "20."),
        ],
        ExpectedLetStatement(True, "int", "y"),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.MUT, "mut"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.IDENT, "word"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "25"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.FLOAT, "34."),
            Token(TokenType.ILLEGAL, "^"),
            Token(TokenType.INT, "2"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
        ],
        ExpectedLetStatement(True, "int", "word"),
    ),
]

VALID_SINGLE_INFIX_OPERATIONS_AND_EXPECTED: list[tuple[list[Token], ExpectedInfixOperation]] = [
    (
        [Token(TokenType.INT, "5"), Token(TokenType.PLUS, "+"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "+", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.MINUS, "-"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "-", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [
            Token(TokenType.INT, "5"),
            Token(TokenType.ASTERIX, "*"),
            Token(TokenType.INT, "5"),
        ],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "*", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.SLASH, "/"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "/", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.GT, ">"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), ">", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.GTEQ, ">="), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), ">=", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.LT, "<"), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "<", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.LTEQ, "<="), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "<=", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [Token(TokenType.INT, "5"), Token(TokenType.EQ, "=="), Token(TokenType.INT, "5")],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "==", ast_nodes.IntegerLiteral(5)),
    ),
    (
        [
            Token(TokenType.INT, "5"),
            Token(TokenType.NOT_EQ, "!="),
            Token(TokenType.INT, "5"),
        ],
        ExpectedInfixOperation(ast_nodes.IntegerLiteral(5), "!=", ast_nodes.IntegerLiteral(5)),
    ),
]

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
