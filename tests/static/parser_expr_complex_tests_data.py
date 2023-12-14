from src.ast import ast_nodes
from src.lexer.tokens import Token, TokenType
from tests.utils.payloads import ExpectedInfixOperation

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
