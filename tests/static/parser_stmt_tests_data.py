from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.lexer.tokens import Token, TokenType
from tests.utils.payloads import (
    ExpectedFunc,
    ExpectedIfStatement,
    ExpectedLetStatement,
    ExpectedParam,
)

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
        ExpectedLetStatement(False, "int", "x", ast_nodes.IntegerLiteral(10)),
    ),
    (
        [
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.IDENT, "_abcdef11_"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.MINUS, "-"),
            Token(TokenType.INT, "5000"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
        ],
        ExpectedLetStatement(
            False,
            "int",
            "_abcdef11_",
            ast_nodes.PrefixOperation("-", ast_nodes.IntegerLiteral(5000)),
        ),
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
        ExpectedLetStatement(True, "int", "y", ast_nodes.FloatLiteral(20.0)),
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
            Token(TokenType.ASTERIX, "*"),
            Token(TokenType.INT, "2"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.ENDL, "\n"),
        ],
        ExpectedLetStatement(
            True,
            "int",
            "word",
            ast_nodes.InfixOperation(
                ast_nodes.IntegerLiteral(25),
                "+",
                ast_nodes.InfixOperation(
                    ast_nodes.FloatLiteral(34.0), "*", ast_nodes.IntegerLiteral(2)
                ),
            ),
        ),
    ),
]

VALID_BLOCK_STATEMENT_AND_EXPECTED: list[tuple[list[Token], list[Statement]]] = [
    (
        [
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.LT, literal="<"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        [
            ast_nodes.ExpressionStatement(
                ast_nodes.InfixOperation(ast_nodes.Identifier("x"), "<", ast_nodes.Identifier("y"))
            )
        ],
    ),
    (
        [
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.LT, literal="<"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        [
            ast_nodes.ExpressionStatement(
                ast_nodes.InfixOperation(ast_nodes.Identifier("x"), "+", ast_nodes.Identifier("y"))
            ),
            ast_nodes.ExpressionStatement(
                ast_nodes.InfixOperation(ast_nodes.Identifier("x"), "<", ast_nodes.Identifier("y"))
            ),
        ],
    ),
    (
        [
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        [],
    ),
    (
        [
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.RCURLY, literal="}"),
        ],
        [],
    ),
]

VALID_IF_STATEMENT_AND_EXPECTED: list[tuple[list[Token], ExpectedIfStatement]] = [
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.TRUE, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.BooleanLiteral(True),
            [],
            None,
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.TRUE, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.BooleanLiteral(True),
            [],
            [],
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.GT, literal=">"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.InfixOperation(ast_nodes.Identifier("x"), ">", ast_nodes.Identifier("y")),
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.InfixOperation(
                        ast_nodes.Identifier("x"), "+", ast_nodes.Identifier("y")
                    )
                )
            ],
            None,
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.GT, literal=">"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.InfixOperation(ast_nodes.Identifier("x"), ">", ast_nodes.Identifier("y")),
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.InfixOperation(
                        ast_nodes.Identifier("x"), "+", ast_nodes.Identifier("y")
                    )
                )
            ],
            [],
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.GT, literal=">"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.PLUS, literal="+"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IDENT, literal="x"),
            Token(TokenType.PLUS, literal="-"),
            Token(TokenType.IDENT, literal="y"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.InfixOperation(ast_nodes.Identifier("x"), ">", ast_nodes.Identifier("y")),
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.InfixOperation(
                        ast_nodes.Identifier("x"), "+", ast_nodes.Identifier("y")
                    )
                )
            ],
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.InfixOperation(
                        ast_nodes.Identifier("x"), "-", ast_nodes.Identifier("y")
                    )
                )
            ],
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.TRUE, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.IF, literal="if"),
            Token(TokenType.FALSE, literal="False"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.BooleanLiteral(True),
            [],
            [
                ast_nodes.IfStatement(
                    ast_nodes.BooleanLiteral(False), ast_nodes.BlockStatement([]), None
                )
            ],
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.TRUE, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IF, literal="if"),
            Token(TokenType.FALSE, literal="False"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.BooleanLiteral(True),
            [],
            [
                ast_nodes.IfStatement(
                    ast_nodes.BooleanLiteral(False), ast_nodes.BlockStatement([]), None
                )
            ],
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.TRUE, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IF, literal="if"),
            Token(TokenType.FALSE, literal="False"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.BooleanLiteral(True),
            [],
            [
                ast_nodes.IfStatement(
                    ast_nodes.BooleanLiteral(False),
                    ast_nodes.BlockStatement([]),
                    ast_nodes.BlockStatement([]),
                )
            ],
        ),
    ),
]
