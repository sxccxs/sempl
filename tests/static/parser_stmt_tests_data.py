"""Test data for statement parsing tests."""
from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.lexer.tokens import Token, TokenType
from src.parser.types import Operator
from tests.utils.payloads import (
    ExpectedFunc,
    ExpectedIfStatement,
    ExpectedLetStatement,
    ExpectedParam,
    ExpectedWhileStatement,
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
            ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.IntegerLiteral(5000)),
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
                Operator.PLUS,
                ast_nodes.InfixOperation(
                    ast_nodes.FloatLiteral(34.0), Operator.MULT, ast_nodes.IntegerLiteral(2)
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
                ast_nodes.InfixOperation(
                    ast_nodes.Identifier("x"), Operator.LT, ast_nodes.Identifier("y")
                )
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
                ast_nodes.InfixOperation(
                    ast_nodes.Identifier("x"), Operator.PLUS, ast_nodes.Identifier("y")
                )
            ),
            ast_nodes.ExpressionStatement(
                ast_nodes.InfixOperation(
                    ast_nodes.Identifier("x"), Operator.LT, ast_nodes.Identifier("y")
                )
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
            Token(TokenType.IDENT, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.Identifier("True"),
            [],
            None,
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="True"),
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
            ast_nodes.Identifier("True"),
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
            ast_nodes.InfixOperation(
                ast_nodes.Identifier("x"), Operator.GT, ast_nodes.Identifier("y")
            ),
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.InfixOperation(
                        ast_nodes.Identifier("x"), Operator.PLUS, ast_nodes.Identifier("y")
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
            ast_nodes.InfixOperation(
                ast_nodes.Identifier("x"), Operator.GT, ast_nodes.Identifier("y")
            ),
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.InfixOperation(
                        ast_nodes.Identifier("x"), Operator.PLUS, ast_nodes.Identifier("y")
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
            ast_nodes.InfixOperation(
                ast_nodes.Identifier("x"), Operator.GT, ast_nodes.Identifier("y")
            ),
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.InfixOperation(
                        ast_nodes.Identifier("x"), Operator.PLUS, ast_nodes.Identifier("y")
                    )
                )
            ],
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.InfixOperation(
                        ast_nodes.Identifier("x"), Operator.MINUS, ast_nodes.Identifier("y")
                    )
                )
            ],
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="False"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.Identifier("True"),
            [],
            [
                ast_nodes.IfStatement(
                    ast_nodes.Identifier("False"), ast_nodes.BlockStatement([]), None
                )
            ],
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="False"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ENDL, literal="\n"),
        ],
        ExpectedIfStatement(
            ast_nodes.Identifier("True"),
            [],
            [
                ast_nodes.IfStatement(
                    ast_nodes.Identifier("False"), ast_nodes.BlockStatement([]), None
                )
            ],
        ),
    ),
    (
        [
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
            Token(TokenType.ELSE, literal="else"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IF, literal="if"),
            Token(TokenType.IDENT, literal="False"),
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
            ast_nodes.Identifier("True"),
            [],
            [
                ast_nodes.IfStatement(
                    ast_nodes.Identifier("False"),
                    ast_nodes.BlockStatement([]),
                    ast_nodes.BlockStatement([]),
                )
            ],
        ),
    ),
]


VALID_FUNC_AND_EXPECTED: list[tuple[list[Token], ExpectedFunc]] = [
    (
        [
            Token(TokenType.FN, "fn"),
            Token(TokenType.IDENT, "func"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.LCURLY, "{"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.RCURLY, "}"),
            Token(TokenType.ENDL, "\n"),
        ],
        ExpectedFunc("func", "int", [], []),
    ),
    (
        [
            Token(TokenType.FN, "fn"),
            Token(TokenType.IDENT, "func"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.IDENT, "a"),
            Token(TokenType.COLON, ":"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.COMA, ","),
            Token(TokenType.IDENT, "b"),
            Token(TokenType.COLON, ":"),
            Token(TokenType.IDENT, "str"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.IDENT, "bool"),
            Token(TokenType.LCURLY, "{"),
            Token(TokenType.RCURLY, "}"),
            Token(TokenType.ENDL, "\n"),
        ],
        ExpectedFunc(
            "func", "bool", [ExpectedParam("a", "int", None), ExpectedParam("b", "str", None)], []
        ),
    ),
    (
        [
            Token(TokenType.FN, "fn"),
            Token(TokenType.IDENT, "f"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.IDENT, "a"),
            Token(TokenType.COLON, ":"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "5"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.ARROW, "->"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.LCURLY, "{"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "int"),
            Token(TokenType.IDENT, "b"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.FLOAT, "10."),
            Token(TokenType.ASTERIX, "*"),
            Token(TokenType.IDENT, "a"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.RETURN, "return"),
            Token(TokenType.IDENT, "b"),
            Token(TokenType.ENDL, "\n"),
            Token(TokenType.RCURLY, "}"),
            Token(TokenType.ENDL, "\n"),
        ],
        ExpectedFunc(
            "f",
            "int",
            [ExpectedParam("a", "int", ast_nodes.IntegerLiteral(5))],
            [
                ast_nodes.LetStatement(
                    False,
                    ast_nodes.Identifier("int"),
                    ast_nodes.Identifier("b"),
                    ast_nodes.InfixOperation(
                        ast_nodes.FloatLiteral(10.0), Operator.MULT, ast_nodes.Identifier("a")
                    ),
                ),
                ast_nodes.ReturnStatement(ast_nodes.Identifier("b")),
            ],
        ),
    ),
]

VALID_WHILE_STATEMENT_AND_EXPECTED: list[tuple[list[Token], ExpectedWhileStatement]] = [
    (
        [
            Token(TokenType.WHILE, literal="while"),
            Token(TokenType.IDENT, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
        ],
        ExpectedWhileStatement(
            ast_nodes.Identifier("True"),
            [],
        ),
    ),
    (
        [
            Token(TokenType.WHILE, literal="while"),
            Token(TokenType.IDENT, literal="True"),
            Token(TokenType.LCURLY, literal="{"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.IDENT, literal="xx"),
            Token(TokenType.LPAREN, literal="("),
            Token(TokenType.RPAREN, literal=")"),
            Token(TokenType.ENDL, literal="\n"),
            Token(TokenType.RCURLY, literal="}"),
        ],
        ExpectedWhileStatement(
            ast_nodes.Identifier("True"),
            [
                ast_nodes.ExpressionStatement(
                    ast_nodes.CallExpression(ast_nodes.Identifier("xx"), []),
                )
            ],
        ),
    ),
]
