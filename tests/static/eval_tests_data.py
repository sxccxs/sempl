from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.evaluation.evaluator import STD_LIB
from src.evaluation.values import value_types
from src.evaluation.values.consts import TrueFalse
from src.evaluation.values.scope import FuncEntry, FuncParam, Scope
from src.evaluation.values.value_base import Value
from src.parser.types import Operator
from tests.utils.payloads import (
    ExpectedEvaluatedFuncCall,
    ExpectedEvaluatedFuncParam,
    ExpectedEvaluatedFunction,
    ExpectedEvaluatedLet,
)

SINGLE_VALID_INFIX_OPERATION_AND_EXPECTED: list[tuple[list[Statement], Value]] = [
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.InfixOperation(
                        left=ast_nodes.InfixOperation(
                            left=ast_nodes.IntegerLiteral(value=5),
                            operator=Operator.PLUS,
                            right=ast_nodes.IntegerLiteral(value=5),
                        ),
                        operator=Operator.PLUS,
                        right=ast_nodes.IntegerLiteral(value=5),
                    ),
                    operator=Operator.MINUS,
                    right=ast_nodes.IntegerLiteral(value=10),
                )
            )
        ],
        value_types.Integer(5),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.FloatLiteral(value=1.1),
                    operator=Operator.PLUS,
                    right=ast_nodes.InfixOperation(
                        left=ast_nodes.FloatLiteral(value=10.0),
                        operator=Operator.MULT,
                        right=ast_nodes.IntegerLiteral(value=2),
                    ),
                )
            )
        ],
        value_types.Float(21.1),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.InfixOperation(
                        left=ast_nodes.IntegerLiteral(value=10),
                        operator=Operator.MULT,
                        right=ast_nodes.IntegerLiteral(value=3),
                    ),
                    operator=Operator.DIV,
                    right=ast_nodes.IntegerLiteral(value=4),
                )
            )
        ],
        value_types.Integer(7),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.InfixOperation(
                        left=ast_nodes.IntegerLiteral(value=10),
                        operator=Operator.MULT,
                        right=ast_nodes.IntegerLiteral(value=2),
                    ),
                    operator=Operator.PLUS,
                    right=ast_nodes.FloatLiteral(value=0.0),
                )
            )
        ],
        value_types.Float(20.0),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.InfixOperation(
                        left=ast_nodes.IntegerLiteral(value=20),
                        operator=Operator.MULT,
                        right=ast_nodes.IntegerLiteral(value=2),
                    ),
                    operator=Operator.DIV,
                    right=ast_nodes.FloatLiteral(value=5.0),
                )
            )
        ],
        value_types.Float(8.0),
    ),
]

SINGLE_VALID_COMPARISON_AND_EXPECTED: list[tuple[list[Statement], TrueFalse]] = [
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(value=True),
                    operator=Operator.EQ,
                    right=ast_nodes.BooleanLiteral(value=True),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(value=False),
                    operator=Operator.EQ,
                    right=ast_nodes.BooleanLiteral(value=False),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(value=True),
                    operator=Operator.EQ,
                    right=ast_nodes.BooleanLiteral(value=False),
                )
            )
        ],
        TrueFalse.FALSE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(value=True),
                    operator=Operator.NOT_EQ,
                    right=ast_nodes.BooleanLiteral(value=False),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.InfixOperation(
                        left=ast_nodes.IntegerLiteral(value=1),
                        operator=Operator.GT,
                        right=ast_nodes.IntegerLiteral(value=2),
                    ),
                    operator=Operator.NOT_EQ,
                    right=ast_nodes.BooleanLiteral(value=True),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(value=False),
                    operator=Operator.EQ,
                    right=ast_nodes.InfixOperation(
                        left=ast_nodes.IntegerLiteral(value=2),
                        operator=Operator.GTEQ,
                        right=ast_nodes.FloatLiteral(value=2.1),
                    ),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.IntegerLiteral(value=0),
                    operator=Operator.EQ,
                    right=ast_nodes.FloatLiteral(value=0.0),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
]

SINGLE_VALID_LET_AND_EXPECTED: list[tuple[list[Statement], ExpectedEvaluatedLet]] = [
    (
        [
            ast_nodes.LetStatement(
                is_mut=True,
                var_type=ast_nodes.Identifier(value="int"),
                var_name=ast_nodes.Identifier(value="x"),
                var_value=ast_nodes.IntegerLiteral(value=10),
            )
        ],
        ExpectedEvaluatedLet("x", value_types.Integer, True, value_types.Integer(10)),
    ),
    (
        [
            ast_nodes.LetStatement(
                is_mut=False,
                var_type=ast_nodes.Identifier(value="float"),
                var_name=ast_nodes.Identifier(value="my_variable"),
                var_value=ast_nodes.InfixOperation(
                    left=ast_nodes.FloatLiteral(value=5.5),
                    operator=Operator.MULT,
                    right=ast_nodes.IntegerLiteral(value=2),
                ),
            )
        ],
        ExpectedEvaluatedLet("my_variable", value_types.Float, False, value_types.Float(11.0)),
    ),
]

SINGLE_VALID_FUNC_DEF_AND_EXPECTED: list[tuple[list[Statement], ExpectedEvaluatedFunction]] = [
    (
        [
            ast_nodes.FuncStatement(
                name=ast_nodes.Identifier(value="func"),
                parameters=[
                    ast_nodes.FuncParameter(
                        name=ast_nodes.Identifier(value="a"),
                        type=ast_nodes.Identifier(value="int"),
                        default_value=None,
                    ),
                    ast_nodes.FuncParameter(
                        name=ast_nodes.Identifier(value="b"),
                        type=ast_nodes.Identifier(value="float"),
                        default_value=ast_nodes.FloatLiteral(value=10.0),
                    ),
                ],
                return_type=ast_nodes.Identifier(value="Singularity"),
                body=ast_nodes.BlockStatement(statements=[]),
            )
        ],
        ExpectedEvaluatedFunction(
            "func",
            value_types.Singularity,
            [
                ExpectedEvaluatedFuncParam("a", value_types.Integer, None),
                ExpectedEvaluatedFuncParam("b", value_types.Float, value_types.Float(10.0)),
            ],
            [],
        ),
    ),
    (
        [
            ast_nodes.FuncStatement(
                name=ast_nodes.Identifier(value="abc"),
                parameters=[],
                return_type=ast_nodes.Identifier(value="Singularity"),
                body=ast_nodes.BlockStatement(statements=[]),
            )
        ],
        ExpectedEvaluatedFunction("abc", value_types.Singularity, [], []),
    ),
    (
        [
            ast_nodes.FuncStatement(
                name=ast_nodes.Identifier(value="x__x"),
                parameters=[
                    ast_nodes.FuncParameter(
                        name=ast_nodes.Identifier(value="num"),
                        type=ast_nodes.Identifier(value="int"),
                        default_value=ast_nodes.IntegerLiteral(value=10),
                    )
                ],
                return_type=ast_nodes.Identifier(value="int"),
                body=ast_nodes.BlockStatement(
                    statements=[
                        ast_nodes.ReturnStatement(return_value=ast_nodes.Identifier(value="num"))
                    ]
                ),
            )
        ],
        ExpectedEvaluatedFunction(
            "x__x",
            value_types.Integer,
            [ExpectedEvaluatedFuncParam("num", value_types.Integer, value_types.Integer(10))],
            [ast_nodes.ReturnStatement(return_value=ast_nodes.Identifier(value="num"))],
        ),
    ),
]

SINGLE_VALID_FUNC_CALL_AND_EXPECTED: list[
    tuple[list[Statement], Scope, ExpectedEvaluatedFuncCall]
] = [
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    callable=ast_nodes.Identifier(value="x"), arguments=[]
                )
            )
        ],
        Scope(
            {
                "x": FuncEntry(
                    [],
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.IntegerLiteral(10))]
                    ),
                    value_types.Type(value_types.Integer),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("x", value_types.Integer(10)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    callable=ast_nodes.Identifier(value="num"),
                    arguments=[ast_nodes.IntegerLiteral(value=10)],
                )
            )
        ],
        Scope(
            {
                "num": FuncEntry(
                    [FuncParam("x", value_types.Type(value_types.Integer), None)],
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.Identifier("x"))]
                    ),
                    value_types.Type(value_types.Integer),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("num", value_types.Integer(10)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    callable=ast_nodes.Identifier(value="num"),
                    arguments=[ast_nodes.IntegerLiteral(value=10)],
                )
            )
        ],
        Scope(
            {
                "num": FuncEntry(
                    [FuncParam("x", value_types.Type(value_types.Integer), value_types.Integer(1))],
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.Identifier("x"))]
                    ),
                    value_types.Type(value_types.Integer),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("num", value_types.Integer(10)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    callable=ast_nodes.Identifier(value="num"),
                    arguments=[],
                )
            )
        ],
        Scope(
            {
                "num": FuncEntry(
                    [FuncParam("x", value_types.Type(value_types.Integer), value_types.Integer(5))],
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.Identifier("x"))]
                    ),
                    value_types.Type(value_types.Integer),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("num", value_types.Integer(5)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    callable=ast_nodes.Identifier(value="num"),
                    arguments=[ast_nodes.IntegerLiteral(5)],
                )
            )
        ],
        Scope(
            {
                "num": FuncEntry(
                    [
                        FuncParam("x", value_types.Type(value_types.Integer), None),
                        FuncParam("y", value_types.Type(value_types.Float), value_types.Float(0.5)),
                    ],
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.Identifier("y"))]
                    ),
                    value_types.Type(value_types.Float),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("num", value_types.Float(0.5)),
    ),
]