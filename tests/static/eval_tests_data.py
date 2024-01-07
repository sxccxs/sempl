"""Test data for evaluation tests."""
from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.evaluation.std_lib import STD_LIB
from src.evaluation.values import value_types
from src.evaluation.values.consts import TrueFalse
from src.evaluation.values.scope import FuncEntry, FuncParam, Scope, VarEntry
from src.evaluation.values.value_base import Value
from src.parser.types import Operator
from tests.utils.payloads import (
    ExpectedChangedVariableValue,
    ExpectedEvaluatedFuncCall,
    ExpectedEvaluatedFuncParam,
    ExpectedEvaluatedFunction,
    ExpectedEvaluatedIndexOperation,
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
        value_types.Int(5),
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
        value_types.Int(7),
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
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(True),
                    operator=Operator.AND,
                    right=ast_nodes.BooleanLiteral(True),
                )
            )
        ],
        value_types.Bool(True),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(False),
                    operator=Operator.AND,
                    right=ast_nodes.BooleanLiteral(True),
                )
            )
        ],
        value_types.Bool(False),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(False),
                    operator=Operator.AND,
                    right=ast_nodes.BooleanLiteral(False),
                )
            )
        ],
        value_types.Bool(False),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(True),
                    operator=Operator.AND,
                    right=ast_nodes.BooleanLiteral(False),
                )
            )
        ],
        value_types.Bool(False),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(True),
                    operator=Operator.OR,
                    right=ast_nodes.BooleanLiteral(True),
                )
            )
        ],
        value_types.Bool(True),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(False),
                    operator=Operator.OR,
                    right=ast_nodes.BooleanLiteral(True),
                )
            )
        ],
        value_types.Bool(True),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(False),
                    operator=Operator.OR,
                    right=ast_nodes.BooleanLiteral(False),
                )
            )
        ],
        value_types.Bool(False),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(True),
                    operator=Operator.OR,
                    right=ast_nodes.BooleanLiteral(False),
                )
            )
        ],
        value_types.Bool(True),
    ),
]

SINGLE_VALID_COMPARISON_AND_EXPECTED: list[tuple[list[Statement], TrueFalse]] = [
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(True),
                    operator=Operator.EQ,
                    right=ast_nodes.BooleanLiteral(True),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(False),
                    operator=Operator.EQ,
                    right=ast_nodes.BooleanLiteral(False),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(True),
                    operator=Operator.EQ,
                    right=ast_nodes.BooleanLiteral(False),
                )
            )
        ],
        TrueFalse.FALSE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(True),
                    operator=Operator.NOT_EQ,
                    right=ast_nodes.BooleanLiteral(False),
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
                    right=ast_nodes.BooleanLiteral(True),
                )
            )
        ],
        TrueFalse.TRUE,
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.InfixOperation(
                    left=ast_nodes.BooleanLiteral(False),
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
                var_type=ast_nodes.Identifier(value="Int"),
                var_name=ast_nodes.Identifier(value="x"),
                var_value=ast_nodes.IntegerLiteral(value=10),
            )
        ],
        ExpectedEvaluatedLet("x", value_types.Int, True, value_types.Int(10)),
    ),
    (
        [
            ast_nodes.LetStatement(
                is_mut=False,
                var_type=ast_nodes.Identifier(value="Float"),
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
                        type=ast_nodes.Identifier(value="Int"),
                        default_value=None,
                    ),
                    ast_nodes.FuncParameter(
                        name=ast_nodes.Identifier(value="b"),
                        type=ast_nodes.Identifier(value="Float"),
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
                ExpectedEvaluatedFuncParam("a", value_types.Int, None),
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
                        type=ast_nodes.Identifier(value="Int"),
                        default_value=ast_nodes.IntegerLiteral(value=10),
                    )
                ],
                return_type=ast_nodes.Identifier(value="Int"),
                body=ast_nodes.BlockStatement(
                    statements=[
                        ast_nodes.ReturnStatement(return_value=ast_nodes.Identifier(value="num"))
                    ]
                ),
            )
        ],
        ExpectedEvaluatedFunction(
            "x__x",
            value_types.Int,
            [ExpectedEvaluatedFuncParam("num", value_types.Int, value_types.Int(10))],
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
                    func=ast_nodes.Identifier(value="x"), arguments=[]
                )
            )
        ],
        Scope(
            {
                "x": FuncEntry(
                    [],
                    value_types.Type(value_types.Int),
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.IntegerLiteral(10))]
                    ),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("x", value_types.Int(10)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    func=ast_nodes.Identifier(value="num"),
                    arguments=[ast_nodes.IntegerLiteral(value=10)],
                )
            )
        ],
        Scope(
            {
                "num": FuncEntry(
                    [FuncParam("x", value_types.Type(value_types.Int), None)],
                    value_types.Type(value_types.Int),
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.Identifier("x"))]
                    ),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("num", value_types.Int(10)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    func=ast_nodes.Identifier(value="num"),
                    arguments=[ast_nodes.IntegerLiteral(value=10)],
                )
            )
        ],
        Scope(
            {
                "num": FuncEntry(
                    [FuncParam("x", value_types.Type(value_types.Int), value_types.Int(1))],
                    value_types.Type(value_types.Int),
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.Identifier("x"))]
                    ),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("num", value_types.Int(10)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    func=ast_nodes.Identifier(value="num"),
                    arguments=[],
                )
            )
        ],
        Scope(
            {
                "num": FuncEntry(
                    [FuncParam("x", value_types.Type(value_types.Int), value_types.Int(5))],
                    value_types.Type(value_types.Int),
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.Identifier("x"))]
                    ),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("num", value_types.Int(5)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.CallExpression(
                    func=ast_nodes.Identifier(value="num"),
                    arguments=[ast_nodes.IntegerLiteral(5)],
                )
            )
        ],
        Scope(
            {
                "num": FuncEntry(
                    [
                        FuncParam("x", value_types.Type(value_types.Int), None),
                        FuncParam("y", value_types.Type(value_types.Float), value_types.Float(0.5)),
                    ],
                    value_types.Type(value_types.Float),
                    ast_nodes.BlockStatement(
                        [ast_nodes.ReturnStatement(ast_nodes.Identifier("y"))]
                    ),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedFuncCall("num", value_types.Float(0.5)),
    ),
]

SINGLE_VALID_ASSIGN_AND_EXPECTED: list[
    tuple[list[Statement], Scope, ExpectedChangedVariableValue]
] = [
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.Assignment(
                    assignee=ast_nodes.Identifier("x"), value=ast_nodes.IntegerLiteral(10)
                )
            )
        ],
        Scope(
            {"x": VarEntry(value_types.Int(20), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(10)),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                expression=ast_nodes.Assignment(
                    assignee=ast_nodes.Identifier("y"),
                    value=ast_nodes.InfixOperation(
                        ast_nodes.IntegerLiteral(20), Operator.MULT, ast_nodes.IntegerLiteral(2)
                    ),
                )
            )
        ],
        Scope(
            {"y": VarEntry(value_types.Int(20), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("y", value_types.Int(40)),
    ),
]

SINGLE_VALID_IF_AND_EXPECTED: list[tuple[list[Statement], Scope, ExpectedChangedVariableValue]] = [
    (
        [
            ast_nodes.IfStatement(
                condition=ast_nodes.BooleanLiteral(True),
                then=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(1)
                            )
                        )
                    ]
                ),
                else_=None,
            ),
        ],
        Scope(
            {"x": VarEntry(value_types.Int(0), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(1)),
    ),
    (
        [
            ast_nodes.IfStatement(
                condition=ast_nodes.BooleanLiteral(False),
                then=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(1)
                            )
                        )
                    ]
                ),
                else_=None,
            ),
        ],
        Scope(
            {"x": VarEntry(value_types.Int(0), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(0)),
    ),
    (
        [
            ast_nodes.IfStatement(
                condition=ast_nodes.BooleanLiteral(False),
                then=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(1)
                            )
                        )
                    ]
                ),
                else_=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(2)
                            )
                        )
                    ]
                ),
            ),
        ],
        Scope(
            {"x": VarEntry(value_types.Int(0), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(2)),
    ),
    (
        [
            ast_nodes.IfStatement(
                condition=ast_nodes.BooleanLiteral(True),
                then=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(1)
                            )
                        )
                    ]
                ),
                else_=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(2)
                            )
                        )
                    ]
                ),
            ),
        ],
        Scope(
            {"x": VarEntry(value_types.Int(0), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(1)),
    ),
    (
        [
            ast_nodes.IfStatement(
                condition=ast_nodes.InfixOperation(
                    ast_nodes.Identifier("x"),
                    Operator.EQ,
                    ast_nodes.IntegerLiteral(1),
                ),
                then=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(1)
                            )
                        )
                    ]
                ),
                else_=ast_nodes.BlockStatement(
                    [
                        ast_nodes.IfStatement(
                            condition=ast_nodes.InfixOperation(
                                ast_nodes.Identifier("x"),
                                Operator.NOT_EQ,
                                ast_nodes.IntegerLiteral(1),
                            ),
                            then=ast_nodes.BlockStatement(
                                [
                                    ast_nodes.ExpressionStatement(
                                        ast_nodes.Assignment(
                                            ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(2)
                                        )
                                    )
                                ]
                            ),
                            else_=None,
                        ),
                    ]
                ),
            ),
        ],
        Scope(
            {"x": VarEntry(value_types.Int(0), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(2)),
    ),
    (
        [
            ast_nodes.IfStatement(
                condition=ast_nodes.InfixOperation(
                    ast_nodes.Identifier("x"),
                    Operator.EQ,
                    ast_nodes.IntegerLiteral(1),
                ),
                then=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(1)
                            )
                        )
                    ]
                ),
                else_=ast_nodes.BlockStatement(
                    [
                        ast_nodes.IfStatement(
                            condition=ast_nodes.InfixOperation(
                                ast_nodes.Identifier("x"),
                                Operator.NOT_EQ,
                                ast_nodes.IntegerLiteral(0),
                            ),
                            then=ast_nodes.BlockStatement(
                                [
                                    ast_nodes.ExpressionStatement(
                                        ast_nodes.Assignment(
                                            ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(2)
                                        )
                                    )
                                ]
                            ),
                            else_=ast_nodes.BlockStatement(
                                [
                                    ast_nodes.ExpressionStatement(
                                        ast_nodes.Assignment(
                                            ast_nodes.Identifier("x"), ast_nodes.IntegerLiteral(3)
                                        )
                                    )
                                ]
                            ),
                        ),
                    ]
                ),
            ),
        ],
        Scope(
            {"x": VarEntry(value_types.Int(0), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(3)),
    ),
]

SINGLE_VALID_WHILE_AND_EXPECTED: list[
    tuple[list[Statement], Scope, ExpectedChangedVariableValue]
] = [
    (
        [
            ast_nodes.WhileStatement(
                condition=ast_nodes.BooleanLiteral(False),
                actions=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"),
                                ast_nodes.InfixOperation(
                                    ast_nodes.Identifier("x"),
                                    Operator.PLUS,
                                    ast_nodes.IntegerLiteral(1),
                                ),
                            )
                        )
                    ]
                ),
            ),
        ],
        Scope(
            {"x": VarEntry(value_types.Int(0), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(0)),
    ),
    (
        [
            ast_nodes.WhileStatement(
                condition=ast_nodes.InfixOperation(
                    ast_nodes.Identifier("x"), Operator.LT, ast_nodes.IntegerLiteral(5)
                ),
                actions=ast_nodes.BlockStatement(
                    [
                        ast_nodes.ExpressionStatement(
                            ast_nodes.Assignment(
                                ast_nodes.Identifier("x"),
                                ast_nodes.InfixOperation(
                                    ast_nodes.Identifier("x"),
                                    Operator.PLUS,
                                    ast_nodes.IntegerLiteral(1),
                                ),
                            )
                        )
                    ]
                ),
            ),
        ],
        Scope(
            {"x": VarEntry(value_types.Int(0), True, value_types.Type(value_types.Int))}, STD_LIB
        ),
        ExpectedChangedVariableValue("x", value_types.Int(5)),
    ),
]

SINGLE_VALID_INDEX_AND_EXPECTED: list[
    tuple[list[Statement], Scope, ExpectedEvaluatedIndexOperation]
] = [
    (
        [
            ast_nodes.ExpressionStatement(
                ast_nodes.IndexOperation(
                    left=ast_nodes.Identifier("x"), index=ast_nodes.IntegerLiteral(0)
                ),
            )
        ],
        Scope(
            {
                "x": VarEntry(
                    value_types.Arr([value_types.Int(0), value_types.Int(1)]),
                    True,
                    value_types.Type(value_types.Arr),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedIndexOperation("x", 0),
    ),
    (
        [
            ast_nodes.ExpressionStatement(
                ast_nodes.IndexOperation(
                    left=ast_nodes.Identifier("x"), index=ast_nodes.IntegerLiteral(1)
                ),
            )
        ],
        Scope(
            {
                "x": VarEntry(
                    value_types.Str("abc"),
                    True,
                    value_types.Type(value_types.Str),
                )
            },
            STD_LIB,
        ),
        ExpectedEvaluatedIndexOperation("x", 1),
    ),
]
