from typing import NamedTuple

from src.ast.abstract import Expression, Statement
from src.evaluation.values.value_base import Value


class ExpectedLetStatement(NamedTuple):
    mut: bool
    type: str
    name: str
    value: Expression


class ExpectedIfStatement(NamedTuple):
    """
    Expected If statement data.
    If no else is expected, `else_statements` has to be None.
    """

    condition: Expression
    then_statements: list[Statement]
    else_statements: list[Statement] | None


class ExpectedParam(NamedTuple):
    name: str
    type: str
    default_value: Expression | None


class ExpectedFunc(NamedTuple):
    name: str
    return_type: str
    parameters: list[ExpectedParam]
    body: list[Statement]


class ExpectedPrefixOperation(NamedTuple):
    operator: str
    operand: Expression


class ExpectedInfixOperation(NamedTuple):
    left_operand: Expression
    operator: str
    right_operand: Expression


class ExpectedCallExpression(NamedTuple):
    callable: Expression
    args: list[Expression]


class ExpectedEvaluatedLet(NamedTuple):
    name: str
    type_: type[Value]
    is_mut: bool
    value: Value


class ExpectedEvaluatedFuncParam(NamedTuple):
    name: str
    type_: type[Value]
    default: Value | None


class ExpectedEvaluatedFunction(NamedTuple):
    name: str
    ret_type: type[Value]
    params: list[ExpectedEvaluatedFuncParam]
    body: list[Statement]


class ExpectedEvaluatedFuncCall(NamedTuple):
    func_name: str
    returned_value: Value
