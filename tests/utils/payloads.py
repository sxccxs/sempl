"""Tests payloads."""
from typing import NamedTuple

from src.ast.abstract import Expression, Statement
from src.evaluation.values.value_base import Value


class ExpectedLetStatement(NamedTuple):
    """Expected let statement payload."""

    mut: bool
    type: str
    name: str
    value: Expression


class ExpectedIfStatement(NamedTuple):
    """
    Expected If statement payload.
    If no else is expected, `else_statements` has to be None.
    """

    condition: Expression
    then_statements: list[Statement]
    else_statements: list[Statement] | None


class ExpectedParam(NamedTuple):
    """Expected function parameter payload."""

    name: str
    type: str
    default_value: Expression | None


class ExpectedFunc(NamedTuple):
    """Expected function statement payload."""

    name: str
    return_type: str
    parameters: list[ExpectedParam]
    body: list[Statement]


class ExpectedPrefixOperation(NamedTuple):
    """Expected prefix operation payload."""

    operator: str
    operand: Expression


class ExpectedInfixOperation(NamedTuple):
    """Expected infix operation payload."""

    left_operand: Expression
    operator: str
    right_operand: Expression


class ExpectedCallExpression(NamedTuple):
    """Expected call expression payload."""

    callable: Expression
    args: list[Expression]


class ExpectedAssignmentExpression(NamedTuple):
    """Expected assignment expression payload."""

    assignee: Expression
    value: Expression


class ExpectedEvaluatedLet(NamedTuple):
    """Expected evaluated let statement payload."""

    name: str
    type_: type[Value]
    is_mut: bool
    value: Value


class ExpectedEvaluatedFuncParam(NamedTuple):
    """Expected evaluated function parameter payload."""

    name: str
    type_: type[Value]
    default: Value | None


class ExpectedEvaluatedFunction(NamedTuple):
    """Expected evaluated function statement payload."""

    name: str
    ret_type: type[Value]
    params: list[ExpectedEvaluatedFuncParam]
    body: list[Statement]


class ExpectedEvaluatedFuncCall(NamedTuple):
    """Expected evaluated function call payload."""

    func_name: str
    returned_value: Value


class ExpectedEvaluatedAssignment(NamedTuple):
    """Expected evaluated assignment payload."""

    var_name: str
    new_value: Value
