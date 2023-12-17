from typing import NamedTuple

from src.ast.abstract import Expression, Statement


class ExpectedLetStatement(NamedTuple):
    mut: bool
    type: str
    name: str


class ExpectedIfStatement(NamedTuple):
    """
    Expected If statement data.
    If no else is expected, `else_statements` has to be None.
    """

    condition: Expression
    then_statements: list[Statement]
    else_statements: list[Statement] | None


class ExpectedPrefixOperation(NamedTuple):
    operator: str
    operand: Expression


class ExpectedInfixOperation(NamedTuple):
    left_operand: Expression
    operator: str
    right_operand: Expression
