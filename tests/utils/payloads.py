from typing import NamedTuple

from src.ast.abstract import Expression


class ExpectedLetStatement(NamedTuple):
    mut: bool
    type: str
    name: str


class ExpectedPrefixOperation(NamedTuple):
    operator: str
    operand: Expression
