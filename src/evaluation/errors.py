"""Evalutaion errors."""
from src.ast.abstract import ASTNode
from src.evaluation.values.value_base import Value
from src.parser.types import Operator


class EvaluationError(Exception):
    """Base Evaluation error."""


class UnsuportedNodeError(EvaluationError):
    """Evaluation error for ast node for which there is no handler."""

    def __init__(self, node: ASTNode) -> None:
        super().__init__(f"Node of type `{type(node)}` is not supported.")


class UnsuportedPrefixOperator(EvaluationError):
    """Evaluation error for prefix operator which is not supported."""

    def __init__(self, operator: Operator) -> None:
        super().__init__(f"Prefix operator `{operator}` is not supported.")


class UnsuportedPrefixOperation(EvaluationError):
    """Evaluation error for prefix opperation which is not supported on given ast node type."""

    def __init__(self, operator: Operator, operand: Value) -> None:
        super().__init__(f"Prefix operator `{operator}` on `{type(operand)}` is not supported.")


class UnsuportedInfixOperation(EvaluationError):
    """Evaluation error for infix opperation which is not supported on given ast node types."""

    def __init__(self, left: Value, operator: Operator, right: Value) -> None:
        super().__init__(
            f"Infix operator `{operator}` on `{type(left)}` and `{type(right)}` is not supported."
        )


class InvalidType(EvaluationError):
    """Evaluation error for type mistmatch."""

    def __init__(self, value: Value, expected_type: type[Value]) -> None:
        super().__init__(
            f"Can't assign value `{value}` of type {type(value)} to variable of type {expected_type}."
        )
