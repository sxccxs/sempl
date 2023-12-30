from src.ast.abstract import ASTNode
from src.evaluation.values.value_base import Value
from src.parser.types import Operator


class EvaluationError(Exception):
    pass


class UnsuportedNodeError(EvaluationError):
    def __init__(self, node: ASTNode) -> None:
        super().__init__(f"Node of type `{type(node)}` is not supported.")


class UnsuportedPrefixOperator(EvaluationError):
    def __init__(self, operator: Operator) -> None:
        super().__init__(f"Prefix operator `{operator}` is not supported.")


class UnsuportedPrefixOperation(EvaluationError):
    def __init__(self, operator: Operator, operand: Value) -> None:
        super().__init__(f"Prefix operator `{operator}` on `{type(operand)}` is not supported.")


class UnsuportedInfixOperation(EvaluationError):
    def __init__(self, left: Value, operator: Operator, right: Value) -> None:
        super().__init__(
            f"Infix operator `{operator}` on `{type(left)}` and `{type(right)}` is not supported."
        )


class InvalidType(EvaluationError):
    def __init__(self, value: Value, expected_type: type[Value]) -> None:
        super().__init__(
            f"Can't assign value {value} of type {type(value)} to variable of type {expected_type}."
        )
