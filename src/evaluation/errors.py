from src.ast.abstract import ASTNode
from src.evaluation.values.values_base import Value
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
        super().__init__(f"Prefix operator `{operator}` on `{operand}` is not supported.")
