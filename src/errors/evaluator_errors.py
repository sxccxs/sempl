"""Evalutaion errors."""
from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

from src.ast.abstract import ASTNode, Expression
from src.errors.error import Error
from src.evaluation.values.value_base import IndexValueMixin, Value
from src.parser.types import Operator

if TYPE_CHECKING:
    from src.evaluation.values.scope import BaseEntry


@dataclass
class EvaluationError(Error):
    """Base Evaluation error."""


@dataclass
class UnsuportedNodeError(EvaluationError):
    """Evaluation error for ast node for which there is no handler."""

    def __init__(self, node: ASTNode) -> None:
        super().__init__(f"Node of type `{type(node)}` is not supported.")


@dataclass
class UnsuportedEntryError(EvaluationError):
    """Evaluation error for scope entry for which there is no handler."""

    def __init__(self, entry: BaseEntry) -> None:
        super().__init__(f"Scope entry of type `{type(entry)}` is not supported.")


@dataclass
class UnsuportedPrefixOperator(EvaluationError):
    """Evaluation error for prefix operator which is not supported."""

    def __init__(self, operator: Operator) -> None:
        super().__init__(f"Prefix operator `{operator}` is not supported.")


@dataclass
class UnsuportedPrefixOperation(EvaluationError):
    """Evaluation error for prefix opperation which is not supported on given ast node type."""

    def __init__(self, operator: Operator, operand: Value) -> None:
        super().__init__(
            f"Prefix operator `{operator}` on `{type(operand).__name__}` is not supported."
        )


@dataclass
class UnsuportedInfixOperation(EvaluationError):
    """Evaluation error for infix opperation which is not supported on given ast node types."""

    def __init__(self, left: Value, operator: Operator, right: Value) -> None:
        super().__init__(
            f"Infix operator `{operator}` on `{type(left).__name__}` and "
            f"`{type(right).__name__}` is not supported."
        )


@dataclass
class MutabilityError(EvaluationError):
    """Evaluation error for type mistmatch."""

    def __init__(self, name: str) -> None:
        super().__init__(f"Identifier `{name}` is not mutable.")


@dataclass
class TypeMistmatchError(EvaluationError):
    """Evaluation error for type mistmatch."""

    def __init__(self, value: Value, expected_type: type[Value]) -> None:
        super().__init__(
            f"Expected type {expected_type}, but received "
            f"value `{value}` of type {type(value).__name__}."
        )


@dataclass
class ExpectedTypeIdentifierError(EvaluationError):
    """Evaluation error if instead of type anything else is provided."""

    def __init__(self, value: Value) -> None:
        super().__init__(
            f"Expected type identifier, but received `{value}` of type {type(value).__name__}."
        )


@dataclass
class ExpectedIdentifierError(EvaluationError):
    """Evaluation error if instead of identifier anything else is provided."""

    def __init__(self, value: Expression) -> None:
        super().__init__(f"Expected an identifier, but received `{value}`.")


@dataclass
class ExpectedSequenceError(EvaluationError):
    """Evaluation error if instead of sequence anything else is provided."""

    def __init__(self, value: Value) -> None:
        super().__init__(f"Expected a sequence, but received `{value}`.")


@dataclass
class ExpectedIndexError(EvaluationError):
    """Evaluation error if instead of index anything else is provided."""

    def __init__(self, value: Value) -> None:
        super().__init__(f"Expected an index, but received `{value}`.")


@dataclass
class IndexOutOfRangeError(EvaluationError):
    """Evaluation error if index is out of range.."""

    def __init__(self, value: IndexValueMixin) -> None:
        super().__init__(f"Index {value.index()} is out of range.")


@dataclass
class NameNotDefinedError(EvaluationError):
    """Evaluation error for identifier is not defined."""

    def __init__(self, name: str) -> None:
        super().__init__(f"Name `{name}` is not defined.")


@dataclass
class UsageError(EvaluationError):
    """Evaluation error for identifier misusage."""

    def __init__(self, name: str, *, as_what: str) -> None:
        super().__init__(f"Name `{name}` can not be used as {as_what}.")


@dataclass
class DivideByZeroError(EvaluationError):
    """Evaluation error for zero division."""

    def __init__(self) -> None:
        super().__init__("Can't divide by zero.")


@dataclass
class FunctionParameterError(EvaluationError):
    """Evaluation error for function parameters errors."""


@dataclass
class ArgumentBindingError(EvaluationError):
    """Evaluation error for argument binding errors."""


@dataclass
class NoReturnError(EvaluationError):
    """Evaluation error for no return."""

    def __init__(self, func_name: str, ret_type: type[Value]) -> None:
        super().__init__(
            f"Function {func_name} has to return a value of type {ret_type.__name__} on all code passes."
        )
