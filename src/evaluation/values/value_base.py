"""Base classes for evaluated values."""
from abc import ABC, abstractmethod
from dataclasses import dataclass
from decimal import Decimal
from typing import Generic, Sequence, TypeVar

V = TypeVar("V")
Numeric = int | float | Decimal


@dataclass(frozen=True)
class Value(ABC):
    """Base class for evaluated value."""

    @abstractmethod
    def __str__(self) -> str:
        ...


@dataclass(frozen=True)
class ValuedValue(Generic[V], Value):
    """Base class for evaluated value containing underlying value."""

    value: V


@dataclass(frozen=True)
class NumericValue(ValuedValue[Numeric]):
    """Base class for evaluated value containing numeric underlying value."""


@dataclass(frozen=True)
class IndexValueMixin(ABC):
    """
    Mixin for evaluated value containing numeric underlying value
    which can be used as index.
    """

    @abstractmethod
    def index(self) -> int:
        """Returns integer value to be used as an index."""


@dataclass(frozen=True)
class SequenceValue(ValuedValue[Sequence[V]]):
    """Base class for evaluated value containing a sequence underyling value."""

    @abstractmethod
    def get_value_from_index(self, index: int) -> Value:
        """Returns data at given index as a Value."""


# T = TypeVar("T", bound=Value)


# @dataclass(frozen=True)
# class GenericValue(Generic[T], Value, ABC):
#     """Type value."""
