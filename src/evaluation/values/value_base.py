"""Base classes for evaluated values."""
from abc import ABC, abstractmethod
from dataclasses import dataclass
from decimal import Decimal
from typing import Any

Numeric = int | float | Decimal


@dataclass(frozen=True)
class Value(ABC):
    """Base class for evaluated value."""

    @abstractmethod
    def __str__(self) -> str:
        ...


@dataclass(frozen=True)
class ValuedValue(Value):
    """Base class for evaluated value containing underlying value."""

    value: Any


@dataclass(frozen=True)
class NumericValue(ValuedValue):
    """Base class for evaluated value containing numeric underlying value."""

    value: Numeric
