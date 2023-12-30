from abc import ABC, abstractmethod
from dataclasses import dataclass
from decimal import Decimal
from typing import Any

Numeric = int | float | Decimal


@dataclass(frozen=True)
class Value(ABC):
    @abstractmethod
    def __str__(self) -> str:
        ...


@dataclass(frozen=True)
class ValuedValue(Value):
    value: Any


@dataclass(frozen=True)
class NumericValue(ValuedValue):
    value: Numeric
