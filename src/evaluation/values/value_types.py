"""Concrete evaluated values types."""
from dataclasses import dataclass

from src.evaluation.values.value_base import NumericValue, Value, ValuedValue


@dataclass(frozen=True)
class Singularity(Value):
    """Singularity value."""

    def __str__(self) -> str:
        return "singularity"


@dataclass(frozen=True)
class NoEffect(Value):
    """No effect value."""

    def __str__(self) -> str:
        return "No effect"


@dataclass(frozen=True)
class Int(NumericValue):
    """Integer value."""

    value: int

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Bool(ValuedValue):
    """Boolean value."""

    value: bool

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Float(NumericValue):
    """Float value."""

    value: float

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class String(ValuedValue):
    """String value."""

    value: str

    def __str__(self) -> str:
        return f'"{self.value}"'


@dataclass(frozen=True)
class Type(ValuedValue):
    """Type value."""

    value: type[Value]

    def __str__(self) -> str:
        return str(self.value.__name__)


@dataclass(frozen=True)
class ReturnValue(ValuedValue):
    """Return value."""

    value: Value

    def __str__(self) -> str:
        return f"return {self.value}"
