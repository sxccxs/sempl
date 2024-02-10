"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Concrete evaluated values types.
"""
from dataclasses import dataclass
from io import StringIO
from typing import override

from src.evaluation.values.value_base import (
    IndexValueMixin,
    NumericValue,
    SequenceValue,
    Value,
    ValuedValue,
)


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
class Int(NumericValue, IndexValueMixin):
    """Integer value."""

    value: int

    @override
    def index(self) -> int:
        return self.value

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Bool(ValuedValue[bool]):
    """Boolean value."""

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Float(NumericValue):
    """Float value."""

    value: float

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Str(SequenceValue[str]):
    """String value."""

    value: str

    @override
    def get_value_from_index(self, index: int) -> Value:
        return Str(self.value[index])

    def __str__(self) -> str:
        return f'"{self.value}"'


@dataclass(frozen=True)
class Arr(SequenceValue[Value]):
    """Array value."""

    value: list[Value]

    @override
    def get_value_from_index(self, index: int) -> Value:
        return self.value[index]

    def __str__(self) -> str:
        ss = StringIO()
        ss.write("[")
        ss.write(", ".join(str(elem) for elem in self.value))
        ss.write("]")
        return ss.getvalue()


@dataclass(frozen=True)
class Type(ValuedValue[type[Value]]):
    """Type value."""

    def __str__(self) -> str:
        return str(self.value.__name__)


@dataclass(frozen=True)
class ReturnValue(ValuedValue[Value]):
    """Return value."""

    def __str__(self) -> str:
        return f"return {self.value}"
