from dataclasses import dataclass

from src.evaluation.values.value_base import NumericValue, Value, ValuedValue


@dataclass(frozen=True)
class Singularity(Value):
    def __str__(self) -> str:
        return "singularity"


@dataclass(frozen=True)
class NoEffect(Value):
    def __str__(self) -> str:
        return "No effect"


@dataclass(frozen=True)
class Int(NumericValue):
    value: int

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Bool(ValuedValue):
    value: bool

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Float(NumericValue):
    value: float

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Type(ValuedValue):
    value: type[Value]

    def __str__(self) -> str:
        return str(self.value.__name__)


@dataclass(frozen=True)
class ReturnValue(ValuedValue):
    value: Value

    def __str__(self) -> str:
        return f"return {self.value}"
