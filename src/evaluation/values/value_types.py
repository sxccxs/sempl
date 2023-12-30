from dataclasses import dataclass

from src.evaluation.values.value_base import NumericValue, Value, ValuedValue


@dataclass(frozen=True)
class Integer(NumericValue):
    value: int

    def __str__(self) -> str:
        return str(self.value)


@dataclass(frozen=True)
class Boolean(ValuedValue):
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
        return str(self.value)
