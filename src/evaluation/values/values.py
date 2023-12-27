from dataclasses import dataclass

from src.evaluation.values.values_base import Value, ValueType


@dataclass(slots=True)
class Integer(Value):
    value: int

    @property
    def value_type(self) -> ValueType:
        return ValueType.INT

    def __str__(self) -> str:
        return str(self.value)


@dataclass(slots=True)
class Boolean(Value):
    value: bool

    @property
    def value_type(self) -> ValueType:
        return ValueType.BOOL

    def __str__(self) -> str:
        return str(self.value)

