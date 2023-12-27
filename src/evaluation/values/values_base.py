from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import StrEnum, auto


class ValueType(StrEnum):
    INT = auto()
    BOOL = auto()


@dataclass(slots=True)
class Value(ABC):
    @property
    @abstractmethod
    def value_type(self) -> ValueType:
        """Type of the value."""

    @abstractmethod
    def __str__(self) -> str:
        ...
