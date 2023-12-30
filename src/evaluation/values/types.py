from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Self, TypeVar

from src.evaluation.values.value_base import Value
from src.evaluation.values.value_types import Type

T = TypeVar("T")

TYPE = str


class BaseEntry(ABC):
    @property
    @abstractmethod
    def value(self) -> Value:
        """Underlying value of an entry."""


@dataclass(slots=True)
class VarEntry(BaseEntry):
    _value: Value
    is_mut: bool
    type: Type

    @property
    def value(self) -> Value:
        return self._value


@dataclass(slots=True)
class TypeEntry(BaseEntry):
    _value: Type

    @property
    def value(self) -> Type:
        return self._value


ScopeEntry = VarEntry | TypeEntry


@dataclass(slots=True)
class Scope:
    store: dict[str, ScopeEntry] = field(default_factory=dict)

    @classmethod
    def from_scope(cls, other: Self) -> Self:
        """Creates scope from already existing."""
        scope = cls()
        scope.store = other.store.copy()
        return scope

    def get(self, name: str, default: T = None) -> ScopeEntry | T:
        """Return the value for name if name is in the scope, else default."""
        return self.store.get(name, default)

    def __getitem__(self, name: str) -> ScopeEntry:
        return self.store[name]

    def __setitem__(self, name: str, value: ScopeEntry) -> None:
        self.store[name] = value

    def __delitem__(self, name: str) -> None:
        del self.store[name]
