from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import TypeVar

from src.ast import ast_nodes
from src.evaluation.values.value_base import Value
from src.evaluation.values.value_types import Type

T = TypeVar("T")

TYPE = str


class BaseEntry(ABC):
    @property
    @abstractmethod
    def value(self) -> Value | None:
        """Underlying value of an entry."""


@dataclass(slots=True)
class VarEntry(BaseEntry):
    var_value: Value
    is_mut: bool
    type_value: Type

    @property
    def value(self) -> Value:
        return self.var_value


@dataclass(frozen=True)
class FuncParam:
    name: str
    type_value: Type
    default_value: Value | None


@dataclass(slots=True)
class FuncEntry(BaseEntry):
    parameters: list[FuncParam]
    body: ast_nodes.BlockStatement
    ret_type: Type

    @property
    def value(self) -> None:
        return None


@dataclass(slots=True)
class TypeEntry(BaseEntry):
    type: Type

    @property
    def value(self) -> Type:
        return self.type


ScopeEntry = VarEntry | FuncEntry | TypeEntry


@dataclass(slots=True)
class Scope:
    store: dict[str, ScopeEntry] = field(default_factory=dict)
    outer_scope: Scope | None = None

    @classmethod
    def from_outer(cls, outer: Scope) -> Scope:
        """Creates scope from already existing."""
        scope = cls()
        scope.outer_scope = outer
        return scope

    def get(self, name: str, default: T = None) -> ScopeEntry | T:
        """Return the value for name if name is in the scope, else default."""
        if (res := self.store.get(name, default)) != default or self.outer_scope is None:
            return res
        return self.outer_scope.get(name, default)

    def __getitem__(self, name: str) -> ScopeEntry:
        if (res := self.get(name)) is not None:
            return res
        raise KeyError(f"No entry in scope for name `{name}`.")

    def __setitem__(self, name: str, value: ScopeEntry) -> None:
        self.store[name] = value

    def __delitem__(self, name: str) -> None:
        del self.store[name]