"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Scope and related objects.
"""
# pylint: disable=too-few-public-methods
from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Callable, override

from result import Result

from src.ast import ast_nodes
from src.errors.evaluator_errors import EvaluationError
from src.evaluation.values.value_base import Value
from src.evaluation.values.value_types import Type


class BaseEntry(ABC):
    """Base class for scope entry."""

    @property
    @abstractmethod
    def value(self) -> Value | None:
        """Underlying value of an entry if any."""


@dataclass(slots=True)
class VarEntry(BaseEntry):
    """Scope entry for variable definition."""

    var_value: Value
    is_mut: bool
    type_value: Type

    @property
    @override
    def value(self) -> Value:
        return self.var_value


@dataclass(frozen=True)
class FuncParam:
    """Payload for function parameter in scope entry."""

    name: str
    type_value: Type
    default_value: Value | None


@dataclass(slots=True)
class BaseFuncEntry(BaseEntry, ABC):
    """Base scope entry for function definition."""

    parameters: list[FuncParam]
    ret_type: Type

    @property
    @override
    def value(self) -> None:
        return None


@dataclass(slots=True)
class FuncEntry(BaseFuncEntry):
    """Scope entry for function definition."""

    body: ast_nodes.BlockStatement


@dataclass(slots=True)
class BuiltInFuncEntry(BaseFuncEntry):
    """Scope entry for built-in function."""

    func: Callable[..., Result[Value, EvaluationError]]


@dataclass(slots=True)
class TypeEntry(BaseEntry):
    """Scope entry for type definition."""

    type: Type

    @property
    @override
    def value(self) -> Type:
        return self.type


# Allowed scope entries
ScopeEntry = VarEntry | BaseFuncEntry | TypeEntry


@dataclass(slots=True)
class Scope:
    """
    Object representing encloused scope.

    Args:
        store (dict[str, ScopeEntry], optional): Predefined entries in scope.
        Defaults to empty dict.
        outer_scope (Scope | None, optional): Outer scope of current scope. If provided,
        will not be modified. Defaults to None.
    """

    store: dict[str, ScopeEntry] = field(default_factory=dict)
    outer_scope: Scope | None = None

    @classmethod
    def from_outer(cls, outer: Scope) -> Scope:
        """Creates scope from already existing."""
        scope = cls()
        scope.outer_scope = outer
        return scope

    def get[T](self, name: str, default: T = None) -> ScopeEntry | T:
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
        del self.store[name]
