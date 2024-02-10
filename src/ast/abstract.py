"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Base nodes for Abstract Syntax Tree.
"""
# pylint: disable=too-few-public-methods
from abc import ABC, abstractmethod


class ASTNode(ABC):
    """Base class for AST."""

    @abstractmethod
    def __str__(self) -> str:
        ...


class Statement(ASTNode, ABC):
    """Base class for Statements in AST."""


class Expression(ASTNode, ABC):
    """Base class for Expressions in AST."""
