"""Enum helper functions."""
from enum import Enum


def enum_contains(enum_type: type[Enum], value: object) -> bool:
    """
    Checks if value is present in enum as value.

    Args:
        enum_type (type[Enum]): Enum type.
        value (object): value to check.
    """
    return value in (i.value for i in enum_type)
