from enum import Enum
from typing import Any


def enum_contains(enum_type: type[Enum], value: Any) -> bool:
    """
    Checks if value is present in enum as value.

    Args:
        enum_type (type[Enum]): Enum type.
        value (Any): value to check.
    """
    return value in (i.value for i in enum_type)
