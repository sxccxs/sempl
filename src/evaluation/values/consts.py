"""Constant Values - ones, for which only one object instance is enough."""
from __future__ import annotations

from enum import Enum

from src.evaluation.values import value_types


class TrueFalse(Enum):
    """The only two True and False constants"""

    TRUE = value_types.Bool(True)
    FALSE = value_types.Bool(False)

    @classmethod
    def from_bool(cls, value: bool) -> TrueFalse:
        """Gets value"""
        return TrueFalse.TRUE if value else TrueFalse.FALSE


SINGULARITY = value_types.Singularity()  # Constant for singularity object
NO_EFFECT = value_types.NoEffect()  # Constant for no effect object
