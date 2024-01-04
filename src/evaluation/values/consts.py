from __future__ import annotations

from enum import Enum

from src.evaluation.values import value_types


class TrueFalse(Enum):
    """The only two True and False constants"""

    TRUE = value_types.Boolean(True)
    FALSE = value_types.Boolean(False)

    @classmethod
    def from_bool(cls, value: bool) -> TrueFalse:
        """Gets value"""
        return TrueFalse.TRUE if value else TrueFalse.FALSE


SINGULARITY = value_types.Singularity()
NO_EFFECT = value_types.NoEffect()
