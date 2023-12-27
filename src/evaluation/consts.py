from __future__ import annotations

from enum import Enum

from src.evaluation.values import values_types


class TrueFalse(Enum):
    """The only two True and False constants"""

    TRUE = values_types.Boolean(True)
    FALSE = values_types.Boolean(False)

    @classmethod
    def from_bool(cls, value: bool) -> TrueFalse:
        """Gets value"""
        return TrueFalse.TRUE if value else TrueFalse.FALSE
