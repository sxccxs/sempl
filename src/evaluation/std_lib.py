"""Std library scope defenition."""
from src.evaluation.values import value_types
from src.evaluation.values.consts import SINGULARITY, TrueFalse
from src.evaluation.values.scope import Scope, TypeEntry, VarEntry

SINGULARITY_TYPE_ENTRY = TypeEntry(value_types.Type(value_types.Singularity))


STD_LIB = Scope(
    {
        "Int": TypeEntry(value_types.Type(value_types.Int)),
        "Float": TypeEntry(value_types.Type(value_types.Float)),
        "Bool": TypeEntry(value_types.Type(value_types.Bool)),
        "Singularity": SINGULARITY_TYPE_ENTRY,
        "OT": SINGULARITY_TYPE_ENTRY,
        "O": VarEntry(SINGULARITY, False, value_types.Type(value_types.Singularity)),
        "True": VarEntry(TrueFalse.TRUE.value, False, value_types.Type(value_types.Bool)),
        "False": VarEntry(TrueFalse.FALSE.value, False, value_types.Type(value_types.Bool)),
    }
)
