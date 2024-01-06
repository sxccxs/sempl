"""Std library scope defenition."""
from src.evaluation.builtin_funcs import INPUT_ENTRY, PRINT_ENTRY
from src.evaluation.values import value_types
from src.evaluation.values.consts import SINGULARITY
from src.evaluation.values.scope import Scope, TypeEntry, VarEntry

SINGULARITY_TYPE_ENTRY = TypeEntry(value_types.Type(value_types.Singularity))


STD_LIB = Scope(
    {
        "Int": TypeEntry(value_types.Type(value_types.Int)),
        "Float": TypeEntry(value_types.Type(value_types.Float)),
        "Bool": TypeEntry(value_types.Type(value_types.Bool)),
        "Str": TypeEntry(value_types.Type(value_types.String)),
        "Singularity": SINGULARITY_TYPE_ENTRY,
        "O": SINGULARITY_TYPE_ENTRY,
        "singularity": VarEntry(SINGULARITY, False, value_types.Type(value_types.Singularity)),
        "print": PRINT_ENTRY,
        "input": INPUT_ENTRY,
    }
)
