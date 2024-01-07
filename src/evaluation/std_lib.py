"""Std library scope defenition."""
from src.evaluation.builtin_funcs import (
    APPEND_ENTRY,
    FLOAT_ENTRY,
    INPUT_ENTRY,
    INT_ENTRY,
    LEN_ENTRY,
    PRINT_ENTRY,
    REMOVE_ENTRY,
)
from src.evaluation.values import value_types
from src.evaluation.values.consts import SINGULARITY
from src.evaluation.values.scope import Scope, TypeEntry, VarEntry

SINGULARITY_TYPE_ENTRY = TypeEntry(value_types.Type(value_types.Singularity))


STD_LIB = Scope(
    {
        value_types.Type.__name__: TypeEntry(value_types.Type(value_types.Type)),
        value_types.Int.__name__: TypeEntry(value_types.Type(value_types.Int)),
        value_types.Float.__name__: TypeEntry(value_types.Type(value_types.Float)),
        value_types.Bool.__name__: TypeEntry(value_types.Type(value_types.Bool)),
        value_types.Str.__name__: TypeEntry(value_types.Type(value_types.Str)),
        value_types.Arr.__name__: TypeEntry(value_types.Type(value_types.Arr)),
        "Singularity": SINGULARITY_TYPE_ENTRY,
        "O": SINGULARITY_TYPE_ENTRY,
        "singularity": VarEntry(SINGULARITY, False, value_types.Type(value_types.Singularity)),
        "print": PRINT_ENTRY,
        "input": INPUT_ENTRY,
        "len": LEN_ENTRY,
        "append": APPEND_ENTRY,
        "remove": REMOVE_ENTRY,
        "int": INT_ENTRY,
        "float": FLOAT_ENTRY,
    }
)
