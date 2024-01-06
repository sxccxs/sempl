"""Module for built-in functions and their entries."""
from typing import Any

from result import Ok

from src.evaluation.values import consts, value_types
from src.evaluation.values.scope import BuiltInFuncEntry, FuncParam
from src.evaluation.values.value_base import SequenceValue, ValuedValue


def _print(value: ValuedValue[Any], end: value_types.String) -> Ok[value_types.Singularity]:
    print(value.value, end=end.value)
    return Ok(consts.SINGULARITY)


def _input(value: value_types.String) -> Ok[value_types.String]:
    return Ok(value_types.String(input(value.value)))


def _len(value: SequenceValue[Any]) -> Ok[value_types.Int]:
    return Ok(value_types.Int(len(value.value)))


PRINT_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("value", value_types.Type(ValuedValue), None),
        FuncParam("end", value_types.Type(value_types.String), value_types.String("\n")),
    ],
    value_types.Type(value_types.Singularity),
    _print,
)

INPUT_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("value", value_types.Type(value_types.String), value_types.String("")),
    ],
    value_types.Type(value_types.String),
    _input,
)

LEN_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("value", value_types.Type(SequenceValue), None),
    ],
    value_types.Type(value_types.Int),
    _len,
)
