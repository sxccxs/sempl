"""Module for built-in functions and their entries."""
from typing import Any

from result import Err, Ok, Result

from src.errors.evaluator_errors import BuiltInError, EvaluationError
from src.evaluation.values import consts, value_types
from src.evaluation.values.scope import BuiltInFuncEntry, FuncParam
from src.evaluation.values.value_base import SequenceValue, Value, ValuedValue


def _print(value: ValuedValue[Any], end: value_types.Str) -> Ok[value_types.Singularity]:
    print(value.value, end=end.value)
    return Ok(consts.SINGULARITY)


def _input(prompt: value_types.Str) -> Ok[value_types.Str]:
    return Ok(value_types.Str(input(prompt.value)))


def _len(seq: SequenceValue[Any]) -> Ok[value_types.Int]:
    return Ok(value_types.Int(len(seq.value)))


def _append(arr: value_types.Arr, value: Value) -> Ok[value_types.Singularity]:
    arr.value.append(value)
    return Ok(consts.SINGULARITY)


def _remove(
    arr: value_types.Arr, value: Value
) -> Result[value_types.Singularity, EvaluationError]:
    try:
        arr.value.remove(value)
    except ValueError:
        return Err(BuiltInError(f"Value `{value}` is not in array."))
    return Ok(consts.SINGULARITY)


PRINT_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("value", value_types.Type(ValuedValue), None),
        FuncParam("end", value_types.Type(value_types.Str), value_types.Str("\n")),
    ],
    value_types.Type(value_types.Singularity),
    _print,
)

INPUT_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("prompt", value_types.Type(value_types.Str), value_types.Str("")),
    ],
    value_types.Type(value_types.Str),
    _input,
)

LEN_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("seq", value_types.Type(SequenceValue), None),
    ],
    value_types.Type(value_types.Int),
    _len,
)

APPEND_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("arr", value_types.Type(value_types.Arr), None),
        FuncParam("value", value_types.Type(value_types.Value), None),
    ],
    value_types.Type(value_types.Singularity),
    _append,
)

REMOVE_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("arr", value_types.Type(value_types.Arr), None),
        FuncParam("value", value_types.Type(value_types.Value), None),
    ],
    value_types.Type(value_types.Singularity),
    _remove,
)
