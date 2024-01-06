from result import Ok

from src.evaluation.values import consts
from src.evaluation.values.scope import BuiltInFuncEntry, FuncParam
from src.evaluation.values.value_base import ValuedValue
from src.evaluation.values.value_types import Singularity, String, Type


def _print(value: ValuedValue, end: String) -> Ok[Singularity]:
    print(value.value, end=end.value)
    return Ok(consts.SINGULARITY)


def _input(value: String) -> Ok[String]:
    return Ok(String(input(value.value)))


PRINT_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("value", Type(ValuedValue), None),
        FuncParam("end", Type(String), String("\n")),
    ],
    Type(Singularity),
    _print,
)

INPUT_ENTRY = BuiltInFuncEntry(
    [
        FuncParam("value", Type(String), String("")),
    ],
    Type(String),
    _input,
)
