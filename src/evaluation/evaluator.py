from result import Err, Ok, Result

from src.evaluation import sub_evaluators
from src.evaluation.errors import EvaluationError
from src.evaluation.values import value_types
from src.evaluation.values.types import Scope, TypeEntry
from src.evaluation.values.value_base import Value
from src.parser.interfaces import IParser

STD_LIB = Scope(
    {
        "int": TypeEntry(value_types.Type(value_types.Integer)),
        "float": TypeEntry(value_types.Type(value_types.Float)),
    }
)


class Evaluator:
    __slots__ = ("parser", "scope")

    def __init__(self, parser: IParser) -> None:
        self.parser = parser
        self.scope = Scope.from_scope(STD_LIB)

    def evaluate(self) -> Result[Value, EvaluationError]:
        """Evaluates program from saved parser with std lib."""
        match self.parser.parse_program():
            case Err(err):
                return Err(EvaluationError(err))
            case Ok(value):
                program = value
        return sub_evaluators.evaluate(program, self.scope)
