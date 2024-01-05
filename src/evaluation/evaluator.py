from result import Err, Ok, Result

from src.evaluation import sub_evaluators
from src.evaluation.errors import EvaluationError
from src.evaluation.values import value_types
from src.evaluation.values.consts import SINGULARITY
from src.evaluation.values.scope import Scope, TypeEntry, VarEntry
from src.evaluation.values.value_base import Value
from src.parser.errors import ParsingError
from src.parser.interfaces import IParser

STD_LIB = Scope(
    {
        "int": TypeEntry(value_types.Type(value_types.Integer)),
        "float": TypeEntry(value_types.Type(value_types.Float)),
        "Singularity": TypeEntry(value_types.Type(value_types.Singularity)),
        "singularity": VarEntry(SINGULARITY, False, value_types.Type(value_types.Singularity)),
    }
)


class Evaluator:
    __slots__ = ("parser", "scope")

    def __init__(self, parser: IParser, scope: Scope = STD_LIB) -> None:
        self.parser = parser
        self.scope = Scope(scope.store.copy())

    def evaluate(self) -> Result[Value, EvaluationError | ParsingError]:
        """Evaluates program from saved parser with std lib."""
        match self.parser.parse_program():
            case Err() as err:
                return err
            case Ok(value):
                program = value
        match sub_evaluators.evaluate(program, self.scope):
            case Err() as err:
                return err
            case Ok(value):
                return Ok(value)
