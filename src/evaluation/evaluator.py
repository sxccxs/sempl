"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

AST evaluator.
"""
from result import Err, Ok, Result

from src.errors.evaluator_errors import EvaluationError
from src.errors.parser_errors import ParsingError
from src.evaluation import sub_evaluators
from src.evaluation.std_lib import STD_LIB
from src.evaluation.values.scope import Scope
from src.evaluation.values.value_base import Value
from src.parser.interfaces import IParser


# pylint: disable=too-few-public-methods
class Evaluator:
    """AST evaluator."""

    __slots__ = ("scope",)

    def __init__(self, scope: Scope = STD_LIB) -> None:
        self.scope = Scope.from_outer(scope)

    def evaluate(self, parser: IParser) -> Result[Value, EvaluationError | ParsingError]:
        """Evaluates program from saved parser with std lib."""
        match parser.parse_program():
            case Err() as err:
                return err
            case Ok(value):
                program = value
        match sub_evaluators.evaluate(program, self.scope):
            case Err() as err:
                return err
            case Ok(value):
                return Ok(value)
