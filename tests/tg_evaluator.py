"""Test group evaluator."""
# pylint: disable=redefined-outer-name
import pytest
from result import is_ok

from src.ast import ast_nodes
from src.evaluation.evaluator import Evaluator
from src.evaluation.std_lib import STD_LIB
from src.evaluation.values import consts, value_types
from src.evaluation.values.scope import FuncEntry, Scope, VarEntry
from src.evaluation.values.value_base import Value
from src.parser.interfaces import IParser
from src.parser.types import Operator
from tests.mock.parser_mock import ParserMock
from tests.static.eval_tests_data import (
    SINGLE_VALID_ASSIGN_AND_EXPECTED,
    SINGLE_VALID_COMPARISON_AND_EXPECTED,
    SINGLE_VALID_FUNC_CALL_AND_EXPECTED,
    SINGLE_VALID_FUNC_DEF_AND_EXPECTED,
    SINGLE_VALID_IF_AND_EXPECTED,
    SINGLE_VALID_INFIX_OPERATION_AND_EXPECTED,
    SINGLE_VALID_LET_AND_EXPECTED,
    SINGLE_VALID_WHILE_AND_EXPECTED,
)
from tests.utils.payloads import (
    ExpectedChangedVariableValue,
    ExpectedEvaluatedAssignment,
    ExpectedEvaluatedFuncCall,
    ExpectedEvaluatedFunction,
    ExpectedEvaluatedLet,
)
from tests.utils.types import YieldFixture


@pytest.fixture
def parser_mock(request: pytest.FixtureRequest) -> YieldFixture[IParser]:
    """Creates parser mock object and provides statements from request to it."""
    parser = ParserMock()
    parser.set_data(request.param)
    yield parser


@pytest.fixture
def scope() -> YieldFixture[Scope]:
    """Provides scope for evaluator."""
    yield STD_LIB


@pytest.fixture
def evaluator(scope: Scope) -> YieldFixture[Evaluator]:
    """Creates evaluator with required scope."""
    print(scope)
    yield Evaluator(scope)


@pytest.fixture
def ok_eval_res(parser_mock: IParser, evaluator: Evaluator) -> YieldFixture[Value]:
    """Gets ok value from evaluation result with parser mock."""
    print(evaluator.scope)
    result = evaluator.evaluate(parser_mock)
    assert is_ok(result), "Evaluation unexpetedly failed."
    yield result.ok_value


class TestEvaluatorTg:
    """Test group for evaluator."""

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        [
            ([ast_nodes.IntegerLiteral(5)], 5),
            ([ast_nodes.IntegerLiteral(10)], 10),
        ],
        indirect=["parser_mock"],
    )
    def test_eval_valid_integer_expression(self, ok_eval_res: Value, expected: int) -> None:
        """
        Tests evaluation of program with one integer literal.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is of type Integer.
        Assert: Returned value has value_type of ValueType.INT.
        Assert: Returned value has expected value.
        """
        assert isinstance(ok_eval_res, value_types.Int), "Result is of invalid type."
        assert ok_eval_res.value == expected, "Invalid result value."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        [
            ([ast_nodes.FloatLiteral(5.0)], 5.0),
            ([ast_nodes.FloatLiteral(10.5)], 10.5),
        ],
        indirect=["parser_mock"],
    )
    def test_eval_valid_float_expression(self, ok_eval_res: Value, expected: float) -> None:
        """
        Tests evaluation of program with one float literal.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is of type Float.
        Assert: Returned value has value_type of ValueType.FLOAT.
        Assert: Returned value has expected value.
        """
        assert isinstance(ok_eval_res, value_types.Float), "Evaluated is of invalid type."
        assert ok_eval_res.value == expected, "Invalid evaluated value."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        [
            (
                [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.IntegerLiteral(5))],
                value_types.Int(-5),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.IntegerLiteral(0))],
                value_types.Int(0),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.FloatLiteral(0.3))],
                value_types.Float(-0.3),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.FloatLiteral(0.0))],
                value_types.Float(0.0),
            ),
        ],
        indirect=["parser_mock"],
    )
    def test_eval_valid_unary_minus(self, ok_eval_res: Value, expected: Value) -> None:
        """
        Tests evaluation of program with one valid unary minus operation.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is of expected type.
        Assert: Returned value is equal to expected.
        """
        assert isinstance(ok_eval_res, type(expected)), "Evaluated is of invalid type."
        assert ok_eval_res == expected, "Invalid evaluated."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        [
            (
                [ast_nodes.PrefixOperation(Operator.PLUS, ast_nodes.IntegerLiteral(5))],
                value_types.Int(5),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.PLUS, ast_nodes.IntegerLiteral(0))],
                value_types.Int(0),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.PLUS, ast_nodes.FloatLiteral(0.3))],
                value_types.Float(0.3),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.PLUS, ast_nodes.FloatLiteral(0.0))],
                value_types.Float(0.0),
            ),
        ],
        indirect=["parser_mock"],
    )
    def test_eval_valid_unary_plus(self, ok_eval_res: Value, expected: Value) -> None:
        """
        Tests evaluation of program with one valid unary plus operation.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is of expected type.
        Assert: Returned value is equal to expected.
        """
        assert isinstance(ok_eval_res, type(expected)), "Evaluated is of invalid type."
        assert ok_eval_res == expected, "Invalid evaluated."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        SINGLE_VALID_INFIX_OPERATION_AND_EXPECTED,
        indirect=["parser_mock"],
    )
    def test_eval_valid_infix_operation(self, ok_eval_res: Value, expected: Value) -> None:
        """
        Tests evaluation of program with one valid infix operation.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is of expected type.
        Assert: Returned value is equal to expected.
        """
        assert isinstance(ok_eval_res, type(expected)), "Evaluated is of invalid type."
        assert ok_eval_res == expected, "Invalid evaluated."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        SINGLE_VALID_COMPARISON_AND_EXPECTED,
        indirect=["parser_mock"],
    )
    def test_eval_valid_comparison(self, ok_eval_res: Value, expected: consts.TrueFalse) -> None:
        """
        Tests evaluation of program with one valid comparison.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is of expected type.
        Assert: Returned value is equal to expected.
        Assert: Correct constant is used.
        """
        assert isinstance(ok_eval_res, type(expected.value)), "Evaluated is of invalid type."
        assert ok_eval_res == expected.value, "Invalid evaluated."
        assert ok_eval_res is expected.value, "Invalid constant."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        SINGLE_VALID_LET_AND_EXPECTED,
        indirect=["parser_mock"],
    )
    def test_eval_valid_let_stmt(
        self, ok_eval_res: Value, evaluator: Evaluator, expected: ExpectedEvaluatedLet
    ) -> None:
        """
        Tests evaluation of program with one valid let statement.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is NoEffect.
        Assert: Returned value is a constant NO_EFFECT.
        Assert: Evaluator's scope contains entry for expected name.
        Assert: Entry is VarEntry.
        Assert: Entry has expected mutability.
        Assert: Entry has expected type.
        Assert: Entry has expected variable value.
        Assert: Entry variable value is of entry type.
        Assert: Entry value is entry variable value.
        """
        assert isinstance(ok_eval_res, value_types.NoEffect), "Evaluated is of invalid type."
        assert ok_eval_res is consts.NO_EFFECT, "Invalid evaluated."
        stored_entry = evaluator.scope.get(expected.name)
        assert stored_entry is not None, "Value was not stored."
        assert isinstance(stored_entry, VarEntry), "Value was stored as a wrond entry type."
        assert stored_entry.is_mut == expected.is_mut, "Invalid mutability."
        assert stored_entry.type_value.value == expected.type_, "Invalid variable type."
        assert stored_entry.var_value == expected.value, "Invalid stored value."
        assert isinstance(
            stored_entry.var_value, stored_entry.type_value.value
        ), "Variable value is of invalid type."
        assert stored_entry.var_value is stored_entry.var_value, "Invalid entry value."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        SINGLE_VALID_FUNC_DEF_AND_EXPECTED,
        indirect=["parser_mock"],
    )
    def test_eval_valid_func_stmt(
        self, ok_eval_res: Value, evaluator: Evaluator, expected: ExpectedEvaluatedFunction
    ) -> None:
        """
        Tests evaluation of program with one valid let statement.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is NoEffect.
        Assert: Returned value is a constant NO_EFFECT.
        Assert: Evaluator's scope contains entry for expected name.
        Assert: Entry value is None.
        Assert: Entry is FuncEntry.
        Assert: Entry has expected return type.
        Assert: Entry has expected body statements.
        Assert: Entry has expected number of arguments.
        For: Each parameter in entry.
            Assert: Parameter name is equal to expected.
            Assert: Parameter type is equal to expected.
            Assert: Parameter default value is equal to expected.
            If parameter has default value:
                Assert: Parameter default value is of parameter type.
        """
        assert isinstance(ok_eval_res, value_types.NoEffect), "Evaluated is of invalid type."
        assert ok_eval_res is consts.NO_EFFECT, "Invalid evaluated."
        stored_entry = evaluator.scope.get(expected.name)
        assert stored_entry is not None, "Value was not stored."
        assert stored_entry.value is None, "Invalid entry value."
        assert isinstance(stored_entry, FuncEntry), "Value was stored as a wrond entry type."
        assert stored_entry.ret_type.value == expected.ret_type, "Invalid return type.."
        assert stored_entry.body.statements == expected.body, "Invalid body."
        assert len(stored_entry.parameters) == len(expected.params)
        for param, eparam in zip(stored_entry.parameters, expected.params, strict=True):
            assert param.name == eparam.name, "Invalid param name."
            assert param.type_value.value == eparam.type_, "Invalid param type"
            assert param.default_value == eparam.default, "Invalid param default value."
            if param.default_value is not None:
                assert isinstance(
                    param.default_value, param.type_value.value
                ), "Param default value is of invalid type."

    @pytest.mark.parametrize(
        ("parser_mock", "scope", "expected"),
        SINGLE_VALID_FUNC_CALL_AND_EXPECTED,
        indirect=["parser_mock"],
    )
    def test_eval_valid_func_call(
        self, ok_eval_res: Value, evaluator: Evaluator, expected: ExpectedEvaluatedFuncCall
    ) -> None:
        """
        Tests evaluation of program with one valid let statement.

        Arrange: Create scope with called function.
        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is of expected type.
        Assert: Returned value is equal to expected.
        Assert: Returned value is of type of FuncEntry in Evaluator's scope.
        """
        assert isinstance(
            ok_eval_res, type(expected.returned_value)
        ), "Evaluated is of invalid type."
        assert ok_eval_res == expected.returned_value, "Evaluated is of invalid value."
        stored_entry: FuncEntry = evaluator.scope[expected.func_name]  # type: ignore
        assert isinstance(
            ok_eval_res, stored_entry.ret_type.value
        ), "Evaluated type is not matched with function return type."

    @pytest.mark.parametrize(
        ("parser_mock", "scope", "expected"),
        SINGLE_VALID_ASSIGN_AND_EXPECTED,
        indirect=["parser_mock"],
    )
    def test_eval_valid_assignment(
        self, ok_eval_res: Value, evaluator: Evaluator, expected: ExpectedEvaluatedAssignment
    ) -> None:
        """
        Tests evaluation of program with one valid let statement.

        Arrange: Create scope with variable defined.
        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Evaluator's scope contains entry for expected variable name.
        Assert: Entry value is not None.
        Assert: Entry is VarEntry.
        Assert: Entry variable value is the returned value.
        Assert: Entry variable value is equal to expected.
        Assert: Entry has expected body statements.
        Assert: Entry has expected number of arguments.
        """
        variable_entry = evaluator.scope.get(expected.var_name)
        assert variable_entry is not None, "Value was not stored."
        assert variable_entry.value is not None, "Invalid entry value."
        assert isinstance(variable_entry, VarEntry), "Value was stored as a wrond entry type."
        assert ok_eval_res is variable_entry.var_value, "Returned and stored value differ."
        assert ok_eval_res == expected.new_value, "Invalid new value."
        assert isinstance(
            ok_eval_res, variable_entry.type_value.value
        ), "New value is of wrong type."

    @pytest.mark.parametrize(
        ("parser_mock", "scope", "expected"),
        SINGLE_VALID_IF_AND_EXPECTED,
        indirect=["parser_mock"],
    )
    def test_eval_valid_if(
        self, ok_eval_res: Value, evaluator: Evaluator, expected: ExpectedChangedVariableValue
    ) -> None:
        """
        Tests evaluation of program with one valid let statement.

        Arrange: Create scope with used variable defined.
        Arrange: Provide statements to Parser Mock, so that used variable
        is set to a different value in each if-else block.

        Act: Evaluate program from parser.
        Assert: Returned value is NO_EFFECT constant.
        Assert: No error returned.
        Assert: Evaluator's scope contains entry for expected variable name.
        Assert: Entry value is not None.
        Assert: Entry is VarEntry.
        Assert: Entry variable value is equal to expected.
        """
        assert ok_eval_res is consts.NO_EFFECT, "Invalid retuned value."
        variable_entry = evaluator.scope.get(expected.var_name)
        assert variable_entry is not None, "Value was not stored."
        assert variable_entry.value is not None, "Invalid entry value."
        assert isinstance(variable_entry, VarEntry), "Value was stored as a wrond entry type."
        assert variable_entry.var_value == expected.new_value, "Invalid value."

    @pytest.mark.parametrize(
        ("parser_mock", "scope", "expected"),
        SINGLE_VALID_WHILE_AND_EXPECTED,
        indirect=["parser_mock"],
    )
    def test_eval_valid_while(
        self, ok_eval_res: Value, evaluator: Evaluator, expected: ExpectedChangedVariableValue
    ) -> None:
        """
        Tests evaluation of program with one valid let statement.

        Arrange: Create scope with used variable defined.
        Arrange: Provide statements to Parser Mock, so that used variable
        is changed in the while

        Act: Evaluate program from parser.
        Assert: Returned value is NO_EFFECT constant.
        Assert: No error returned.
        Assert: Evaluator's scope contains entry for expected variable name.
        Assert: Entry value is not None.
        Assert: Entry is VarEntry.
        Assert: Entry variable value is equal to expected.
        """
        assert ok_eval_res is consts.NO_EFFECT, "Invalid retuned value."
        variable_entry = evaluator.scope.get(expected.var_name)
        assert variable_entry is not None, "Value was not stored."
        assert variable_entry.value is not None, "Invalid entry value."
        assert isinstance(variable_entry, VarEntry), "Value was stored as a wrond entry type."
        assert variable_entry.var_value == expected.new_value, "Invalid value."
