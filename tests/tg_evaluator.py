import pytest

from src.ast import ast_nodes
from src.evaluation.values import values_types
from src.evaluation.values.values_base import Value, ValueType
from src.parser.types import Operator


class TestEvaluatorTg:
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
        assert isinstance(ok_eval_res, values_types.Integer), "Result is of invalid type."
        assert ok_eval_res.value_type == ValueType.INT, "Invalid result's type."
        assert ok_eval_res.value == expected, "Invalid result value."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        [
            ([ast_nodes.BooleanLiteral(True)], True),
            ([ast_nodes.BooleanLiteral(False)], False),
        ],
        indirect=["parser_mock"],
    )
    def test_eval_valid_boolean_expression(self, ok_eval_res: Value, expected: bool) -> None:
        """
        Tests evaluation of program with one boolean literal.

        Arrange: Provide statements to Parser Mock.

        Act: Evaluate program from parser.
        Assert: No error returned.
        Assert: Returned value is of type Boolean.
        Assert: Returned value has value_type of ValueType.BOOL.
        Assert: Returned value has expected value.
        """
        assert isinstance(ok_eval_res, values_types.Boolean), "Evaluated is of invalid type."
        assert ok_eval_res.value_type == ValueType.BOOL, "Invalid evaluated's type."
        assert ok_eval_res.value == expected, "Invalid evaluated value."

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
        assert isinstance(ok_eval_res, values_types.Float), "Evaluated is of invalid type."
        assert ok_eval_res.value_type == ValueType.FLOAT, "Invalid evaluated's type."
        assert ok_eval_res.value == expected, "Invalid evaluated value."

    @pytest.mark.parametrize(
        ("parser_mock", "expected"),
        [
            (
                [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.IntegerLiteral(5))],
                values_types.Integer(-5),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.IntegerLiteral(0))],
                values_types.Integer(0),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.FloatLiteral(0.3))],
                values_types.Float(-0.3),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.MINUS, ast_nodes.FloatLiteral(0.0))],
                values_types.Float(0.0),
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
                values_types.Integer(5),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.PLUS, ast_nodes.IntegerLiteral(0))],
                values_types.Integer(0),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.PLUS, ast_nodes.FloatLiteral(0.3))],
                values_types.Float(0.3),
            ),
            (
                [ast_nodes.PrefixOperation(Operator.PLUS, ast_nodes.FloatLiteral(0.0))],
                values_types.Float(0.0),
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
