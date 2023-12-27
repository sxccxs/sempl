# pylint: disable=not-an-iterable
# because pylint gives false error on iterating over union of iterables.
from typing import Iterable

from result import Err, Ok, Result, do

from src.ast import ast_nodes
from src.ast.abstract import Statement
from src.evaluation import consts, errors
from src.evaluation.errors import EvaluationError
from src.evaluation.values import value_types
from src.evaluation.values.value_base import Value
from src.helpers.result_helpers import results_gather
from src.parser.types import Operator


def evaluate(node: ast_nodes.ASTNode) -> Result[Value, EvaluationError]:
    """
    Evalueates given AST node to a Value.

    Args:
        node (ast_nodes.ASTNode): AST node.

    Returns:
        Value: Evaluated value.
    """
    match node:
        case ast_nodes.IntegerLiteral() as int_lit:
            return Ok(value_types.Integer(int_lit.value))
        case ast_nodes.FloatLiteral() as fl_lit:
            return Ok(value_types.Float(fl_lit.value))
        case ast_nodes.BooleanLiteral() as bool_lit:
            return Ok(consts.TrueFalse.from_bool(bool_lit.value).value)
        case ast_nodes.Program() as prog:
            return evaluate_statements(prog.statements)
        case ast_nodes.ExpressionStatement() as expr_stmt:
            return evaluate(expr_stmt.expression)
        case ast_nodes.PrefixOperation() as pre_op:
            return do(
                Ok(val)
                for operand in evaluate(pre_op.right)
                for val in evaluate_prefix_expression(pre_op.operator, operand)
            )
        case ast_nodes.InfixOperation() as in_op:
            return do(
                Ok(val)
                for left in evaluate(in_op.left)
                for right in evaluate(in_op.right)
                for val in evaluate_infix_expression(left, in_op.operator, right)
            )
        case _:
            return Err(errors.UnsuportedNodeError(node))


def evaluate_statements(stmts: Iterable[Statement]) -> Result[Value, EvaluationError]:
    """
    Evaluates all provided statements.

    Args:
        stmts (Iterable[Statement]): Statements to evaluate.

    Returns:
        Result[Value, EvaluationError]: Value of last evaluated statement or first occured error.
    """
    results: list[Result[Value, EvaluationError]] = [evaluate(stmt) for stmt in stmts]
    match results_gather(results):
        case Err() as err:
            return err
        case Ok(vals):
            return Ok(vals[-1])


def evaluate_prefix_expression(
    operator: Operator, operand: Value
) -> Result[Value, EvaluationError]:
    """Evaluates given prefix operation."""
    match operator:
        case Operator.MINUS:
            return evaluate_prefix_minus_expression(operand)
        case Operator.PLUS:
            return evaluate_prefix_plus_expression(operand)
        case _:
            return Err(errors.UnsuportedPrefixOperator(operator))


def evaluate_prefix_minus_expression(operand: Value) -> Result[Value, EvaluationError]:
    """Applies unary minus on given operand if possible."""
    match operand:
        case value_types.Integer() as i:
            return Ok(value_types.Integer(-1 * i.value))
        case value_types.Float() as f:
            return Ok(value_types.Float(-1 * f.value))
        case _:
            return Err(errors.UnsuportedPrefixOperation(Operator.MINUS, operand))


def evaluate_prefix_plus_expression(operand: Value) -> Result[Value, EvaluationError]:
    """Applies unary plus on given operand if possible."""
    match operand:
        case value_types.Integer() as i:
            return Ok(i)
        case value_types.Float() as f:
            return Ok(f)
        case _:
            return Err(errors.UnsuportedPrefixOperation(Operator.PLUS, operand))


def evaluate_infix_expression(
    left_operand: Value, operator: Operator, right_operand: Value
) -> Result[Value, EvaluationError]:
    """Applies given binary operator on given operands if possible."""
    match (left_operand, right_operand):
        case (value_types.Integer(), value_types.Integer()):
            return evaluate_integer_infix_expression(left_operand, operator, right_operand)
        case (
            value_types.Integer() | value_types.Float(),
            value_types.Integer() | value_types.Float(),
        ):
            return evaluate_float_infix_expression(left_operand, operator, right_operand)
        case _:
            return Err(errors.UnsuportedInfixOperation(left_operand, operator, right_operand))


def evaluate_integer_infix_expression(
    left_operand: value_types.Integer, operator: Operator, right_operand: value_types.Integer
) -> Result[value_types.Integer, EvaluationError]:
    """
    Applies given binary operator on given integer operands,
    with result of an integer value if possible.
    """
    left = left_operand.value
    right = right_operand.value
    match operator:
        case Operator.PLUS:
            return Ok(value_types.Integer(left + right))
        case Operator.MINUS:
            return Ok(value_types.Integer(left - right))
        case Operator.MULT:
            return Ok(value_types.Integer(left * right))
        case Operator.DIV:
            return Ok(value_types.Integer(left // right))
        case _:
            return Err(errors.UnsuportedInfixOperation(left_operand, operator, right_operand))


def evaluate_float_infix_expression(
    left_operand: value_types.Float | value_types.Integer,
    operator: Operator,
    right_operand: value_types.Float | value_types.Integer,
) -> Result[value_types.Float, EvaluationError]:
    """
    Applies given binary operator on given integer/float operands,
    with result of a float value if possible.
    """
    left = left_operand.value
    right = right_operand.value
    match operator:
        case Operator.PLUS:
            return Ok(value_types.Float(left + right))
        case Operator.MINUS:
            return Ok(value_types.Float(left - right))
        case Operator.MULT:
            return Ok(value_types.Float(left * right))
        case Operator.DIV:
            return Ok(value_types.Float(left / right))
        case _:
            return Err(errors.UnsuportedInfixOperation(left_operand, operator, right_operand))
