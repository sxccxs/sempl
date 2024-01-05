"""Results helper functions."""
from itertools import chain
from typing import Iterable, TypeVar, overload

from result import Err, Ok, Result

from src.errors.error import Error

T = TypeVar("T")
E = TypeVar("E")

ErrT = TypeVar("ErrT", bound=Error)


@overload
def results_gather(*results_it: Result[T, E]) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        *results (Result[T, E]): Results to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


@overload
def results_gather(*results_it: Iterable[Result[T, E]]) -> Result[list[T], E]:
    """
    Returns ok values of provided results iterables as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        *results_it (Iterable[Result[T, E]]): Results iterables to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


@overload
def results_gather(results: Iterable[Result[T, E]]) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        results (Iterable[Result[T, E]]): Results to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


def results_gather(
    results: Iterable[Result[T, E]] | Result[T, E],
    *results_it: Iterable[Result[T, E]] | Result[T, E],
) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.
    """
    results_iterable: Iterable[Result[T, E]]
    if isinstance(results, (Ok, Err)):
        results_iterable = (results, *results_it)  # type: ignore
    else:
        results_iterable = chain(results, *results_it)  # type: ignore

    values: list[T] = []
    for res in results_iterable:
        match res:
            case Err() as err:
                return err
            case Ok(value):
                values.append(value)
    return Ok(values)


@overload
def err_with_note(err: ErrT, note: str, template: str = "Error in {}.") -> Err[ErrT]:
    """
    Creates full note from given template and note,
    adds it to the given Error and returns it as Err[Error].

    Args:
        err (ErrT): Error.
        note (str): Note part.
        template (str, optional): Note template. Defaults to "Error in {}.".
    """


@overload
def err_with_note(err: Err[ErrT], note: str, template: str = "Error in {}.") -> Err[ErrT]:
    """
    Creates full note from given template and note,
    adds it to the Error of the given Err[Error] and returns it as Err[Error].

    Args:
        err (Err[ErrT]): Err[Error].
        note (str): Note part.
        template (str, optional): Note template. Defaults to "Error in {}.".
    """


def err_with_note(err: Err[ErrT] | ErrT, note: str, template: str = "Error in {}.") -> Err[ErrT]:
    """
    Creates full note from given template and note,
    adds it to the given Error or Err[Error] and returns it as Err[Error].

    Args:
        err (ErrT | Err[ErrT]): Error or Err[Error].
        note (str): Note part.
        template (str, optional): Note template. Defaults to "Error in {}.".
    """
    exc = err if not isinstance(err, Err) else err.err_value
    exc.add_note(template.format(note))
    return Err(exc)
