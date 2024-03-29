"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Results helper functions.
"""
from itertools import chain
from typing import Iterable, overload

from result import Err, Ok, Result

from src.errors.error import Error


@overload
def results_gather[T, E](*results_it: Result[T, E]) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        *results_it (Result[T, E]): Results to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


@overload
def results_gather[T, E](results: Iterable[Result[T, E]]) -> Result[list[T], E]:
    """
    Returns ok values of provided results as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        results (Iterable[Result[T, E]]): Results to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


@overload
def results_gather[T, E](*results_it: Iterable[Result[T, E]]) -> Result[list[T], E]:
    """
    Returns ok values of provided results iterables as a list, if all results were OK.
    Else returns first encountered Err result.
    Ok values order corresponds to the order of results.

    Args:
        *results_it (Iterable[Result[T, E]]): Results iterables to gather.

    Returns:
        Result[list[T], E]: Result of gathering.
    """


def results_gather[T, E](
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
def err_with_note[E: Error](err: E, note: str, template: str = "Error in {}.") -> Err[E]:
    """
    Creates full note from given template and note,
    adds it to the given Error and returns it as Err[Error].

    Args:
        err (E): Error.
        note (str): Note part.
        template (str, optional): Note template. Defaults to "Error in {}.".
    """


@overload
def err_with_note[E: Error](err: Err[E], note: str, template: str = "Error in {}.") -> Err[E]:
    """
    Creates full note from given template and note,
    adds it to the Error of the given Err[Error] and returns it as Err[Error].

    Args:
        err (Err[E]): Err[Error].
        note (str): Note part.
        template (str, optional): Note template. Defaults to "Error in {}.".
    """


def err_with_note[E: Error](err: Err[E] | E, note: str, template: str = "Error in {}.") -> Err[E]:
    """
    Creates full note from given template and note,
    adds it to the given Error or Err[Error] and returns it as Err[Error].

    Args:
        err (E | Err[E]): Error or Err[Error].
        note (str): Note part.
        template (str, optional): Note template. Defaults to "Error in {}.".
    """
    exc = err if not isinstance(err, Err) else err.err_value
    exc.add_note(template.format(note))
    return Err(exc)
