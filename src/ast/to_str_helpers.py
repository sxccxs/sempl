from io import StringIO
from typing import Iterable


def sio_write_with_after_value(io: StringIO, *, after_value: str, values: Iterable[str]) -> None:
    """
    Writes given string values to given StringIO with provided after-value after each.

    Args:
        io (StringIO): _description_
        after_value (str): _description_
        values (Iterable[str]): _description_
    """
    for value in values:
        io.write(f"{value}{after_value}")


def sio_write_with_sep(io: StringIO, *, sep: str, values: Iterable[str]) -> None:
    """
    Writes given string values to given StringIO with provided separatorin between.

    Args:
        io (StringIO): String stream.
        sep (str): Separator value
        values (Iterable[str]): Values to write.
    """
    io.write(sep.join(str(value) for value in values))
