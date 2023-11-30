import sys
from io import StringIO
from sys import stdin, stdout
from types import TracebackType
from typing import Self, TextIO, overload

from src.lexer.lexer import Lexer
from src.lexer.tokens import TokenType


class REPL:
    """Implements basic read-evaluate-print loop."""

    __slots__ = ("in_stream", "out_stream", "prompt_in", "prompt_out")

    def __init__(
        self,
        in_stream: TextIO = stdin,
        out_stream: TextIO = stdout,
        *,
        prompt_in: str = ">>> ",
        prompt_out: str = "<<< ",
    ) -> None:
        """
        Args:
            in_stream (TextIO, optional): Input stream. Defaults to stdin.
            out_stream (TextIO, optional): Output stram. Defaults to stdout.
            prompt_in (str, optional): Input prefix. Defaults to ">>> ".
            prompt_out (str, optional): Output prefix. Defaults to "<<< ".
        """
        self.in_stream = in_stream
        self.out_stream = out_stream
        self.prompt_in = prompt_in
        self.prompt_out = prompt_out

    def __enter__(self) -> Self:
        return REPL()

    @overload
    def __exit__(
        self,
        exc_type: type[BaseException],
        exc_val: BaseException,
        exc_tb: TracebackType,
    ) -> None:
        ...

    @overload
    def __exit__(
        self,
        exc_type: None,
        exc_val: None,
        exc_tb: None,
    ) -> None:
        ...

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        if exc_type is not KeyboardInterrupt:
            return
        self.out_stream.write("\n")
        self.out_stream.flush()
        sys.exit(0)

    def write(self, text: str) -> None:
        """Writes text with output prefix to output stream.

        Args:
            text (str): text to write.
        """
        self.out_stream.write(f"{self.prompt_out}{text}")

    def writeln(self, text: str) -> None:
        """Writes text with output prefix and a new line after to output stream.

        Args:
            text (str): text to write.
        """
        self.write(text)
        self.out_stream.write("\n")

    def flush(self) -> None:
        """Flushes output stream."""
        self.out_stream.flush()

    def readline(self) -> str:
        """Writes input prefix to output stream and reads line from input stream."""
        self.out_stream.write(self.prompt_in)
        self.out_stream.flush()
        return self.in_stream.readline()

    def run(self) -> None:
        """Starts read-evaluate-print loop."""
        self.writeln("REPL started.")
        while True:
            line_stream = StringIO(self.readline())
            lexer = Lexer(line_stream)
            while (token := lexer.next_token()).type != TokenType.EOF:
                self.writeln(str(token))
            break
