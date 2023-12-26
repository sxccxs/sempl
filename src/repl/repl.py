import sys
from io import StringIO
from types import TracebackType
from typing import Self, TextIO

from result import Err, Ok

from src.ast.ast_nodes import Program
from src.lexer.lexer import Lexer
from src.parser.parser import Parser
from src.repl.structures.payloads import BracesStorage


class REPL:
    """Implements basic read-evaluate-print loop."""

    __slots__ = ("in_stream", "out_stream", "prompt_in", "prompt_out", "prompt_block")

    def __init__(
        self,
        in_stream: TextIO = sys.stdin,
        out_stream: TextIO = sys.stdout,
        *,
        prompt_in: str = ">>>",
        prompt_out: str = "<<<",
        prompt_block: str = "...",
    ) -> None:
        """
        Args:
            in_stream (TextIO, optional): Input stream. Defaults to stdin.
            out_stream (TextIO, optional): Output stram. Defaults to stdout.
            prompt_in (str, optional): Input prefix. Defaults to ">>>".
            prompt_out (str, optional): Output prefix. Defaults to "<<<".
            prompt_block (str, optional): Intput prefix in the multiline code block.
            Defaults to "<<<".
        """
        self.in_stream = in_stream
        self.out_stream = out_stream
        self.prompt_in = prompt_in
        self.prompt_out = prompt_out
        self.prompt_block = prompt_block

    def write(self, text: str = "", *, prefix: str | None = None) -> None:
        """Writes text with output prefix to output stream.

        Args:
            text (str): text to write.
            prefix (str, optional): Prefix to write before the text.
            If None `prompt_out` is used. Defaults to None.
        """
        if prefix is None:
            prefix = self.prompt_out
        self.out_stream.write(f"{prefix} {text}")

    def writeln(self, text: str = "", *, prefix: str | None = None) -> None:
        """Writes text with output prefix and a new line after to output stream.

        Args:
            text (str): text to write.
            prefix (str, optional): Prefix to write before the text.
            If None `prompt_out` is used. Defaults to None.
        """
        self.write(text, prefix=prefix)
        self.out_stream.write("\n")

    def flush(self) -> None:
        """Flushes output stream."""
        self.out_stream.flush()

    def readline(self, *, prefix: str | None = None) -> str:
        """
        Writes input prefix to output stream and reads line from input stream.

        Args:
            prefix (str, optional): Prefix to write before the read.
            If None `prompt_in` is used. Defaults to None.
        """
        if prefix is None:
            prefix = self.prompt_in
        self.out_stream.write(prefix)
        self.out_stream.write(" ")
        self.out_stream.flush()
        return self.in_stream.readline()

    def run(self) -> None:
        """Starts read-evaluate-print loop."""
        self.writeln("REPL started.")
        while True:
            line_stream = StringIO(self._read_valid_code_block())
            lexer = Lexer(line_stream)
            parser = Parser(lexer)
            match parser.parse_program():
                case Err(err):
                    self.writeln(f"Unexpected error while parsing: {err}")
                case Ok(program):
                    self._write_program(program)

    def __enter__(self) -> Self:
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        """Exits program with 0 code if received `KeyboardInterupt` exception.
        Does nothing otherwise.
        """
        if exc_type is not KeyboardInterrupt:
            return

        self.out_stream.write("\n")
        self.out_stream.flush()
        sys.exit(0)

    def _read_valid_code_block(self) -> str:
        """Reads a block of code, such that all braces are closed."""
        ss = StringIO()
        storage = BracesStorage()
        line = self.readline()
        storage.add_line(line)
        ss.write(line)

        while not storage.all_closed():
            line = self.readline(prefix=self.prompt_block)
            storage.add_line(line)
            ss.write(line)

        return ss.getvalue()

    def _write_program(self, program: Program) -> None:
        self.writeln(f"{'-'*5}Parsed result{'-'*5}")
        for line in str(program).split("\n"):
            self.writeln(line)
        self.writeln()
        self.writeln(f"{'-'*5}Parsed result end{'-'*5}")
