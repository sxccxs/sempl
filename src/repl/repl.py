"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

REPL.
"""
import sys
from io import StringIO
from typing import TextIO

from result import Err, Ok

from src.errors.error import Error
from src.evaluation.evaluator import Evaluator
from src.evaluation.values.value_types import NoEffect
from src.lexer.lexer import Lexer
from src.parser.parser import Parser
from src.repl.structures.payloads import BracesStorage


# pylint: disable=too-many-arguments
class REPL:
    """Implements basic read-evaluate-print loop."""

    __slots__ = ("evaluator", "in_stream", "out_stream", "prompt_in", "prompt_out", "prompt_block")

    def __init__(
        self,
        in_stream: TextIO = sys.stdin,
        out_stream: TextIO = sys.stdout,
        *,
        prompt_in: str = ">>> ",
        prompt_out: str = "<<< ",
        prompt_block: str = "... ",
    ) -> None:
        """
        Args:
            in_stream (TextIO, optional): Input stream. Defaults to sys.stdin.
            out_stream (TextIO, optional): Output stram. Defaults to sys.stdout.
            prompt_in (str, optional): Input prefix. Defaults to ">>> ".
            prompt_out (str, optional): Output prefix. Defaults to "<<< ".
            prompt_block (str, optional): Intput prefix in the multiline code block.
            Defaults to "<<< ".
        """
        self.evaluator = Evaluator()
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
        self.out_stream.write(f"{prefix}{text}")

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
        self.write("", prefix=prefix)
        self.out_stream.flush()
        return self.in_stream.readline()

    def run(self) -> None:
        """Runs read-evaluate-print loop."""
        try:
            self._run()
        except KeyboardInterrupt:
            self.writeln(prefix="")
            self.run()

    def _run(self) -> None:
        """Runs read-evaluate-print loop without error handling."""
        while True:
            text = self._read_valid_code_block()
            if text == "exit\n":
                return
            line_stream = StringIO(text)
            match self.evaluator.evaluate(Parser(Lexer(line_stream))):
                case Err(err):
                    self._print_error(err)
                case Ok(NoEffect()):
                    pass
                case Ok(value):
                    self.writeln(str(value))

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

    def _print_error(self, error: Error) -> None:
        self.writeln("-" * (50 + len(self.prompt_out)), prefix="")
        for line in str(error).split("\n"):
            self.writeln(line)
        self.writeln(prefix="")
