"""
Copyright (c) 2024 Hryhorii Biloshenko.
All Rights Reserved.

Sempl runner.
"""
import argparse
import pathlib
import sys

from result import Err, Ok

from src.evaluation.evaluator import Evaluator
from src.lexer.lexer import Lexer
from src.parser.parser import Parser
from src.repl.repl import REPL


def run_repl() -> None:
    """Runs REPL."""
    repl = REPL()
    repl.writeln("REPL started.")
    repl.run()


def run_file() -> None:
    """Runs code from file."""
    parser = argparse.ArgumentParser(description="Sempl interpretator", prog="sempl")
    parser.add_argument("file", type=pathlib.Path)
    args = parser.parse_args()
    code_file: pathlib.Path = args.file
    with code_file.open("r") as f:
        match Evaluator().evaluate(Parser(Lexer(f))):
            case Err(err):
                print(str(err))
            case Ok():
                pass


def main() -> None:
    """Main function."""
    func = run_repl if len(sys.argv) == 1 else run_file
    try:
        func()
    except KeyboardInterrupt:
        print("Keyboard Interrupt")
    except SystemExit:
        pass
    except BaseException:  # pylint: disable=broad-exception-caught
        # do not allow any python exception out.
        print("Unexpected error.")


if __name__ == "__main__":
    main()
