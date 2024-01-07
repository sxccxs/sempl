"Sempl main"
import argparse
import pathlib
import sys

from result import Err, Ok

from src.evaluation.evaluator import Evaluator
from src.lexer.lexer import Lexer
from src.parser.parser import Parser
from src.repl.repl import REPL

if len(sys.argv) == 1:
    repl = REPL()
    repl.writeln("REPL started.")
    repl.run()
    sys.exit(0)


parser = argparse.ArgumentParser(description="Sempl interpretator", prog="sempl")
parser.add_argument("file", type=pathlib.Path)
args = parser.parse_args()
with open(args.file, "r", encoding="utf-8") as f:
    try:
        match Evaluator().evaluate(Parser(Lexer(f))):
            case Err(err):
                print(str(err))
            case Ok(value):
                pass
    except KeyboardInterrupt:
        print("Keyboard Interrupt")
        sys.exit(0)
    except BaseException:  # pylint: disable=broad-exception-caught
        # do not allow any python exception out.
        print("Unexpected error.")
