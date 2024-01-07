
# SEMPL

## Annotation
SEMPL is an iterpreted, staticly-typed programing language.

## Installation

### Poetry
Installing all dependencies:
```
poetry install
```
Instaling only main dependencies:
```
poetry install --only-root
```
Installing only developement dependencies:
```
poetry install --only dev
```
### Pip
Installing all dependencies:
```
python -m pip install -r requirements.txt

```

## Running

### REPL
To start REPl you can use python:
```
python ./sempl.py
```
or use predifined script
```
chmod +x ./sempl
./sempl
```

### File
To run code from file you can use python:
```
python ./sempl.py [filename]
```
or use predifined script
```
chmod +x ./sempl
./sempl [filename]
```

## Tests
Running all tests (requires dev dependencies):
```
pytest ${TEST_DIR}/tg*.py -vvv --md-report --md-report-output=./reports/report.md
```
(Needed pytest parameters can be used to selecet some tests.)