
# SEMPL

## Annotation
SEMPL is an iterpreted, staticly-typed programing language.

## Installation

### Poetry
Installing all dependencies:
```
poetry install --no-root
```
Instaling only main dependencies:
```
poetry install --only main
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
or use predefined script
```
chmod +x ./sempl
./sempl
```

### File
To run code from file you can use python:
```
python ./sempl.py [filename]
```
or use predefined script
```
chmod +x ./sempl
./sempl [filename]
```

## Tests
Running all tests (requires dev dependencies):
```
pytest ./tests/tg*.py -vvv --md-report --md-report-output=./reports/report.md
```
(Needed pytest parameters can be used to select some tests.)

## Copyright
Copyright (c) 2024 Hryhorii Biloshenko - All Rights Reserved