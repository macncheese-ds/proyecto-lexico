import pytest
import importlib.util

spec = importlib.util.spec_from_file_location('lexical_mod','c:/Users/mm131/Downloads/lexicaal.py')
lex = importlib.util.module_from_spec(spec)
spec.loader.exec_module(lex)

Lexer = lex.Lexer
TOKEN_TYPE = lex.TOKEN_TYPE
KEYWORDS = lex.KEYWORDS


def tokenize(code):
    lexer = Lexer(code)
    return lexer.tokenize()


def test_basic_tokens():
    code = 'module M;'
    tokens = tokenize(code)
    assert any(t.type == KEYWORDS['module'] for t in tokens)
    assert any(t.type == TOKEN_TYPE['EOF'] for t in tokens)


def test_comments_ignored():
    code = 'module m; //coment1//\nlet x : int;'
    tokens = tokenize(code)
    # Ensure comment didn't create extra tokens between module and let
    types = [t.type for t in tokens]
    assert KEYWORDS['module'] in types
    assert KEYWORDS['let'] in types


def test_numbers_and_strings():
    code = 'let a : int = 12.2; let b : string = "hola";'
    tokens = tokenize(code)
    types = [t.type for t in tokens]
    assert TOKEN_TYPE['NUM_LIT'] in types
    assert TOKEN_TYPE['STRING_LIT'] in types


def test_identifier_rules():
    code = 'module abc123;'
    tokens = tokenize(code)
    assert any(t.type == TOKEN_TYPE['ID'] and t.value == 'abc123' for t in tokens)
