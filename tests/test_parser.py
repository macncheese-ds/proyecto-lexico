import importlib.util

spec = importlib.util.spec_from_file_location('lexical_mod','c:/Users/mm131/Downloads/lexicaal.py')
lex = importlib.util.module_from_spec(spec)
spec.loader.exec_module(lex)

Lexer = lex.Lexer
Parser = lex.Parser
KEYWORDS = lex.KEYWORDS
TOKEN_TYPE = lex.TOKEN_TYPE


def tokenize_and_parse(code):
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    return parser.parse()


def test_parse_module_import():
    code = 'module prueba; import std.io;'
    ast = tokenize_and_parse(code)
    assert ast is not None


def test_parse_simple_function():
    code = '''module M;
fn f() { return; }
'''
    ast = tokenize_and_parse(code)
    assert ast is not None
