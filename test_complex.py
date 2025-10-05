from lexicaal import Lexer, Parser

code = '''module calculadora;

const PI: int = 3;

struct Punto {
    x: int;
    y: int;
};

fn suma(a: int, b: int) -> int {
    return a + b;
}

fn main() -> int {
    let resultado: int = suma(5, 10);
    if (resultado > 0) {
        return resultado;
    } else {
        return 0;
    }
}'''

try:
    print('=== ANÁLISIS LÉXICO ===')
    lexer = Lexer(code)
    tokens = lexer.tokenize()
    print(f'Tokens generados: {len(tokens)}')
    
    print('\n=== ANÁLISIS SINTÁCTICO ===')
    parser = Parser(tokens)
    ast = parser.parse()
    print('✓ Análisis sintáctico completado exitosamente')
    print(f'Estructura del programa: {ast["type"]}')
    print(f'Nodos principales: {len(ast["children"])}')
    
    print('\n=== ESTRUCTURA DEL AST ===')
    for i, child in enumerate(ast['children']):
        if 'type' in child:
            print(f'{i+1}. {child["type"]}')
    
    print('\n¡COMPILACIÓN EXITOSA!')
    
except Exception as e:
    print(f'ERROR: {e}')