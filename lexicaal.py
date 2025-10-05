import re
import sys

try:
    from PyQt6.QtWidgets import (
        QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
        QTextEdit, QPushButton, QTableWidget, QTableWidgetItem,
        QMessageBox, QFileDialog, QHeaderView, QSplitter, QLabel
    )
    from PyQt6.QtCore import Qt, QCoreApplication
    from PyQt6.QtGui import QFont, QColor
    HAS_PYQT = True
except Exception:
    HAS_PYQT = False

KEYWORDS = {
    'module': 'KEYWORD_MODULE', 'import': 'KEYWORD_IMPORT', 'as': 'KEYWORD_AS',
    'type': 'KEYWORD_TYPE', 'struct': 'KEYWORD_STRUCT', 'const': 'KEYWORD_CONST',
    'let': 'KEYWORD_LET', 'fn': 'KEYWORD_FN', 'int': 'KEYWORD_INT',
    'bool': 'KEYWORD_BOOL', 'string': 'KEYWORD_STRING', 'if': 'KEYWORD_IF',
    'else': 'KEYWORD_ELSE', 'while': 'KEYWORD_WHILE', 'return': 'KEYWORD_RETURN',
    'true': 'BOOL_LIT', 'false': 'BOOL_LIT',
}

TOKEN_TYPE = {
    'ID': 'ID', 'NUM_LIT': 'NUM_LIT', 'STRING_LIT': 'STRING_LIT',
    'ASSIGN': 'OP_ASSIGN', 'ARROW': 'OP_ARROW', 'EQ': 'OP_EQ', 'NE': 'OP_NE',
    'LE': 'OP_LE', 'GE': 'OP_GE', 'AND': 'OP_AND', 'OR': 'OP_OR',
    'ADD': 'OP_ADD', 'SUB': 'OP_SUB', 'MUL': 'OP_MUL', 'DIV': 'OP_DIV', 'MOD': 'OP_MOD', 'NOT': 'OP_NOT',
    'LT': 'OP_LT', 'GT': 'OP_GT', 'LPAREN': 'SEP_LPAREN', 'RPAREN': 'SEP_RPAREN',
    'LBRACE': 'SEP_LBRACE', 'RBRACE': 'SEP_RBRACE', 'LBRACKET': 'SEP_LBRACKET', 'RBRACKET': 'SEP_RBRACKET',
    'COMMA': 'SEP_COMMA', 'COLON': 'SEP_COLON', 'SEMI': 'SEP_SEMI', 'DOT': 'SEP_DOT', 'EOF': 'EOF',
}

TOKEN_REGEX = [
    (r'//[^\n]*', 'COMMENT'),
    (r'"([^"\\]|\\.)*"', TOKEN_TYPE['STRING_LIT']),
    (r'->', TOKEN_TYPE['ARROW']),
    (r'==', TOKEN_TYPE['EQ']),
    (r'!=', TOKEN_TYPE['NE']),
    (r'<=', TOKEN_TYPE['LE']),
    (r'>=', TOKEN_TYPE['GE']),
    (r'&&', TOKEN_TYPE['AND']),
    (r'\|\|', TOKEN_TYPE['OR']),
    (r'\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?|\.\d+(?:[eE][+\-]?\d+)?', TOKEN_TYPE['NUM_LIT']),
    (r'[a-zA-Z_][a-zA-Z0-9_]*', TOKEN_TYPE['ID']),
    (r'=', TOKEN_TYPE['ASSIGN']),
    (r'[+\-]', TOKEN_TYPE['ADD']),
    (r'\*', TOKEN_TYPE['MUL']),
    (r'/', TOKEN_TYPE['DIV']),
    (r'%', TOKEN_TYPE['MOD']),
    (r'!', TOKEN_TYPE['NOT']),
    (r'<', TOKEN_TYPE['LT']),
    (r'>', TOKEN_TYPE['GT']),
    (r'\(', TOKEN_TYPE['LPAREN']),
    (r'\)', TOKEN_TYPE['RPAREN']),
    (r'\{', TOKEN_TYPE['LBRACE']),
    (r'\}', TOKEN_TYPE['RBRACE']),
    (r'\[', TOKEN_TYPE['LBRACKET']),
    (r'\]', TOKEN_TYPE['RBRACKET']),
    (r',', TOKEN_TYPE['COMMA']),
    (r':', TOKEN_TYPE['COLON']),
    (r';', TOKEN_TYPE['SEMI']),
    (r'\.', TOKEN_TYPE['DOT']),
    (r'\s+', 'WHITESPACE'),
]

COMPILED_TOKEN_REGEX = [(re.compile(p), t) for p, t in TOKEN_REGEX]

class LexerError(Exception):
    pass

class ParserError(Exception):
    pass

class Token:
    def __init__(self, type, value, line, column):
        self.type = type
        self.value = value
        self.line = line
        self.column = column
    
    def __repr__(self):
        return f"Token({self.type}, '{self.value}', L{self.line}, C{self.column})"

class Lexer:
    def __init__(self, code):
        self.code = code
        self.tokens = []
        self.line = 1
        self.column = 1
        self.pos = 0

    def tokenize(self):
        self.tokens = []
        self.line = 1
        self.column = 1
        self.pos = 0
        length = len(self.code)
        
        while self.pos < length:
            match = None
            match_type = None
            
            for regex, ttype in COMPILED_TOKEN_REGEX:
                m = regex.match(self.code, self.pos)
                if m:
                    if match is None or len(m.group(0)) > len(match.group(0)):
                        match = m
                        match_type = ttype
            
            if not match:
                ch = self.code[self.pos]
                raise LexerError(f"Símbolo no reconocido '{ch}' en L{self.line} C{self.column}")
            
            text = match.group(0)
            
            if match_type in ('WHITESPACE', 'COMMENT'):
                newlines = text.count('\n')
                if newlines:
                    self.line += newlines
                    last_n = text.rfind('\n')
                    self.column = len(text) - last_n
                else:
                    self.column += len(text)
                self.pos += len(text)
                continue
            
            token_line = self.line
            token_col = self.column
            
            if match_type == TOKEN_TYPE['ID']:
                ttype = KEYWORDS.get(text, TOKEN_TYPE['ID'])
            else:
                ttype = match_type
            
            self.tokens.append(Token(ttype, text, token_line, token_col))
            
            newlines = text.count('\n')
            if newlines:
                self.line += newlines
                last_n = text.rfind('\n')
                self.column = len(text) - last_n
            else:
                self.column += len(text)
            
            self.pos += len(text)
        
        self.tokens.append(Token(TOKEN_TYPE['EOF'], 'EOF', self.line, self.column))
        return self.tokens

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.i = 0
        self.current = tokens[0] if tokens else None

    def advance(self):
        self.i += 1
        if self.i < len(self.tokens):
            self.current = self.tokens[self.i]
        return self.current

    def match(self, expected):
        if self.current and self.current.type == expected:
            tok = self.current
            self.advance()
            return tok
        line = getattr(self.current, 'line', 0) if self.current else 0
        column = getattr(self.current, 'column', 0) if self.current else 0
        current_type = self.current.type if self.current else 'EOF'
        raise ParserError(f"Se esperaba {expected} pero se encontró {current_type} en L{line} C{column}")

    def parse(self):
        if not self.tokens:
            raise ParserError("No hay tokens para analizar")
        ast = {'type': 'Program', 'children': []}
        self.parse_ModuleDecl(ast)
        self.parse_ImportList(ast)
        self.parse_TopList(ast)
        if self.current and self.current.type != TOKEN_TYPE['EOF']:
            raise ParserError(f"Token inesperado después del programa: {self.current}")
        return ast

    def parse_ModuleDecl(self, parent):
        node = {'type': 'ModuleDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['module']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_ImportList(self, parent):
        node = {'type': 'ImportList', 'children': []}
        while self.current and self.current.type == KEYWORDS['import']:
            decl = {'type': 'ImportDecl', 'children': []}
            decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['import']).value})
            self.parse_QualID(decl)
            if self.current and self.current.type == KEYWORDS['as']:
                decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['as']).value})
                decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
            decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
            node['children'].append(decl)
        parent['children'].append(node)

    def parse_QualID(self, parent):
        node = {'type': 'QualID', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        while self.current and self.current.type == TOKEN_TYPE['DOT']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        parent['children'].append(node)

    def parse_TopList(self, parent):
        node = {'type': 'TopList', 'children': []}
        tops = {KEYWORDS['type'], KEYWORDS['struct'], KEYWORDS['const'], KEYWORDS['let'], KEYWORDS['fn']}
        while self.current and self.current.type in tops:
            if self.current.type == KEYWORDS['type']:
                node['children'].append(self.parse_TypeDecl())
            elif self.current.type == KEYWORDS['struct']:
                node['children'].append(self.parse_StructDecl())
            elif self.current.type == KEYWORDS['const']:
                node['children'].append(self.parse_ConstDecl())
            elif self.current.type == KEYWORDS['let']:
                node['children'].append(self.parse_LetDecl())
            elif self.current.type == KEYWORDS['fn']:
                node['children'].append(self.parse_FunDecl())
        parent['children'].append(node)

    def parse_TypeDecl(self):
        node = {'type': 'TypeDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['type']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
        node['children'].append({'type': 'Type', 'value': self.parse_Type()})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        return node

    def parse_StructDecl(self):
        node = {'type': 'StructDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['struct']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACE']).value})
        while self.current and self.current.type == TOKEN_TYPE['ID']:
            f = {'type': 'Field', 'children': []}
            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
            f['children'].append({'type': 'Type', 'value': self.parse_Type()})
            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
            node['children'].append(f)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        return node

    def parse_ConstDecl(self):
        node = {'type': 'ConstDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['const']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        node['children'].append({'type': 'Type', 'value': self.parse_Type()})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
        node['children'].append({'type': 'Expr', 'value': self.parse_Expr()})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        return node

    def parse_LetDecl(self):
        node = {'type': 'LetDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['let']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        node['children'].append({'type': 'Type', 'value': self.parse_Type()})
        if self.current and self.current.type == TOKEN_TYPE['ASSIGN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
            node['children'].append({'type': 'Expr', 'value': self.parse_Expr()})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        return node

    def parse_FunDecl(self):
        node = {'type': 'FunDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['fn']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
        if self.current and self.current.type == TOKEN_TYPE['ID']:
            node['children'].append(self.parse_Param())
            while self.current and self.current.type == TOKEN_TYPE['COMMA']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COMMA']).value})
                node['children'].append(self.parse_Param())
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        if self.current and self.current.type == TOKEN_TYPE['ARROW']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ARROW']).value})
            node['children'].append({'type': 'Type', 'value': self.parse_Type()})
        node['children'].append({'type': 'Block', 'value': self.parse_Block()})
        return node

    def parse_Param(self):
        node = {'type': 'Param', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        node['children'].append({'type': 'Type', 'value': self.parse_Type()})
        return node

    def parse_Type(self):
        if self.current and self.current.type == KEYWORDS['int']:
            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['int']).value}
        elif self.current and self.current.type == KEYWORDS['bool']:
            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['bool']).value}
        elif self.current and self.current.type == KEYWORDS['string']:
            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['string']).value}
        elif self.current and self.current.type == TOKEN_TYPE['LPAREN']:
            self.match(TOKEN_TYPE['LPAREN'])
            inner = self.parse_Type()
            self.match(TOKEN_TYPE['RPAREN'])
            t = {'type': 'ParenType', 'value': inner}
        elif self.current and self.current.type == TOKEN_TYPE['ID']:
            q = {'type': 'QualID', 'value': self.match(TOKEN_TYPE['ID']).value}
            while self.current and self.current.type == TOKEN_TYPE['DOT']:
                self.match(TOKEN_TYPE['DOT'])
                q_val = self.match(TOKEN_TYPE['ID']).value
                q = {'type': 'QualID', 'value': q['value'] + '.' + q_val}
            t = q
        else:
            line = getattr(self.current, 'line', 0) if self.current else 0
            column = getattr(self.current, 'column', 0) if self.current else 0
            raise ParserError(f"Se esperaba un tipo en L{line} C{column}")
        
        while self.current and self.current.type == TOKEN_TYPE['LBRACKET']:
            self.match(TOKEN_TYPE['LBRACKET'])
            self.match(TOKEN_TYPE['RBRACKET'])
            t = {'type': 'ArrayType', 'value': t}
        
        return t

    def parse_Block(self):
        self.match(TOKEN_TYPE['LBRACE'])
        stmts = []
        while self.current and self.current.type != TOKEN_TYPE['RBRACE']:
            stmts.append(self.parse_Stmt())
        self.match(TOKEN_TYPE['RBRACE'])
        return {'type': 'Block', 'stmts': stmts}

    def parse_Stmt(self):
        if self.current and self.current.type == TOKEN_TYPE['LBRACE']:
            return {'type': 'Block', 'value': self.parse_Block()}
        if self.current and self.current.type == KEYWORDS['let']:
            return self.parse_LetDecl()
        if self.current and self.current.type == KEYWORDS['if']:
            self.match(KEYWORDS['if'])
            self.match(TOKEN_TYPE['LPAREN'])
            cond = self.parse_Expr()
            self.match(TOKEN_TYPE['RPAREN'])
            then = self.parse_Stmt()
            els = None
            if self.current and self.current.type == KEYWORDS['else']:
                self.match(KEYWORDS['else'])
                els = self.parse_Stmt()
            return {'type': 'If', 'cond': cond, 'then': then, 'else': els}
        if self.current and self.current.type == KEYWORDS['while']:
            self.match(KEYWORDS['while'])
            self.match(TOKEN_TYPE['LPAREN'])
            cond = self.parse_Expr()
            self.match(TOKEN_TYPE['RPAREN'])
            body = self.parse_Stmt()
            return {'type': 'While', 'cond': cond, 'body': body}
        if self.current and self.current.type == KEYWORDS['return']:
            self.match(KEYWORDS['return'])
            if self.current and self.current.type == TOKEN_TYPE['SEMI']:
                self.match(TOKEN_TYPE['SEMI'])
                return {'type': 'Return', 'value': None}
            v = self.parse_Expr()
            self.match(TOKEN_TYPE['SEMI'])
            return {'type': 'Return', 'value': v}
        
        expr = self.parse_Expr()
        self.match(TOKEN_TYPE['SEMI'])
        return {'type': 'ExprStmt', 'expr': expr}

    def parse_Expr(self):
        return self.parse_Assign()

    def parse_Assign(self):
        left = self.parse_Or()
        if self.current and self.current.type == TOKEN_TYPE['ASSIGN']:
            self.match(TOKEN_TYPE['ASSIGN'])
            right = self.parse_Assign()
            return {'type': 'Assign', 'left': left, 'right': right}
        return left

    def parse_Or(self):
        left = self.parse_And()
        while self.current and self.current.type == TOKEN_TYPE['OR']:
            self.match(TOKEN_TYPE['OR'])
            right = self.parse_And()
            left = {'type': 'Or', 'left': left, 'right': right}
        return left

    def parse_And(self):
        left = self.parse_Eq()
        while self.current and self.current.type == TOKEN_TYPE['AND']:
            self.match(TOKEN_TYPE['AND'])
            right = self.parse_Eq()
            left = {'type': 'And', 'left': left, 'right': right}
        return left

    def parse_Eq(self):
        left = self.parse_Rel()
        while self.current and self.current.type in (TOKEN_TYPE['EQ'], TOKEN_TYPE['NE']):
            if self.current.type == TOKEN_TYPE['EQ']:
                self.match(TOKEN_TYPE['EQ'])
                right = self.parse_Rel()
                left = {'type': 'Eq', 'left': left, 'right': right}
            else:
                self.match(TOKEN_TYPE['NE'])
                right = self.parse_Rel()
                left = {'type': 'Ne', 'left': left, 'right': right}
        return left

    def parse_Rel(self):
        left = self.parse_Add()
        while self.current and self.current.type in (TOKEN_TYPE['LT'], TOKEN_TYPE['LE'], TOKEN_TYPE['GT'], TOKEN_TYPE['GE']):
            t = self.current.type
            self.advance()
            right = self.parse_Add()
            left = {'type': 'Rel', 'op': t, 'left': left, 'right': right}
        return left

    def parse_Add(self):
        left = self.parse_Mul()
        while self.current and self.current.type in (TOKEN_TYPE['ADD'], TOKEN_TYPE['SUB']):
            t = self.current.type
            self.advance()
            right = self.parse_Mul()
            left = {'type': 'Add' if t == TOKEN_TYPE['ADD'] else 'Sub', 'left': left, 'right': right}
        return left

    def parse_Mul(self):
        left = self.parse_Unary()
        while self.current and self.current.type in (TOKEN_TYPE['MUL'], TOKEN_TYPE['DIV'], TOKEN_TYPE['MOD']):
            t = self.current.type
            self.advance()
            right = self.parse_Unary()
            op_name = 'Mul' if t == TOKEN_TYPE['MUL'] else ('Div' if t == TOKEN_TYPE['DIV'] else 'Mod')
            left = {'type': op_name, 'left': left, 'right': right}
        return left

    def parse_Unary(self):
        if self.current and self.current.type == TOKEN_TYPE['NOT']:
            self.match(TOKEN_TYPE['NOT'])
            return {'type': 'Not', 'expr': self.parse_Unary()}
        if self.current and self.current.type == TOKEN_TYPE['SUB']:
            self.match(TOKEN_TYPE['SUB'])
            return {'type': 'Neg', 'expr': self.parse_Unary()}
        return self.parse_Postfix()

    def parse_Postfix(self):
        node = self.parse_Primary()
        while True:
            if self.current and self.current.type == TOKEN_TYPE['LPAREN']:
                self.match(TOKEN_TYPE['LPAREN'])
                args = []
                if self.current and self.current.type != TOKEN_TYPE['RPAREN']:
                    args.append(self.parse_Expr())
                    while self.current and self.current.type == TOKEN_TYPE['COMMA']:
                        self.match(TOKEN_TYPE['COMMA'])
                        args.append(self.parse_Expr())
                self.match(TOKEN_TYPE['RPAREN'])
                node = {'type': 'Call', 'fn': node, 'args': args}
            elif self.current and self.current.type == TOKEN_TYPE['LBRACKET']:
                self.match(TOKEN_TYPE['LBRACKET'])
                idx = self.parse_Expr()
                self.match(TOKEN_TYPE['RBRACKET'])
                node = {'type': 'Index', 'expr': node, 'index': idx}
            elif self.current and self.current.type == TOKEN_TYPE['DOT']:
                self.match(TOKEN_TYPE['DOT'])
                name = self.match(TOKEN_TYPE['ID']).value
                node = {'type': 'Field', 'expr': node, 'field': name}
            else:
                break
        return node

    def parse_Primary(self):
        if not self.current:
            raise ParserError("Final de entrada inesperado")
        
        t = self.current.type
        if t == TOKEN_TYPE['ID']:
            v = self.match(TOKEN_TYPE['ID']).value
            return {'type': 'Var', 'name': v}
        if t == TOKEN_TYPE['NUM_LIT']:
            v = self.match(TOKEN_TYPE['NUM_LIT']).value
            return {'type': 'Num', 'value': v}
        if t == TOKEN_TYPE['STRING_LIT']:
            v = self.match(TOKEN_TYPE['STRING_LIT']).value
            return {'type': 'Str', 'value': v}
        if t in ('BOOL_LIT', KEYWORDS['true'], KEYWORDS['false']):
            v = self.match(t).value
            return {'type': 'Bool', 'value': v}
        if t == TOKEN_TYPE['LPAREN']:
            self.match(TOKEN_TYPE['LPAREN'])
            v = self.parse_Expr()
            self.match(TOKEN_TYPE['RPAREN'])
            return v
        
        raise ParserError(f"Se esperaba una expresión primaria en {self.current}")

if HAS_PYQT:
    class CompilerIDE(QMainWindow):
        def __init__(self):
            super().__init__()
            self.setWindowTitle('Mini Compilador LL(1)')
            self.setGeometry(100, 100, 1200, 800)
            self.current_filepath = None
            self.init_ui()

        def init_ui(self):
            main = QWidget()
            layout = QVBoxLayout(main)
            self.setCentralWidget(main)
            
            self.code_editor = QTextEdit()
            self.code_editor.setPlaceholderText("module ejemplo;\n\nfn main() {\n    let x: int = 42;\n    return x;\n}")
            self.code_editor.setFont(QFont("Consolas", 11))
            self.code_editor.setStyleSheet("background-color: #2b2b2b; color: #ffffff; border: 1px solid #555;")
            layout.addWidget(self.code_editor)
            
            btn_layout = QHBoxLayout()
            
            self.btn_open = QPushButton('Abrir')
            self.btn_save = QPushButton('Guardar')
            self.btn_compile = QPushButton('COMPILAR')
            self.btn_clear = QPushButton('Limpiar')
            
            self.btn_open.clicked.connect(self.open_file)
            self.btn_save.clicked.connect(self.save_file)
            self.btn_compile.clicked.connect(self.compile_code)
            self.btn_clear.clicked.connect(self.clear_all)
            
            btn_style = """
            QPushButton {
                background-color: #4CAF50;
                color: white;
                padding: 8px 16px;
                border-radius: 4px;
                font-weight: bold;
            }
            QPushButton:hover {
                background-color: #45a049;
            }
            """
            
            for btn in [self.btn_open, self.btn_save, self.btn_compile, self.btn_clear]:
                btn.setStyleSheet(btn_style)
            
            self.btn_compile.setStyleSheet(btn_style.replace('#4CAF50', '#2196F3').replace('#45a049', '#1976D2'))
            
            btn_layout.addWidget(self.btn_open)
            btn_layout.addWidget(self.btn_save)
            btn_layout.addStretch()
            btn_layout.addWidget(self.btn_clear)
            btn_layout.addWidget(self.btn_compile)
            layout.addLayout(btn_layout)
            
            splitter = QSplitter(Qt.Orientation.Horizontal)
            
            self.token_table = QTableWidget()
            self.token_table.setColumnCount(4)
            self.token_table.setHorizontalHeaderLabels(['Línea', 'Columna', 'Tipo', 'Valor'])
            self.token_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
            self.token_table.setStyleSheet("""
                QTableWidget {
                    background-color: #f5f5f5;
                    alternate-background-color: #e9e9e9;
                    selection-background-color: #b3d9ff;
                    gridline-color: #ddd;
                }
                QHeaderView::section {
                    background-color: #4CAF50;
                    color: white;
                    padding: 6px;
                    font-weight: bold;
                }
            """)
            
            self.result_area = QTextEdit()
            self.result_area.setReadOnly(True)
            self.result_area.setFont(QFont("Consolas", 10))
            self.result_area.setStyleSheet("background-color: #1e1e1e; color: #00ff00; border: 1px solid #555;")
            
            splitter.addWidget(self.token_table)
            splitter.addWidget(self.result_area)
            splitter.setSizes([300, 300])
            
            layout.addWidget(splitter)

        def open_file(self):
            file_path, _ = QFileDialog.getOpenFileName(self, "Abrir archivo", "", "Archivos de código (*.lc *.txt);;Todos (*.*)")
            if file_path:
                try:
                    with open(file_path, 'r', encoding='utf-8') as f:
                        self.code_editor.setText(f.read())
                    self.current_filepath = file_path
                    self.setWindowTitle(f'Mini Compilador LL(1) - {file_path}')
                    self.result_area.setText(f"Archivo cargado: {file_path}")
                except Exception as e:
                    QMessageBox.critical(self, "Error", f"No se pudo abrir el archivo: {e}")

        def save_file(self):
            if self.current_filepath:
                file_path = self.current_filepath
            else:
                file_path, _ = QFileDialog.getSaveFileName(self, "Guardar archivo", "", "Archivos de código (*.lc);;Todos (*.*)")
            
            if file_path:
                try:
                    with open(file_path, 'w', encoding='utf-8') as f:
                        f.write(self.code_editor.toPlainText())
                    self.current_filepath = file_path
                    self.setWindowTitle(f'Mini Compilador LL(1) - {file_path}')
                    self.result_area.setText(f"Archivo guardado: {file_path}")
                except Exception as e:
                    QMessageBox.critical(self, "Error", f"No se pudo guardar el archivo: {e}")

        def clear_all(self):
            self.code_editor.clear()
            self.token_table.setRowCount(0)
            self.result_area.clear()
            self.setWindowTitle('Mini Compilador LL(1)')

        def compile_code(self):
            src = self.code_editor.toPlainText().strip()
            if not src:
                self.result_area.setText("Error: No hay código para compilar")
                return
                
            try:
                self.result_area.setText("Iniciando análisis léxico...")
                
                lexer = Lexer(src)
                tokens = lexer.tokenize()
                
                self.token_table.setRowCount(len(tokens))
                for i, tok in enumerate(tokens):
                    self.token_table.setItem(i, 0, QTableWidgetItem(str(tok.line)))
                    self.token_table.setItem(i, 1, QTableWidgetItem(str(tok.column)))
                    self.token_table.setItem(i, 2, QTableWidgetItem(tok.type))
                    self.token_table.setItem(i, 3, QTableWidgetItem(tok.value))
                
                self.result_area.append(f"✓ Análisis léxico completado. {len(tokens)} tokens generados.")
                self.result_area.append("Iniciando análisis sintáctico...")
                
                parser = Parser(tokens)
                ast = parser.parse()
                
                self.result_area.append("✓ Análisis sintáctico completado.")
                self.result_area.append("✓ COMPILACIÓN EXITOSA")
                self.result_area.append(f"Estructura del programa: {ast['type']}")
                self.result_area.append(f"Nodos del AST: {len(ast['children'])}")
                
                QMessageBox.information(self, 'Éxito', '¡Compilación completada sin errores!')
                
            except (LexerError, ParserError) as e:
                self.result_area.append(f"✗ ERROR: {str(e)}")
                QMessageBox.critical(self, 'Error de Compilación', str(e))
            except Exception as e:
                self.result_area.append(f"✗ ERROR INTERNO: {str(e)}")
                QMessageBox.critical(self, 'Error', f"Error inesperado: {str(e)}")

else:
    class CompilerIDE:
        pass

def test_basic():
    code = """module test;
    fn main() -> int {
        let x: int = 42;
        return x;
    }"""
    
    try:
        lexer = Lexer(code)
        tokens = lexer.tokenize()
        print(f"Tokens generados: {len(tokens)}")
        
        parser = Parser(tokens)
        ast = parser.parse()
        print("Análisis sintáctico exitoso")
        return True
    except Exception as e:
        print(f"Error en test: {e}")
        return False

if __name__ == '__main__':
    if HAS_PYQT:
        app = QCoreApplication.instance()
        if app is None:
            app = QApplication(sys.argv)
        win = CompilerIDE()
        win.show()
        sys.exit(app.exec())
    else:
        print("PyQt6 no disponible. Ejecutando test básico...")
        test_basic()
