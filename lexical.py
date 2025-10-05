import reimport reimport re

import sys

import sysimport sys

try:

    from PyQt6.QtWidgets import (

        QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,

        QTextEdit, QPushButton, QTableWidget, QTableWidgetItem,try:try:

        QMessageBox, QFileDialog, QHeaderView, QSplitter, QLabel

    )    from PyQt6.QtWidgets import (    from PyQt6.QtWidgets import QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout, QTextEdit, QPushButton, QTableWidget, QTableWidgetItem, QMessageBox

    from PyQt6.QtCore import Qt, QCoreApplication

    from PyQt6.QtGui import QFont, QColor        QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,    from PyQt6.QtCore import QCoreApplication

    HAS_PYQT = True

except Exception:        QTextEdit, QPushButton, QTableWidget, QTableWidgetItem,    from PyQt6.QtGui import QFont, QColor

    HAS_PYQT = False

        QMessageBox, QFileDialog, QHeaderView, QSplitter, QLabel    HAS_PYQT = True

KEYWORDS = {

    'module': 'KEYWORD_MODULE', 'import': 'KEYWORD_IMPORT', 'as': 'KEYWORD_AS',    )except Exception:

    'type': 'KEYWORD_TYPE', 'struct': 'KEYWORD_STRUCT', 'const': 'KEYWORD_CONST',

    'let': 'KEYWORD_LET', 'fn': 'KEYWORD_FN', 'int': 'KEYWORD_INT',    from PyQt6.QtCore import Qt, QCoreApplication    HAS_PYQT = False

    'bool': 'KEYWORD_BOOL', 'string': 'KEYWORD_STRING', 'if': 'KEYWORD_IF',

    'else': 'KEYWORD_ELSE', 'while': 'KEYWORD_WHILE', 'return': 'KEYWORD_RETURN',    from PyQt6.QtGui import QFont, QColor

    'true': 'BOOL_LIT', 'false': 'BOOL_LIT',

}    HAS_PYQT = TrueKEYWORDS = {



TOKEN_TYPE = {except Exception:    'module': 'KEYWORD_MODULE', 'import': 'KEYWORD_IMPORT', 'as': 'KEYWORD_AS',

    'ID': 'ID', 'NUM_LIT': 'NUM_LIT', 'STRING_LIT': 'STRING_LIT',

    'ASSIGN': 'OP_ASSIGN', 'ARROW': 'OP_ARROW', 'EQ': 'OP_EQ', 'NE': 'OP_NE',    HAS_PYQT = False    'type': 'KEYWORD_TYPE', 'struct': 'KEYWORD_STRUCT', 'const': 'KEYWORD_CONST',

    'LE': 'OP_LE', 'GE': 'OP_GE', 'AND': 'OP_AND', 'OR': 'OP_OR',

    'ADD': 'OP_ADD', 'SUB': 'OP_SUB', 'MUL': 'OP_MUL', 'DIV': 'OP_DIV', 'MOD': 'OP_MOD', 'NOT': 'OP_NOT',    'let': 'KEYWORD_LET', 'fn': 'KEYWORD_FN', 'int': 'KEYWORD_INT',

    'LT': 'OP_LT', 'GT': 'OP_GT', 'LPAREN': 'SEP_LPAREN', 'RPAREN': 'SEP_RPAREN',

    'LBRACE': 'SEP_LBRACE', 'RBRACE': 'SEP_RBRACE', 'LBRACKET': 'SEP_LBRACKET', 'RBRACKET': 'SEP_RBRACKET',KEYWORDS = {    'bool': 'KEYWORD_BOOL', 'string': 'KEYWORD_STRING', 'if': 'KEYWORD_IF',

    'COMMA': 'SEP_COMMA', 'COLON': 'SEP_COLON', 'SEMI': 'SEP_SEMI', 'DOT': 'SEP_DOT', 'EOF': 'EOF',

}    'module': 'KEYWORD_MODULE', 'import': 'KEYWORD_IMPORT', 'as': 'KEYWORD_AS',    'else': 'KEYWORD_ELSE', 'while': 'KEYWORD_WHILE', 'return': 'KEYWORD_RETURN',



TOKEN_REGEX = [    'type': 'KEYWORD_TYPE', 'struct': 'KEYWORD_STRUCT', 'const': 'KEYWORD_CONST',    'true': 'BOOL_LIT', 'false': 'BOOL_LIT',

    (r'//[^\n]*', 'COMMENT'),

    (r'"([^"\\]|\\.)*"', TOKEN_TYPE['STRING_LIT']),    'let': 'KEYWORD_LET', 'fn': 'KEYWORD_FN', 'int': 'KEYWORD_INT',}

    (r'->', TOKEN_TYPE['ARROW']),

    (r'==', TOKEN_TYPE['EQ']),    'bool': 'KEYWORD_BOOL', 'string': 'KEYWORD_STRING', 'if': 'KEYWORD_IF',

    (r'!=', TOKEN_TYPE['NE']),

    (r'<=', TOKEN_TYPE['LE']),    'else': 'KEYWORD_ELSE', 'while': 'KEYWORD_WHILE', 'return': 'KEYWORD_RETURN',TOKEN_TYPE = {

    (r'>=', TOKEN_TYPE['GE']),

    (r'&&', TOKEN_TYPE['AND']),    'true': 'BOOL_LIT', 'false': 'BOOL_LIT',    'ID': 'ID', 'NUM_LIT': 'NUM_LIT', 'STRING_LIT': 'STRING_LIT',

    (r'\|\|', TOKEN_TYPE['OR']),

    (r'\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?|\.\d+(?:[eE][+\-]?\d+)?', TOKEN_TYPE['NUM_LIT']),}    'ASSIGN': 'OP_ASSIGN', 'ARROW': 'OP_ARROW', 'EQ': 'OP_EQ', 'NE': 'OP_NE',

    (r'[a-zA-Z_][a-zA-Z0-9_]*', TOKEN_TYPE['ID']),

    (r'=', TOKEN_TYPE['ASSIGN']),    'LE': 'OP_LE', 'GE': 'OP_GE', 'AND': 'OP_AND', 'OR': 'OP_OR',

    (r'[+\-]', TOKEN_TYPE['ADD']),

    (r'\*', TOKEN_TYPE['MUL']),TOKEN_TYPE = {    'ADD': 'OP_ADD', 'SUB': 'OP_SUB', 'MUL': 'OP_MUL', 'DIV': 'OP_DIV', 'MOD': 'OP_MOD', 'NOT': 'OP_NOT',

    (r'/', TOKEN_TYPE['DIV']),

    (r'%', TOKEN_TYPE['MOD']),    'ID': 'ID', 'NUM_LIT': 'NUM_LIT', 'STRING_LIT': 'STRING_LIT',    'LT': 'OP_LT', 'GT': 'OP_GT', 'LPAREN': 'SEP_LPAREN', 'RPAREN': 'SEP_RPAREN',

    (r'!', TOKEN_TYPE['NOT']),

    (r'<', TOKEN_TYPE['LT']),    'ASSIGN': 'OP_ASSIGN', 'ARROW': 'OP_ARROW', 'EQ': 'OP_EQ', 'NE': 'OP_NE',    'LBRACE': 'SEP_LBRACE', 'RBRACE': 'SEP_RBRACE', 'LBRACKET': 'SEP_LBRACKET', 'RBRACKET': 'SEP_RBRACKET',

    (r'>', TOKEN_TYPE['GT']),

    (r'\(', TOKEN_TYPE['LPAREN']),    'LE': 'OP_LE', 'GE': 'OP_GE', 'AND': 'OP_AND', 'OR': 'OP_OR',    'COMMA': 'SEP_COMMA', 'COLON': 'SEP_COLON', 'SEMI': 'SEP_SEMI', 'DOT': 'SEP_DOT', 'EOF': 'EOF',

    (r'\)', TOKEN_TYPE['RPAREN']),

    (r'\{', TOKEN_TYPE['LBRACE']),    'ADD': 'OP_ADD', 'SUB': 'OP_SUB', 'MUL': 'OP_MUL', 'DIV': 'OP_DIV', 'MOD': 'OP_MOD', 'NOT': 'OP_NOT',}

    (r'\}', TOKEN_TYPE['RBRACE']),

    (r'\[', TOKEN_TYPE['LBRACKET']),    'LT': 'OP_LT', 'GT': 'OP_GT', 'LPAREN': 'SEP_LPAREN', 'RPAREN': 'SEP_RPAREN',

    (r'\]', TOKEN_TYPE['RBRACKET']),

    (r',', TOKEN_TYPE['COMMA']),    'LBRACE': 'SEP_LBRACE', 'RBRACE': 'SEP_RBRACE', 'LBRACKET': 'SEP_LBRACKET', 'RBRACKET': 'SEP_RBRACKET',TOKEN_REGEX = [

    (r':', TOKEN_TYPE['COLON']),

    (r';', TOKEN_TYPE['SEMI']),    'COMMA': 'SEP_COMMA', 'COLON': 'SEP_COLON', 'SEMI': 'SEP_SEMI', 'DOT': 'SEP_DOT', 'EOF': 'EOF',    (r'//[^\n]*', 'COMMENT'),

    (r'\.', TOKEN_TYPE['DOT']),

    (r'\s+', 'WHITESPACE'),}    (r'"([^"\\]|\\.)*"', TOKEN_TYPE['STRING_LIT']),

]

    (r'->', TOKEN_TYPE['ARROW']),

COMPILED_TOKEN_REGEX = [(re.compile(p), t) for p, t in TOKEN_REGEX]

TOKEN_REGEX = [    (r'==', TOKEN_TYPE['EQ']),

class LexerError(Exception):

    pass    (r'//[^\n]*', 'COMMENT'),    (r'!=', TOKEN_TYPE['NE']),



class ParserError(Exception):    (r'"([^"\\]|\\.)*"', TOKEN_TYPE['STRING_LIT']),    (r'<=', TOKEN_TYPE['LE']),

    pass

    (r'->', TOKEN_TYPE['ARROW']),    (r'>=', TOKEN_TYPE['GE']),

class Token:

    def __init__(self, type, value, line, column):    (r'==', TOKEN_TYPE['EQ']),    (r'&&', TOKEN_TYPE['AND']),

        self.type = type

        self.value = value    (r'!=', TOKEN_TYPE['NE']),    (r'\|\|', TOKEN_TYPE['OR']),

        self.line = line

        self.column = column    (r'<=', TOKEN_TYPE['LE']),    (r'\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?|\.\d+(?:[eE][+\-]?\d+)?', TOKEN_TYPE['NUM_LIT']),

    

    def __repr__(self):    (r'>=', TOKEN_TYPE['GE']),    (r'[a-zA-Z_][a-zA-Z0-9_]*', TOKEN_TYPE['ID']),

        return f"Token({self.type}, '{self.value}', L{self.line}, C{self.column})"

    (r'&&', TOKEN_TYPE['AND']),    (r'=', TOKEN_TYPE['ASSIGN']),

class Lexer:

    def __init__(self, code):    (r'\|\|', TOKEN_TYPE['OR']),    (r'[+\-]', TOKEN_TYPE['ADD']),

        self.code = code

        self.tokens = []    (r'\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?|\.\d+(?:[eE][+\-]?\d+)?', TOKEN_TYPE['NUM_LIT']),    (r'\*', TOKEN_TYPE['MUL']),

        self.line = 1

        self.column = 1    (r'[a-zA-Z_][a-zA-Z0-9_]*', TOKEN_TYPE['ID']),    (r'/', TOKEN_TYPE['DIV']),

        self.pos = 0

    (r'=', TOKEN_TYPE['ASSIGN']),    (r'%', TOKEN_TYPE['MOD']),

    def tokenize(self):

        self.tokens = []    (r'[+\-]', TOKEN_TYPE['ADD']),    (r'!', TOKEN_TYPE['NOT']),

        self.line = 1

        self.column = 1    (r'\*', TOKEN_TYPE['MUL']),    (r'<', TOKEN_TYPE['LT']),

        self.pos = 0

        length = len(self.code)    (r'/', TOKEN_TYPE['DIV']),    (r'>', TOKEN_TYPE['GT']),

        

        while self.pos < length:    (r'%', TOKEN_TYPE['MOD']),    (r'\(', TOKEN_TYPE['LPAREN']),

            match = None

            match_type = None    (r'!', TOKEN_TYPE['NOT']),    (r'\)', TOKEN_TYPE['RPAREN']),

            

            for regex, ttype in COMPILED_TOKEN_REGEX:    (r'<', TOKEN_TYPE['LT']),    (r'\{', TOKEN_TYPE['LBRACE']),

                m = regex.match(self.code, self.pos)

                if m:    (r'>', TOKEN_TYPE['GT']),    (r'\}', TOKEN_TYPE['RBRACE']),

                    if match is None or len(m.group(0)) > len(match.group(0)):

                        match = m    (r'\(', TOKEN_TYPE['LPAREN']),    (r'\[', TOKEN_TYPE['LBRACKET']),

                        match_type = ttype

                (r'\)', TOKEN_TYPE['RPAREN']),    (r'\]', TOKEN_TYPE['RBRACKET']),

            if not match:

                ch = self.code[self.pos]    (r'\{', TOKEN_TYPE['LBRACE']),    (r',', TOKEN_TYPE['COMMA']),

                raise LexerError(f"Unrecognized symbol '{ch}' at L{self.line} C{self.column}")

                (r'\}', TOKEN_TYPE['RBRACE']),    (r':', TOKEN_TYPE['COLON']),

            text = match.group(0)

                (r'\[', TOKEN_TYPE['LBRACKET']),    (r';', TOKEN_TYPE['SEMI']),

            if match_type in ('WHITESPACE', 'COMMENT'):

                newlines = text.count('\n')    (r'\]', TOKEN_TYPE['RBRACKET']),    (r'\.', TOKEN_TYPE['DOT']),

                if newlines:

                    self.line += newlines    (r',', TOKEN_TYPE['COMMA']),    (r'\s+', 'WHITESPACE'),

                    last_n = text.rfind('\n')

                    self.column = len(text) - last_n    (r':', TOKEN_TYPE['COLON']),]

                else:

                    self.column += len(text)    (r';', TOKEN_TYPE['SEMI']),

                self.pos += len(text)

                continue    (r'\.', TOKEN_TYPE['DOT']),COMPILED_TOKEN_REGEX = [(re.compile(p), t) for p, t in TOKEN_REGEX]

            

            token_line = self.line    (r'\s+', 'WHITESPACE'),

            token_col = self.column

            ]class LexerError(Exception):

            if match_type == TOKEN_TYPE['ID']:

                ttype = KEYWORDS.get(text, TOKEN_TYPE['ID'])    pass

            else:

                ttype = match_typeCOMPILED_TOKEN_REGEX = [(re.compile(p), t) for p, t in TOKEN_REGEX]

            

            self.tokens.append(Token(ttype, text, token_line, token_col))class ParserError(Exception):

            

            newlines = text.count('\n')class LexerError(Exception):    pass

            if newlines:

                self.line += newlines    pass

                last_n = text.rfind('\n')

                self.column = len(text) - last_nclass Token:

            else:

                self.column += len(text)class ParserError(Exception):    def __init__(self, type, value, line, column):

            

            self.pos += len(text)    pass        self.type = type

        

        self.tokens.append(Token(TOKEN_TYPE['EOF'], 'EOF', self.line, self.column))        self.value = value

        return self.tokens

class Token:        self.line = line

class Parser:

    def __init__(self, tokens):    def __init__(self, type, value, line, column):        self.column = column

        self.tokens = tokens

        self.i = 0        self.type = type    def __repr__(self):

        self.current = tokens[0] if tokens else None

        self.value = value        return f"Token({self.type}, '{self.value}', L{self.line}, C{self.column})"

    def advance(self):

        self.i += 1        self.line = line

        if self.i < len(self.tokens):

            self.current = self.tokens[self.i]        self.column = columnclass Lexer:

        return self.current

        def __init__(self, code):

    def match(self, expected):

        if self.current and self.current.type == expected:    def __repr__(self):        self.code = code

            tok = self.current

            self.advance()        return f"Token({self.type}, '{self.value}', L{self.line}, C{self.column})"        self.tokens = []

            return tok

        line = getattr(self.current, 'line', 0) if self.current else 0        self.line = 1

        column = getattr(self.current, 'column', 0) if self.current else 0

        current_type = self.current.type if self.current else 'EOF'class Lexer:        self.column = 1

        raise ParserError(f"Expected {expected} but found {current_type} at L{line} C{column}")

    def __init__(self, code):        self.pos = 0

    def parse(self):

        ast = {'type': 'Program', 'children': []}        self.code = code

        self.parse_ModuleDecl(ast)

        self.parse_ImportList(ast)        self.tokens = []    def tokenize(self):

        self.parse_TopList(ast)

        if self.current and self.current.type != TOKEN_TYPE['EOF']:        self.line = 1        self.tokens = []

            raise ParserError(f"Unexpected token after program: {self.current}")

        return ast        self.column = 1        self.line = 1



    def parse_ModuleDecl(self, parent):        self.pos = 0        self.column = 1

        node = {'type': 'ModuleDecl', 'children': []}

        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['module']).value})        self.pos = 0

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})    def tokenize(self):        length = len(self.code)

        parent['children'].append(node)

        self.tokens = []        while self.pos < length:

    def parse_QualID(self, parent):

        node = {'type': 'QualID', 'children': []}        self.line = 1            match = None

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

        while self.current and self.current.type == TOKEN_TYPE['DOT']:        self.column = 1            match_type = None

            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})

            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})        self.pos = 0            for regex, ttype in COMPILED_TOKEN_REGEX:

        parent['children'].append(node)

        length = len(self.code)                m = regex.match(self.code, self.pos)

    def parse_ImportList(self, parent):

        node = {'type': 'ImportList', 'children': []}                        if m:

        while self.current and self.current.type == KEYWORDS['import']:

            decl = {'type': 'ImportDecl', 'children': []}        while self.pos < length:                    if match is None or len(m.group(0)) > len(match.group(0)):

            decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['import']).value})

            self.parse_QualID(decl)            match = None                        match = m

            if self.current and self.current.type == KEYWORDS['as']:

                decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['as']).value})            match_type = None                        match_type = ttype

                decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

            decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})                        if not match:

            node['children'].append(decl)

        parent['children'].append(node)            for regex, ttype in COMPILED_TOKEN_REGEX:                ch = self.code[self.pos]



    def parse_TopList(self, parent):                m = regex.match(self.code, self.pos)                raise LexerError(f"Unrecognized symbol '{ch}' at L{self.line} C{self.column}")

        node = {'type': 'TopList', 'children': []}

        tops = {KEYWORDS['type'], KEYWORDS['struct'], KEYWORDS['const'], KEYWORDS['let'], KEYWORDS['fn']}                if m:            text = match.group(0)

        while self.current and self.current.type in tops:

            if self.current.type == KEYWORDS['type']:                    if match is None or len(m.group(0)) > len(match.group(0)):            if match_type in ('WHITESPACE', 'COMMENT'):

                node['children'].append(self.parse_TypeDecl())

            elif self.current.type == KEYWORDS['struct']:                        match = m                newlines = text.count('\n')

                node['children'].append(self.parse_StructDecl())

            elif self.current.type == KEYWORDS['const']:                        match_type = ttype                if newlines:

                node['children'].append(self.parse_ConstDecl())

            elif self.current.type == KEYWORDS['let']:                                self.line += newlines

                node['children'].append(self.parse_LetDecl())

            elif self.current.type == KEYWORDS['fn']:            if not match:                    last_n = text.rfind('\n')

                node['children'].append(self.parse_FunDecl())

        parent['children'].append(node)                ch = self.code[self.pos]                    self.column = len(text) - last_n



    def parse_TypeDecl(self):                raise LexerError(f"Unrecognized symbol '{ch}' at L{self.line} C{self.column}")                else:

        node = {'type': 'TypeDecl', 'children': []}

        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['type']).value})                                self.column += len(text)

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})            text = match.group(0)                self.pos += len(text)

        node['children'].append({'type': 'Type', 'value': self.parse_Type()})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})                            continue

        return node

            if match_type in ('WHITESPACE', 'COMMENT'):            token_line = self.line

    def parse_StructDecl(self):

        node = {'type': 'StructDecl', 'children': []}                newlines = text.count('\n')            token_col = self.column

        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['struct']).value})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})                if newlines:            if match_type == TOKEN_TYPE['ID']:

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACE']).value})

        while self.current and self.current.type == TOKEN_TYPE['ID']:                    self.line += newlines                ttype = KEYWORDS.get(text, TOKEN_TYPE['ID'])

            f = {'type': 'Field', 'children': []}

            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})                    last_n = text.rfind('\n')            else:

            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})

            f['children'].append({'type': 'Type', 'value': self.parse_Type()})                    self.column = len(text) - last_n                ttype = match_type

            node['children'].append(f)

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})                else:            self.tokens.append(Token(ttype, text, token_line, token_col))

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

        return node                    self.column += len(text)            newlines = text.count('\n')



    def parse_ConstDecl(self):                self.pos += len(text)            if newlines:

        node = {'type': 'ConstDecl', 'children': []}

        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['const']).value})                continue                self.line += newlines

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})                            last_n = text.rfind('\n')

        node['children'].append({'type': 'Type', 'value': self.parse_Type()})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})            token_line = self.line                self.column = len(text) - last_n

        node['children'].append({'type': 'Expr', 'value': self.parse_Expr()})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})            token_col = self.column            else:

        return node

                            self.column += len(text)

    def parse_LetDecl(self):

        node = {'type': 'LetDecl', 'children': []}            if match_type == TOKEN_TYPE['ID']:            self.pos += len(text)

        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['let']).value})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})                ttype = KEYWORDS.get(text, TOKEN_TYPE['ID'])        self.tokens.append(Token(TOKEN_TYPE['EOF'], 'EOF', self.line, self.column))

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})

        node['children'].append({'type': 'Type', 'value': self.parse_Type()})            else:        return self.tokens

        if self.current and self.current.type == TOKEN_TYPE['ASSIGN']:

            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})                ttype = match_type

            node['children'].append({'type': 'Expr', 'value': self.parse_Expr()})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})            class Parser:

        return node

            self.tokens.append(Token(ttype, text, token_line, token_col))    def __init__(self, tokens):

    def parse_FunDecl(self):

        node = {'type': 'FunDecl', 'children': []}                    self.tokens = tokens

        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['fn']).value})

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})            newlines = text.count('\n')        self.i = 0

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})

        if self.current and self.current.type == TOKEN_TYPE['ID']:            if newlines:        self.current = tokens[0]

            node['children'].append(self.parse_Param())

            while self.current and self.current.type == TOKEN_TYPE['COMMA']:                self.line += newlines

                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COMMA']).value})

                node['children'].append(self.parse_Param())                last_n = text.rfind('\n')    def advance(self):

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})

        if self.current and self.current.type == TOKEN_TYPE['ARROW']:                self.column = len(text) - last_n        self.i += 1

            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ARROW']).value})

            node['children'].append({'type': 'Type', 'value': self.parse_Type()})            else:        if self.i < len(self.tokens):

        node['children'].append({'type': 'Block', 'value': self.parse_Block()})

        return node                self.column += len(text)            self.current = self.tokens[self.i]



    def parse_Param(self):                    return self.current

        node = {'type': 'Param', 'children': []}

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})            self.pos += len(text)

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})

        node['children'].append({'type': 'Type', 'value': self.parse_Type()})            def match(self, expected):

        return node

        self.tokens.append(Token(TOKEN_TYPE['EOF'], 'EOF', self.line, self.column))        if self.current.type == expected:

    def parse_Type(self):

        if self.current and self.current.type == KEYWORDS['int']:        return self.tokens            tok = self.current

            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['int']).value}

        elif self.current and self.current.type == KEYWORDS['bool']:            self.advance()

            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['bool']).value}

        elif self.current and self.current.type == KEYWORDS['string']:class Parser:            return tok

            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['string']).value}

        elif self.current and self.current.type == TOKEN_TYPE['LPAREN']:    def __init__(self, tokens):        raise ParserError(f"Expected {expected} but found {self.current.type} at L{getattr(self.current,'line',0)} C{getattr(self.current,'column',0)}")

            self.match(TOKEN_TYPE['LPAREN'])

            inner = self.parse_Type()        self.tokens = tokens

            self.match(TOKEN_TYPE['RPAREN'])

            t = {'type': 'ParenType', 'value': inner}        self.i = 0    def parse(self):

        elif self.current and self.current.type == TOKEN_TYPE['ID']:

            q = {'type': 'QualID', 'value': self.match(TOKEN_TYPE['ID']).value}        self.current = tokens[0] if tokens else None        ast = {'type': 'Program', 'children': []}

            while self.current and self.current.type == TOKEN_TYPE['DOT']:

                self.match(TOKEN_TYPE['DOT'])        self.parse_ModuleDecl(ast)

                q_val = self.match(TOKEN_TYPE['ID']).value

                q = {'type': 'QualID', 'value': q['value'] + '.' + q_val}    def advance(self):        self.parse_ImportList(ast)

            t = q

        else:        self.i += 1        self.parse_TopList(ast)

            line = getattr(self.current, 'line', 0) if self.current else 0

            column = getattr(self.current, 'column', 0) if self.current else 0        if self.i < len(self.tokens):        if self.current.type != TOKEN_TYPE['EOF']:

            raise ParserError(f"Tipo esperado en L{line} C{column}")

                    self.current = self.tokens[self.i]            raise ParserError(f"Unexpected token after program: {self.current}")

        while self.current and self.current.type == TOKEN_TYPE['LBRACKET']:

            self.match(TOKEN_TYPE['LBRACKET'])        return self.current        return ast

            self.match(TOKEN_TYPE['RBRACKET'])

            t = {'type': 'ArrayType', 'value': t}

        

        return t    def match(self, expected):    def parse_ModuleDecl(self, parent):



    def parse_Block(self):        if self.current and self.current.type == expected:        node = {'type': 'ModuleDecl', 'children': []}

        self.match(TOKEN_TYPE['LBRACE'])

        stmts = []            tok = self.current        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['module']).value})

        while self.current and self.current.type != TOKEN_TYPE['RBRACE']:

            stmts.append(self.parse_Stmt())            self.advance()        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

        self.match(TOKEN_TYPE['RBRACE'])

        return {'type': 'Block', 'stmts': stmts}            return tok        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})



    def parse_Stmt(self):        line = getattr(self.current, 'line', 0) if self.current else 0        parent['children'].append(node)

        if self.current and self.current.type == TOKEN_TYPE['LBRACE']:

            return {'type': 'Block', 'value': self.parse_Block()}        column = getattr(self.current, 'column', 0) if self.current else 0

        if self.current and self.current.type == KEYWORDS['let']:

            return self.parse_LetDecl()        current_type = self.current.type if self.current else 'EOF'    def parse_QualID(self, parent):

        if self.current and self.current.type == KEYWORDS['if']:

            self.match(KEYWORDS['if'])        raise ParserError(f"Expected {expected} but found {current_type} at L{line} C{column}")        node = {'type': 'QualID', 'children': []}

            self.match(TOKEN_TYPE['LPAREN'])

            cond = self.parse_Expr()        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

            self.match(TOKEN_TYPE['RPAREN'])

            then = self.parse_Stmt()    def parse(self):        while self.current.type == TOKEN_TYPE['DOT']:

            els = None

            if self.current and self.current.type == KEYWORDS['else']:        ast = {'type': 'Program', 'children': []}            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})

                self.match(KEYWORDS['else'])

                els = self.parse_Stmt()        self.parse_ModuleDecl(ast)            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

            return {'type': 'If', 'cond': cond, 'then': then, 'else': els}

        if self.current and self.current.type == KEYWORDS['while']:        self.parse_ImportList(ast)        parent['children'].append(node)

            self.match(KEYWORDS['while'])

            self.match(TOKEN_TYPE['LPAREN'])        self.parse_TopList(ast)

            cond = self.parse_Expr()

            self.match(TOKEN_TYPE['RPAREN'])        if self.current and self.current.type != TOKEN_TYPE['EOF']:    def parse_ImportList(self, parent):

            body = self.parse_Stmt()

            return {'type': 'While', 'cond': cond, 'body': body}            raise ParserError(f"Unexpected token after program: {self.current}")        node = {'type': 'ImportList', 'children': []}

        if self.current and self.current.type == KEYWORDS['return']:

            self.match(KEYWORDS['return'])        return ast        while self.current.type == KEYWORDS['import']:

            if self.current and self.current.type == TOKEN_TYPE['SEMI']:

                self.match(TOKEN_TYPE['SEMI'])            decl = {'type': 'ImportDecl', 'children': []}

                return {'type': 'Return', 'value': None}

            v = self.parse_Expr()    def parse_ModuleDecl(self, parent):            decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['import']).value})

            self.match(TOKEN_TYPE['SEMI'])

            return {'type': 'Return', 'value': v}        node = {'type': 'ModuleDecl', 'children': []}            self.parse_QualID(decl)

        

        expr = self.parse_Expr()        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['module']).value})            if self.current.type == KEYWORDS['as']:

        self.match(TOKEN_TYPE['SEMI'])

        return {'type': 'ExprStmt', 'expr': expr}        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})                decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['as']).value})



    def parse_Expr(self):        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})                decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

        return self.parse_Assign()

        parent['children'].append(node)            decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

    def parse_Assign(self):

        left = self.parse_Or()            node['children'].append(decl)

        if self.current and self.current.type == TOKEN_TYPE['ASSIGN']:

            self.match(TOKEN_TYPE['ASSIGN'])    def parse_QualID(self, parent):        parent['children'].append(node)

            right = self.parse_Assign()

            return {'type': 'Assign', 'left': left, 'right': right}        node = {'type': 'QualID', 'children': []}

        return left

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})    def parse_TopList(self, parent):

    def parse_Or(self):

        left = self.parse_And()        while self.current and self.current.type == TOKEN_TYPE['DOT']:        node = {'type': 'TopList', 'children': []}

        while self.current and self.current.type == TOKEN_TYPE['OR']:

            self.match(TOKEN_TYPE['OR'])            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})        tops = set([KEYWORDS['type'], KEYWORDS['struct'], KEYWORDS['const'], KEYWORDS['let'], KEYWORDS['fn']])

            right = self.parse_And()

            left = {'type': 'Or', 'left': left, 'right': right}            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})        while self.current.type in tops:

        return left

        parent['children'].append(node)            if self.current.type == KEYWORDS['type']:

    def parse_And(self):

        left = self.parse_Eq()                node['children'].append(self.parse_TypeDecl())

        while self.current and self.current.type == TOKEN_TYPE['AND']:

            self.match(TOKEN_TYPE['AND'])    def parse_ImportList(self, parent):            elif self.current.type == KEYWORDS['struct']:

            right = self.parse_Eq()

            left = {'type': 'And', 'left': left, 'right': right}        node = {'type': 'ImportList', 'children': []}                node['children'].append(self.parse_StructDecl())

        return left

        while self.current and self.current.type == KEYWORDS['import']:            elif self.current.type == KEYWORDS['const']:

    def parse_Eq(self):

        left = self.parse_Rel()            decl = {'type': 'ImportDecl', 'children': []}                node['children'].append(self.parse_ConstDecl())

        while self.current and self.current.type in (TOKEN_TYPE['EQ'], TOKEN_TYPE['NE']):

            if self.current.type == TOKEN_TYPE['EQ']:            decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['import']).value})            elif self.current.type == KEYWORDS['let']:

                self.match(TOKEN_TYPE['EQ'])

                right = self.parse_Rel()            self.parse_QualID(decl)                node['children'].append(self.parse_LetDecl())

                left = {'type': 'Eq', 'left': left, 'right': right}

            else:            if self.current and self.current.type == KEYWORDS['as']:            elif self.current.type == KEYWORDS['fn']:

                self.match(TOKEN_TYPE['NE'])

                right = self.parse_Rel()                decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['as']).value})                node['children'].append(self.parse_FunDecl())

                left = {'type': 'Ne', 'left': left, 'right': right}

        return left                decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})        parent['children'].append(node)



    def parse_Rel(self):            decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

        left = self.parse_Add()

        while self.current and self.current.type in (TOKEN_TYPE['LT'], TOKEN_TYPE['LE'], TOKEN_TYPE['GT'], TOKEN_TYPE['GE']):            node['children'].append(decl)    def parse_TypeDecl(self):

            t = self.current.type

            self.advance()        parent['children'].append(node)        node = {'type': 'TypeDecl', 'children': []}

            right = self.parse_Add()

            left = {'type': 'Rel', 'op': t, 'left': left, 'right': right}        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['type']).value})

        return left

    def parse_TopList(self, parent):        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

    def parse_Add(self):

        left = self.parse_Mul()        node = {'type': 'TopList', 'children': []}        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})

        while self.current and self.current.type in (TOKEN_TYPE['ADD'], TOKEN_TYPE['SUB']):

            t = self.current.type        tops = {KEYWORDS['type'], KEYWORDS['struct'], KEYWORDS['const'], KEYWORDS['let'], KEYWORDS['fn']}        node['children'].append({'type': 'Type', 'value': self.parse_Type()})

            self.advance()

            right = self.parse_Mul()        while self.current and self.current.type in tops:        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

            left = {'type': 'Add' if t == TOKEN_TYPE['ADD'] else 'Sub', 'left': left, 'right': right}

        return left            if self.current.type == KEYWORDS['type']:        return node



    def parse_Mul(self):                node['children'].append(self.parse_TypeDecl())

        left = self.parse_Unary()

        while self.current and self.current.type in (TOKEN_TYPE['MUL'], TOKEN_TYPE['DIV'], TOKEN_TYPE['MOD']):            elif self.current.type == KEYWORDS['struct']:    def parse_StructDecl(self):

            t = self.current.type

            self.advance()                node['children'].append(self.parse_StructDecl())        node = {'type': 'StructDecl', 'children': []}

            right = self.parse_Unary()

            op_name = 'Mul' if t == TOKEN_TYPE['MUL'] else ('Div' if t == TOKEN_TYPE['DIV'] else 'Mod')            elif self.current.type == KEYWORDS['const']:        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['struct']).value})

            left = {'type': op_name, 'left': left, 'right': right}

        return left                node['children'].append(self.parse_ConstDecl())        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})



    def parse_Unary(self):            elif self.current.type == KEYWORDS['let']:        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACE']).value})

        if self.current and self.current.type == TOKEN_TYPE['NOT']:

            self.match(TOKEN_TYPE['NOT'])                node['children'].append(self.parse_LetDecl())        fields = []

            return {'type': 'Not', 'expr': self.parse_Unary()}

        if self.current and self.current.type == TOKEN_TYPE['SUB']:            elif self.current.type == KEYWORDS['fn']:        while self.current.type == TOKEN_TYPE['ID']:

            self.match(TOKEN_TYPE['SUB'])

            return {'type': 'Neg', 'expr': self.parse_Unary()}                node['children'].append(self.parse_FunDecl())            f = {'type': 'Field', 'children': []}

        return self.parse_Postfix()

        parent['children'].append(node)            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

    def parse_Postfix(self):

        node = self.parse_Primary()            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})

        while True:

            if self.current and self.current.type == TOKEN_TYPE['LPAREN']:    def parse_TypeDecl(self):            f['children'].append({'type': 'Type', 'value': self.parse_Type()})

                self.match(TOKEN_TYPE['LPAREN'])

                args = []        node = {'type': 'TypeDecl', 'children': []}            fields.append(f)

                if self.current and self.current.type != TOKEN_TYPE['RPAREN']:

                    args.append(self.parse_Expr())        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['type']).value})        node['children'].extend(fields)

                    while self.current and self.current.type == TOKEN_TYPE['COMMA']:

                        self.match(TOKEN_TYPE['COMMA'])        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})

                        args.append(self.parse_Expr())

                self.match(TOKEN_TYPE['RPAREN'])        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

                node = {'type': 'Call', 'fn': node, 'args': args}

            elif self.current and self.current.type == TOKEN_TYPE['LBRACKET']:        node['children'].append({'type': 'Type', 'value': self.parse_Type()})        return node

                self.match(TOKEN_TYPE['LBRACKET'])

                idx = self.parse_Expr()        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

                self.match(TOKEN_TYPE['RBRACKET'])

                node = {'type': 'Index', 'expr': node, 'index': idx}        return node    def parse_ConstDecl(self):

            elif self.current and self.current.type == TOKEN_TYPE['DOT']:

                self.match(TOKEN_TYPE['DOT'])        node = {'type': 'ConstDecl', 'children': []}

                name = self.match(TOKEN_TYPE['ID']).value

                node = {'type': 'Field', 'expr': node, 'field': name}    def parse_StructDecl(self):        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['const']).value})

            else:

                break        node = {'type': 'StructDecl', 'children': []}        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

        return node

        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['struct']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})

    def parse_Primary(self):

        if not self.current:        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})        node['children'].append({'type': 'Type', 'value': self.parse_Type()})

            raise ParserError("Unexpected end of input")

                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACE']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})

        t = self.current.type

        if t == TOKEN_TYPE['ID']:        while self.current and self.current.type == TOKEN_TYPE['ID']:        node['children'].append({'type': 'Expr', 'value': self.parse_Expr()})

            v = self.match(TOKEN_TYPE['ID']).value

            return {'type': 'Var', 'name': v}            f = {'type': 'Field', 'children': []}        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

        if t == TOKEN_TYPE['NUM_LIT']:

            v = self.match(TOKEN_TYPE['NUM_LIT']).value            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})        return node

            return {'type': 'Num', 'value': v}

        if t == TOKEN_TYPE['STRING_LIT']:            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})

            v = self.match(TOKEN_TYPE['STRING_LIT']).value

            return {'type': 'Str', 'value': v}            f['children'].append({'type': 'Type', 'value': self.parse_Type()})    def parse_LetDecl(self):

        if t in ('BOOL_LIT', KEYWORDS['true'], KEYWORDS['false']):

            v = self.match(t).value            node['children'].append(f)        node = {'type': 'LetDecl', 'children': []}

            return {'type': 'Bool', 'value': v}

        if t == TOKEN_TYPE['LPAREN']:        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['let']).value})

            self.match(TOKEN_TYPE['LPAREN'])

            v = self.parse_Expr()        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

            self.match(TOKEN_TYPE['RPAREN'])

            return v        return node        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})

        

        raise ParserError(f"Primary expression expected at {self.current}")        node['children'].append({'type': 'Type', 'value': self.parse_Type()})



if HAS_PYQT:    def parse_ConstDecl(self):        if self.current.type == TOKEN_TYPE['ASSIGN']:

    class CompilerIDE(QMainWindow):

        def __init__(self):        node = {'type': 'ConstDecl', 'children': []}            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})

            super().__init__()

            self.setWindowTitle('Mini Compilador LL(1)')        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['const']).value})            node['children'].append({'type': 'Expr', 'value': self.parse_Expr()})

            self.setGeometry(100, 100, 1000, 700)

            self.current_filepath = None        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

            self.init_ui()

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})        return node

        def init_ui(self):

            main = QWidget()        node['children'].append({'type': 'Type', 'value': self.parse_Type()})

            layout = QVBoxLayout(main)

            self.setCentralWidget(main)        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})    def parse_FunDecl(self):

            

            self.code_editor = QTextEdit()        node['children'].append({'type': 'Expr', 'value': self.parse_Expr()})        node = {'type': 'FunDecl', 'children': []}

            self.code_editor.setPlaceholderText("Escribe tu código aquí...")

            layout.addWidget(self.code_editor)        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['fn']).value})

            

            btn_layout = QHBoxLayout()        return node        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

            self.btn_compile = QPushButton('Compilar')

            self.btn_compile.clicked.connect(self.compile_code)        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})

            btn_layout.addWidget(self.btn_compile)

            layout.addLayout(btn_layout)    def parse_LetDecl(self):        params = []

            

            self.token_table = QTableWidget()        node = {'type': 'LetDecl', 'children': []}        if self.current.type == TOKEN_TYPE['ID']:

            self.token_table.setColumnCount(4)

            self.token_table.setHorizontalHeaderLabels(['Línea', 'Columna', 'Tipo', 'Valor'])        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['let']).value})            params.append(self.parse_Param())

            layout.addWidget(self.token_table)

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})            while self.current.type == TOKEN_TYPE['COMMA']:

        def compile_code(self):

            src = self.code_editor.toPlainText()        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})                self.match(TOKEN_TYPE['COMMA'])

            try:

                lexer = Lexer(src)        node['children'].append({'type': 'Type', 'value': self.parse_Type()})                params.append(self.parse_Param())

                tokens = lexer.tokenize()

                        if self.current and self.current.type == TOKEN_TYPE['ASSIGN']:        node['children'].extend(params)

                self.token_table.setRowCount(len(tokens))

                for i, tok in enumerate(tokens):            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})

                    self.token_table.setItem(i, 0, QTableWidgetItem(str(tok.line)))

                    self.token_table.setItem(i, 1, QTableWidgetItem(str(tok.column)))            node['children'].append({'type': 'Expr', 'value': self.parse_Expr()})        if self.current.type == TOKEN_TYPE['ARROW']:

                    self.token_table.setItem(i, 2, QTableWidgetItem(tok.type))

                    self.token_table.setItem(i, 3, QTableWidgetItem(tok.value))        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ARROW']).value})

                

                parser = Parser(tokens)        return node            node['children'].append({'type': 'Type', 'value': self.parse_Type()})

                ast = parser.parse()

                QMessageBox.information(self, 'Éxito', 'Compilación exitosa!')        node['children'].append({'type': 'Block', 'value': self.parse_Block()})

                

            except Exception as e:    def parse_FunDecl(self):        return node

                QMessageBox.critical(self, 'Error', str(e))

        node = {'type': 'FunDecl', 'children': []}

else:

    class CompilerIDE:        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['fn']).value})    def parse_Param(self):

        pass

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})        node = {'type': 'Param', 'children': []}

if __name__ == '__main__' and HAS_PYQT:

    app = QCoreApplication.instance()        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})

    if app is None:

        app = QApplication(sys.argv)        if self.current and self.current.type == TOKEN_TYPE['ID']:        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})

    win = CompilerIDE()

    win.show()            node['children'].append(self.parse_Param())        node['children'].append({'type': 'Type', 'value': self.parse_Type()})

    sys.exit(app.exec())
            while self.current and self.current.type == TOKEN_TYPE['COMMA']:        return node

                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COMMA']).value})

                node['children'].append(self.parse_Param())    def parse_Type(self):

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})        if self.current.type == KEYWORDS['int']:

        if self.current and self.current.type == TOKEN_TYPE['ARROW']:            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['int']).value}

            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ARROW']).value})        elif self.current.type == KEYWORDS['bool']:

            node['children'].append({'type': 'Type', 'value': self.parse_Type()})            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['bool']).value}

        node['children'].append({'type': 'Block', 'value': self.parse_Block()})        elif self.current.type == KEYWORDS['string']:

        return node            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['string']).value}

        elif self.current.type == TOKEN_TYPE['LPAREN']:

    def parse_Param(self):            self.match(TOKEN_TYPE['LPAREN'])

        node = {'type': 'Param', 'children': []}            inner = self.parse_Type()

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})            self.match(TOKEN_TYPE['RPAREN'])

        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})            t = {'type': 'ParenType', 'value': inner}

        node['children'].append({'type': 'Type', 'value': self.parse_Type()})        elif self.current.type == TOKEN_TYPE['ID']:

        return node            q = {'type': 'QualID', 'value': self.match(TOKEN_TYPE['ID']).value}

            while self.current.type == TOKEN_TYPE['DOT']:

    def parse_Type(self):                self.match(TOKEN_TYPE['DOT'])

        if self.current and self.current.type == KEYWORDS['int']:                q_val = self.match(TOKEN_TYPE['ID']).value

            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['int']).value}                q = {'type': 'QualID', 'value': q['value'] + '.' + q_val}

        elif self.current and self.current.type == KEYWORDS['bool']:            t = q

            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['bool']).value}        else:

        elif self.current and self.current.type == KEYWORDS['string']:            raise ParserError(f"Tipo esperado en L{getattr(self.current,'line',0)} C{getattr(self.current,'column',0)}")

            t = {'type': 'SimpleType', 'value': self.match(KEYWORDS['string']).value}        while self.current.type == TOKEN_TYPE['LBRACKET']:

        elif self.current and self.current.type == TOKEN_TYPE['LPAREN']:            self.match(TOKEN_TYPE['LBRACKET'])

            self.match(TOKEN_TYPE['LPAREN'])            self.match(TOKEN_TYPE['RBRACKET'])

            inner = self.parse_Type()            t = {'type': 'ArrayType', 'value': t}

            self.match(TOKEN_TYPE['RPAREN'])        return t

            t = {'type': 'ParenType', 'value': inner}

        elif self.current and self.current.type == TOKEN_TYPE['ID']:    def parse_Block(self):

            q = {'type': 'QualID', 'value': self.match(TOKEN_TYPE['ID']).value}        self.match(TOKEN_TYPE['LBRACE'])

            while self.current and self.current.type == TOKEN_TYPE['DOT']:        stmts = []

                self.match(TOKEN_TYPE['DOT'])        while self.current.type != TOKEN_TYPE['RBRACE']:

                q_val = self.match(TOKEN_TYPE['ID']).value            stmts.append(self.parse_Stmt())

                q = {'type': 'QualID', 'value': q['value'] + '.' + q_val}        self.match(TOKEN_TYPE['RBRACE'])

            t = q        return {'type': 'Block', 'stmts': stmts}

        else:

            line = getattr(self.current, 'line', 0) if self.current else 0    def parse_Stmt(self):

            column = getattr(self.current, 'column', 0) if self.current else 0        if self.current.type == TOKEN_TYPE['LBRACE']:

            raise ParserError(f"Tipo esperado en L{line} C{column}")            return {'type': 'Block', 'value': self.parse_Block()}

                if self.current.type == KEYWORDS['let']:

        while self.current and self.current.type == TOKEN_TYPE['LBRACKET']:            return self.parse_LetDecl()

            self.match(TOKEN_TYPE['LBRACKET'])        if self.current.type == KEYWORDS['if']:

            self.match(TOKEN_TYPE['RBRACKET'])            self.match(KEYWORDS['if'])

            t = {'type': 'ArrayType', 'value': t}            self.match(TOKEN_TYPE['LPAREN'])

                    cond = self.parse_Expr()

        return t            self.match(TOKEN_TYPE['RPAREN'])

            then = self.parse_Stmt()

    def parse_Block(self):            els = None

        self.match(TOKEN_TYPE['LBRACE'])            if self.current.type == KEYWORDS['else']:

        stmts = []                self.match(KEYWORDS['else'])

        while self.current and self.current.type != TOKEN_TYPE['RBRACE']:                els = self.parse_Stmt()

            stmts.append(self.parse_Stmt())            return {'type': 'If', 'cond': cond, 'then': then, 'else': els}

        self.match(TOKEN_TYPE['RBRACE'])        if self.current.type == KEYWORDS['while']:

        return {'type': 'Block', 'stmts': stmts}            self.match(KEYWORDS['while'])

            self.match(TOKEN_TYPE['LPAREN'])

    def parse_Stmt(self):            cond = self.parse_Expr()

        if self.current and self.current.type == TOKEN_TYPE['LBRACE']:            self.match(TOKEN_TYPE['RPAREN'])

            return {'type': 'Block', 'value': self.parse_Block()}            body = self.parse_Stmt()

        if self.current and self.current.type == KEYWORDS['let']:            return {'type': 'While', 'cond': cond, 'body': body}

            return self.parse_LetDecl()        if self.current.type == KEYWORDS['return']:

        if self.current and self.current.type == KEYWORDS['if']:            self.match(KEYWORDS['return'])

            self.match(KEYWORDS['if'])            if self.current.type == TOKEN_TYPE['SEMI']:

            self.match(TOKEN_TYPE['LPAREN'])                self.match(TOKEN_TYPE['SEMI'])

            cond = self.parse_Expr()                return {'type': 'Return', 'value': None}

            self.match(TOKEN_TYPE['RPAREN'])            v = self.parse_Expr()

            then = self.parse_Stmt()            self.match(TOKEN_TYPE['SEMI'])

            els = None            return {'type': 'Return', 'value': v}

            if self.current and self.current.type == KEYWORDS['else']:        expr = self.parse_Expr()

                self.match(KEYWORDS['else'])        self.match(TOKEN_TYPE['SEMI'])

                els = self.parse_Stmt()        return {'type': 'ExprStmt', 'expr': expr}

            return {'type': 'If', 'cond': cond, 'then': then, 'else': els}

        if self.current and self.current.type == KEYWORDS['while']:    def parse_Expr(self):

            self.match(KEYWORDS['while'])        return self.parse_Assign()

            self.match(TOKEN_TYPE['LPAREN'])

            cond = self.parse_Expr()    def parse_Assign(self):

            self.match(TOKEN_TYPE['RPAREN'])        left = self.parse_Or()

            body = self.parse_Stmt()        if self.current.type == TOKEN_TYPE['ASSIGN']:

            return {'type': 'While', 'cond': cond, 'body': body}            self.match(TOKEN_TYPE['ASSIGN'])

        if self.current and self.current.type == KEYWORDS['return']:            right = self.parse_Assign()

            self.match(KEYWORDS['return'])            return {'type': 'Assign', 'left': left, 'right': right}

            if self.current and self.current.type == TOKEN_TYPE['SEMI']:        return left

                self.match(TOKEN_TYPE['SEMI'])

                return {'type': 'Return', 'value': None}    def parse_Or(self):

            v = self.parse_Expr()        left = self.parse_And()

            self.match(TOKEN_TYPE['SEMI'])        while self.current.type == TOKEN_TYPE['OR']:

            return {'type': 'Return', 'value': v}            self.match(TOKEN_TYPE['OR'])

                    right = self.parse_And()

        expr = self.parse_Expr()            left = {'type': 'Or', 'left': left, 'right': right}

        self.match(TOKEN_TYPE['SEMI'])        return left

        return {'type': 'ExprStmt', 'expr': expr}

    def parse_And(self):

    def parse_Expr(self):        left = self.parse_Eq()

        return self.parse_Assign()        while self.current.type == TOKEN_TYPE['AND']:

            self.match(TOKEN_TYPE['AND'])

    def parse_Assign(self):            right = self.parse_Eq()

        left = self.parse_Or()            left = {'type': 'And', 'left': left, 'right': right}

        if self.current and self.current.type == TOKEN_TYPE['ASSIGN']:        return left

            self.match(TOKEN_TYPE['ASSIGN'])

            right = self.parse_Assign()    def parse_Eq(self):

            return {'type': 'Assign', 'left': left, 'right': right}        left = self.parse_Rel()

        return left        while self.current.type in (TOKEN_TYPE['EQ'], TOKEN_TYPE['NE']):

            if self.current.type == TOKEN_TYPE['EQ']:

    def parse_Or(self):                self.match(TOKEN_TYPE['EQ'])

        left = self.parse_And()                right = self.parse_Rel()

        while self.current and self.current.type == TOKEN_TYPE['OR']:                left = {'type': 'Eq', 'left': left, 'right': right}

            self.match(TOKEN_TYPE['OR'])            else:

            right = self.parse_And()                self.match(TOKEN_TYPE['NE'])

            left = {'type': 'Or', 'left': left, 'right': right}                right = self.parse_Rel()

        return left                left = {'type': 'Ne', 'left': left, 'right': right}

        return left

    def parse_And(self):

        left = self.parse_Eq()    def parse_Rel(self):

        while self.current and self.current.type == TOKEN_TYPE['AND']:        left = self.parse_Add()

            self.match(TOKEN_TYPE['AND'])        while self.current.type in (TOKEN_TYPE['LT'], TOKEN_TYPE['LE'], TOKEN_TYPE['GT'], TOKEN_TYPE['GE']):

            right = self.parse_Eq()            t = self.current.type

            left = {'type': 'And', 'left': left, 'right': right}            self.advance()

        return left            right = self.parse_Add()

            left = {'type': 'Rel', 'op': t, 'left': left, 'right': right}

    def parse_Eq(self):        return left

        left = self.parse_Rel()

        while self.current and self.current.type in (TOKEN_TYPE['EQ'], TOKEN_TYPE['NE']):    def parse_Add(self):

            if self.current.type == TOKEN_TYPE['EQ']:        left = self.parse_Mul()

                self.match(TOKEN_TYPE['EQ'])        while self.current.type in (TOKEN_TYPE['ADD'], TOKEN_TYPE['SUB']):

                right = self.parse_Rel()            t = self.current.type

                left = {'type': 'Eq', 'left': left, 'right': right}            self.advance()

            else:            right = self.parse_Mul()

                self.match(TOKEN_TYPE['NE'])            left = {'type': 'Add' if t == TOKEN_TYPE['ADD'] else 'Sub', 'left': left, 'right': right}

                right = self.parse_Rel()        return left

                left = {'type': 'Ne', 'left': left, 'right': right}

        return left    def parse_Mul(self):

        left = self.parse_Unary()

    def parse_Rel(self):        while self.current.type in (TOKEN_TYPE['MUL'], TOKEN_TYPE['DIV'], TOKEN_TYPE['MOD']):

        left = self.parse_Add()            t = self.current.type

        while self.current and self.current.type in (TOKEN_TYPE['LT'], TOKEN_TYPE['LE'], TOKEN_TYPE['GT'], TOKEN_TYPE['GE']):            self.advance()

            t = self.current.type            right = self.parse_Unary()

            self.advance()            left = {'type': 'Mul' if t == TOKEN_TYPE['MUL'] else ('Div' if t == TOKEN_TYPE['DIV'] else 'Mod'), 'left': left, 'right': right}

            right = self.parse_Add()        return left

            left = {'type': 'Rel', 'op': t, 'left': left, 'right': right}

        return left    def parse_Unary(self):

        if self.current.type == TOKEN_TYPE['NOT']:

    def parse_Add(self):            self.match(TOKEN_TYPE['NOT'])

        left = self.parse_Mul()            return {'type': 'Not', 'expr': self.parse_Unary()}

        while self.current and self.current.type in (TOKEN_TYPE['ADD'], TOKEN_TYPE['SUB']):        if self.current.type == TOKEN_TYPE['SUB']:

            t = self.current.type            self.match(TOKEN_TYPE['SUB'])

            self.advance()            return {'type': 'Neg', 'expr': self.parse_Unary()}

            right = self.parse_Mul()        return self.parse_Postfix()

            left = {'type': 'Add' if t == TOKEN_TYPE['ADD'] else 'Sub', 'left': left, 'right': right}

        return left    def parse_Postfix(self):

        node = self.parse_Primary()

    def parse_Mul(self):        while True:

        left = self.parse_Unary()            if self.current.type == TOKEN_TYPE['LPAREN']:

        while self.current and self.current.type in (TOKEN_TYPE['MUL'], TOKEN_TYPE['DIV'], TOKEN_TYPE['MOD']):                self.match(TOKEN_TYPE['LPAREN'])

            t = self.current.type                args = []

            self.advance()                if self.current.type != TOKEN_TYPE['RPAREN']:

            right = self.parse_Unary()                    args.append(self.parse_Expr())

            op_name = 'Mul' if t == TOKEN_TYPE['MUL'] else ('Div' if t == TOKEN_TYPE['DIV'] else 'Mod')                    while self.current.type == TOKEN_TYPE['COMMA']:

            left = {'type': op_name, 'left': left, 'right': right}                        self.match(TOKEN_TYPE['COMMA'])

        return left                        args.append(self.parse_Expr())

                self.match(TOKEN_TYPE['RPAREN'])

    def parse_Unary(self):                node = {'type': 'Call', 'fn': node, 'args': args}

        if self.current and self.current.type == TOKEN_TYPE['NOT']:            elif self.current.type == TOKEN_TYPE['LBRACKET']:

            self.match(TOKEN_TYPE['NOT'])                self.match(TOKEN_TYPE['LBRACKET'])

            return {'type': 'Not', 'expr': self.parse_Unary()}                idx = self.parse_Expr()

        if self.current and self.current.type == TOKEN_TYPE['SUB']:                self.match(TOKEN_TYPE['RBRACKET'])

            self.match(TOKEN_TYPE['SUB'])                node = {'type': 'Index', 'expr': node, 'index': idx}

            return {'type': 'Neg', 'expr': self.parse_Unary()}            elif self.current.type == TOKEN_TYPE['DOT']:

        return self.parse_Postfix()                self.match(TOKEN_TYPE['DOT'])

                name = self.match(TOKEN_TYPE['ID']).value

    def parse_Postfix(self):                node = {'type': 'Field', 'expr': node, 'field': name}

        node = self.parse_Primary()            else:

        while True:                break

            if self.current and self.current.type == TOKEN_TYPE['LPAREN']:        return node

                self.match(TOKEN_TYPE['LPAREN'])

                args = []    def parse_Primary(self):

                if self.current and self.current.type != TOKEN_TYPE['RPAREN']:        t = self.current.type

                    args.append(self.parse_Expr())        if t == TOKEN_TYPE['ID']:

                    while self.current and self.current.type == TOKEN_TYPE['COMMA']:            v = self.match(TOKEN_TYPE['ID']).value

                        self.match(TOKEN_TYPE['COMMA'])            return {'type': 'Var', 'name': v}

                        args.append(self.parse_Expr())        if t == TOKEN_TYPE['NUM_LIT']:

                self.match(TOKEN_TYPE['RPAREN'])            v = self.match(TOKEN_TYPE['NUM_LIT']).value

                node = {'type': 'Call', 'fn': node, 'args': args}            return {'type': 'Num', 'value': v}

            elif self.current and self.current.type == TOKEN_TYPE['LBRACKET']:        if t == TOKEN_TYPE['STRING_LIT']:

                self.match(TOKEN_TYPE['LBRACKET'])            v = self.match(TOKEN_TYPE['STRING_LIT']).value

                idx = self.parse_Expr()            return {'type': 'Str', 'value': v}

                self.match(TOKEN_TYPE['RBRACKET'])        if t == KEYWORDS['true'] or t == KEYWORDS['false'] or t == 'BOOL_LIT':

                node = {'type': 'Index', 'expr': node, 'index': idx}            v = self.match(t).value

            elif self.current and self.current.type == TOKEN_TYPE['DOT']:            return {'type': 'Bool', 'value': v}

                self.match(TOKEN_TYPE['DOT'])        if t == TOKEN_TYPE['LPAREN']:

                name = self.match(TOKEN_TYPE['ID']).value            self.match(TOKEN_TYPE['LPAREN'])

                node = {'type': 'Field', 'expr': node, 'field': name}            v = self.parse_Expr()

            else:            self.match(TOKEN_TYPE['RPAREN'])

                break            return v

        return node        raise ParserError(f"Primary expression expected at {self.current}")



    def parse_Primary(self):if HAS_PYQT:

        if not self.current:    class CompilerIDE(QMainWindow):

            raise ParserError("Unexpected end of input")        def __init__(self):

                    super().__init__()

        t = self.current.type            self.setWindowTitle('Mini Compilador LL(1)')

        if t == TOKEN_TYPE['ID']:            self.setGeometry(100, 100, 1000, 700)

            v = self.match(TOKEN_TYPE['ID']).value            self.current_filepath = None

            return {'type': 'Var', 'name': v}            self.init_ui()

        if t == TOKEN_TYPE['NUM_LIT']:

            v = self.match(TOKEN_TYPE['NUM_LIT']).value        def init_ui(self):

            return {'type': 'Num', 'value': v}            main = QWidget()

        if t == TOKEN_TYPE['STRING_LIT']:            layout = QVBoxLayout(main)

            v = self.match(TOKEN_TYPE['STRING_LIT']).value            self.setCentralWidget(main)

            return {'type': 'Str', 'value': v}            self.code_editor = QTextEdit()

        if t in ('BOOL_LIT', KEYWORDS['true'], KEYWORDS['false']):            layout.addWidget(self.code_editor)

            v = self.match(t).value            btn_layout = QHBoxLayout()

            return {'type': 'Bool', 'value': v}            self.btn_compile = QPushButton('Compilar')

        if t == TOKEN_TYPE['LPAREN']:            self.btn_compile.clicked.connect(self.compile_code)

            self.match(TOKEN_TYPE['LPAREN'])            btn_layout.addWidget(self.btn_compile)

            v = self.parse_Expr()            layout.addLayout(btn_layout)

            self.match(TOKEN_TYPE['RPAREN'])            self.token_table = QTableWidget()

            return v            layout.addWidget(self.token_table)

        

        raise ParserError(f"Primary expression expected at {self.current}")        def compile_code(self):

            src = self.code_editor.toPlainText()

if HAS_PYQT:            try:

    class CompilerIDE(QMainWindow):                lexer = Lexer(src)

        def __init__(self):                tokens = lexer.tokenize()

            super().__init__()                self.token_table.setRowCount(len(tokens))

            self.setWindowTitle('Mini Compilador LL(1)')                for i, tok in enumerate(tokens):

            self.setGeometry(100, 100, 1000, 700)                    self.token_table.setItem(i, 0, QTableWidgetItem(str(tok.line)))

            self.current_filepath = None                    self.token_table.setItem(i, 1, QTableWidgetItem(str(tok.column)))

            self.init_ui()                    self.token_table.setItem(i, 2, QTableWidgetItem(tok.type))

                    self.token_table.setItem(i, 3, QTableWidgetItem(tok.value))

        def init_ui(self):            except Exception as e:

            main = QWidget()                QMessageBox.critical(self, 'Error', str(e))

            layout = QVBoxLayout(main)

            self.setCentralWidget(main)else:

                class CompilerIDE:

            self.code_editor = QTextEdit()        pass

            self.code_editor.setPlaceholderText("Escribe tu código aquí...")

            layout.addWidget(self.code_editor)if __name__ == '__main__' and HAS_PYQT:

                app = QCoreApplication.instance()

            btn_layout = QHBoxLayout()    if app is None:

            self.btn_compile = QPushButton('Compilar')        app = QApplication(sys.argv)

            self.btn_compile.clicked.connect(self.compile_code)    win = CompilerIDE()

            btn_layout.addWidget(self.btn_compile)    win.show()

            layout.addLayout(btn_layout)    sys.exit(app.exec())

            import re

            self.token_table = QTableWidget()import sys

            self.token_table.setColumnCount(4)

            self.token_table.setHorizontalHeaderLabels(['Línea', 'Columna', 'Tipo', 'Valor'])try:

            layout.addWidget(self.token_table)    from PyQt6.QtWidgets import QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout, QTextEdit, QPushButton, QTableWidget, QTableWidgetItem, QMessageBox

    from PyQt6.QtCore import QCoreApplication

        def compile_code(self):    from PyQt6.QtGui import QFont, QColor

            src = self.code_editor.toPlainText()    HAS_PYQT = True

            try:except Exception:

                lexer = Lexer(src)    HAS_PYQT = False

                tokens = lexer.tokenize()

                KEYWORDS = {

                self.token_table.setRowCount(len(tokens))    'module': 'KEYWORD_MODULE', 'import': 'KEYWORD_IMPORT', 'as': 'KEYWORD_AS',

                for i, tok in enumerate(tokens):    'type': 'KEYWORD_TYPE', 'struct': 'KEYWORD_STRUCT', 'const': 'KEYWORD_CONST',

                    self.token_table.setItem(i, 0, QTableWidgetItem(str(tok.line)))    'let': 'KEYWORD_LET', 'fn': 'KEYWORD_FN', 'int': 'KEYWORD_INT',

                    self.token_table.setItem(i, 1, QTableWidgetItem(str(tok.column)))    'bool': 'KEYWORD_BOOL', 'string': 'KEYWORD_STRING', 'if': 'KEYWORD_IF',

                    self.token_table.setItem(i, 2, QTableWidgetItem(tok.type))    'else': 'KEYWORD_ELSE', 'while': 'KEYWORD_WHILE', 'return': 'KEYWORD_RETURN',

                    self.token_table.setItem(i, 3, QTableWidgetItem(tok.value))    'true': 'BOOL_LIT', 'false': 'BOOL_LIT',

                }

                parser = Parser(tokens)

                ast = parser.parse()TOKEN_TYPE = {

                QMessageBox.information(self, 'Éxito', 'Compilación exitosa!')    'ID': 'ID', 'NUM_LIT': 'NUM_LIT', 'STRING_LIT': 'STRING_LIT',

                    'ASSIGN': 'OP_ASSIGN', 'ARROW': 'OP_ARROW', 'EQ': 'OP_EQ', 'NE': 'OP_NE',

            except Exception as e:    'LE': 'OP_LE', 'GE': 'OP_GE', 'AND': 'OP_AND', 'OR': 'OP_OR',

                QMessageBox.critical(self, 'Error', str(e))    'ADD': 'OP_ADD', 'SUB': 'OP_SUB', 'MUL': 'OP_MUL', 'DIV': 'OP_DIV', 'MOD': 'OP_MOD', 'NOT': 'OP_NOT',

    'LT': 'OP_LT', 'GT': 'OP_GT', 'LPAREN': 'SEP_LPAREN', 'RPAREN': 'SEP_RPAREN',

else:    'LBRACE': 'SEP_LBRACE', 'RBRACE': 'SEP_RBRACE', 'LBRACKET': 'SEP_LBRACKET', 'RBRACKET': 'SEP_RBRACKET',

    class CompilerIDE:    'COMMA': 'SEP_COMMA', 'COLON': 'SEP_COLON', 'SEMI': 'SEP_SEMI', 'DOT': 'SEP_DOT', 'EOF': 'EOF',

        pass}



if __name__ == '__main__' and HAS_PYQT:TOKEN_REGEX = [

    app = QCoreApplication.instance()    (r'//[^\n]*', 'COMMENT'),

    if app is None:    (r'"([^"\\]|\\.)*"', TOKEN_TYPE['STRING_LIT']),

        app = QApplication(sys.argv)    (r'->', TOKEN_TYPE['ARROW']),

    win = CompilerIDE()    (r'==', TOKEN_TYPE['EQ']),

    win.show()    (r'!=', TOKEN_TYPE['NE']),

    sys.exit(app.exec())    (r'<=', TOKEN_TYPE['LE']),
    (r'>=', TOKEN_TYPE['GE']),
    (r'&&', TOKEN_TYPE['AND']),
    (r'\|\|', TOKEN_TYPE['OR']),
    (r'\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?|\.\d+(?:[eE][+\-]?\d+)?', TOKEN_TYPE['NUM_LIT']),
    (r'[a-zA-Z_][a-zA-Z0-9_]*', TOKEN_TYPE['ID']),
    (r'==', TOKEN_TYPE['EQ']),
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
                raise LexerError(f"Unrecognized symbol '{ch}' at L{self.line} C{self.column}")
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
        self.current = tokens[0]

    def advance(self):
        self.i += 1
        if self.i < len(self.tokens):
            self.current = self.tokens[self.i]
        return self.current

    def match(self, expected):
        if self.current.type == expected:
            tok = self.current
            self.advance()
            return tok
        raise ParserError(f"Expected {expected} but found {self.current.type} at L{getattr(self.current,'line',0)} C{getattr(self.current,'column',0)}")

    def parse(self):
        ast = {'type': 'Program', 'children': []}
        self.parse_ModuleDecl(ast)
        self.parse_ImportList(ast)
        self.parse_TopList(ast)
        if self.current.type != TOKEN_TYPE['EOF']:
            raise ParserError(f"Unexpected token after program: {self.current}")
        return ast

    def parse_ModuleDecl(self, parent):
        node = {'type': 'ModuleDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['module']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_QualID(self, parent):
        node = {'type': 'QualID', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        while self.current.type == TOKEN_TYPE['DOT']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        parent['children'].append(node)

    def parse_ImportList(self, parent):
        node = {'type': 'ImportList', 'children': []}
        while self.current.type == KEYWORDS['import']:
            decl = {'type': 'ImportDecl', 'children': []}
            decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['import']).value})
            self.parse_QualID(decl)
            if self.current.type == KEYWORDS['as']:
                decl['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['as']).value})
                decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
            decl['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
            node['children'].append(decl)
        parent['children'].append(node)

    def parse_TopList(self, parent):
        node = {'type': 'TopList', 'children': []}
        tops = set([KEYWORDS['type'], KEYWORDS['struct'], KEYWORDS['const'], KEYWORDS['let'], KEYWORDS['fn']])
        while self.current.type in tops:
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
        fields = []
        while self.current.type == TOKEN_TYPE['ID']:
            f = {'type': 'Field', 'children': []}
            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
            f['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
            f['children'].append({'type': 'Type', 'value': self.parse_Type()})
            fields.append(f)
        node['children'].extend(fields)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        return node

import sys
import re

# Try to import PyQt6, but allow the module to be used without GUI for testing
import sys
import re

try:
    from PyQt6.QtWidgets import (
        QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
        QTextEdit, QPushButton, QTableWidget, QTableWidgetItem,
        QMessageBox, QFileDialog, QHeaderView, QSplitter,
        QLabel
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
    'ID': 'ID',
    'NUM_LIT': 'NUM_LIT',
    'STRING_LIT': 'STRING_LIT',
    'ASSIGN': 'OP_ASSIGN',
    'ARROW': 'OP_ARROW',
    'EQ': 'OP_EQ',
    'NE': 'OP_NE',
    'LE': 'OP_LE',
    'GE': 'OP_GE',
    'AND': 'OP_AND',
    'OR': 'OP_OR',
    'ADD': 'OP_ADD',
    'SUB': 'OP_SUB',
    'MUL': 'OP_MUL',
    'DIV': 'OP_DIV',
    'MOD': 'OP_MOD',
    'NOT': 'OP_NOT',
    'LT': 'OP_LT',
    'GT': 'OP_GT',
    'LPAREN': 'SEP_LPAREN',
    'RPAREN': 'SEP_RPAREN',
    'LBRACE': 'SEP_LBRACE',
    'RBRACE': 'SEP_RBRACE',
    'LBRACKET': 'SEP_LBRACKET',
    'RBRACKET': 'SEP_RBRACKET',
    'COMMA': 'SEP_COMMA',
    'COLON': 'SEP_COLON',
    'SEMI': 'SEP_SEMI',
    'DOT': 'SEP_DOT',
    'EOF': 'EOF',
}

TOKEN_REGEX = [
    (r'//[A-Za-z0-9]+//', 'COMMENT'),
    (r'"([^"\\]|\\.)*"', TOKEN_TYPE['STRING_LIT']),
    (r'->', TOKEN_TYPE['ARROW']),
    (r'==', TOKEN_TYPE['EQ']),
    (r'!=', TOKEN_TYPE['NE']),
    (r'<=', TOKEN_TYPE['LE']),
    (r'>=', TOKEN_TYPE['GE']),
    (r'&&', TOKEN_TYPE['AND']),
    (r'\|\|', TOKEN_TYPE['OR']),
    (r'\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?|\.\d+(?:[eE][+\-]?\d+)?', TOKEN_TYPE['NUM_LIT']),
    (r'[a-zA-Z][a-zA-Z0-9]*', TOKEN_TYPE['ID']),
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

COMPILED_TOKEN_REGEX = [(re.compile(p, re.DOTALL), t) for p, t in TOKEN_REGEX]

class LexerError(Exception):
    def __init__(self, message, line, column):
        super().__init__(f"Error Léxico en L{line}, C{column}: {message}")
        self.message = message
        self.line = line
        self.column = column

class ParserError(Exception):
    def __init__(self, message, token):
        super().__init__(f"Error Sintáctico en L{token.line}, C{token.column}: {message} (Token: '{token.value}')")
        self.message = message
        self.token = token

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
        self.position = 0

    def tokenize(self):
        self.tokens = []
        self.line = 1
        self.column = 1
        self.position = 0
        while self.position < len(self.code):
            match = None
            for pattern, token_type in COMPILED_TOKEN_REGEX:
                m = pattern.match(self.code, self.position)
                if m:
                    if match is None or len(m.group(0)) > len(match.group(0)):
                        match = m
                        match_type = token_type
            if match:
                value = match.group(0)
                length = len(value)
                if match_type in ('COMMENT', 'WHITESPACE'):
                    lines_found = value.count('\n')
                    if lines_found > 0:
                        self.line += lines_found
                        self.column = length - value.rfind('\n')
                    else:
                        self.column += length
                else:
                    token_line = self.line
                    token_column = self.column
                    if match_type == TOKEN_TYPE['ID']:
                        token_type = KEYWORDS.get(value, TOKEN_TYPE['ID'])
                    else:
                        token_type = match_type
                    token = Token(token_type, value, token_line, token_column)
                    self.tokens.append(token)
                    lines_found = value.count('\n')
                    if lines_found > 0:
                        self.line += lines_found
                        self.column = length - value.rfind('\n')
                    else:
                        self.column += length
                self.position += length
            else:
                current_char = self.code[self.position]
                raise LexerError(f"Símbolo no reconocido: '{current_char}'", self.line, self.column)
        self.tokens.append(Token(TOKEN_TYPE['EOF'], 'EOF', self.line, self.column))
        return self.tokens

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current_index = 0
        self.current_token = self.tokens[0]
        self.ast = {'type': 'Program', 'children': []}

    def advance(self):
        self.current_index += 1
        if self.current_index < len(self.tokens):
            self.current_token = self.tokens[self.current_index]
        return self.current_token

    def match(self, expected):
        if self.current_token.type == expected:
            tok = self.current_token
            self.advance()
            return tok
        else:
            raise ParserError(f"Se esperaba '{expected}' pero se encontró '{self.current_token.type}'", self.current_token)

    def parse(self):
        self.parse_Program(self.ast)
        if self.current_token.type != TOKEN_TYPE['EOF']:
            raise ParserError("El código continúa después de la estructura principal del programa.", self.current_token)
        return self.ast

    def parse_Program(self, parent):
        node = {'type': 'Program', 'children': []}
        self.parse_ModuleDecl(node)
        self.parse_ImportList(node)
        self.parse_TopList(node)
        self.match(TOKEN_TYPE['EOF'])
        parent['children'].append(node)

    def parse_ModuleDecl(self, parent):
        node = {'type': 'ModuleDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['module']).value})
        self.parse_QualID(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_QualID(self, parent):
        node = {'type': 'QualID', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        self.parse_QualIDTail(node)
        parent['children'].append(node)

    def parse_QualIDTail(self, parent):
        if self.current_token.type == TOKEN_TYPE['DOT']:
            node = {'type': 'QualIDTail', 'children': []}
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
            self.parse_QualIDTail(node)
            parent['children'].append(node)
        else:
            return

    def parse_ImportDecl(self, parent):
        node = {'type': 'ImportDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['import']).value})
        self.parse_QualID(node)
        self.parse_AsOpt(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_AsOpt(self, parent):
        node = {'type': 'AsOpt', 'children': []}
        if self.current_token.type == KEYWORDS['as']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['as']).value})
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        parent['children'].append(node)

    def parse_ImportList(self, parent):
        node = {'type': 'ImportList', 'children': []}
        while self.current_token.type == KEYWORDS['import']:
            self.parse_ImportDecl(node)
        parent['children'].append(node)

    def parse_TopList(self, parent):
        node = {'type': 'TopList', 'children': []}
        top_first = [KEYWORDS['type'], KEYWORDS['struct'], KEYWORDS['const'], KEYWORDS['let'], KEYWORDS['fn']]
        while self.current_token.type in top_first:
            self.parse_TopDecl(node)
        parent['children'].append(node)

    def parse_TopDecl(self, parent):
        if self.current_token.type == KEYWORDS['type']:
            self.parse_TypeDecl(parent)
        elif self.current_token.type == KEYWORDS['struct']:
            self.parse_StructDecl(parent)
        elif self.current_token.type == KEYWORDS['const']:
            self.parse_ConstDecl(parent)
        elif self.current_token.type == KEYWORDS['let']:
            self.parse_LetDecl(parent)
        elif self.current_token.type == KEYWORDS['fn']:
            self.parse_FunDecl(parent)
        else:
            raise ParserError('Declaración superior esperada', self.current_token)

    def parse_TypeDecl(self, parent):
        node = {'type': 'TypeDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['type']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
        self.parse_Type(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_StructDecl(self, parent):
        node = {'type': 'StructDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['struct']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACE']).value})
        self.parse_FieldList(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_FieldList(self, parent):
        node = {'type': 'FieldList', 'children': []}
        if self.current_token.type == TOKEN_TYPE['ID']:
            self.parse_Field(node)
            while self.current_token.type == TOKEN_TYPE['ID']:
                self.parse_Field(node)
        parent['children'].append(node)

    def parse_Field(self, parent):
        node = {'type': 'Field', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        self.parse_Type(node)
        parent['children'].append(node)

    def parse_ConstDecl(self, parent):
        node = {'type': 'ConstDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['const']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        self.parse_Type(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
        self.parse_Expr(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_LetDecl(self, parent):
        node = {'type': 'LetDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['let']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        self.parse_Type(node)
        if self.current_token.type == TOKEN_TYPE['ASSIGN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
            self.parse_Expr(node)
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        else:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_FunDecl(self, parent):
        node = {'type': 'FunDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['fn']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
        self.parse_ParamListOpt(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        if self.current_token.type == TOKEN_TYPE['ARROW']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ARROW']).value})
            self.parse_Type(node)
        self.parse_Block(node)
        parent['children'].append(node)

    def parse_ParamListOpt(self, parent):
        node = {'type': 'ParamListOpt', 'children': []}
        if self.current_token.type == TOKEN_TYPE['ID']:
            self.parse_ParamList(node)
        parent['children'].append(node)

    def parse_ParamList(self, parent):
        node = {'type': 'ParamList', 'children': []}
        self.parse_Param(node)
        while self.current_token.type == TOKEN_TYPE['COMMA']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COMMA']).value})
            self.parse_Param(node)
        parent['children'].append(node)

    def parse_Param(self, parent):
        node = {'type': 'Param', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        self.parse_Type(node)
        parent['children'].append(node)

    def parse_Type(self, parent):
        node = {'type': 'Type', 'children': []}
        self.parse_SimpleType(node)
        while self.current_token.type == TOKEN_TYPE['LBRACKET']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACKET']).value})
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACKET']).value})
        parent['children'].append(node)

    def parse_SimpleType(self, parent):
        node = {'type': 'SimpleType', 'children': []}
        if self.current_token.type == KEYWORDS['int']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['int']).value})
        elif self.current_token.type == KEYWORDS['bool']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['bool']).value})
        elif self.current_token.type == KEYWORDS['string']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['string']).value})
        elif self.current_token.type == TOKEN_TYPE['LPAREN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
            self.parse_Type(node)
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        elif self.current_token.type == TOKEN_TYPE['ID']:
            self.parse_QualID(node)
        else:
            raise ParserError('Tipo simple esperado', self.current_token)
        parent['children'].append(node)

    def parse_Block(self, parent):
        node = {'type': 'Block', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACE']).value})
        self.parse_StmtList(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})
        parent['children'].append(node)

    def parse_StmtList(self, parent):
        node = {'type': 'StmtList', 'children': []}
        while self.current_token.type in (TOKEN_TYPE['LBRACE'], KEYWORDS['let'], TOKEN_TYPE['ID'], KEYWORDS['if'], KEYWORDS['while'], KEYWORDS['return']):
            self.parse_Stmt(node)
        parent['children'].append(node)

    def parse_Stmt(self, parent):
        if self.current_token.type == TOKEN_TYPE['LBRACE']:
            self.parse_Block(parent)
        elif self.current_token.type == KEYWORDS['let']:
            self.parse_LetDecl(parent)
        elif self.current_token.type == KEYWORDS['if']:
            self.parse_IfStmt(parent)
        elif self.current_token.type == KEYWORDS['while']:
            self.parse_WhileStmt(parent)
        elif self.current_token.type == KEYWORDS['return']:
            self.parse_ReturnStmt(parent)
        else:
            self.parse_Expr(parent)
            parent['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

    def parse_IfStmt(self, parent):
        node = {'type': 'IfStmt', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['if']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
        self.parse_Expr(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        self.parse_Stmt(node)
        if self.current_token.type == KEYWORDS['else']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['else']).value})
            self.parse_Stmt(node)
        parent['children'].append(node)

    def parse_WhileStmt(self, parent):
        node = {'type': 'WhileStmt', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['while']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
        self.parse_Expr(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        self.parse_Stmt(node)
        parent['children'].append(node)

    def parse_ReturnStmt(self, parent):
        node = {'type': 'ReturnStmt', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['return']).value})
        if self.current_token.type == TOKEN_TYPE['SEMI']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        else:
            self.parse_Expr(node)
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent['children'].append(node)

    def parse_Expr(self, parent):
        self.parse_Assign(parent)

    def parse_Assign(self, parent):
        node = {'type': 'Assign', 'children': []}
        self.parse_Or(node)
        if self.current_token.type == TOKEN_TYPE['ASSIGN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
            self.parse_Assign(node)
        parent['children'].append(node)

    def parse_Or(self, parent):
        node = {'type': 'Or', 'children': []}
        self.parse_And(node)
        while self.current_token.type == TOKEN_TYPE['OR']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['OR']).value})
            self.parse_And(node)
        parent['children'].append(node)

    def parse_And(self, parent):
        node = {'type': 'And', 'children': []}
        self.parse_Eq(node)
        while self.current_token.type == TOKEN_TYPE['AND']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['AND']).value})
            self.parse_Eq(node)
        parent['children'].append(node)

    def parse_Eq(self, parent):
        node = {'type': 'Eq', 'children': []}
        self.parse_Rel(node)
        while self.current_token.type in (TOKEN_TYPE['EQ'], TOKEN_TYPE['NE']):
            if self.current_token.type == TOKEN_TYPE['EQ']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['EQ']).value})
            else:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['NE']).value})
            self.parse_Rel(node)
        parent['children'].append(node)

    def parse_Rel(self, parent):
        node = {'type': 'Rel', 'children': []}
        self.parse_Add(node)
        while self.current_token.type in (TOKEN_TYPE['LT'], TOKEN_TYPE['LE'], TOKEN_TYPE['GT'], TOKEN_TYPE['GE']):
            t = self.current_token.type
            if t == TOKEN_TYPE['LT']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LT']).value})
            elif t == TOKEN_TYPE['LE']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LE']).value})
            elif t == TOKEN_TYPE['GT']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['GT']).value})
            else:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['GE']).value})
            self.parse_Add(node)
        parent['children'].append(node)

    def parse_Add(self, parent):
        node = {'type': 'Add', 'children': []}
        self.parse_Mul(node)
        while self.current_token.type in (TOKEN_TYPE['ADD'], TOKEN_TYPE['SUB']):
            if self.current_token.type == TOKEN_TYPE['ADD']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ADD']).value})
            else:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SUB']).value})
            self.parse_Mul(node)
        parent['children'].append(node)

    def parse_Mul(self, parent):
        node = {'type': 'Mul', 'children': []}
        self.parse_Unary(node)
        while self.current_token.type in (TOKEN_TYPE['MUL'], TOKEN_TYPE['DIV'], TOKEN_TYPE['MOD']):
            t = self.current_token.type
            if t == TOKEN_TYPE['MUL']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['MUL']).value})
            elif t == TOKEN_TYPE['DIV']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DIV']).value})
            else:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['MOD']).value})
            self.parse_Unary(node)
        parent['children'].append(node)

    def parse_Unary(self, parent):
        node = {'type': 'Unary', 'children': []}
        if self.current_token.type == TOKEN_TYPE['NOT']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['NOT']).value})
            self.parse_Unary(node)
        elif self.current_token.type == TOKEN_TYPE['SUB']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SUB']).value})
            self.parse_Unary(node)
        else:
            self.parse_Postfix(node)
        parent['children'].append(node)

    def parse_Postfix(self, parent):
        node = {'type': 'Postfix', 'children': []}
        self.parse_Primary(node)
        while True:
            if self.current_token.type == TOKEN_TYPE['LPAREN']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
                self.parse_ArgListOpt(node)
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
            elif self.current_token.type == TOKEN_TYPE['LBRACKET']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACKET']).value})
                self.parse_Expr(node)
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACKET']).value})
            elif self.current_token.type == TOKEN_TYPE['DOT']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
            else:
                break
        parent['children'].append(node)

    def parse_Primary(self, parent):
        node = {'type': 'Primary', 'children': []}
        t = self.current_token.type
        if t == TOKEN_TYPE['ID']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        elif t == TOKEN_TYPE['NUM_LIT']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['NUM_LIT']).value})
        elif t == TOKEN_TYPE['STRING_LIT']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['STRING_LIT']).value})
        elif t == KEYWORDS.get('true') or t == KEYWORDS.get('false') or t == 'BOOL_LIT':
            node['children'].append({'type': 'Terminal', 'value': self.match(self.current_token.type).value})
        elif t == TOKEN_TYPE['LPAREN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
            self.parse_Expr(node)
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        else:
            raise ParserError("Se esperaba una expresión primaria", self.current_token)
        parent['children'].append(node)

    def parse_ArgListOpt(self, parent):
        if self.current_token.type in (TOKEN_TYPE['ID'], TOKEN_TYPE['NUM_LIT'], TOKEN_TYPE['STRING_LIT'], TOKEN_TYPE['LPAREN'], KEYWORDS.get('true')):
            self.parse_ArgList(parent)
        else:
            return

    def parse_ArgList(self, parent):
        node = {'type': 'ArgList', 'children': []}
        self.parse_Expr(node)
        while self.current_token.type == TOKEN_TYPE['COMMA']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COMMA']).value})
            self.parse_Expr(node)
        parent['children'].append(node)

    def parse_IfStmt(self, parent):
        # already implemented above; placeholder to keep structure
        pass

        node = {'type': 'QualID', 'children': []}
        
        # 1. ID
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        
        # 2. QualIDTail
        self.parse_QualIDTail(node)
        
        parent_node['children'].append(node)

    def parse_QualIDTail(self, parent_node):
        # QualIDTail -> '.' ID QualIDTail | ε
        # First(QualIDTail) = {'.'}
        # Follow(QualIDTail) = {';', 'as', '(', '->', '[', '}', 'EOF'}
        
        if self.current_token.type == TOKEN_TYPE['DOT']:
            node = {'type': 'QualIDTail', 'children': []}
            
            # 1. '.'
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})
            
            # 2. ID
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
            
            # 3. QualIDTail (recursivo)
            self.parse_QualIDTail(node)
            
            parent_node['children'].append(node)
        else:
            # Caso épsilon (vacío): si el lookahead está en Follow(QualIDTail)
            # No hacemos nada y la recursión se detiene.
            pass # épsilon

    def parse_ImportList(self, parent_node):
        # ImportList -> ImportDecl ImportList | ε
        # First(ImportList) = {'import'}
        # Follow(ImportList) = {First(TopDecl), '}'}
        
        # Asumiendo First(TopDecl) incluye {'type', 'struct', 'const', 'let', 'fn'}
        top_decl_first = [KEYWORDS['type'], KEYWORDS['struct'], KEYWORDS['const'], KEYWORDS['let'], KEYWORDS['fn']]
        
        if self.current_token.type == KEYWORDS['import']:
            node = {'type': 'ImportList', 'children': []}
            while self.current_token.type == KEYWORDS['import']:
                self.parse_ImportDecl(node)
            parent_node['children'].append(node)
        elif self.current_token.type in top_decl_first or self.current_token.type == TOKEN_TYPE['EOF']:
            # Caso épsilon
            pass
        else:
            raise ParserError("Se esperaba 'import' o una declaración de nivel superior", self.current_token)


    def parse_TopList(self, parent_node):
        # TopList -> TopDecl TopList | ε
        # Esto debe contener la lógica para todas las declaraciones:
        # First(TopDecl) = {'type', 'struct', 'const', 'let', 'fn'}
        top_decl_first = [KEYWORDS['type'], KEYWORDS['struct'], KEYWORDS['const'], KEYWORDS['let'], KEYWORDS['fn']]

        if self.current_token.type in top_decl_first:
            node = {'type': 'TopList', 'children': []}
            while self.current_token.type in top_decl_first:
                self.parse_TopDecl(node)
            parent_node['children'].append(node)
        elif self.current_token.type == TOKEN_TYPE['EOF']:
            # Caso épsilon
            pass
        else:
            # Si hay código que no es una declaración válida y no es EOF, es un error
            pass # El error será detectado por el parse_Program al fallar el EOF check, si no hay TopList

    # --- DEBES CONTINUAR LA IMPLEMENTACIÓN DE TODAS LAS REGLAS AQUÍ ---
    # Ejemplos de reglas de expresión para referencia de la estructura:
    
    def parse_Expr(self, parent_node):
        # Expr -> Assign
        self.parse_Assign(parent_node)

    def parse_Assign(self, parent_node):
        node = {'type': 'Assign', 'children': []}
        self.parse_Or(node)
        if self.current_token.type == TOKEN_TYPE['ASSIGN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
            self.parse_Assign(node)
        parent_node['children'].append(node)
        
    def parse_Or(self, parent_node):
        # Or -> And OrTail
        # ...
        node = {'type': 'Or', 'children': []}
        self.parse_And(node)
        # OrTail -> (|| And)*
        while self.current_token.type == TOKEN_TYPE['OR']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['OR']).value})
            self.parse_And(node)

        parent_node['children'].append(node)

    # And/Eq/Rel/Add/Mul/Unary/Postfix/Primary implementations
    def parse_And(self, parent_node):
        node = {'type': 'And', 'children': []}
        self.parse_Eq(node)
        while self.current_token.type == TOKEN_TYPE['AND']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['AND']).value})
            self.parse_Eq(node)
        parent_node['children'].append(node)

    def parse_Eq(self, parent_node):
        node = {'type': 'Eq', 'children': []}
        self.parse_Rel(node)
        while self.current_token.type in (TOKEN_TYPE['EQ'], TOKEN_TYPE['NE']):
            if self.current_token.type == TOKEN_TYPE['EQ']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['EQ']).value})
            else:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['NE']).value})
            self.parse_Rel(node)
        parent_node['children'].append(node)

    def parse_Rel(self, parent_node):
        node = {'type': 'Rel', 'children': []}
        self.parse_Add(node)
        while self.current_token.type in (TOKEN_TYPE['LT'], TOKEN_TYPE['LE'], TOKEN_TYPE['GT'], TOKEN_TYPE['GE']):
            t = self.current_token.type
            if t == TOKEN_TYPE['LT']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LT']).value})
            elif t == TOKEN_TYPE['LE']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LE']).value})
            elif t == TOKEN_TYPE['GT']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['GT']).value})
            else:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['GE']).value})
            self.parse_Add(node)
        parent_node['children'].append(node)

    def parse_Add(self, parent_node):
        node = {'type': 'Add', 'children': []}
        self.parse_Mul(node)
        while self.current_token.type == TOKEN_TYPE['ADD'] or self.current_token.type == TOKEN_TYPE['SUB']:
            if self.current_token.type == TOKEN_TYPE['ADD']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ADD']).value})
            else:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SUB']).value})
            self.parse_Mul(node)
        parent_node['children'].append(node)

    def parse_Mul(self, parent_node):
        node = {'type': 'Mul', 'children': []}
        self.parse_Unary(node)
        while self.current_token.type in (TOKEN_TYPE['MUL'], TOKEN_TYPE['DIV'], TOKEN_TYPE['MOD']):
            t = self.current_token.type
            if t == TOKEN_TYPE['MUL']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['MUL']).value})
            elif t == TOKEN_TYPE['DIV']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DIV']).value})
            else:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['MOD']).value})
            self.parse_Unary(node)
        parent_node['children'].append(node)

    def parse_Unary(self, parent_node):
        node = {'type': 'Unary', 'children': []}
        if self.current_token.type == TOKEN_TYPE['NOT']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['NOT']).value})
            self.parse_Unary(node)
        elif self.current_token.type == TOKEN_TYPE['SUB']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SUB']).value})
            self.parse_Unary(node)
        else:
            self.parse_Postfix(node)
        parent_node['children'].append(node)

    def parse_Postfix(self, parent_node):
        # Postfix -> Primary PostfixTail*
        node = {'type': 'Postfix', 'children': []}
        self.parse_Primary(node)
        while True:
            if self.current_token.type == TOKEN_TYPE['LPAREN']:
                # Call
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
                self.parse_ArgListOpt(node)
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
            elif self.current_token.type == TOKEN_TYPE['LBRACKET']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACKET']).value})
                self.parse_Expr(node)
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACKET']).value})
            elif self.current_token.type == TOKEN_TYPE['DOT']:
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['DOT']).value})
                node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
            else:
                break
        parent_node['children'].append(node)

    def parse_Primary(self, parent_node):
        node = {'type': 'Primary', 'children': []}
        t = self.current_token.type
        if t == TOKEN_TYPE['ID']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        elif t == TOKEN_TYPE['NUM_LIT']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['NUM_LIT']).value})
        elif t == TOKEN_TYPE['STRING_LIT']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['STRING_LIT']).value})
        elif t == KEYWORDS.get('true') or t == KEYWORDS.get('false') or t == 'BOOL_LIT':
            # Accept boolean literals
            node['children'].append({'type': 'Terminal', 'value': self.match(self.current_token.type).value})
        elif t == TOKEN_TYPE['LPAREN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
            self.parse_Expr(node)
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        else:
            raise ParserError("Se esperaba una expresión primaria", self.current_token)
        parent_node['children'].append(node)

    def parse_ArgListOpt(self, parent_node):
        if self.current_token.type in (TOKEN_TYPE['ID'], TOKEN_TYPE['NUM_LIT'], TOKEN_TYPE['STRING_LIT'], TOKEN_TYPE['LPAREN'], KEYWORDS.get('true')):
            self.parse_ArgList(parent_node)
        else:
            # épsilon
            pass

    def parse_ArgList(self, parent_node):
        node = {'type': 'ArgList', 'children': []}
        self.parse_Expr(node)
        while self.current_token.type == TOKEN_TYPE['COMMA']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COMMA']).value})
            self.parse_Expr(node)
        parent_node['children'].append(node)

    # --- Declaraciones, Tipos, Bloques y Sentencias ---
    def parse_ImportDecl(self, parent_node):
        node = {'type': 'ImportDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['import']).value})
        self.parse_QualID(node)
        self.parse_AsOpt(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent_node['children'].append(node)

    def parse_AsOpt(self, parent_node):
        node = {'type': 'AsOpt', 'children': []}
        if self.current_token.type == KEYWORDS['as']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['as']).value})
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        else:
            pass
        parent_node['children'].append(node)

    def parse_TopDecl(self, parent_node):
        # Dispatch based on current token
        if self.current_token.type == KEYWORDS['type']:
            self.parse_TypeDecl(parent_node)
        elif self.current_token.type == KEYWORDS['struct']:
            self.parse_StructDecl(parent_node)
        elif self.current_token.type == KEYWORDS['const']:
            self.parse_ConstDecl(parent_node)
        elif self.current_token.type == KEYWORDS['let']:
            self.parse_LetDecl(parent_node)
        elif self.current_token.type == KEYWORDS['fn']:
            self.parse_FunDecl(parent_node)
        else:
            raise ParserError("Declaración superior esperada", self.current_token)

    def parse_TypeDecl(self, parent_node):
        node = {'type': 'TypeDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['type']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
        self.parse_Type(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent_node['children'].append(node)

    def parse_StructDecl(self, parent_node):
        node = {'type': 'StructDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['struct']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACE']).value})
        self.parse_FieldList(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent_node['children'].append(node)

    def parse_FieldList(self, parent_node):
        node = {'type': 'FieldList', 'children': []}
        # can be epsilon
        if self.current_token.type == TOKEN_TYPE['ID']:
            self.parse_Field(node)
            while self.current_token.type == TOKEN_TYPE['ID']:
                self.parse_Field(node)
        parent_node['children'].append(node)

    def parse_Field(self, parent_node):
        node = {'type': 'Field', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        self.parse_Type(node)
        parent_node['children'].append(node)

    def parse_ConstDecl(self, parent_node):
        node = {'type': 'ConstDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['const']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        self.parse_Type(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
        self.parse_Expr(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent_node['children'].append(node)

    def parse_LetDecl(self, parent_node):
        node = {'type': 'LetDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['let']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        self.parse_Type(node)
        # LetTail: either = Expr ;  or ;
        if self.current_token.type == TOKEN_TYPE['ASSIGN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ASSIGN']).value})
            self.parse_Expr(node)
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        else:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent_node['children'].append(node)

    def parse_FunDecl(self, parent_node):
        node = {'type': 'FunDecl', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['fn']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
        self.parse_ParamListOpt(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        # RetType
        if self.current_token.type == TOKEN_TYPE['ARROW']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ARROW']).value})
            self.parse_Type(node)
        # Block
        self.parse_Block(node)
        parent_node['children'].append(node)

    def parse_ParamListOpt(self, parent_node):
        node = {'type': 'ParamListOpt', 'children': []}
        if self.current_token.type == TOKEN_TYPE['ID']:
            self.parse_ParamList(node)
        parent_node['children'].append(node)

    def parse_ParamList(self, parent_node):
        node = {'type': 'ParamList', 'children': []}
        self.parse_Param(node)
        while self.current_token.type == TOKEN_TYPE['COMMA']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COMMA']).value})
            self.parse_Param(node)
        parent_node['children'].append(node)

    def parse_Param(self, parent_node):
        node = {'type': 'Param', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['ID']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['COLON']).value})
        self.parse_Type(node)
        parent_node['children'].append(node)

    def parse_Type(self, parent_node):
        node = {'type': 'Type', 'children': []}
        # SimpleType ArrOrFunType
        self.parse_SimpleType(node)
        # ArrOrFunType -> [] ArrOrFunType | epsilon
        while self.current_token.type == TOKEN_TYPE['LBRACKET']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACKET']).value})
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACKET']).value})
        parent_node['children'].append(node)

    def parse_SimpleType(self, parent_node):
        node = {'type': 'SimpleType', 'children': []}
        if self.current_token.type == KEYWORDS['int']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['int']).value})
        elif self.current_token.type == KEYWORDS['bool']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['bool']).value})
        elif self.current_token.type == KEYWORDS['string']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['string']).value})
        elif self.current_token.type == TOKEN_TYPE['LPAREN']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
            self.parse_Type(node)
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        elif self.current_token.type == TOKEN_TYPE['ID']:
            # QualID
            self.parse_QualID(node)
        else:
            raise ParserError('Tipo simple esperado', self.current_token)
        parent_node['children'].append(node)

    def parse_Block(self, parent_node):
        node = {'type': 'Block', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LBRACE']).value})
        self.parse_StmtList(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RBRACE']).value})
        parent_node['children'].append(node)

    def parse_StmtList(self, parent_node):
        node = {'type': 'StmtList', 'children': []}
        # zero or more Stmt
        while self.current_token.type in (TOKEN_TYPE['LBRACE'], KEYWORDS['let'], TOKEN_TYPE['ID'], KEYWORDS['if'], KEYWORDS['while'], KEYWORDS['return']):
            self.parse_Stmt(node)
        parent_node['children'].append(node)

    def parse_Stmt(self, parent_node):
        if self.current_token.type == TOKEN_TYPE['LBRACE']:
            self.parse_Block(parent_node)
        elif self.current_token.type == KEYWORDS['let']:
            self.parse_LetDecl(parent_node)
        elif self.current_token.type == KEYWORDS['if']:
            self.parse_IfStmt(parent_node)
        elif self.current_token.type == KEYWORDS['while']:
            self.parse_WhileStmt(parent_node)
        elif self.current_token.type == KEYWORDS['return']:
            self.parse_ReturnStmt(parent_node)
        else:
            # ExprStmt
            self.parse_Expr(parent_node)
            parent_node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})

    def parse_IfStmt(self, parent_node):
        node = {'type': 'IfStmt', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['if']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
        self.parse_Expr(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        self.parse_Stmt(node)
        # ElseOpt
        if self.current_token.type == KEYWORDS['else']:
            node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['else']).value})
            self.parse_Stmt(node)
        parent_node['children'].append(node)

    def parse_WhileStmt(self, parent_node):
        node = {'type': 'WhileStmt', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['while']).value})
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['LPAREN']).value})
        self.parse_Expr(node)
        node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['RPAREN']).value})
        self.parse_Stmt(node)
        parent_node['children'].append(node)

    def parse_ReturnStmt(self, parent_node):
        node = {'type': 'ReturnStmt', 'children': []}
        node['children'].append({'type': 'Terminal', 'value': self.match(KEYWORDS['return']).value})
        if self.current_token.type == TOKEN_TYPE['SEMI']:
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        else:
            self.parse_Expr(node)
            node['children'].append({'type': 'Terminal', 'value': self.match(TOKEN_TYPE['SEMI']).value})
        parent_node['children'].append(node)

    
# --- Interfaz Gráfica (PyQt6) ---

if HAS_PYQT:
    class CompilerIDE(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Mini Compilador LL(1) - Python/PyQt6")
        self.setGeometry(100, 100, 1200, 800)
        self.current_filepath = None
        self.init_ui()

    def init_ui(self):
        main_widget = QWidget()
        main_layout = QVBoxLayout(main_widget)
        self.setCentralWidget(main_widget)

        # Configurar la fuente principal
        self.font = QFont("Inter", 10)
        
        # --- Área de Código Fuente (Arriba) ---
        self.code_editor = QTextEdit()
        self.code_editor.setFont(QFont("Consolas", 11))
        self.code_editor.setStyleSheet("background-color: #282c34; color: #abb2bf; border: 1px solid #555;")
        self.code_editor.setPlaceholderText("Escribe tu código fuente LL(1) aquí...")
        
        # Contenedor para botones
        button_layout = QHBoxLayout()
        
        self.btn_open = QPushButton("Abrir Código")
        self.btn_save = QPushButton("Guardar Código")
        self.btn_compile = QPushButton("COMPILAR y ANALIZAR")
        
        self.btn_open.clicked.connect(self.open_file)
        self.btn_save.clicked.connect(self.save_file)
        self.btn_compile.clicked.connect(self.compile_code)
        
        # Estilo de botones
        button_style = """
        QPushButton {
            background-color: #61afef;
            color: white;
            padding: 10px;
            border-radius: 8px;
            font-weight: bold;
        }
        QPushButton:hover {
            background-color: #569cd6;
        }
        """
        self.btn_compile.setStyleSheet(button_style.replace('#61afef', '#98c379').replace('#569cd6', '#87b469'))
        self.btn_open.setStyleSheet(button_style)
        self.btn_save.setStyleSheet(button_style)
        
        button_layout.addWidget(self.btn_open)
        button_layout.addWidget(self.btn_save)
        button_layout.addStretch(1)
        button_layout.addWidget(self.btn_compile)
        
        # --- Área de Salida (Abajo) ---
        
        # 1. Consola de Errores/Éxito
        self.error_console = QTextEdit()
        self.error_console.setReadOnly(True)
        self.error_console.setFont(self.font)
        self.error_console.setFixedHeight(100)
        self.error_console.setStyleSheet("background-color: #1e2127; color: #c678dd; border: 1px solid #555; padding: 5px;")
        
        # 2. Tabla de Tokens
        self.token_table = QTableWidget()
        self.token_table.setColumnCount(4)
        self.token_table.setHorizontalHeaderLabels(["Línea", "Columna", "Tipo", "Valor"])
        self.token_table.horizontalHeader().setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        self.token_table.setStyleSheet("QTableWidget { background-color: #282c34; color: #abb2bf; border: 1px solid #555; }"
                                       "QHeaderView::section { background-color: #4b5263; color: white; }")
        
        # Splitter para el área de salida
        output_splitter = QSplitter(Qt.Orientation.Vertical)
        output_splitter.addWidget(QLabel("Tabla de Tokens:"))
        output_splitter.addWidget(self.token_table)
        output_splitter.addWidget(QLabel("Consola de Errores y Mensajes:"))
        output_splitter.addWidget(self.error_console)
        
        # Splitter principal (Código y Salida)
        main_splitter = QSplitter(Qt.Orientation.Vertical)
        main_splitter.addWidget(self.code_editor)
        main_splitter.addWidget(output_splitter)
        main_splitter.setSizes([500, 300]) # Proporciones iniciales
        
        main_layout.addLayout(button_layout)
        main_layout.addWidget(main_splitter)
        
        self.show_message("IDE listo. ¡Escribe tu código y pulsa COMPILAR!", QColor("#c678dd"))

    def show_message(self, message, color=QColor("#61afef")):
        """Muestra un mensaje en la consola de errores."""
        self.error_console.setText(f"<span style='color: {color.name()}; font-weight: bold;'>{message}</span>")

    def populate_token_table(self, tokens):
        """Llena la QTableWidget con la lista de tokens."""
        self.token_table.setRowCount(0) # Limpiar tabla
        self.token_table.setRowCount(len(tokens))
        
        for i, token in enumerate(tokens):
            self.token_table.setItem(i, 0, QTableWidgetItem(str(token.line)))
            self.token_table.setItem(i, 1, QTableWidgetItem(str(token.column)))
            self.token_table.setItem(i, 2, QTableWidgetItem(token.type))
            self.token_table.setItem(i, 3, QTableWidgetItem(token.value))
            
            # Estilo para filas
            if token.type == TOKEN_TYPE['EOF']:
                self.token_table.item(i, 2).setBackground(QColor("#e06c75"))

    def open_file(self):
        """Abre un archivo de código fuente."""
        filepath, _ = QFileDialog.getOpenFileName(self, "Abrir Código Fuente", "", "Archivos de Código (*.lc *.txt);;Todos los Archivos (*)")
        if filepath:
            try:
                with open(filepath, 'r', encoding='utf-8') as f:
                    self.code_editor.setText(f.read())
                self.current_filepath = filepath
                self.setWindowTitle(f"Mini Compilador LL(1) - {filepath}")
                self.show_message(f"Archivo cargado exitosamente: {filepath}", QColor("#98c379"))
            except Exception as e:
                QMessageBox.critical(self, "Error de Archivo", f"No se pudo abrir el archivo: {e}")

    def save_file(self):
        """Guarda el código fuente."""
        if self.current_filepath:
            filepath = self.current_filepath
        else:
            filepath, _ = QFileDialog.getSaveFileName(self, "Guardar Código Fuente", "", "Archivos de Código (*.lc *.txt);;Todos los Archivos (*)")
        
        if filepath:
            try:
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.write(self.code_editor.toPlainText())
                self.current_filepath = filepath
                self.setWindowTitle(f"Mini Compilador LL(1) - {filepath}")
                self.show_message(f"Archivo guardado exitosamente: {filepath}", QColor("#98c379"))
            except Exception as e:
                QMessageBox.critical(self, "Error de Archivo", f"No se pudo guardar el archivo: {e}")

    def compile_code(self):
        """Ejecuta los análisis léxico y sintáctico."""
        source_code = self.code_editor.toPlainText()
        self.token_table.setRowCount(0)
        self.show_message("Iniciando análisis...", QColor("#61afef"))

        # 1. Análisis Léxico
        try:
            lexer = Lexer(source_code)
            tokens = lexer.tokenize()
            self.populate_token_table(tokens)
            self.show_message("Análisis Léxico completado. ¡Iniciando Análisis Sintáctico!", QColor("#98c379"))
        except LexerError as e:
            self.show_message(str(e), QColor("#e06c75"))
            return

        # 2. Análisis Sintáctico
        try:
            parser = Parser(tokens)
            ast = parser.parse() # Inicia el parseo
            
            # Si llega aquí sin excepción, es un éxito sintáctico (¡para las reglas implementadas!)
            success_message = "COMPILACIÓN EXITOSA. El código pasó los análisis Léxico y Sintáctico (parcialmente implementado)."
            self.show_message(success_message, QColor("#98c379"))
            
            # Opcional: imprimir el AST simple en la consola para depuración
            print("--- AST (Simple) ---")
            print(ast)
            
        except ParserError as e:
            self.show_message(str(e), QColor("#e06c75"))
            return
        except Exception as e:
             # Errores inesperados
             self.show_message(f"Error interno inesperado: {e}", QColor("#e06c75"))

else:
    class CompilerIDE:
        pass

if __name__ == '__main__' and HAS_PYQT:
    # Asegura que QApplication se cree solo una vez
    app = QCoreApplication.instance()
    if app is None:
        app = QApplication(sys.argv)
        
    ide = CompilerIDE()
    ide.show()
    sys.exit(app.exec())
