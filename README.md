Mini compilador LL(1) - Entrega

Este proyecto implementa un mini entorno de compilación para un lenguaje didáctico LL(1).

Cómo ejecutar

1) Crear un entorno Python e instalar dependencias:

```powershell
pip install -r requirements.txt
```

2) Ejecutar la interfaz gráfica:

```powershell
python lexical.py
```

3) Ejecutar tests:

```powershell
pytest -q
```

Archivos importantes
- `lexical.py`: Lexer, Parser y GUI (PyQt6).
- `tests/`: pruebas unitarias para lexer y parser.
- `examples/example1.lc`: ejemplo mínimo.
