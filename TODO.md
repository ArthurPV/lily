# Lily's Todo

- [x] Lexer
- [ ] Parser
- [ ] Analysis
- [ ] Compiler
- [ ] Bytecode
- [ ] Transpiler

## Lexer

- [x] Location
- [x] Source
- [x] Diagnostic
- [x] Token
- [x] Lexer
- [x] Tests

### Location

> Create a record that control Lily's location of compiler.
```ocaml
type location = {
	line: int;
	col: int;
	s_line: int;
	s_col: int;
	e_line: int;
	e_col: int;
}
```

> Initialize location record
```ocaml
let new_location = {
	line: 1;
	col: 1;
	s_line: 1;
	s_col: 1;
	e_line: 1;
	e_col: 1;
}
```

> Copy location.
```ocaml
let copy_location loc = { (* some code *) }
```

### Diagnostic

> Create a record that control lily's error of compiler.
```ocaml
type diagnostic_kind =
	| Error (* @printer *)
	| Waring (* @printer *)
	| Note (* @printer *)
	| Internal (* @printer *)
[@@deriving show]

type diagnostic = {
	msg: string;
	kind: diagnostic_kind;
	loc: location;
	filename: string;
}
```

> Show get line error.
```ocaml
val get_line_error : diagnostic -> diagnostic_kind -> string
```

### Source

> Source that store filename and content among anoter things.
```ocaml
type source = {
	filename: string;
	content: string;
	mutable c: char;
	len: int;
	mutable pos: int;
}

val read_file : string -> string
(*              ^^^^^^ 
                filename *)
```

### Token

> List all tokens of lily compiler
```ocaml
type separator = (* ... *)

type operator = (* ... *)

type keyword = (* ... *)

type literal = (* ... *)

type comment = (* ... *)

type tokens =
	| Separator of separator
	| Operator of operator
	| Keyword of keyword
	| Literal of literal
	| Comment of comment
	| Identifier of string
```

### Lexer

> Convert character to token
```ocaml
type lexer = {
	src: source;
	loc: location;
	mutable tokens: (token * location) array;
	mutable errors: diagnostic array;
}

(*

Example:

'+' -> Plus
'-' -> Minus

*)
```

## Parser

- [ ] Parse expression
- [x] Parse declaration
- [ ] Parse statement
- [ ] Tests

> Convert token to declaration, expression, statement or also doc
```ocaml
type parser = {
  lexer: lexer;
  mutable pos: int;
  mutable nodes (ast * location) array;
  mutable errors: diagnostic array;
  mutable current_token: token;
  mutable previous_token: token;
  mutable current_location: location;
  mutable previous_location: location;
}
```

### Expression

- [x] Parse assign (=, +=, -=, *=, /=, %=, ^=)
- [x] Parse logical or (or)
- [x] Parse logical and (and)
- [x] Parse equality (==, !=)
- [x] Parse comparison (<, >, <=, >=)
- [x] Parse range (..)
- [x] Parse term (+, -)
- [x] Parse factor (*, /, %)
- [x] Parse exposent (^)
- [x] Parse unary (-, +, not)
- [x] Parse grouping (paren expression)
- [x] Parse function call (<id>(<args,...>))
- [x] Parse class call (new <id>(<args>))
- [x] Parse record call (<id>{<args>})
- [x] Parse anonymous function (fun(<args>) => <body>)
- [x] Parse identifier access (<id>.<id>.<function_call>)
- [x] Parse self access (self.<id>.<id>.<function_call>)
- [x] Parse tuple ((<value>,...))
- [x] Parse array ([<value>,...])
- [x] Parse variant (<id_upper>(<args>))
- [x] Parse primary expression
- [x] Parse expr2

### Declaration
- [x] Parse variable
- [x] Parse constant
- [x] Parse multiple variable
- [x] Parse multiple constant
- [x] Parse function
- [x] Parse module
- [x] Parse type
- [x] Parse alias
- [x] Parse record
- [x] Parse enum
- [x] Parse object
- [x] Parse property
- [x] Parse class
- [x] Parse method
- [x] Parse trait
- [x] Parse import
- [x] Parse pub

### Statement
- [x] Parse if
- [x] Parse match
- [x] Parse try
- [x] Parse return
- [x] Parse await
- [x] Parse while
- [ ] Parse for

## Analysis

- [ ] Scope
- [ ] Type check

### Scope

```ocaml
type from_access =
  [ `Fun
  | `Constant
  | `Module
  | `Alias
  | `Record
  | `Enum
  | `Variant
  | `Class
  | `None]

type scope_access =
  [ `Fun of from_access * string * argument array * location
  | `Identifier of from_access * string * location
  | `Type of from_access * string * data_type array * location
  | `Variant of from_access * string array * location
  | `IdentifierAddr of scope_access array ]
```

```ocaml
type scope = {
  parse: parser;
  mutable global: scope_access array;
  mutable global_pub: scope_access array;
}
```

- [ ] Multiple declarations of (method, class, enum,...) within a scope
- [ ] Too many argument in call
- [ ] Violation access rule (public, private)
- [ ] Referencing a variable before its declaration
- [ ] Referencing identifier without preview declaration
- [ ] Tests

### Type check

- [ ] Type mismatches
- [ ] Infer type
- [ ] Tests

## Bytecode

- [ ] VM
- [ ] Chunk
- [x] Opcode
