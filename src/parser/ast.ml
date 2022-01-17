open Lily_lexer.Location

type data_type =
  [ `I8
  | `I16
  | `I32
  | `I64
  | `U8
  | `U16
  | `U32
  | `U64
  | `F32
  | `F64
  | `String
  | `Char
  | `Usize
  | `Isize
  | `Bool
  | `Unit
  | `SelfArg
  | `Tuple of data_type array
  | `GroupingDataType of data_type
  | `Generics of string
  | `CustomType of string * data_type array option ]
[@@deriving show]

type literal_ast =
  | Bool of bool
  | Char of char
  | Int of Stdint.int64
  | Float of float
  | String of string
  | Unit

type argument_proto = { id : string; data_type : data_type option }
type field = { id : string; data_type : data_type; is_pub : bool }
type variant = { id : string; data_type : data_type option }

type argument_kind = Default of ast | Normal

and argument = {
  id : string;
  kind : argument_kind;
  data_type : data_type option;
  loc: location;
}

and argument_method = {
  id_mth : string option;
  kind_mth : argument_kind;
  data_type_mth : data_type option;
}

and case = { expr : expr; body : (ast * location) array }
and if_t = expr * (ast * location) array

(* NOTE: ast option it used in analysis, but for the moment the value is None *)
and expr =
  | Grouping of expr
  | Positive of expr
  | Negative of expr
  | Not of expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Exp of expr * expr
  | Range of expr * expr
  | Lt of expr * expr
  | Gt of expr * expr
  | Le of expr * expr
  | Ge of expr * expr
  | Eq of expr * expr
  | Ne of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Assign of expr * expr
  | AddAssign of expr * expr
  | SubAssign of expr * expr
  | MulAssign of expr * expr
  | DivAssign of expr * expr
  | ModAssign of expr * expr
  | ExpAssign of expr * expr
  | FunctionCall of expr * (ast * ast option) array
  | ClassCall of expr * (ast * ast option) array
  | RecordCall of expr * (string * expr option * ast option) array
  | Identifier of string * ast option
  | IdentifierAccess of expr array * ast option
  | SelfAccess of expr array * ast option
  | AnonymousFunction of argument array * (ast * location) array
  | In of string * ast
  | Tuple of expr array
  | Array of expr array
  | Variant of expr * expr option
  | Undef
  | Nil
  | Literal of literal_ast

and decl =
  | Fun of {
      id : string;
      poly_args : data_type array;
      args : argument array;
      return_type : data_type option;
      body : (ast * location) array;
      is_pub : bool;
      is_async : bool;
      is_test : bool;
      is_export : bool;
    }
  | Variable of {
      id : string;
      data_type : data_type option;
      expr : expr;
      is_mut : bool;
    }
  | Constant of {
      id : string;
      data_type : data_type;
      expr : expr;
      is_pub : bool;
    }
  | Module of {
      id : string;
      body : (ast * location) array;
      is_pub : bool;
      is_test : bool;
    }
  | Alias of {
      id : string;
      poly_args : data_type array;
      data_type : data_type;
      is_pub : bool;
    }
  | Record of {
      id : string;
      poly_args : data_type array;
      fields : (field * location) array;
      is_pub : bool;
    }
  | Enum of {
      id : string;
      poly_args : data_type array;
      variants : (variant * location) array;
      is_pub : bool;
    }
  | Class of {
      id : string;
      poly_args : data_type array;
      inh : string array;
      body : (ast * location) array;
      is_pub : bool;
    }
  | Trait of {
      id : string;
      poly_args : data_type array;
      body : (ast * location) array;
      is_pub : bool;
    }
  | Method of {
      id : string;
      poly_args : data_type array;
      args : argument_method array;
      return_type : data_type option;
      body : (ast * location) array;
      is_pub : bool;
    }
  | Pub of (decl * location) array
  | Property of string * data_type * bool
  | Import of { import : string; is_pub : bool; _as : string option }

and stmt =
  | Return of expr
  | If of {
      if_ : if_t;
      elif_ : if_t array option;
      else_ : (ast * location) array option;
    }
  | Await of expr
  | Try of {
      try_body : (ast * location) array;
      catch_expr : expr;
      catch_body : (ast * location) array;
    }
  | Match of { expr : ast; case : case array; else_case : case option }
  | While of { cond : expr; body : (ast * location) array }
  | For of { expr : expr; body : (ast * location) array }
  | Break
  | Next

and ast = Expr of expr | Decl of decl | Stmt of stmt | Doc of string
