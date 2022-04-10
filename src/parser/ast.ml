open Lily_lexer.Location

type data_type =
  [ `I8
  | `I16
  | `I32
  | `I64
  | `I128
  | `U8
  | `U16
  | `U32
  | `U64
  | `U128
  | `F32
  | `F64
  | `String
  | `Char
  | `Usize
  | `Isize
  | `Bool
  | `Unit
  | `SelfArg
  | `Err
  | `Fun of data_type array * data_type
  | `Array of data_type
  | `Tuple of data_type array
  | `GroupingDataType of data_type
  | `Generics of string
  | `CustomType of string * data_type array option ]
[@@deriving show]

let rec show_data_type = function
  | `I8 -> "Int8"
  | `I16 -> "Int16"
  | `I32 -> "Int32"
  | `I64 -> "Int64"
  | `I128 -> "Int128"
  | `U8 -> "Uint8"
  | `U16 -> "Uint16"
  | `U32 -> "Uint32"
  | `U64 -> "Uint64"
  | `U128 -> "Uint128"
  | `F32 -> "Float32"
  | `F64 -> "Float64"
  | `String -> "String"
  | `Char -> "Char"
  | `Usize -> "Usize"
  | `Isize -> "Isize"
  | `Bool -> "Bool"
  | `Unit -> "Unit"
  | `SelfArg -> "self"
  | `Err -> "Err"
  | `Array dt -> dt |> show_data_type |> Printf.sprintf "[%s]"
  | `Tuple _ -> "Tuple"
  | `GroupingDataType _ -> "Grouping"
  | `Generics _ -> "Generics"
  | `CustomType _ -> "CustomType"
  | _ -> failwith "todo"

type literal_ast =
  | Bool of bool
      [@printer
        fun fmt b -> if b then fprintf fmt "True" else fprintf fmt "False"]
  | Char of char [@printer fun fmt c -> fprintf fmt "%c" c]
  | Int32 of Stdint.int32
      [@printer
        fun fmt i -> fprintf fmt "Int32(%s)" (Stdint.Int32.to_string i)]
  | Int64 of Stdint.int64
      [@printer
        fun fmt i -> fprintf fmt "Int64(%s)" (Stdint.Int64.to_string i)]
  | Int128 of Stdint.int128
      [@printer
        fun fmt i -> fprintf fmt "Int128(%s)" (Stdint.Int128.to_string i)]
  | Float of float
      [@printer fun fmt f -> fprintf fmt "%s" (Float.to_string f)]
  | String of string [@printer fun fmt s -> fprintf fmt "%s" s]
  | Unit [@printer fun fmt _ -> fprintf fmt "()"]
[@@deriving show]

(* type argument_proto = { id : string; data_type : data_type option } *)

type field = {
  id : string; [@printer fun fmt s -> fprintf fmt "%s" s]
  data_type : data_type;
      [@printer fun fmt dt -> fprintf fmt "%s" (show_data_type dt)]
  is_pub : bool;
      [@printer
        fun fmt b -> if b then fprintf fmt "True" else fprintf fmt "False"]
}
[@@deriving show]

type variant = {
  id : string; [@printer fun fmt s -> fprintf fmt "%s" s]
  data_type : data_type option;
      [@printer
        fun fmt dt_op ->
          match dt_op with
          | Some dt -> fprintf fmt "%s" (show_data_type dt)
          | None -> fprintf fmt "None"]
}
[@@deriving show]

type argument_kind =
  | Default of ast
      [@printer fun fmt a -> fprintf fmt "Default: %s" (show_ast a)]
  | Normal [@printer fun fmt _ -> fprintf fmt "Normal"]
[@@deriving show]

and argument = {
  id : string; [@printer fun fmt -> fprintf fmt "%s"]
  kind : argument_kind;
      [@printer fun fmt k -> fprintf fmt "%s" (show_argument_kind k)]
  data_type : data_type option;
      [@printer
        fun fmt dt ->
          match dt with
          | Some d -> fprintf fmt "%s" (show_data_type d)
          | None -> fprintf fmt "None"]
  loc : location; [@printer fun fmt _ -> fprintf fmt "Location"]
}
[@@deriving show]

and argument_method = {
  id_mth : string option;
      [@printer
        fun fmt id ->
          match id with
          | Some s -> fprintf fmt "%s" s
          | None -> fprintf fmt "None"]
  kind_mth : argument_kind;
      [@printer fun fmt k -> fprintf fmt "%s" (show_argument_kind k)]
  data_type_mth : data_type option;
      [@printer
        fun fmt dt ->
          match dt with
          | Some d -> fprintf fmt "%s" (show_data_type d)
          | None -> fprintf fmt "None"]
}
[@@deriving show]

and case = {
  expr : expr; [@printer fun fmt e -> fprintf fmt "%s" (show_expr e)]
  cond : expr option;
  body : (ast * location) array;
      [@printer
        fun fmt body_arr ->
          let rec loop ?(i = 0) ?(body = []) () =
            if i < Array.length body_arr then
              loop ~i:(i + 1)
                ~body:
                  (Printf.sprintf "(%s, %s)"
                     (show_ast (match body_arr.(i) with a, _ -> a))
                     (show_location (match body_arr.(i) with _, l -> l))
                  :: body)
                ()
            else body |> List.rev |> String.concat ", "
          in
          let s = loop () in
          fprintf fmt "[%s]" s]
}
[@@deriving show]

and args_fun_call = string option * ast [@@deriving show]
and if_t = expr * (ast * location) array

and poly_args_kind =
  | Datatype of data_type
  | RestrictedDatatype of data_type * data_type
[@@deriving show]

(* NOTE: ast option it used in analysis, but for the moment the value is
   None *)
and expr =
  | Grouping of expr [@printer fun fmt e -> fprintf fmt "(%s)" (show_expr e)]
  | Positive of expr [@printer fun fmt e -> fprintf fmt "+%s" (show_expr e)]
  | Negative of expr [@printer fun fmt e -> fprintf fmt "-%s" (show_expr e)]
  | Not of expr [@printer fun fmt e -> fprintf fmt "not %s" (show_expr e)]
  | Mul of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s * %s" (show_expr e1) (show_expr e2)]
  | Div of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s / %s" (show_expr e1) (show_expr e2)]
  | Mod of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s %% %s" (show_expr e1) (show_expr e2)]
  | Add of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s + %s" (show_expr e1) (show_expr e2)]
  | Sub of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s - %s" (show_expr e1) (show_expr e2)]
  | Exp of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s ^ %s" (show_expr e1) (show_expr e2)]
  | Range of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s..%s" (show_expr e1) (show_expr e2)]
  | Lt of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s < %s" (show_expr e1) (show_expr e2)]
  | Gt of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s > %s" (show_expr e1) (show_expr e2)]
  | Le of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s <= %s" (show_expr e1) (show_expr e2)]
  | Ge of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s >= %s" (show_expr e1) (show_expr e2)]
  | Eq of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s == %s" (show_expr e1) (show_expr e2)]
  | Ne of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s != %s" (show_expr e1) (show_expr e2)]
  | And of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s and %s" (show_expr e1) (show_expr e2)]
  | Or of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s or %s" (show_expr e1) (show_expr e2)]
  | Assign of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s = %s" (show_expr e1) (show_expr e2)]
  | AddAssign of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s += %s" (show_expr e1) (show_expr e2)]
  | SubAssign of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s -= %s" (show_expr e1) (show_expr e2)]
  | MulAssign of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s *= %s" (show_expr e1) (show_expr e2)]
  | DivAssign of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s /= %s" (show_expr e1) (show_expr e2)]
  | ModAssign of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s %%= %s" (show_expr e1) (show_expr e2)]
  | ExpAssign of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "%s ^= %s" (show_expr e1) (show_expr e2)]
  | Wildcard [@printer fun fmt _ -> fprintf fmt "_"]
  | FunctionCall of expr * args_fun_call array
      (* TODO: create a function for convert to string expr * (ast * ast
         option) array if it's possible *)
      [@printer
        fun fmt (e, tp_arr) ->
          let rec loop ?(i = 0) ?(l = []) () =
            if i < Array.length tp_arr then
              loop ~i:(i + 1) ~l:(show_args_fun_call tp_arr.(i) :: l) ()
            else l |> List.rev |> String.concat ", "
          in
          let tp_s = loop () in
          fprintf fmt "%s(%s)" (show_expr e) tp_s]
  | ClassCall of expr * ast array
      [@printer
        fun fmt (e, tp_arr) ->
          let rec loop ?(i = 0) ?(l = []) () =
            if i < Array.length tp_arr then
              loop ~i:(i + 1)
                ~l:(Printf.sprintf "%s" (show_ast tp_arr.(i)) :: l)
                ()
            else l |> List.rev |> String.concat ", "
          in
          let tp_s = loop () in
          fprintf fmt "new %s(%s)" (show_expr e) tp_s]
  | RecordCall of expr * (string * expr option) array
      [@printer
        fun fmt (e, tp_arr) ->
          let rec loop ?(i = 0) ?(l = []) () =
            if i < Array.length tp_arr then
              loop ~i:(i + 1)
                ~l:
                  (Printf.sprintf "(%s, %s)"
                     (match tp_arr.(i) with s, _ -> s)
                     (match match tp_arr.(i) with _, e_op -> e_op with
                     | Some expr -> show_expr expr
                     | None -> "None")
                  :: l)
                ()
            else l |> List.rev |> String.concat ", "
          in
          let tp_s = loop () in
          fprintf fmt "%s { %s }" (show_expr e) tp_s]
  | Identifier of string * ast option
      [@printer
        fun fmt (s, a_op) ->
          fprintf fmt "(%s, %s)" s
            (match a_op with Some a -> show_ast a | None -> "None")]
  | IdentifierAccess of expr array * ast option
      [@printer
        fun fmt (s_arr, a_op) ->
          let rec loop ?(i = 0) ?(l = []) () =
            if i < Array.length s_arr then
              loop ~i:(i + 1) ~l:(show_expr s_arr.(i) :: l) ()
            else l |> List.rev |> String.concat "."
          in
          let s = loop () in
          fprintf fmt "(%s, %s)" s
            (match a_op with Some a -> show_ast a | None -> "None")]
  | SelfAccess of expr array * ast option
      [@printer
        fun fmt (s_arr, a_op) ->
          let rec loop ?(i = 0) ?(l = []) () =
            if i < Array.length s_arr then
              loop ~i:(i + 1)
                ~l:(Printf.sprintf "%s" (show_expr s_arr.(i)) :: l)
                ()
            else l |> List.rev |> String.concat "."
          in
          let s = loop () in
          fprintf fmt "(self.%s, %s)" s
            (match a_op with Some a -> show_ast a | None -> "None")]
  | ArrayAccess of expr * expr array
  | TupleAccess of expr * expr array
  | AnonymousFunction of argument array * (ast * location) array
      [@printer
        fun fmt (args, body) ->
          let rec loop_args ?(i = 0) ?(l = []) () =
            if i < Array.length args then
              loop_args ~i:(i + 1)
                ~l:(Printf.sprintf "%s" (show_argument args.(i)) :: l)
                ()
            else l |> List.rev |> String.concat ", "
          in
          let args_s = loop_args () in
          let rec loop_body ?(i = 0) ?(l = []) () =
            if i < Array.length body then
              loop_body ~i:(i + 1)
                ~l:
                  (Printf.sprintf "%s\n"
                     (show_ast (match body.(i) with a, _ -> a))
                  :: l)
                ()
            else l |> List.rev |> String.concat ""
          in
          let body_s = loop_body () in
          fprintf fmt "fun(%s) =>\n%send" args_s body_s]
  | In of expr * expr
      [@printer
        fun fmt (e1, e2) ->
          fprintf fmt "(%s, %s)" (show_expr e1) (show_expr e2)]
  (* TODO: replace string by expr *)
  | Tuple of expr array
      [@printer
        fun fmt arr ->
          let rec loop ?(i = 0) ?(l = []) () =
            if i < Array.length arr then
              loop ~i:(i + 1)
                ~l:(Printf.sprintf "%s" (show_expr arr.(i)) :: l)
                ()
            else l |> List.rev |> String.concat ", "
          in
          let s = loop () in
          fprintf fmt "(%s)" s]
  | Array of expr array
      [@printer
        fun fmt arr ->
          let rec loop ?(i = 0) ?(l = []) () =
            if i < Array.length arr then
              loop ~i:(i + 1)
                ~l:(Printf.sprintf "%s" (show_expr arr.(i)) :: l)
                ()
            else l |> List.rev |> String.concat ", "
          in
          let s = loop () in
          fprintf fmt "[%s]" s]
  | Variant of expr * expr option
      [@printer
        fun fmt (s, e_op) ->
          fprintf fmt "%s%s" (show_expr s)
            (match e_op with Some e -> show_expr e | None -> "")]
  | Self [@printer fun fmt _ -> fprintf fmt "self"]
  | Undef [@printer fun fmt _ -> fprintf fmt "undef"]
  | Nil [@printer fun fmt _ -> fprintf fmt "nil"]
  | Literal of literal_ast
      [@printer fun fmt l -> fprintf fmt "%s" (show_literal_ast l)]
[@@deriving show]

and decl =
  | Fun of {
      id : string; [@printer fun fmt s -> fprintf fmt "%s" s]
      poly_args : poly_args_kind array;
          [@printer
            fun fmt poly_args_arr ->
              let rec loop_poly_args ?(i = 0) ?(l = []) () =
                if i < Array.length poly_args_arr then
                  loop_poly_args ~i:(i + 1)
                    ~l:(show_poly_args_kind poly_args_arr.(i) :: l)
                    ()
                else l |> List.rev |> String.concat ", "
              in
              let poly_args = loop_poly_args () in
              fprintf fmt "[%s]" poly_args]
      args : argument array;
          [@printer
            fun fmt args_arr ->
              let rec loop_args ?(i = 0) ?(l = []) () =
                if i < Array.length args_arr then
                  loop_args ~i:(i + 1)
                    ~l:(show_argument args_arr.(i) :: l)
                    ()
                else l |> List.rev |> String.concat ", "
              in
              let args = loop_args () in
              fprintf fmt "[%s]" args]
      return_type : data_type option;
          [@printer
            fun fmt dt_op ->
              fprintf fmt "%s"
                (match dt_op with
                | Some dt -> show_data_type dt
                | None -> "None")]
      body : (ast * location) array;
          [@printer
            fun fmt body_arr ->
              let rec loop_body ?(i = 0) ?(l = []) () =
                if i < Array.length body_arr then
                  loop_body ~i:(i + 1)
                    ~l:
                      (Printf.sprintf "%s"
                         (show_ast (match body_arr.(i) with n, _ -> n))
                      :: l)
                    ()
                else l |> List.rev |> String.concat ", "
              in
              let body_s = loop_body () in
              fprintf fmt "[%s]" body_s]
      is_pub : bool;
          [@printer
            fun fmt b -> fprintf fmt "%s" (if b then "True" else "False")]
      is_async : bool;
          [@printer
            fun fmt b -> fprintf fmt "%s" (if b then "True" else "False")]
      is_test : bool;
          [@printer
            fun fmt b -> fprintf fmt "%s" (if b then "True" else "False")]
      is_export : bool;
          [@printer
            fun fmt b -> fprintf fmt "%s" (if b then "True" else "False")]
    }
  | Variable of {
      id : string;
      data_type : data_type option;
      expr : expr;
      is_mut : bool;
    }
  (*[@printer fun fmt v -> fprintf fmt "%s%s%s := %s" (match v.is_mut with
    true -> "mut " | false -> "") v.id (match v.data_type with | Some dt -> "
    " ^ show_data_type dt | None -> "") (show_expr v.expr)]*)
  | Constant of {
      id : string; [@printer fun fmt s -> fprintf fmt "%s" s]
      data_type : data_type;
          [@printer fun fmt dt -> fprintf fmt "%s" (show_data_type dt)]
      expr : expr; [@printer fun fmt e -> fprintf fmt "%s" (show_expr e)]
      is_pub : bool;
          [@printer
            fun fmt b -> fprintf fmt "%s" (if b then "True" else "False")]
    }
  | Module of {
      id : string;
      body : (ast * location) array;
          [@printer
            fun fmt arr ->
              let arr_ast = arr |> Array.map (fun (x, _) -> x) in
              let rec loop ?(i = 0) ?(l = []) () =
                if i < Array.length arr_ast then
                  loop ~i:(i + 1) ~l:(show_ast arr_ast.(i) :: l) ()
                else l |> List.rev |> String.concat ", "
              in
              fprintf fmt "[%s]" (loop ())]
      is_pub : bool;
      is_test : bool;
    }
  (* [@printer fun fmt m -> fprintf fmt "Hello"] *)
  | Alias of {
      id : string;
      poly_args : poly_args_kind array;
      data_type : data_type;
      is_pub : bool;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Record of {
      id : string;
      poly_args : poly_args_kind array;
      fields : (field * location) array;
      is_pub : bool;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Enum of {
      id : string;
      poly_args : poly_args_kind array;
      variants : (variant * location) array;
      is_pub : bool;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Error of {
      id : string;
      poly_args : poly_args_kind array;
      variant : data_type option * location;
      is_pub : bool;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Class of {
      id : string;
      poly_args : poly_args_kind array;
      inh : expr array;
      body : (ast * location) array;
      is_pub : bool;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Trait of {
      id : string;
      poly_args : poly_args_kind array;
      body : (ast * location) array;
      is_pub : bool;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Method of {
      id : string;
      poly_args : poly_args_kind array;
      args : argument_method array;
      return_type : data_type option;
      body : (ast * location) array;
      is_pub : bool;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Property of string * data_type * bool
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Include of expr
      [@printer fun fmt e -> fprintf fmt "include %s" (show_expr e)]
  | Import of {
      import : string; [@printer fun fmt s -> fprintf fmt "%s" s]
      is_pub : bool;
          [@printer
            fun fmt b -> fprintf fmt "%s" (if b then "True" else "False")]
      _as : string option;
          [@printer
            fun fmt s_op ->
              fprintf fmt "%s"
                (match s_op with Some s -> s | None -> "None")]
    }
[@@deriving show]

and stmt =
  | Return of expr [@printer fun fmt e -> fprintf fmt "%s" (show_expr e)]
  | If of {
      if_ : if_t;
      elif_ : if_t array option;
      else_ : (ast * location) array option;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Await of expr (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Try of {
      try_body : (ast * location) array;
      catch_expr : expr;
      catch_body : (ast * location) array;
    }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | Match of { expr : ast; case : case array }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | While of { cond : expr; body : (ast * location) array }
  (* [@printer fun fmt _ -> fprintf fmt "Hello"] *)
  | For of { expr : expr; body : (ast * location) array }
  (* [@printer fun fmt { expr; body; } -> fprintf fmt "Hello"] *)
  | Break [@printer fun fmt _ -> fprintf fmt "break"]
  | Next [@printer fun fmt _ -> fprintf fmt "next"]
[@@deriving show]

and ast =
  | Expr of expr [@printer fun fmt e -> fprintf fmt "%s" (show_expr e)]
  | Decl of decl [@printer fun fmt d -> fprintf fmt "%s" (show_decl d)]
  | Stmt of stmt [@printer fun fmt s -> fprintf fmt "%s" (show_stmt s)]
  | Doc of string [@printer fun fmt -> fprintf fmt "%s"]
[@@deriving show]

let ast_to_expr = function Expr e -> e | _ -> failwith "unreachable"
let expr_to_ast = function e -> Expr e
