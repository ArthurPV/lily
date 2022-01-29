module Diagnostic = Diagnostic
open Location
open Source
open Token

type lexer = {
  src : source;
  loc : location;
  mutable tokens : (token * location) array;
  mutable errors : Diagnostic.diagnostic array;
}

val new_lexer : source -> lexer
val tok_of_tokens : lexer -> idx:int -> token
val loc_of_tokens : lexer -> idx:int -> location
val get_keyword : string -> token
val next_char : lexer -> unit
val previous_char : lexer -> unit
val start_token : lexer -> unit
val end_token : lexer -> unit
val peek_char : lexer -> n:int -> char option
val is_digit : lexer -> bool
val is_ident : lexer -> bool
val is_hex : lexer -> bool
val is_oct : lexer -> bool
val is_bin : lexer -> bool
val is_num : lexer -> bool

val new_diagnostic :
  lexer ->
  Diagnostic.diagnostic_kind ->
  string ->
  location ->
  Diagnostic.diagnostic

val get_escape : lexer -> c:char -> string
val scan_comment_one : lexer -> token
val scan_comment_multi : lexer -> token
val scan_comment_doc : lexer -> token
val scan_identifier : ?id:char list -> lexer -> token
val scan_char : lexer -> token
val scan_string : lexer -> token
val scan_hex : lexer -> token
val scan_oct : lexer -> token
val scan_bin : lexer -> token
val scan_num : ?num:char list -> ?is_float:bool -> lexer -> token
val get_all_num : lexer -> token
val run : lexer -> unit
