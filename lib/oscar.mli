(** Oscar: A simple monadic parser combinator library *)

(** {1 Types} *)

type input = { text : string; pos : int }
(** The input state for the parser. *)

type error = { desc : string; pos : int }
(** The result of a parser error. *)

type 'a parser = { run : input -> (input * 'a, error) result }
(** The type of a parser that returns a value of type ['a]. *)

(** {1 Running Parsers} *)

val make_input : string -> input
(** [make_input text] creates a new input from a string starting at position 0.
*)

val parse : 'a parser -> string -> ('a, error) result
(** [parse p text] runs the parser [p] on the string [text]. Returns [Ok x] if
    parsing succeeded, or [Error e] otherwise. *)

val gen_parser : (input -> (input * 'a, error) result) -> 'a parser
(** [gen_parser f] creates a parser from a custom function [f]. *)

(** {1 Combinators} *)

val return : 'a -> 'a parser
(** [return x] is a parser that always succeeds with value [x] without consuming
    any input. *)

val bind : 'a parser -> ('a -> 'b parser) -> 'b parser
(** [bind p f] runs the parser [p] to get a value, then applies [f] to that
    value to get a new parser, and runs it. *)

val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
(** Infix version of [bind]. *)

val fail : error -> 'a parser
(** [fail e] is a parser that always fails with error [e]. *)

val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser
(** Applicative sequence: [pf <*> pa] runs [pf] to get a function, then [pa] to
    get an argument, and applies the function. *)

val ( *> ) : 'a parser -> 'b parser -> 'b parser
(** Sequence: [pa *> pb] runs [pa], discards its result, then runs [pb]. *)

val ( <* ) : 'a parser -> 'b parser -> 'a parser
(** Sequence: [pa <* pb] runs [pa], then runs [pb], and returns the result of
    [pa]. *)

val fmap : ('a -> 'b) -> 'a parser -> 'b parser
(** Functor map: [fmap f p] applies [f] to the result of [p]. *)

val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
(** Alias for [fmap]. *)

val void : 'a parser -> unit parser
(** ignore. *)

val empty : 'a parser
(** A parser that always fails. *)

val ( <|> ) : 'a parser -> 'a parser -> 'a parser
(** Choice: [pa <|> pb] tries [pa]. If it fails, it tries [pb]. *)

val optional : 'a parser -> 'a option parser
(** Optional: [optional p] returns [Some x] if [p] succeeds, or [None] if [p]
    fails. *)

val ( <?> ) : 'a parser -> string -> 'a parser
(** Label: [p <?> msg] replaces the error message of [p] with [msg] if it fails.
*)

val many : 'a parser -> 'a list parser
(** Zero or more: [many p] runs [p] repeatedly until it fails, and returns a
    list of results. *)

val many1 : 'a parser -> 'a list parser
(** One or more: [many1 p] runs [p] one or more times, and returns a list of
    results. *)

val sep_by : 'a parser -> 'b parser -> 'b list parser
(** Separated: [sep_by sep p] parses zero or more [p] separated by [sep]. *)

val sep_by1 : 'a parser -> 'b parser -> 'b list parser
(** Separated: [sep_by1 sep p] parses one or more [p] separated by [sep]. *)

val fix : ('a parser -> 'a parser) -> 'a parser
(** Fixed-point combinator for recursive parsers. *)

(** {1 Primitive Parsers} *)

val any_char : char parser
(** Parses any single character. *)

val satisfy : (char -> bool) -> char parser
(** Parses a character satisfying the predicate. *)

val char : char -> char parser
(** Parses the specific character. *)

val string : string -> string parser
(** Parses the specific string. *)

val advance : int -> unit parser
(** Consumes [n] characters. *)

val take_while : (char -> bool) -> string parser
(** Consumes characters satisfying the predicate. *)

val take_while1 : (char -> bool) -> string parser
(** Consumes one or more characters satisfying the predicate. *)

val skip_while : (char -> bool) -> unit parser
(** Skips characters satisfying the predicate. *)

val skip_while1 : (char -> bool) -> unit parser
(** Skips at least one characters satisfying the predicate. *)

val not_null : string parser -> string parser
(** Ensures the parser [p] returns a non-empty string. *)

val end_of_file : unit parser
(** [end_of_file] succeeds if the end of the input has been reached, and fails
    otherwise. *)

val skip_many : 'a parser -> unit parser
(** [skip_many p] runs [p] repeatedly until it fails, and ignores the results.
*)

val peek_char : char option parser
(** [peek_char] looks at the next character without consuming it. *)
