module MenhirBasics : sig
  exception Error

  val _eRR : 'a -> 'b

  type token =
    | WITH
    | WHEN
    | VARIANT
    | UNDERSCORE
    | UIDENT of string
    | TRUE
    | TILDE
    | THEN
    | STRING of string
    | STAR
    | SEMICOLON
    | RIGHTSQ
    | RIGHTPAR
    | RIGHTBRC
    | REQUIRES
    | REC
    | RAISES
    | QUOTE_LIDENT of string
    | QUESTION
    | PURE
    | PREDICATE
    | OR
    | OPPREF of string
    | OP4 of string
    | OP3 of string
    | OP2 of string
    | OP1 of string
    | OLD
    | NOT
    | MUTABLE
    | MODIFIES
    | MODEL
    | MATCH
    | LTGT
    | LRARROW
    | LIDENT of string
    | LET
    | LEFTSQRIGHTSQ
    | LEFTSQ
    | LEFTPAR
    | LEFTBRCRIGHTBRC
    | LEFTBRCCOLON
    | LEFTBRC
    | INVARIANT
    | INTEGER of (string * char option)
    | IN
    | IF
    | FUNCTION
    | FUN
    | FORALL
    | FLOAT of string
    | FALSE
    | EXISTS
    | EQUIVALENT
    | EQUAL
    | EPHEMERAL
    | EOF
    | ENSURES
    | ELSE
    | DOTDOT
    | DOT
    | DIVERGES
    | CONSUMES
    | COMMA
    | COLONRIGHTBRC
    | COLONCOLON
    | COLON
    | COERCION
    | CHECKS
    | CHAR of char
    | BARBAR
    | BAR
    | BACKQUOTE_LIDENT of string
    | AXIOM
    | ATTRIBUTE of string
    | AS
    | ARROW
    | AND
    | AMPAMP
end

exception Error

val _eRR : 'a -> 'b

type token = MenhirBasics.token =
  | WITH
  | WHEN
  | VARIANT
  | UNDERSCORE
  | UIDENT of string
  | TRUE
  | TILDE
  | THEN
  | STRING of string
  | STAR
  | SEMICOLON
  | RIGHTSQ
  | RIGHTPAR
  | RIGHTBRC
  | REQUIRES
  | REC
  | RAISES
  | QUOTE_LIDENT of string
  | QUESTION
  | PURE
  | PREDICATE
  | OR
  | OPPREF of string
  | OP4 of string
  | OP3 of string
  | OP2 of string
  | OP1 of string
  | OLD
  | NOT
  | MUTABLE
  | MODIFIES
  | MODEL
  | MATCH
  | LTGT
  | LRARROW
  | LIDENT of string
  | LET
  | LEFTSQRIGHTSQ
  | LEFTSQ
  | LEFTPAR
  | LEFTBRCRIGHTBRC
  | LEFTBRCCOLON
  | LEFTBRC
  | INVARIANT
  | INTEGER of (string * char option)
  | IN
  | IF
  | FUNCTION
  | FUN
  | FORALL
  | FLOAT of string
  | FALSE
  | EXISTS
  | EQUIVALENT
  | EQUAL
  | EPHEMERAL
  | EOF
  | ENSURES
  | ELSE
  | DOTDOT
  | DOT
  | DIVERGES
  | CONSUMES
  | COMMA
  | COLONRIGHTBRC
  | COLONCOLON
  | COLON
  | COERCION
  | CHECKS
  | CHAR of char
  | BARBAR
  | BAR
  | BACKQUOTE_LIDENT of string
  | AXIOM
  | ATTRIBUTE of string
  | AS
  | ARROW
  | AND
  | AMPAMP

val mk_loc : Lexing.position * Lexing.position -> Ppxlib.Location.t
val mk_pid : string -> Lexing.position * Lexing.position -> Gospel.Uast.Preid.t

val mk_term :
  Gospel.Uast.term_desc -> Lexing.position * Lexing.position -> Gospel.Uast.term

val mk_pat :
  Gospel.Uast.pat_desc ->
  Lexing.position * Lexing.position ->
  Gospel.Uast.pattern

val get_op : Lexing.position * Lexing.position -> Gospel.Uast.qualid
val set_op : Lexing.position * Lexing.position -> Gospel.Uast.qualid
val sub_op : Lexing.position * Lexing.position -> Gospel.Uast.qualid
val above_op : Lexing.position * Lexing.position -> Gospel.Uast.qualid
val below_op : Lexing.position * Lexing.position -> Gospel.Uast.qualid
val id_anonymous : Ppxlib.Location.t -> Gospel.Uast.Preid.t
val array_get : Lexing.position * Lexing.position -> Gospel.Uast.qualid
val empty_vspec : Gospel.Uast.val_spec
val empty_fspec : Gospel.Uast.fun_spec
val loc_of_qualid : Gospel.Uast.qualid -> Ppxlib.Location.t
val qualid_preid : Gospel.Uast.qualid -> Gospel.Uast.Preid.t

module Tables : sig
  exception Error

  val _eRR : 'a -> 'b

  type token = MenhirBasics.token =
    | WITH
    | WHEN
    | VARIANT
    | UNDERSCORE
    | UIDENT of string
    | TRUE
    | TILDE
    | THEN
    | STRING of string
    | STAR
    | SEMICOLON
    | RIGHTSQ
    | RIGHTPAR
    | RIGHTBRC
    | REQUIRES
    | REC
    | RAISES
    | QUOTE_LIDENT of string
    | QUESTION
    | PURE
    | PREDICATE
    | OR
    | OPPREF of string
    | OP4 of string
    | OP3 of string
    | OP2 of string
    | OP1 of string
    | OLD
    | NOT
    | MUTABLE
    | MODIFIES
    | MODEL
    | MATCH
    | LTGT
    | LRARROW
    | LIDENT of string
    | LET
    | LEFTSQRIGHTSQ
    | LEFTSQ
    | LEFTPAR
    | LEFTBRCRIGHTBRC
    | LEFTBRCCOLON
    | LEFTBRC
    | INVARIANT
    | INTEGER of (string * char option)
    | IN
    | IF
    | FUNCTION
    | FUN
    | FORALL
    | FLOAT of string
    | FALSE
    | EXISTS
    | EQUIVALENT
    | EQUAL
    | EPHEMERAL
    | EOF
    | ENSURES
    | ELSE
    | DOTDOT
    | DOT
    | DIVERGES
    | CONSUMES
    | COMMA
    | COLONRIGHTBRC
    | COLONCOLON
    | COLON
    | COERCION
    | CHECKS
    | CHAR of char
    | BARBAR
    | BAR
    | BACKQUOTE_LIDENT of string
    | AXIOM
    | ATTRIBUTE of string
    | AS
    | ARROW
    | AND
    | AMPAMP

  val token2terminal : token -> int
  val error_terminal : int
  val token2value : token -> Obj.t
  val default_reduction : int * string
  val error : int * string
  val start : int
  val action : (int * string) * (int * string)
  val lhs : int * string
  val goto : (int * string) * (int * string)

  val semantic_action :
    ((int, Obj.t, MenhirBasics.token) MenhirLib.EngineTypes.env ->
    (int, Obj.t) MenhirLib.EngineTypes.stack)
    array

  val trace : 'a option
end

module MenhirInterpreter : sig
  module ET : sig
    type state = int

    val number : state -> int

    type token = Tables.token
    type terminal = int
    type nonterminal = int
    type semantic_value = Obj.t

    val token2terminal : token -> terminal
    val token2value : token -> semantic_value
    val error_terminal : terminal
    val error_value : semantic_value
    val foreach_terminal : (terminal -> 'a -> 'a) -> 'a -> 'a

    type production = int

    val production_index : production -> int
    val find_production : int -> production

    val default_reduction :
      state ->
      ('env -> production -> 'answer) ->
      ('env -> 'answer) ->
      'env ->
      'answer

    val action :
      state ->
      terminal ->
      semantic_value ->
      ('env -> bool -> terminal -> semantic_value -> state -> 'answer) ->
      ('env -> production -> 'answer) ->
      ('env -> 'answer) ->
      'env ->
      'answer

    val maybe_shift_t : state -> terminal -> state option
    val may_reduce_prod : state -> terminal -> production -> bool
    val goto_nt : state -> nonterminal -> state
    val goto_prod : state -> production -> state
    val maybe_goto_nt : state -> nonterminal -> state option
    val lhs : production -> nonterminal
    val is_start : production -> bool

    exception Error

    type semantic_action =
      (state, semantic_value, token) MenhirLib.EngineTypes.env ->
      (state, semantic_value) MenhirLib.EngineTypes.stack

    val semantic_action : production -> semantic_action
    val may_reduce : state -> production -> bool
    val log : bool

    module Log : sig
      val state : state -> unit
      val shift : terminal -> state -> unit
      val reduce_or_accept : production -> unit

      val lookahead_token :
        terminal -> Lexing.position -> Lexing.position -> unit

      val initiating_error_handling : unit -> unit
      val resuming_error_handling : unit -> unit
      val handling_error : state -> unit
    end
  end

  module TI : sig
    type state = ET.state
    type token = ET.token
    type semantic_value = ET.semantic_value

    exception Error

    val entry :
      [ `Legacy | `Simplified ] ->
      state ->
      (Lexing.lexbuf -> token) ->
      Lexing.lexbuf ->
      semantic_value

    type production = ET.production

    type 'a env =
      (ET.state, ET.semantic_value, ET.token) MenhirLib.EngineTypes.env

    type 'a checkpoint = 'a MenhirLib.Engine.Make(ET).checkpoint = private
      | InputNeeded of 'a env
      | Shifting of 'a env * 'a env * bool
      | AboutToReduce of 'a env * production
      | HandlingError of 'a env
      | Accepted of 'a
      | Rejected

    val offer :
      'a checkpoint ->
      token
      * MenhirLib.IncrementalEngine.position
      * MenhirLib.IncrementalEngine.position ->
      'a checkpoint

    type strategy = [ `Legacy | `Simplified ]

    val resume : ?strategy:strategy -> 'a checkpoint -> 'a checkpoint

    type supplier =
      unit ->
      token
      * MenhirLib.IncrementalEngine.position
      * MenhirLib.IncrementalEngine.position

    val lexer_lexbuf_to_supplier :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> supplier

    val loop : ?strategy:strategy -> supplier -> 'a checkpoint -> 'a

    val loop_handle :
      ('a -> 'answer) ->
      ('a checkpoint -> 'answer) ->
      supplier ->
      'a checkpoint ->
      'answer

    val loop_handle_undo :
      ('a -> 'answer) ->
      ('a checkpoint -> 'a checkpoint -> 'answer) ->
      supplier ->
      'a checkpoint ->
      'answer

    val shifts : 'a checkpoint -> 'a env option

    val acceptable :
      'a checkpoint -> token -> MenhirLib.IncrementalEngine.position -> bool

    type 'a lr1state = state

    val number : 'a lr1state -> int
    val production_index : production -> int
    val find_production : int -> production

    type element = MenhirLib.Engine.Make(ET).element =
      | Element :
          'a lr1state
          * 'a
          * MenhirLib.IncrementalEngine.position
          * MenhirLib.IncrementalEngine.position
          -> element

    type stack = element MenhirLib.General.stream

    val stack : 'a env -> stack
    val top : 'a env -> element option
    val pop_many : int -> 'a env -> 'a env option
    val get : int -> 'a env -> element option
    val current_state_number : 'a env -> int
    val equal : 'a env -> 'a env -> bool

    val positions :
      'a env ->
      MenhirLib.IncrementalEngine.position
      * MenhirLib.IncrementalEngine.position

    val env_has_default_reduction : 'a env -> bool
    val state_has_default_reduction : 'a lr1state -> bool
    val pop : 'a env -> 'a env option
    val force_reduction : production -> 'a env -> 'a env
    val input_needed : 'a env -> 'a checkpoint
    val start : state -> Lexing.position -> semantic_value checkpoint
  end

  type state = ET.state
  type token = ET.token
  type semantic_value = ET.semantic_value

  exception Error

  val entry :
    [ `Legacy | `Simplified ] ->
    state ->
    (Lexing.lexbuf -> token) ->
    Lexing.lexbuf ->
    semantic_value

  type production = ET.production

  type 'a env =
    (ET.state, ET.semantic_value, ET.token) MenhirLib.EngineTypes.env

  type 'a checkpoint = 'a MenhirLib.Engine.Make(ET).checkpoint = private
    | InputNeeded of 'a env
    | Shifting of 'a env * 'a env * bool
    | AboutToReduce of 'a env * production
    | HandlingError of 'a env
    | Accepted of 'a
    | Rejected

  val offer :
    'a checkpoint ->
    token
    * MenhirLib.IncrementalEngine.position
    * MenhirLib.IncrementalEngine.position ->
    'a checkpoint

  type strategy = [ `Legacy | `Simplified ]

  val resume : ?strategy:strategy -> 'a checkpoint -> 'a checkpoint

  type supplier =
    unit ->
    token
    * MenhirLib.IncrementalEngine.position
    * MenhirLib.IncrementalEngine.position

  val lexer_lexbuf_to_supplier :
    (Lexing.lexbuf -> token) -> Lexing.lexbuf -> supplier

  val loop : ?strategy:strategy -> supplier -> 'a checkpoint -> 'a

  val loop_handle :
    ('a -> 'answer) ->
    ('a checkpoint -> 'answer) ->
    supplier ->
    'a checkpoint ->
    'answer

  val loop_handle_undo :
    ('a -> 'answer) ->
    ('a checkpoint -> 'a checkpoint -> 'answer) ->
    supplier ->
    'a checkpoint ->
    'answer

  val shifts : 'a checkpoint -> 'a env option

  val acceptable :
    'a checkpoint -> token -> MenhirLib.IncrementalEngine.position -> bool

  type 'a lr1state = state

  val number : 'a lr1state -> int
  val production_index : production -> int
  val find_production : int -> production

  type element = MenhirLib.Engine.Make(ET).element =
    | Element :
        'a lr1state
        * 'a
        * MenhirLib.IncrementalEngine.position
        * MenhirLib.IncrementalEngine.position
        -> element

  type stack = element MenhirLib.General.stream

  val stack : 'a env -> stack
  val top : 'a env -> element option
  val pop_many : int -> 'a env -> 'a env option
  val get : int -> 'a env -> element option
  val current_state_number : 'a env -> int
  val equal : 'a env -> 'a env -> bool

  val positions :
    'a env ->
    MenhirLib.IncrementalEngine.position * MenhirLib.IncrementalEngine.position

  val env_has_default_reduction : 'a env -> bool
  val state_has_default_reduction : 'a lr1state -> bool
  val pop : 'a env -> 'a env option
  val force_reduction : production -> 'a env -> 'a env
  val input_needed : 'a env -> 'a checkpoint
  val start : state -> Lexing.position -> semantic_value checkpoint

  module Symbols : sig
    type _ terminal =
      | T_error : unit terminal
      | T_WITH : unit terminal
      | T_WHEN : unit terminal
      | T_VARIANT : unit terminal
      | T_UNDERSCORE : unit terminal
      | T_UIDENT : string terminal
      | T_TRUE : unit terminal
      | T_TILDE : unit terminal
      | T_THEN : unit terminal
      | T_STRING : string terminal
      | T_STAR : unit terminal
      | T_SEMICOLON : unit terminal
      | T_RIGHTSQ : unit terminal
      | T_RIGHTPAR : unit terminal
      | T_RIGHTBRC : unit terminal
      | T_REQUIRES : unit terminal
      | T_REC : unit terminal
      | T_RAISES : unit terminal
      | T_QUOTE_LIDENT : string terminal
      | T_QUESTION : unit terminal
      | T_PURE : unit terminal
      | T_PREDICATE : unit terminal
      | T_OR : unit terminal
      | T_OPPREF : string terminal
      | T_OP4 : string terminal
      | T_OP3 : string terminal
      | T_OP2 : string terminal
      | T_OP1 : string terminal
      | T_OLD : unit terminal
      | T_NOT : unit terminal
      | T_MUTABLE : unit terminal
      | T_MODIFIES : unit terminal
      | T_MODEL : unit terminal
      | T_MATCH : unit terminal
      | T_LTGT : unit terminal
      | T_LRARROW : unit terminal
      | T_LIDENT : string terminal
      | T_LET : unit terminal
      | T_LEFTSQRIGHTSQ : unit terminal
      | T_LEFTSQ : unit terminal
      | T_LEFTPAR : unit terminal
      | T_LEFTBRCRIGHTBRC : unit terminal
      | T_LEFTBRCCOLON : unit terminal
      | T_LEFTBRC : unit terminal
      | T_INVARIANT : unit terminal
      | T_INTEGER : (string * char option) terminal
      | T_IN : unit terminal
      | T_IF : unit terminal
      | T_FUNCTION : unit terminal
      | T_FUN : unit terminal
      | T_FORALL : unit terminal
      | T_FLOAT : string terminal
      | T_FALSE : unit terminal
      | T_EXISTS : unit terminal
      | T_EQUIVALENT : unit terminal
      | T_EQUAL : unit terminal
      | T_EPHEMERAL : unit terminal
      | T_EOF : unit terminal
      | T_ENSURES : unit terminal
      | T_ELSE : unit terminal
      | T_DOTDOT : unit terminal
      | T_DOT : unit terminal
      | T_DIVERGES : unit terminal
      | T_CONSUMES : unit terminal
      | T_COMMA : unit terminal
      | T_COLONRIGHTBRC : unit terminal
      | T_COLONCOLON : unit terminal
      | T_COLON : unit terminal
      | T_COERCION : unit terminal
      | T_CHECKS : unit terminal
      | T_CHAR : char terminal
      | T_BARBAR : unit terminal
      | T_BAR : unit terminal
      | T_BACKQUOTE_LIDENT : string terminal
      | T_AXIOM : unit terminal
      | T_ATTRIBUTE : string terminal
      | T_AS : unit terminal
      | T_ARROW : unit terminal
      | T_AND : unit terminal
      | T_AMPAMP : unit terminal

    type _ nonterminal =
      | N_val_spec_header : Gospel.Uast.spec_header nonterminal
      | N_val_spec_body : Gospel.Uast.val_spec nonterminal
      | N_val_spec : Gospel.Uast.val_spec nonterminal
      | N_uqualid : Gospel.Uast.qualid nonterminal
      | N_uident : Gospel.Uast.Preid.t nonterminal
      | N_type_spec_model : Gospel.Uast.field nonterminal
      | N_type_spec : Gospel.Uast.type_spec nonterminal
      | N_typ : Gospel.Uast.pty nonterminal
      | N_ty_tuple : Gospel.Uast.pty list nonterminal
      | N_ty_arg : Gospel.Uast.pty nonterminal
      | N_ts_invariants
          : (Gospel.Uast.Preid.t * Gospel.Uast.term list) option nonterminal
      | N_ts_invariant : Gospel.Uast.term nonterminal
      | N_ts_ephemeral : bool nonterminal
      | N_term_sub_ : Gospel.Uast.term_desc nonterminal
      | N_term_rec_field_term_
          : (Gospel.Uast.qualid * Gospel.Uast.term) nonterminal
      | N_term_dot_ : Gospel.Uast.term_desc nonterminal
      | N_term_dot : Gospel.Uast.term nonterminal
      | N_term_block_ : Gospel.Uast.term_desc nonterminal
      | N_term_arg_ : Gospel.Uast.term_desc nonterminal
      | N_term_arg : Gospel.Uast.term nonterminal
      | N_term_ : Gospel.Uast.term_desc nonterminal
      | N_term : Gospel.Uast.term nonterminal
      | N_separated_nonempty_list_COMMA_typ_ : Gospel.Uast.pty list nonterminal
      | N_separated_nonempty_list_COMMA_term_
          : Gospel.Uast.term list nonterminal
      | N_separated_nonempty_list_COMMA_ret_value_
          : Gospel.Uast.labelled_arg list nonterminal
      | N_separated_nonempty_list_COMMA_quant_vars_
          : Gospel.Uast.binder list list nonterminal
      | N_separated_nonempty_list_COMMA_mk_pat_pat_uni___
          : Gospel.Uast.pattern list nonterminal
      | N_separated_nonempty_list_BAR_separated_pair_guarded_pattern_ARROW_term__
          : ((Gospel.Uast.pattern * Gospel.Uast.term option) * Gospel.Uast.term)
            list
            nonterminal
      | N_separated_nonempty_list_BAR_raises_
          : (Gospel.Uast.qualid
            * (Gospel.Uast.pattern * Gospel.Uast.term) option)
            list
            nonterminal
      | N_semicolon_list1_term_rec_field_term__
          : (Gospel.Uast.qualid * Gospel.Uast.term) list nonterminal
      | N_semicolon_list1_pattern_rec_field_pattern__
          : (Gospel.Uast.qualid * Gospel.Uast.pattern) list nonterminal
      | N_ret_value : Gospel.Uast.labelled_arg nonterminal
      | N_ret_name : Gospel.Uast.labelled_arg list nonterminal
      | N_raises
          : (Gospel.Uast.qualid
            * (Gospel.Uast.pattern * Gospel.Uast.term) option)
            nonterminal
      | N_quote_lident : Gospel.Uast.Preid.t nonterminal
      | N_quant_vars : Gospel.Uast.binder list nonterminal
      | N_quant : Gospel.Uast.quant nonterminal
      | N_qualid : Gospel.Uast.qualid nonterminal
      | N_prefix_op : Gospel.Uast.Preid.t nonterminal
      | N_pattern_rec_field_pattern_
          : (Gospel.Uast.qualid * Gospel.Uast.pattern) nonterminal
      | N_pattern_ : Gospel.Uast.pat_desc nonterminal
      | N_pattern : Gospel.Uast.pattern nonterminal
      | N_pat_uni_ : Gospel.Uast.pat_desc nonterminal
      | N_pat_conj_ : Gospel.Uast.pat_desc nonterminal
      | N_pat_arg_no_lpar_ : Gospel.Uast.pat_desc nonterminal
      | N_pat_arg_no_lpar : Gospel.Uast.pattern nonterminal
      | N_pat_arg_ : Gospel.Uast.pat_desc nonterminal
      | N_pat_arg : Gospel.Uast.pattern nonterminal
      | N_params : Gospel.Uast.param list nonterminal
      | N_param : Gospel.Uast.param list nonterminal
      | N_option_val_spec_header_ : Gospel.Uast.spec_header option nonterminal
      | N_option_preceded_EQUAL_term__ : Gospel.Uast.term option nonterminal
      | N_option_preceded_COLON_fun_typ__ : Gospel.Uast.pty option nonterminal
      | N_option_cast_ : Gospel.Uast.pty option nonterminal
      | N_option_UNDERSCORE_ : unit option nonterminal
      | N_op_symbol : string nonterminal
      | N_nonempty_list_ts_invariant_ : Gospel.Uast.term list nonterminal
      | N_nonempty_list_pat_arg_ : Gospel.Uast.pattern list nonterminal
      | N_nonempty_list_located_term_arg__
          : (Gospel.Uast.term * Lexing.position * Lexing.position) list
            nonterminal
      | N_nonempty_list_lident_ : Gospel.Uast.Preid.t list nonterminal
      | N_nonempty_list_binder_var_ : Gospel.Uast.Preid.t list nonterminal
      | N_nonempty_func_spec : Gospel.Uast.fun_spec nonterminal
      | N_mk_term_term_dot__ : Gospel.Uast.term nonterminal
      | N_mk_term_term_block__ : Gospel.Uast.term nonterminal
      | N_mk_term_term_arg__ : Gospel.Uast.term nonterminal
      | N_mk_term_term__ : Gospel.Uast.term nonterminal
      | N_mk_pat_pattern__ : Gospel.Uast.pattern nonterminal
      | N_mk_pat_pat_uni__ : Gospel.Uast.pattern nonterminal
      | N_mk_pat_pat_conj__ : Gospel.Uast.pattern nonterminal
      | N_mk_pat_pat_arg_no_lpar__ : Gospel.Uast.pattern nonterminal
      | N_mk_pat_pat_arg__ : Gospel.Uast.pattern nonterminal
      | N_match_cases_term_
          : (Gospel.Uast.pattern * Gospel.Uast.term option * Gospel.Uast.term)
            list
            nonterminal
      | N_lqualid_rich : Gospel.Uast.qualid nonterminal
      | N_lqualid : Gospel.Uast.qualid nonterminal
      | N_loption_separated_nonempty_list_COMMA_term__
          : Gospel.Uast.term list nonterminal
      | N_loption_separated_nonempty_list_COMMA_ret_value__
          : Gospel.Uast.labelled_arg list nonterminal
      | N_loption_separated_nonempty_list_COMMA_mk_pat_pat_uni____
          : Gospel.Uast.pattern list nonterminal
      | N_loption_params_ : Gospel.Uast.param list nonterminal
      | N_located_term_arg_
          : (Gospel.Uast.term * Lexing.position * Lexing.position) nonterminal
      | N_list_type_spec_model_ : Gospel.Uast.field list nonterminal
      | N_list_fun_arg_ : Gospel.Uast.labelled_arg list nonterminal
      | N_list_attr_ : string list nonterminal
      | N_lident_rich : Gospel.Uast.Preid.t nonterminal
      | N_lident_op_id : Gospel.Uast.Preid.t nonterminal
      | N_lident_op : string nonterminal
      | N_lident : Gospel.Uast.Preid.t nonterminal
      | N_ident_rich : Gospel.Uast.Preid.t nonterminal
      | N_guarded_pattern
          : (Gospel.Uast.pattern * Gospel.Uast.term option) nonterminal
      | N_func_spec : Gospel.Uast.fun_spec nonterminal
      | N_func_name : Gospel.Uast.Preid.t nonterminal
      | N_func : Gospel.Uast.function_ nonterminal
      | N_fun_typ : Gospel.Uast.pty nonterminal
      | N_fun_arg : Gospel.Uast.labelled_arg nonterminal
      | N_field_pattern_pattern_
          : (Gospel.Uast.qualid * Gospel.Uast.pattern) list nonterminal
      | N_field_list1_term_
          : (Gospel.Uast.qualid * Gospel.Uast.term) list nonterminal
      | N_constant : Ppxlib.constant nonterminal
      | N_comma_list2_term_ : Gospel.Uast.term list nonterminal
      | N_comma_list2_mk_pat_pat_uni___ : Gospel.Uast.pattern list nonterminal
      | N_comma_list1_term_ : Gospel.Uast.term list nonterminal
      | N_comma_list1_quant_vars_ : Gospel.Uast.binder list list nonterminal
      | N_comma_list1_mk_pat_pat_uni___ : Gospel.Uast.pattern list nonterminal
      | N_comma_list_ret_value_ : Gospel.Uast.labelled_arg list nonterminal
      | N_cast : Gospel.Uast.pty nonterminal
      | N_boption_REC_ : bool nonterminal
      | N_boption_MUTABLE_ : bool nonterminal
      | N_binder_var : Gospel.Uast.Preid.t nonterminal
      | N_bar_list1_separated_pair_guarded_pattern_ARROW_term__
          : ((Gospel.Uast.pattern * Gospel.Uast.term option) * Gospel.Uast.term)
            list
            nonterminal
      | N_bar_list1_raises_
          : (Gospel.Uast.qualid
            * (Gospel.Uast.pattern * Gospel.Uast.term) option)
            list
            nonterminal
      | N_axiom : Gospel.Uast.axiom nonterminal
      | N_attrs_lident_op_id_ : Gospel.Uast.Preid.t nonterminal
      | N_attrs_lident_ : Gospel.Uast.Preid.t nonterminal
      | N_attr : string nonterminal
  end

  type 'a terminal = 'a Symbols.terminal =
    | T_error : unit terminal
    | T_WITH : unit terminal
    | T_WHEN : unit terminal
    | T_VARIANT : unit terminal
    | T_UNDERSCORE : unit terminal
    | T_UIDENT : string terminal
    | T_TRUE : unit terminal
    | T_TILDE : unit terminal
    | T_THEN : unit terminal
    | T_STRING : string terminal
    | T_STAR : unit terminal
    | T_SEMICOLON : unit terminal
    | T_RIGHTSQ : unit terminal
    | T_RIGHTPAR : unit terminal
    | T_RIGHTBRC : unit terminal
    | T_REQUIRES : unit terminal
    | T_REC : unit terminal
    | T_RAISES : unit terminal
    | T_QUOTE_LIDENT : string terminal
    | T_QUESTION : unit terminal
    | T_PURE : unit terminal
    | T_PREDICATE : unit terminal
    | T_OR : unit terminal
    | T_OPPREF : string terminal
    | T_OP4 : string terminal
    | T_OP3 : string terminal
    | T_OP2 : string terminal
    | T_OP1 : string terminal
    | T_OLD : unit terminal
    | T_NOT : unit terminal
    | T_MUTABLE : unit terminal
    | T_MODIFIES : unit terminal
    | T_MODEL : unit terminal
    | T_MATCH : unit terminal
    | T_LTGT : unit terminal
    | T_LRARROW : unit terminal
    | T_LIDENT : string terminal
    | T_LET : unit terminal
    | T_LEFTSQRIGHTSQ : unit terminal
    | T_LEFTSQ : unit terminal
    | T_LEFTPAR : unit terminal
    | T_LEFTBRCRIGHTBRC : unit terminal
    | T_LEFTBRCCOLON : unit terminal
    | T_LEFTBRC : unit terminal
    | T_INVARIANT : unit terminal
    | T_INTEGER : (string * char option) terminal
    | T_IN : unit terminal
    | T_IF : unit terminal
    | T_FUNCTION : unit terminal
    | T_FUN : unit terminal
    | T_FORALL : unit terminal
    | T_FLOAT : string terminal
    | T_FALSE : unit terminal
    | T_EXISTS : unit terminal
    | T_EQUIVALENT : unit terminal
    | T_EQUAL : unit terminal
    | T_EPHEMERAL : unit terminal
    | T_EOF : unit terminal
    | T_ENSURES : unit terminal
    | T_ELSE : unit terminal
    | T_DOTDOT : unit terminal
    | T_DOT : unit terminal
    | T_DIVERGES : unit terminal
    | T_CONSUMES : unit terminal
    | T_COMMA : unit terminal
    | T_COLONRIGHTBRC : unit terminal
    | T_COLONCOLON : unit terminal
    | T_COLON : unit terminal
    | T_COERCION : unit terminal
    | T_CHECKS : unit terminal
    | T_CHAR : char terminal
    | T_BARBAR : unit terminal
    | T_BAR : unit terminal
    | T_BACKQUOTE_LIDENT : string terminal
    | T_AXIOM : unit terminal
    | T_ATTRIBUTE : string terminal
    | T_AS : unit terminal
    | T_ARROW : unit terminal
    | T_AND : unit terminal
    | T_AMPAMP : unit terminal

  type 'a nonterminal = 'a Symbols.nonterminal =
    | N_val_spec_header : Gospel.Uast.spec_header nonterminal
    | N_val_spec_body : Gospel.Uast.val_spec nonterminal
    | N_val_spec : Gospel.Uast.val_spec nonterminal
    | N_uqualid : Gospel.Uast.qualid nonterminal
    | N_uident : Gospel.Uast.Preid.t nonterminal
    | N_type_spec_model : Gospel.Uast.field nonterminal
    | N_type_spec : Gospel.Uast.type_spec nonterminal
    | N_typ : Gospel.Uast.pty nonterminal
    | N_ty_tuple : Gospel.Uast.pty list nonterminal
    | N_ty_arg : Gospel.Uast.pty nonterminal
    | N_ts_invariants
        : (Gospel.Uast.Preid.t * Gospel.Uast.term list) option nonterminal
    | N_ts_invariant : Gospel.Uast.term nonterminal
    | N_ts_ephemeral : bool nonterminal
    | N_term_sub_ : Gospel.Uast.term_desc nonterminal
    | N_term_rec_field_term_
        : (Gospel.Uast.qualid * Gospel.Uast.term) nonterminal
    | N_term_dot_ : Gospel.Uast.term_desc nonterminal
    | N_term_dot : Gospel.Uast.term nonterminal
    | N_term_block_ : Gospel.Uast.term_desc nonterminal
    | N_term_arg_ : Gospel.Uast.term_desc nonterminal
    | N_term_arg : Gospel.Uast.term nonterminal
    | N_term_ : Gospel.Uast.term_desc nonterminal
    | N_term : Gospel.Uast.term nonterminal
    | N_separated_nonempty_list_COMMA_typ_ : Gospel.Uast.pty list nonterminal
    | N_separated_nonempty_list_COMMA_term_ : Gospel.Uast.term list nonterminal
    | N_separated_nonempty_list_COMMA_ret_value_
        : Gospel.Uast.labelled_arg list nonterminal
    | N_separated_nonempty_list_COMMA_quant_vars_
        : Gospel.Uast.binder list list nonterminal
    | N_separated_nonempty_list_COMMA_mk_pat_pat_uni___
        : Gospel.Uast.pattern list nonterminal
    | N_separated_nonempty_list_BAR_separated_pair_guarded_pattern_ARROW_term__
        : ((Gospel.Uast.pattern * Gospel.Uast.term option) * Gospel.Uast.term)
          list
          nonterminal
    | N_separated_nonempty_list_BAR_raises_
        : (Gospel.Uast.qualid * (Gospel.Uast.pattern * Gospel.Uast.term) option)
          list
          nonterminal
    | N_semicolon_list1_term_rec_field_term__
        : (Gospel.Uast.qualid * Gospel.Uast.term) list nonterminal
    | N_semicolon_list1_pattern_rec_field_pattern__
        : (Gospel.Uast.qualid * Gospel.Uast.pattern) list nonterminal
    | N_ret_value : Gospel.Uast.labelled_arg nonterminal
    | N_ret_name : Gospel.Uast.labelled_arg list nonterminal
    | N_raises
        : (Gospel.Uast.qualid * (Gospel.Uast.pattern * Gospel.Uast.term) option)
          nonterminal
    | N_quote_lident : Gospel.Uast.Preid.t nonterminal
    | N_quant_vars : Gospel.Uast.binder list nonterminal
    | N_quant : Gospel.Uast.quant nonterminal
    | N_qualid : Gospel.Uast.qualid nonterminal
    | N_prefix_op : Gospel.Uast.Preid.t nonterminal
    | N_pattern_rec_field_pattern_
        : (Gospel.Uast.qualid * Gospel.Uast.pattern) nonterminal
    | N_pattern_ : Gospel.Uast.pat_desc nonterminal
    | N_pattern : Gospel.Uast.pattern nonterminal
    | N_pat_uni_ : Gospel.Uast.pat_desc nonterminal
    | N_pat_conj_ : Gospel.Uast.pat_desc nonterminal
    | N_pat_arg_no_lpar_ : Gospel.Uast.pat_desc nonterminal
    | N_pat_arg_no_lpar : Gospel.Uast.pattern nonterminal
    | N_pat_arg_ : Gospel.Uast.pat_desc nonterminal
    | N_pat_arg : Gospel.Uast.pattern nonterminal
    | N_params : Gospel.Uast.param list nonterminal
    | N_param : Gospel.Uast.param list nonterminal
    | N_option_val_spec_header_ : Gospel.Uast.spec_header option nonterminal
    | N_option_preceded_EQUAL_term__ : Gospel.Uast.term option nonterminal
    | N_option_preceded_COLON_fun_typ__ : Gospel.Uast.pty option nonterminal
    | N_option_cast_ : Gospel.Uast.pty option nonterminal
    | N_option_UNDERSCORE_ : unit option nonterminal
    | N_op_symbol : string nonterminal
    | N_nonempty_list_ts_invariant_ : Gospel.Uast.term list nonterminal
    | N_nonempty_list_pat_arg_ : Gospel.Uast.pattern list nonterminal
    | N_nonempty_list_located_term_arg__
        : (Gospel.Uast.term * Lexing.position * Lexing.position) list
          nonterminal
    | N_nonempty_list_lident_ : Gospel.Uast.Preid.t list nonterminal
    | N_nonempty_list_binder_var_ : Gospel.Uast.Preid.t list nonterminal
    | N_nonempty_func_spec : Gospel.Uast.fun_spec nonterminal
    | N_mk_term_term_dot__ : Gospel.Uast.term nonterminal
    | N_mk_term_term_block__ : Gospel.Uast.term nonterminal
    | N_mk_term_term_arg__ : Gospel.Uast.term nonterminal
    | N_mk_term_term__ : Gospel.Uast.term nonterminal
    | N_mk_pat_pattern__ : Gospel.Uast.pattern nonterminal
    | N_mk_pat_pat_uni__ : Gospel.Uast.pattern nonterminal
    | N_mk_pat_pat_conj__ : Gospel.Uast.pattern nonterminal
    | N_mk_pat_pat_arg_no_lpar__ : Gospel.Uast.pattern nonterminal
    | N_mk_pat_pat_arg__ : Gospel.Uast.pattern nonterminal
    | N_match_cases_term_
        : (Gospel.Uast.pattern * Gospel.Uast.term option * Gospel.Uast.term)
          list
          nonterminal
    | N_lqualid_rich : Gospel.Uast.qualid nonterminal
    | N_lqualid : Gospel.Uast.qualid nonterminal
    | N_loption_separated_nonempty_list_COMMA_term__
        : Gospel.Uast.term list nonterminal
    | N_loption_separated_nonempty_list_COMMA_ret_value__
        : Gospel.Uast.labelled_arg list nonterminal
    | N_loption_separated_nonempty_list_COMMA_mk_pat_pat_uni____
        : Gospel.Uast.pattern list nonterminal
    | N_loption_params_ : Gospel.Uast.param list nonterminal
    | N_located_term_arg_
        : (Gospel.Uast.term * Lexing.position * Lexing.position) nonterminal
    | N_list_type_spec_model_ : Gospel.Uast.field list nonterminal
    | N_list_fun_arg_ : Gospel.Uast.labelled_arg list nonterminal
    | N_list_attr_ : string list nonterminal
    | N_lident_rich : Gospel.Uast.Preid.t nonterminal
    | N_lident_op_id : Gospel.Uast.Preid.t nonterminal
    | N_lident_op : string nonterminal
    | N_lident : Gospel.Uast.Preid.t nonterminal
    | N_ident_rich : Gospel.Uast.Preid.t nonterminal
    | N_guarded_pattern
        : (Gospel.Uast.pattern * Gospel.Uast.term option) nonterminal
    | N_func_spec : Gospel.Uast.fun_spec nonterminal
    | N_func_name : Gospel.Uast.Preid.t nonterminal
    | N_func : Gospel.Uast.function_ nonterminal
    | N_fun_typ : Gospel.Uast.pty nonterminal
    | N_fun_arg : Gospel.Uast.labelled_arg nonterminal
    | N_field_pattern_pattern_
        : (Gospel.Uast.qualid * Gospel.Uast.pattern) list nonterminal
    | N_field_list1_term_
        : (Gospel.Uast.qualid * Gospel.Uast.term) list nonterminal
    | N_constant : Ppxlib.constant nonterminal
    | N_comma_list2_term_ : Gospel.Uast.term list nonterminal
    | N_comma_list2_mk_pat_pat_uni___ : Gospel.Uast.pattern list nonterminal
    | N_comma_list1_term_ : Gospel.Uast.term list nonterminal
    | N_comma_list1_quant_vars_ : Gospel.Uast.binder list list nonterminal
    | N_comma_list1_mk_pat_pat_uni___ : Gospel.Uast.pattern list nonterminal
    | N_comma_list_ret_value_ : Gospel.Uast.labelled_arg list nonterminal
    | N_cast : Gospel.Uast.pty nonterminal
    | N_boption_REC_ : bool nonterminal
    | N_boption_MUTABLE_ : bool nonterminal
    | N_binder_var : Gospel.Uast.Preid.t nonterminal
    | N_bar_list1_separated_pair_guarded_pattern_ARROW_term__
        : ((Gospel.Uast.pattern * Gospel.Uast.term option) * Gospel.Uast.term)
          list
          nonterminal
    | N_bar_list1_raises_
        : (Gospel.Uast.qualid * (Gospel.Uast.pattern * Gospel.Uast.term) option)
          list
          nonterminal
    | N_axiom : Gospel.Uast.axiom nonterminal
    | N_attrs_lident_op_id_ : Gospel.Uast.Preid.t nonterminal
    | N_attrs_lident_ : Gospel.Uast.Preid.t nonterminal
    | N_attr : string nonterminal

  type 'a symbol =
    | T : 'a Symbols.terminal -> 'a symbol
    | N : 'a Symbols.nonterminal -> 'a symbol

  type xsymbol = X : 'a symbol -> xsymbol
  type item = int * int

  val compare_terminals : 'a Symbols.terminal -> 'b Symbols.terminal -> int

  val compare_nonterminals :
    'a Symbols.nonterminal -> 'b Symbols.nonterminal -> int

  val compare_symbols : xsymbol -> xsymbol -> int
  val compare_productions : int -> int -> int
  val compare_items : item -> item -> int
  val incoming_symbol : ET.state -> 'a symbol
  val items : ET.state -> item list
  val lhs : int -> xsymbol
  val rhs : int -> xsymbol list
  val nullable : 'a Symbols.nonterminal -> bool
  val first : 'a Symbols.nonterminal -> 'b Symbols.terminal -> bool
  val xfirst : xsymbol -> 'a Symbols.terminal -> bool
  val foreach_terminal : (xsymbol -> 'a -> 'a) -> 'a -> 'a
  val foreach_terminal_but_error : (xsymbol -> 'a -> 'a) -> 'a -> 'a

  val feed :
    'a symbol ->
    MenhirLib.IncrementalEngine.position ->
    'a ->
    MenhirLib.IncrementalEngine.position ->
    'b TI.env ->
    'b TI.env
end

val val_spec :
  (Lexing.lexbuf -> MenhirInterpreter.token) ->
  Lexing.lexbuf ->
  Gospel.Uast.val_spec

val type_spec :
  (Lexing.lexbuf -> MenhirInterpreter.token) ->
  Lexing.lexbuf ->
  Gospel.Uast.type_spec

val func_spec :
  (Lexing.lexbuf -> MenhirInterpreter.token) ->
  Lexing.lexbuf ->
  Gospel.Uast.fun_spec

val func :
  (Lexing.lexbuf -> MenhirInterpreter.token) ->
  Lexing.lexbuf ->
  Gospel.Uast.function_

val axiom :
  (Lexing.lexbuf -> MenhirInterpreter.token) ->
  Lexing.lexbuf ->
  Gospel.Uast.axiom

module Incremental : sig
  val val_spec :
    Lexing.position -> Gospel.Uast.val_spec MenhirInterpreter.checkpoint

  val type_spec :
    Lexing.position -> Gospel.Uast.type_spec MenhirInterpreter.checkpoint

  val func_spec :
    Lexing.position -> Gospel.Uast.fun_spec MenhirInterpreter.checkpoint

  val func :
    Lexing.position -> Gospel.Uast.function_ MenhirInterpreter.checkpoint

  val axiom : Lexing.position -> Gospel.Uast.axiom MenhirInterpreter.checkpoint
end
