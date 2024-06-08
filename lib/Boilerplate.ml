(**
   Boilerplate to be used as a template when mapping the move_on_aptos CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_block_doc_comment_marker (env : env) (tok : CST.block_doc_comment_marker) =
  (* block_doc_comment_marker *) token env tok

let map_line_comment_explicit (env : env) (() : CST.line_comment_explicit) =
  R.Tuple []

let map_pat_57a456d (env : env) (tok : CST.pat_57a456d) =
  (* pattern \d[\d_]* *) token env tok

let map_number_type (env : env) (x : CST.number_type) =
  (match x with
  | `U8 tok -> R.Case ("U8",
      (* "u8" *) token env tok
    )
  | `U16 tok -> R.Case ("U16",
      (* "u16" *) token env tok
    )
  | `U32 tok -> R.Case ("U32",
      (* "u32" *) token env tok
    )
  | `U64 tok -> R.Case ("U64",
      (* "u64" *) token env tok
    )
  | `U128 tok -> R.Case ("U128",
      (* "u128" *) token env tok
    )
  | `U256 tok -> R.Case ("U256",
      (* "u256" *) token env tok
    )
  )

let map_imm_tok_prec_p2_slash (env : env) (tok : CST.imm_tok_prec_p2_slash) =
  (* "/" *) token env tok

let map_ability (env : env) (x : CST.ability) =
  (match x with
  | `Copy tok -> R.Case ("Copy",
      (* "copy" *) token env tok
    )
  | `Drop tok -> R.Case ("Drop",
      (* "drop" *) token env tok
    )
  | `Store tok -> R.Case ("Store",
      (* "store" *) token env tok
    )
  | `Key tok -> R.Case ("Key",
      (* "key" *) token env tok
    )
  )

let map_block_comment_content (env : env) (tok : CST.block_comment_content) =
  (* block_comment_content *) token env tok

let map_block_comment_explicit (env : env) (() : CST.block_comment_explicit) =
  R.Tuple []

let map_reuseable_keywords (env : env) (x : CST.reuseable_keywords) =
  (match x with
  | `For tok -> R.Case ("For",
      (* "for" *) token env tok
    )
  | `While tok -> R.Case ("While",
      (* "while" *) token env tok
    )
  | `Friend tok -> R.Case ("Friend",
      (* "friend" *) token env tok
    )
  )

let map_tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot (env : env) (tok : CST.tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot) =
  (* tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot *) token env tok

let map_bool_literal (env : env) (x : CST.bool_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_pat_9ddd3d6 (env : env) (tok : CST.pat_9ddd3d6) =
  (* pattern "x\\\"[\\da-fA-F]*\\\"" *) token env tok

let map_quantifier_directive (env : env) (x : CST.quantifier_directive) =
  (match x with
  | `Exists tok -> R.Case ("Exists",
      (* "exists" *) token env tok
    )
  | `Forall tok -> R.Case ("Forall",
      (* "forall" *) token env tok
    )
  | `Choose tok -> R.Case ("Choose",
      (* "choose" *) token env tok
    )
  | `Min tok -> R.Case ("Min",
      (* "min" *) token env tok
    )
  )

let map_pat_47a8622 (env : env) (tok : CST.pat_47a8622) =
  (* pattern 0[xX][\da-fA-F][\da-fA-F_]* *) token env tok

let map_pat_05318a0 (env : env) (tok : CST.pat_05318a0) =
  (* pattern 0o[0-7][0-7_]* *) token env tok

let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_imm_tok_prec_p1_pat_4fd4a56 (env : env) (tok : CST.imm_tok_prec_p1_pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_pat_cb917db (env : env) (tok : CST.pat_cb917db) =
  (* pattern 0b[01][01_]* *) token env tok

let map_imm_tok_prec_p2_slashslash (env : env) (tok : CST.imm_tok_prec_p2_slashslash) =
  (* "//" *) token env tok

let map_doc_line_comment (env : env) (tok : CST.doc_line_comment) =
  (* doc_line_comment *) token env tok

let map_identifier (env : env) (tok : CST.identifier) =
  (* identifier *) token env tok

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Num_type x -> R.Case ("Num_type",
      map_number_type env x
    )
  | `Bool tok -> R.Case ("Bool",
      (* "bool" *) token env tok
    )
  | `Addr tok -> R.Case ("Addr",
      (* "address" *) token env tok
    )
  | `Vec tok -> R.Case ("Vec",
      (* "vector" *) token env tok
    )
  )

let map_constraints (env : env) ((v1, v2, v3) : CST.constraints) =
  let v1 = (* ":" *) token env v1 in
  let v2 = map_ability env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = map_ability env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

let map_abilities (env : env) ((v1, v2) : CST.abilities) =
  let v1 = map_ability env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_ability env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_block_comment (env : env) ((v1, v2, v3) : CST.block_comment) =
  let v1 = (* "/*" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Blk_doc_comm_marker_opt_blk_comm_content (v1, v2) -> R.Case ("Blk_doc_comm_marker_opt_blk_comm_content",
            let v1 = (* block_doc_comment_marker *) token env v1 in
            let v2 =
              (match v2 with
              | Some tok -> R.Option (Some (
                  (* block_comment_content *) token env tok
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        | `Blk_comm_content tok -> R.Case ("Blk_comm_content",
            (* block_comment_content *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 = (* "*/" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_byte_string (env : env) (x : CST.byte_string) =
  (match x with
  | `Pat_9ddd3d6 x -> R.Case ("Pat_9ddd3d6",
      map_pat_9ddd3d6 env x
    )
  | `Tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot x -> R.Case ("Tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot",
      map_tok_bdquot_rep_choice_imm_tok_bslash_choice_pat_9c2bd89_dquot env x
    )
  )

let map_number (env : env) (x : CST.number) =
  (match x with
  | `Pat_57a456d x -> R.Case ("Pat_57a456d",
      map_pat_57a456d env x
    )
  | `Pat_47a8622 x -> R.Case ("Pat_47a8622",
      map_pat_47a8622 env x
    )
  | `Pat_cb917db x -> R.Case ("Pat_cb917db",
      map_pat_cb917db env x
    )
  | `Pat_05318a0 x -> R.Case ("Pat_05318a0",
      map_pat_05318a0 env x
    )
  )

let map_module_member_modifier (env : env) (x : CST.module_member_modifier) =
  (match x with
  | `Visi (v1, v2) -> R.Case ("Visi",
      let v1 = (* "public" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3) -> R.Option (Some (
            let v1 = (* "(" *) token env v1 in
            let v2 =
              (match v2 with
              | `Script tok -> R.Case ("Script",
                  (* "script" *) token env tok
                )
              | `Friend tok -> R.Case ("Friend",
                  (* "friend" *) token env tok
                )
              )
            in
            let v3 = (* ")" *) token env v3 in
            R.Tuple [v1; v2; v3]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Native tok -> R.Case ("Native",
      (* "native" *) token env tok
    )
  | `Entry tok -> R.Case ("Entry",
      (* "entry" *) token env tok
    )
  )

let map_line_comment (env : env) ((v1, v2) : CST.line_comment) =
  let v1 = (* "//" *) token env v1 in
  let v2 =
    (match v2 with
    | `Imm_tok_prec_p2_slas_pat_4fd4a56 (v1, v2) -> R.Case ("Imm_tok_prec_p2_slas_pat_4fd4a56",
        let v1 = map_imm_tok_prec_p2_slashslash env v1 in
        let v2 = map_pat_4fd4a56 env v2 in
        R.Tuple [v1; v2]
      )
    | `Imm_tok_prec_p2_slash_doc_line_comm (v1, v2) -> R.Case ("Imm_tok_prec_p2_slash_doc_line_comm",
        let v1 = map_imm_tok_prec_p2_slash env v1 in
        let v2 = (* doc_line_comment *) token env v2 in
        R.Tuple [v1; v2]
      )
    | `Imm_tok_prec_p1_pat_4fd4a56 x -> R.Case ("Imm_tok_prec_p1_pat_4fd4a56",
        map_imm_tok_prec_p1_pat_4fd4a56 env x
      )
    )
  in
  R.Tuple [v1; v2]

let map_attribute_name (env : env) ((v1, v2) : CST.attribute_name) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "::" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_use_alias (env : env) ((v1, v2) : CST.use_alias) =
  let v1 = (* "as" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  R.Tuple [v1; v2]

let map_spec_apply_fragment (env : env) (x : CST.spec_apply_fragment) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  )

let map_discouraged_name (env : env) (x : CST.discouraged_name) =
  (match x with
  | `Prim_type x -> R.Case ("Prim_type",
      map_primitive_type env x
    )
  | `Quan_dire x -> R.Case ("Quan_dire",
      map_quantifier_directive env x
    )
  | `Reus_keywos x -> R.Case ("Reus_keywos",
      map_reuseable_keywords env x
    )
  )

let map_type_param (env : env) ((v1, v2) : CST.type_param) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_constraints env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_numerical_addr (env : env) (x : CST.numerical_addr) =
  map_number env x

let map_use_member (env : env) (x : CST.use_member) =
  (match x with
  | `Id_opt_use_alias (v1, v2) -> R.Case ("Id_opt_use_alias",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_use_alias env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_var_name (env : env) (x : CST.var_name) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Disc_name x -> R.Case ("Disc_name",
      map_discouraged_name env x
    )
  )

let map_type_params (env : env) ((v1, v2, v3, v4) : CST.type_params) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_type_param env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_param env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_struct_type_parameter (env : env) ((v1, v2) : CST.struct_type_parameter) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "phantom" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = map_type_param env v2 in
  R.Tuple [v1; v2]

let map_anon_choice_id_9d8f098 (env : env) (x : CST.anon_choice_id_9d8f098) =
  (match x with
  | `Id tok -> R.Case ("Id",
      (* identifier *) token env tok
    )
  | `Nume_addr x -> R.Case ("Nume_addr",
      map_numerical_addr env x
    )
  )

let map_spec_apply_pattern (env : env) ((v1, v2, v3) : CST.spec_apply_pattern) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Public tok -> R.Case ("Public",
            (* "public" *) token env tok
          )
        | `Inte tok -> R.Case ("Inte",
            (* "internal" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 =
    R.List (List.map (map_spec_apply_fragment env) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_struct_def_name (env : env) ((v1, v2) : CST.struct_def_name) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3, v4) -> R.Option (Some (
        let v1 = (* "<" *) token env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_struct_type_parameter env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_struct_type_parameter env v2 in
                  R.Tuple [v1; v2]
                ) v2)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        let v4 = (* ">" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let map_module_ident_ (env : env) ((v1, v2, v3) : CST.module_ident_) =
  let v1 = map_anon_choice_id_9d8f098 env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_anon_choice_choice_id_1705670 (env : env) (x : CST.anon_choice_choice_id_1705670) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      map_anon_choice_id_9d8f098 env x
    )
  | `Disc_name x -> R.Case ("Disc_name",
      map_discouraged_name env x
    )
  )

let map_value (env : env) (x : CST.value) =
  (match x with
  | `AT_choice_id (v1, v2) -> R.Case ("AT_choice_id",
      let v1 = (* "@" *) token env v1 in
      let v2 = map_anon_choice_id_9d8f098 env v2 in
      R.Tuple [v1; v2]
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_bool_literal env x
    )
  | `Num x -> R.Case ("Num",
      map_numerical_addr env x
    )
  | `Typed_num (v1, v2) -> R.Case ("Typed_num",
      let v1 = map_numerical_addr env v1 in
      let v2 = map_number_type env v2 in
      R.Tuple [v1; v2]
    )
  | `Byte_str x -> R.Case ("Byte_str",
      map_byte_string env x
    )
  )

let map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 (env : env) ((v1, v2) : CST.anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6) =
  let v1 = map_spec_apply_pattern env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_spec_apply_pattern env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_use_decl (env : env) ((v1, v2, v3, v4) : CST.use_decl) =
  let v1 = (* "use" *) token env v1 in
  let v2 = map_module_ident_ env v2 in
  let v3 =
    (match v3 with
    | `Opt_use_alias opt -> R.Case ("Opt_use_alias",
        (match opt with
        | Some x -> R.Option (Some (
            map_use_alias env x
          ))
        | None -> R.Option None)
      )
    | `COLONCOLON_use_member (v1, v2) -> R.Case ("COLONCOLON_use_member",
        let v1 = (* "::" *) token env v1 in
        let v2 = map_use_member env v2 in
        R.Tuple [v1; v2]
      )
    | `COLONCOLON_LCURL_opt_use_member_rep_COMMA_use_member_opt_COMMA_RCURL (v1, v2, v3, v4, v5) -> R.Case ("COLONCOLON_LCURL_opt_use_member_rep_COMMA_use_member_opt_COMMA_RCURL",
        let v1 = (* "::" *) token env v1 in
        let v2 = (* "{" *) token env v2 in
        let v3 =
          (match v3 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = map_use_member env v1 in
              let v2 =
                R.List (List.map (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_use_member env v2 in
                  R.Tuple [v1; v2]
                ) v2)
              in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        let v4 =
          (match v4 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        let v5 = (* "}" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    )
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_anon_choice_var_name_32a5fbc (env : env) (x : CST.anon_choice_var_name_32a5fbc) =
  (match x with
  | `Choice_id x -> R.Case ("Choice_id",
      map_var_name env x
    )
  | `Choice_choice_id_COLONCOLON_id_opt_COLONCOLON_id (v1, v2, v3, v4) -> R.Case ("Choice_choice_id_COLONCOLON_id_opt_COLONCOLON_id",
      let v1 = map_anon_choice_choice_id_1705670 env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "::" *) token env v1 in
            let v2 = (* identifier *) token env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_attribute_val (env : env) (x : CST.attribute_val) =
  (match x with
  | `Value x -> R.Case ("Value",
      map_value env x
    )
  | `Choice_choice_id x -> R.Case ("Choice_choice_id",
      map_anon_choice_var_name_32a5fbc env x
    )
  )

let rec map_anon_type__rep_COMMA_type__ac59fb8 (env : env) ((v1, v2) : CST.anon_type__rep_COMMA_type__ac59fb8) =
  let v1 = map_type__ env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type__ env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_ref_type (env : env) (x : CST.ref_type) =
  (match x with
  | `AMP_type (v1, v2) -> R.Case ("AMP_type",
      let v1 = (* "&" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  | `AMPmut_type (v1, v2) -> R.Case ("AMPmut_type",
      let v1 = (* "&mut" *) token env v1 in
      let v2 = map_type_ env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Choice_choice_choice_id_opt_type_args (v1, v2) -> R.Case ("Choice_choice_choice_id_opt_type_args",
      let v1 =
        (match v1 with
        | `Choice_choice_id x -> R.Case ("Choice_choice_id",
            map_anon_choice_var_name_32a5fbc env x
          )
        | `Prim_type x -> R.Case ("Prim_type",
            map_primitive_type env x
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `BAR_opt_type__rep_COMMA_type__opt_COMMA_BAR_opt_type_ (v1, v2, v3, v4, v5) -> R.Case ("BAR_opt_type__rep_COMMA_type__opt_COMMA_BAR_opt_type_",
      let v1 = (* "|" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_type__rep_COMMA_type__ac59fb8 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "|" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_type__ env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `LPAR_opt_type__rep_COMMA_type__opt_COMMA_RPAR (v1, v2, v3, v4) -> R.Case ("LPAR_opt_type__rep_COMMA_type__opt_COMMA_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_type__rep_COMMA_type__ac59fb8 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_type__ (env : env) (x : CST.type__) =
  (match x with
  | `Type x -> R.Case ("Type",
      map_type_ env x
    )
  | `Ref_type x -> R.Case ("Ref_type",
      map_ref_type env x
    )
  )

and map_type_args (env : env) ((v1, v2, v3, v4) : CST.type_args) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_type__rep_COMMA_type__ac59fb8 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ">" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_friend_decl (env : env) ((v1, v2, v3) : CST.friend_decl) =
  let v1 = (* "friend" *) token env v1 in
  let v2 = map_anon_choice_var_name_32a5fbc env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

let map_spec_pragma_prop (env : env) ((v1, v2) : CST.spec_pragma_prop) =
  let v1 = map_var_name env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_attribute_val env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

let rec map_anon_attr_rep_COMMA_attr_246bec5 (env : env) ((v1, v2) : CST.anon_attr_rep_COMMA_attr_246bec5) =
  let v1 = map_attribute env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_attribute env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_attribute (env : env) (x : CST.attribute) =
  (match x with
  | `Choice_attr_name x -> R.Case ("Choice_attr_name",
      (match x with
      | `Attr_name x -> R.Case ("Attr_name",
          map_attribute_name env x
        )
      | `Attr_name_EQ_attr_val (v1, v2, v3) -> R.Case ("Attr_name_EQ_attr_val",
          let v1 = map_attribute_name env v1 in
          let v2 = (* "=" *) token env v2 in
          let v3 = map_attribute_val env v3 in
          R.Tuple [v1; v2; v3]
        )
      | `Attr_name_LPAR_opt_attr_rep_COMMA_attr_opt_COMMA_RPAR (v1, v2, v3, v4, v5) -> R.Case ("Attr_name_LPAR_opt_attr_rep_COMMA_attr_opt_COMMA_RPAR",
          let v1 = map_attribute_name env v1 in
          let v2 = (* "(" *) token env v2 in
          let v3 =
            (match v3 with
            | Some x -> R.Option (Some (
                map_anon_attr_rep_COMMA_attr_246bec5 env x
              ))
            | None -> R.Option None)
          in
          let v4 =
            (match v4 with
            | Some tok -> R.Option (Some (
                (* "," *) token env tok
              ))
            | None -> R.Option None)
          in
          let v5 = (* ")" *) token env v5 in
          R.Tuple [v1; v2; v3; v4; v5]
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_address_specifier (env : env) (x : CST.address_specifier) =
  (match x with
  | `STAR tok -> R.Case ("STAR",
      (* "*" *) token env tok
    )
  | `Nume_addr x -> R.Case ("Nume_addr",
      map_numerical_addr env x
    )
  | `Choice_choice_id_opt_opt_type_args_LPAR_id_RPAR (v1, v2) -> R.Case ("Choice_choice_id_opt_opt_type_args_LPAR_id_RPAR",
      let v1 = map_anon_choice_var_name_32a5fbc env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2, v3, v4) -> R.Option (Some (
            let v1 =
              (match v1 with
              | Some x -> R.Option (Some (
                  map_type_args env x
                ))
              | None -> R.Option None)
            in
            let v2 = (* "(" *) token env v2 in
            let v3 = (* identifier *) token env v3 in
            let v4 = (* ")" *) token env v4 in
            R.Tuple [v1; v2; v3; v4]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  )

let map_typed_metavariable (env : env) ((v1, v2, v3) : CST.typed_metavariable) =
  let v1 = (* identifier *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = map_type__ env v3 in
  R.Tuple [v1; v2; v3]

let rec map_bind (env : env) (x : CST.bind) =
  (match x with
  | `Var_name x -> R.Case ("Var_name",
      map_var_name env x
    )
  | `Choice_choice_id_opt_type_args_LCURL_opt_bind_field_rep_COMMA_bind_field_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6) -> R.Case ("Choice_choice_id_opt_type_args_LCURL_opt_bind_field_rep_COMMA_bind_field_opt_COMMA_RCURL",
      let v1 = map_anon_choice_var_name_32a5fbc env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_bind_field env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_bind_field env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* "}" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  )

and map_bind_field (env : env) (x : CST.bind_field) =
  (match x with
  | `Var_name_opt_COLON_bind (v1, v2) -> R.Case ("Var_name_opt_COLON_bind",
      let v1 = map_var_name env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* ":" *) token env v1 in
            let v2 = map_bind env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f (env : env) ((v1, v2) : CST.anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f) =
  let v1 = map_spec_pragma_prop env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_spec_pragma_prop env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_attributes (env : env) (xs : CST.attributes) =
  R.List (List.map (fun (v1, v2, v3, v4, v5) ->
    let v1 = (* "#" *) token env v1 in
    let v2 = (* "[" *) token env v2 in
    let v3 =
      (match v3 with
      | Some x -> R.Option (Some (
          map_anon_attr_rep_COMMA_attr_246bec5 env x
        ))
      | None -> R.Option None)
    in
    let v4 =
      (match v4 with
      | Some tok -> R.Option (Some (
          (* "," *) token env tok
        ))
      | None -> R.Option None)
    in
    let v5 = (* "]" *) token env v5 in
    R.Tuple [v1; v2; v3; v4; v5]
  ) xs)

let map_access_specifier (env : env) ((v1, v2, v3) : CST.access_specifier) =
  let v1 =
    (match v1 with
    | `Choice_choice_STAR x -> R.Case ("Choice_choice_STAR",
        (match x with
        | `Choice_STAR x -> R.Case ("Choice_STAR",
            map_spec_apply_fragment env x
          )
        | `Disc_name x -> R.Case ("Disc_name",
            map_discouraged_name env x
          )
        )
      )
    | `Choice_choice_choice_STAR_COLONCOLON_choice_STAR_opt_COLONCOLON_choice_STAR (v1, v2, v3, v4) -> R.Case ("Choice_choice_choice_STAR_COLONCOLON_choice_STAR_opt_COLONCOLON_choice_STAR",
        let v1 =
          (match v1 with
          | `Choice_choice_STAR x -> R.Case ("Choice_choice_STAR",
              (match x with
              | `Choice_STAR x -> R.Case ("Choice_STAR",
                  map_spec_apply_fragment env x
                )
              | `Nume_addr x -> R.Case ("Nume_addr",
                  map_numerical_addr env x
                )
              )
            )
          | `Disc_name x -> R.Case ("Disc_name",
              map_discouraged_name env x
            )
          )
        in
        let v2 = (* "::" *) token env v2 in
        let v3 = map_spec_apply_fragment env v3 in
        let v4 =
          (match v4 with
          | Some (v1, v2) -> R.Option (Some (
              let v1 = (* "::" *) token env v1 in
              let v2 = map_spec_apply_fragment env v2 in
              R.Tuple [v1; v2]
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_args env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "(" *) token env v1 in
        let v2 = map_address_specifier env v2 in
        let v3 = (* ")" *) token env v3 in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_field_annot (env : env) (x : CST.field_annot) =
  (match x with
  | `Id_COLON_type_ x -> R.Case ("Id_COLON_type_",
      map_typed_metavariable env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Id_COLON_type_ x -> R.Case ("Id_COLON_type_",
      map_typed_metavariable env x
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_anon_bind_rep_COMMA_bind_38cc8c1 (env : env) ((v1, v2) : CST.anon_bind_rep_COMMA_bind_38cc8c1) =
  let v1 = map_bind env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_bind env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_spec_pragma (env : env) ((v1, v2, v3, v4) : CST.spec_pragma) =
  let v1 = (* "pragma" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_condition_props (env : env) ((v1, v2, v3, v4) : CST.condition_props) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_spec_pragma_prop_rep_COMMA_spec_pragma_prop_588d25f env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "]" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_access_specifier_list (env : env) ((v1, v2, v3) : CST.access_specifier_list) =
  let v1 = map_access_specifier env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_access_specifier env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

let map_struct_body (env : env) (x : CST.struct_body) =
  (match x with
  | `LCURL_opt_field_annot_rep_COMMA_field_annot_opt_COMMA_RCURL (v1, v2, v3, v4) -> R.Case ("LCURL_opt_field_annot_rep_COMMA_field_annot_opt_COMMA_RCURL",
      let v1 = (* "{" *) token env v1 in
      let v2 =
        (match v2 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_field_annot env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_field_annot env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "}" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_anon_param_rep_COMMA_param_f5c95b4 (env : env) ((v1, v2) : CST.anon_param_rep_COMMA_param_f5c95b4) =
  let v1 = map_parameter env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_parameter env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

let map_bind_list (env : env) (x : CST.bind_list) =
  (match x with
  | `Bind x -> R.Case ("Bind",
      map_bind env x
    )
  | `LPAR_opt_bind_rep_COMMA_bind_opt_COMMA_RPAR (v1, v2, v3, v4) -> R.Case ("LPAR_opt_bind_rep_COMMA_bind_opt_COMMA_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_bind_rep_COMMA_bind_38cc8c1 env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* ")" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

let map_lambda_bind_list (env : env) ((v1, v2, v3, v4) : CST.lambda_bind_list) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_bind_rep_COMMA_bind_38cc8c1 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "|" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

let map_struct_decl (env : env) ((v1, v2, v3, v4) : CST.struct_decl) =
  let v1 = (* "struct" *) token env v1 in
  let v2 = map_struct_def_name env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "has" *) token env v1 in
        let v2 = map_abilities env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | `Struct_body x -> R.Case ("Struct_body",
        map_struct_body env x
      )
    | `SEMI tok -> R.Case ("SEMI",
        (* ";" *) token env tok
      )
    )
  in
  R.Tuple [v1; v2; v3; v4]

let map_spec_target_signature_opt (env : env) ((v1, v2, v3, v4, v5, v6) : CST.spec_target_signature_opt) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_anon_param_rep_COMMA_param_f5c95b4 env x
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type__ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_spec_func_signatures (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8) : CST.spec_func_signatures) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v3 = (* "(" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_param_rep_COMMA_param_f5c95b4 env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 = (* ")" *) token env v6 in
  let v7 = (* ":" *) token env v7 in
  let v8 = map_type__ env v8 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8]

let map_spec_block_target (env : env) (x : CST.spec_block_target) =
  (match x with
  | `Id_opt_spec_target_sign_opt (v1, v2) -> R.Case ("Id_opt_spec_target_sign_opt",
      let v1 = (* identifier *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_spec_target_signature_opt env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Module tok -> R.Case ("Module",
      (* "module" *) token env tok
    )
  | `Schema_id_opt_type_params (v1, v2, v3) -> R.Case ("Schema_id_opt_type_params",
      let v1 = (* "schema" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_type_params env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3]
    )
  )

let rec map_anon_choice_blk_9815434 (env : env) (x : CST.anon_choice_blk_9815434) =
  (match x with
  | `Seq x -> R.Case ("Seq",
      map_block env x
    )
  | `SEMI tok -> R.Case ("SEMI",
      (* ";" *) token env tok
    )
  )

and map_anon_expr_rep_COMMA_expr_8e432c6 (env : env) ((v1, v2) : CST.anon_expr_rep_COMMA_expr_8e432c6) =
  let v1 = map_expr env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_assignment (env : env) ((v1, v2, v3) : CST.assignment) =
  let v1 = map_unary_expr env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expr env v3 in
  R.Tuple [v1; v2; v3]

and map_bin_op_expr (env : env) (x : CST.bin_op_expr) =
  (match x with
  | `Op_expr_EQEQGT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_EQEQGT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "==>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_LTEQEQGT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_LTEQEQGT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "<==>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_BARBAR_op_expr (v1, v2, v3) -> R.Case ("Op_expr_BARBAR_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_AMPAMP_op_expr (v1, v2, v3) -> R.Case ("Op_expr_AMPAMP_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_EQEQ_op_expr (v1, v2, v3) -> R.Case ("Op_expr_EQEQ_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_BANGEQ_op_expr (v1, v2, v3) -> R.Case ("Op_expr_BANGEQ_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_LT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_LT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_GT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_GT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_LTEQ_op_expr (v1, v2, v3) -> R.Case ("Op_expr_LTEQ_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_GTEQ_op_expr (v1, v2, v3) -> R.Case ("Op_expr_GTEQ_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_DOTDOT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_DOTDOT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* ".." *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_BAR_op_expr (v1, v2, v3) -> R.Case ("Op_expr_BAR_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_HAT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_HAT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_AMP_op_expr (v1, v2, v3) -> R.Case ("Op_expr_AMP_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_LTLT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_LTLT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_GTGT_op_expr (v1, v2, v3) -> R.Case ("Op_expr_GTGT_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_PLUS_op_expr (v1, v2, v3) -> R.Case ("Op_expr_PLUS_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_DASH_op_expr (v1, v2, v3) -> R.Case ("Op_expr_DASH_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_STAR_op_expr (v1, v2, v3) -> R.Case ("Op_expr_STAR_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_SLASH_op_expr (v1, v2, v3) -> R.Case ("Op_expr_SLASH_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Op_expr_PERC_op_expr (v1, v2, v3) -> R.Case ("Op_expr_PERC_op_expr",
      let v1 = map_op_expr env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_op_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) (x : CST.block) =
  map_sequence env x

and map_call_args (env : env) ((v1, v2, v3, v4) : CST.call_args) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_expr_rep_COMMA_expr_8e432c6 env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* ")" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_control_body (env : env) (x : CST.control_body) =
  (match x with
  | `Seq x -> R.Case ("Seq",
      map_block env x
    )
  | `Expr x -> R.Case ("Expr",
      map_expr env x
    )
  )

and map_dot_or_index_chain (env : env) (x : CST.dot_or_index_chain) =
  (match x with
  | `Access_field (v1, v2, v3) -> R.Case ("Access_field",
      let v1 = map_dot_or_index_chain env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Rece_call (v1, v2, v3, v4, v5) -> R.Case ("Rece_call",
      let v1 = map_dot_or_index_chain env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* identifier *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "::" *) token env v1 in
            let v2 = map_type_args env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = map_call_args env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Mem_access (v1, v2, v3, v4) -> R.Case ("Mem_access",
      let v1 = map_dot_or_index_chain env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expr env v3 in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Term x -> R.Case ("Term",
      map_term env x
    )
  )

and map_expr (env : env) (x : CST.expr) =
  (match x with
  | `Assign x -> R.Case ("Assign",
      map_assignment env x
    )
  | `Op_expr x -> R.Case ("Op_expr",
      map_op_expr env x
    )
  | `Quan x -> R.Case ("Quan",
      map_quantifier env x
    )
  | `Lambda_bind_list_expr (v1, v2) -> R.Case ("Lambda_bind_list_expr",
      let v1 = map_lambda_bind_list env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  | `Deep_ellips (v1, v2, v3) -> R.Case ("Deep_ellips",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Field_access_ellips_expr (v1, v2, v3) -> R.Case ("Field_access_ellips_expr",
      let v1 = map_dot_or_index_chain env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = (* "..." *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_expr_field (env : env) ((v1, v2) : CST.expr_field) =
  let v1 = (* identifier *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_expr env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_let_expr (env : env) ((v1, v2, v3, v4) : CST.let_expr) =
  let v1 = (* "let" *) token env v1 in
  let v2 = map_bind_list env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type__ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expr env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_name_expr (env : env) (x : CST.name_expr) =
  (match x with
  | `Choice_choice_id_opt_type_args (v1, v2) -> R.Case ("Choice_choice_id_opt_type_args",
      let v1 = map_anon_choice_var_name_32a5fbc env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Choice_choice_id_opt_type_args_call_args (v1, v2, v3) -> R.Case ("Choice_choice_id_opt_type_args_call_args",
      let v1 = map_anon_choice_var_name_32a5fbc env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_call_args env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Choice_choice_id_opt_type_args_LCURL_opt_expr_field_rep_COMMA_expr_field_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6) -> R.Case ("Choice_choice_id_opt_type_args_LCURL_opt_expr_field_rep_COMMA_expr_field_opt_COMMA_RCURL",
      let v1 = map_anon_choice_var_name_32a5fbc env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = map_expr_field env v1 in
            let v2 =
              R.List (List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expr_field env v2 in
                R.Tuple [v1; v2]
              ) v2)
            in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* "}" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Choice_choice_id_BANG_call_args (v1, v2, v3) -> R.Case ("Choice_choice_id_BANG_call_args",
      let v1 = map_anon_choice_var_name_32a5fbc env v1 in
      let v2 = (* "!" *) token env v2 in
      let v3 = map_call_args env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_op_expr (env : env) (x : CST.op_expr) =
  (match x with
  | `Un_expr x -> R.Case ("Un_expr",
      map_unary_expr env x
    )
  | `Bin_op_expr x -> R.Case ("Bin_op_expr",
      map_bin_op_expr env x
    )
  )

and map_parenthesized_expr (env : env) ((v1, v2, v3) : CST.parenthesized_expr) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expr env v2 in
  let v3 = (* ")" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_quantifier (env : env) (x : CST.quantifier) =
  (match x with
  | `Choice_forall_quan_bind_rep_COMMA_quan_bind_opt_triggs_opt_where_expr_COLON_expr (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Choice_forall_quan_bind_rep_COMMA_quan_bind_opt_triggs_opt_where_expr_COLON_expr",
      let v1 =
        (match v1 with
        | `Forall tok -> R.Case ("Forall",
            (* "forall" *) token env tok
          )
        | `Exists tok -> R.Case ("Exists",
            (* "exists" *) token env tok
          )
        )
      in
      let v2 = map_quantifier_bind env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_quantifier_bind env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_triggers env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "where" *) token env v1 in
            let v2 = map_expr env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_expr env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Choose_opt_min_quan_bind_where_expr (v1, v2, v3, v4, v5) -> R.Case ("Choose_opt_min_quan_bind_where_expr",
      let v1 = (* "choose" *) token env v1 in
      let v2 =
        (match v2 with
        | Some tok -> R.Option (Some (
            (* "min" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v3 = map_quantifier_bind env v3 in
      let v4 = (* "where" *) token env v4 in
      let v5 = map_expr env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_quantifier_bind (env : env) (x : CST.quantifier_bind) =
  (match x with
  | `Id_COLON_type_ x -> R.Case ("Id_COLON_type_",
      map_typed_metavariable env x
    )
  | `Id_in_expr (v1, v2, v3) -> R.Case ("Id_in_expr",
      let v1 = (* identifier *) token env v1 in
      let v2 = (* "in" *) token env v2 in
      let v3 = map_expr env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_return_expr (env : env) (x : CST.return_expr) =
  (match x with
  | `Ret tok -> R.Case ("Ret",
      (* "return" *) token env tok
    )
  | `Ret_expr (v1, v2) -> R.Case ("Ret_expr",
      let v1 = (* "return" *) token env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_sequence (env : env) ((v1, v2, v3, v4, v5) : CST.sequence) =
  let v1 = (* "{" *) token env v1 in
  let v2 = R.List (List.map (map_use_decl env) v2) in
  let v3 = R.List (List.map (map_sequence_item env) v3) in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_expr env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_sequence_item (env : env) ((v1, v2) : CST.sequence_item) =
  let v1 =
    (match v1 with
    | `Expr x -> R.Case ("Expr",
        map_expr env x
      )
    | `Let_expr x -> R.Case ("Let_expr",
        map_let_expr env x
      )
    )
  in
  let v2 = (* ";" *) token env v2 in
  R.Tuple [v1; v2]

and map_spec_apply (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.spec_apply) =
  let v1 = (* "apply" *) token env v1 in
  let v2 = map_expr env v2 in
  let v3 = (* "to" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 env x
      ))
    | None -> R.Option None)
  in
  let v5 =
    (match v5 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v6 =
    (match v6 with
    | Some (v1, v2, v3) -> R.Option (Some (
        let v1 = (* "except" *) token env v1 in
        let v2 =
          (match v2 with
          | Some x -> R.Option (Some (
              map_anon_spec_apply_pat_rep_COMMA_spec_apply_pat_d9a21d6 env x
            ))
          | None -> R.Option None)
        in
        let v3 =
          (match v3 with
          | Some tok -> R.Option (Some (
              (* "," *) token env tok
            ))
          | None -> R.Option None)
        in
        R.Tuple [v1; v2; v3]
      ))
    | None -> R.Option None)
  in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_spec_axiom (env : env) ((v1, v2, v3, v4, v5) : CST.spec_axiom) =
  let v1 = (* "axiom" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_expr env v4 in
  let v5 = (* ";" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_spec_block (env : env) ((v1, v2) : CST.spec_block) =
  let v1 = (* "spec" *) token env v1 in
  let v2 =
    (match v2 with
    | `Spec_func x -> R.Case ("Spec_func",
        map_spec_func env x
      )
    | `Opt_spec_blk_target_LCURL_rep_use_decl_rep_spec_blk_member_RCURL (v1, v2, v3, v4, v5) -> R.Case ("Opt_spec_blk_target_LCURL_rep_use_decl_rep_spec_blk_member_RCURL",
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_spec_block_target env x
            ))
          | None -> R.Option None)
        in
        let v2 = (* "{" *) token env v2 in
        let v3 = R.List (List.map (map_use_decl env) v3) in
        let v4 = R.List (List.map (map_spec_block_member env) v4) in
        let v5 = (* "}" *) token env v5 in
        R.Tuple [v1; v2; v3; v4; v5]
      )
    )
  in
  R.Tuple [v1; v2]

and map_spec_block_member (env : env) (x : CST.spec_block_member) =
  (match x with
  | `Choice_spec_inva x -> R.Case ("Choice_spec_inva",
      (match x with
      | `Spec_inva x -> R.Case ("Spec_inva",
          map_spec_invariant env x
        )
      | `Spec_cond x -> R.Case ("Spec_cond",
          map_spec_condition env x
        )
      | `Spec_func x -> R.Case ("Spec_func",
          map_spec_func env x
        )
      | `Spec_var x -> R.Case ("Spec_var",
          map_spec_variable env x
        )
      | `Spec_incl x -> R.Case ("Spec_incl",
          map_spec_include env x
        )
      | `Spec_apply x -> R.Case ("Spec_apply",
          map_spec_apply env x
        )
      | `Spec_pragma x -> R.Case ("Spec_pragma",
          map_spec_pragma env x
        )
      | `Spec_let x -> R.Case ("Spec_let",
          map_spec_let env x
        )
      | `Spec_update x -> R.Case ("Spec_update",
          map_spec_update env x
        )
      | `Spec_axiom x -> R.Case ("Spec_axiom",
          map_spec_axiom env x
        )
      )
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

and map_spec_condition (env : env) (x : CST.spec_condition) =
  (match x with
  | `Choice_assert_opt_cond_props_expr_SEMI (v1, v2, v3, v4) -> R.Case ("Choice_assert_opt_cond_props_expr_SEMI",
      let v1 =
        (match v1 with
        | `Assert tok -> R.Case ("Assert",
            (* "assert" *) token env tok
          )
        | `Assume tok -> R.Case ("Assume",
            (* "assume" *) token env tok
          )
        | `Ensures tok -> R.Case ("Ensures",
            (* "ensures" *) token env tok
          )
        | `Requis tok -> R.Case ("Requis",
            (* "requires" *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_condition_props env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_expr env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Aborts_if_opt_cond_props_expr_opt_with_expr_SEMI (v1, v2, v3, v4, v5) -> R.Case ("Aborts_if_opt_cond_props_expr_opt_with_expr_SEMI",
      let v1 = (* "aborts_if" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_condition_props env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_expr env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "with" *) token env v1 in
            let v2 = map_expr env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Choice_aborts_with_opt_cond_props_expr_rep_COMMA_expr_SEMI (v1, v2, v3, v4, v5) -> R.Case ("Choice_aborts_with_opt_cond_props_expr_rep_COMMA_expr_SEMI",
      let v1 =
        (match v1 with
        | `Aborts_with tok -> R.Case ("Aborts_with",
            (* "aborts_with" *) token env tok
          )
        | `Modifs tok -> R.Case ("Modifs",
            (* "modifies" *) token env tok
          )
        )
      in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_condition_props env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_expr env v3 in
      let v4 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expr env v2 in
          R.Tuple [v1; v2]
        ) v4)
      in
      let v5 = (* ";" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Emits_opt_cond_props_expr_to_expr_opt_if_expr_SEMI (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("Emits_opt_cond_props_expr_to_expr_opt_if_expr_SEMI",
      let v1 = (* "emits" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_condition_props env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_expr env v3 in
      let v4 = (* "to" *) token env v4 in
      let v5 = map_expr env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "if" *) token env v1 in
            let v2 = map_expr env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      let v7 = (* ";" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  )

and map_spec_func (env : env) (x : CST.spec_func) =
  (match x with
  | `Fun_spec_func_signas_choice_seq (v1, v2, v3) -> R.Case ("Fun_spec_func_signas_choice_seq",
      let v1 = (* "fun" *) token env v1 in
      let v2 = map_spec_func_signatures env v2 in
      let v3 = map_anon_choice_blk_9815434 env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Native_fun_spec_func_signas_SEMI (v1, v2, v3, v4) -> R.Case ("Native_fun_spec_func_signas_SEMI",
      let v1 = (* "native" *) token env v1 in
      let v2 = (* "fun" *) token env v2 in
      let v3 = map_spec_func_signatures env v3 in
      let v4 = (* ";" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_spec_include (env : env) ((v1, v2, v3, v4) : CST.spec_include) =
  let v1 = (* "include" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_expr env v3 in
  let v4 = (* ";" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_spec_invariant (env : env) ((v1, v2, v3, v4, v5, v6) : CST.spec_invariant) =
  let v1 = (* "invariant" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "update" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_condition_props env x
      ))
    | None -> R.Option None)
  in
  let v5 = map_expr env v5 in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_spec_let (env : env) ((v1, v2, v3, v4, v5, v6) : CST.spec_let) =
  let v1 = (* "let" *) token env v1 in
  let v2 =
    (match v2 with
    | Some tok -> R.Option (Some (
        (* "post" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v3 = map_var_name env v3 in
  let v4 = (* "=" *) token env v4 in
  let v5 = map_expr env v5 in
  let v6 = (* ";" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

and map_spec_loop_invariant (env : env) (x : CST.spec_loop_invariant) =
  map_spec_block env x

and map_spec_update (env : env) ((v1, v2, v3) : CST.spec_update) =
  let v1 = (* "update" *) token env v1 in
  let v2 = map_assignment env v2 in
  let v3 = (* ";" *) token env v3 in
  R.Tuple [v1; v2; v3]

and map_spec_variable (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.spec_variable) =
  let v1 =
    (match v1 with
    | Some x -> R.Option (Some (
        (match x with
        | `Global tok -> R.Case ("Global",
            (* "global" *) token env tok
          )
        | `Local tok -> R.Case ("Local",
            (* "local" *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v2 = (* identifier *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_type__ env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expr env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

and map_term (env : env) (x : CST.term) =
  (match x with
  | `Brk tok -> R.Case ("Brk",
      (* "break" *) token env tok
    )
  | `Cont tok -> R.Case ("Cont",
      (* "continue" *) token env tok
    )
  | `Vec_opt_type_args_LBRACK_opt_expr_rep_COMMA_expr_opt_COMMA_RBRACK (v1, v2, v3, v4, v5, v6) -> R.Case ("Vec_opt_type_args_LBRACK_opt_expr_rep_COMMA_expr_opt_COMMA_RBRACK",
      let v1 = (* "vector" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_type_args env x
          ))
        | None -> R.Option None)
      in
      let v3 = (* "[" *) token env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_anon_expr_rep_COMMA_expr_8e432c6 env x
          ))
        | None -> R.Option None)
      in
      let v5 =
        (match v5 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v6 = (* "]" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Value x -> R.Case ("Value",
      map_value env x
    )
  | `LPAR_opt_expr_rep_COMMA_expr_opt_COMMA_RPAR x -> R.Case ("LPAR_opt_expr_rep_COMMA_expr_opt_COMMA_RPAR",
      map_call_args env x
    )
  | `LPAR_expr_COLON_type__RPAR (v1, v2, v3, v4, v5) -> R.Case ("LPAR_expr_COLON_type__RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = (* ":" *) token env v3 in
      let v4 = map_type__ env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `LPAR_expr_as_type__RPAR (v1, v2, v3, v4, v5) -> R.Case ("LPAR_expr_as_type__RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expr env v2 in
      let v3 = (* "as" *) token env v3 in
      let v4 = map_type__ env v4 in
      let v5 = (* ")" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Blk x -> R.Case ("Blk",
      map_block env x
    )
  | `Name_expr x -> R.Case ("Name_expr",
      map_name_expr env x
    )
  | `Spec_blk x -> R.Case ("Spec_blk",
      map_spec_loop_invariant env x
    )
  | `If_expr (v1, v2, v3, v4) -> R.Case ("If_expr",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expr env v2 in
      let v3 = map_control_body env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) -> R.Option (Some (
            let v1 = (* "else" *) token env v1 in
            let v2 = map_control_body env v2 in
            R.Tuple [v1; v2]
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `While_expr (v1, v2, v3, v4) -> R.Case ("While_expr",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expr env v2 in
      let v3 = map_control_body env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_spec_loop_invariant env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Loop_expr (v1, v2) -> R.Case ("Loop_expr",
      let v1 = (* "loop" *) token env v1 in
      let v2 = map_control_body env v2 in
      R.Tuple [v1; v2]
    )
  | `Ret_expr x -> R.Case ("Ret_expr",
      map_return_expr env x
    )
  | `Abort_expr (v1, v2) -> R.Case ("Abort_expr",
      let v1 = (* "abort" *) token env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `For_loop_expr (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> R.Case ("For_loop_expr",
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_var_name env v3 in
      let v4 = (* "in" *) token env v4 in
      let v5 = map_unary_expr env v5 in
      let v6 = (* ".." *) token env v6 in
      let v7 = map_unary_expr env v7 in
      let v8 =
        (match v8 with
        | Some x -> R.Option (Some (
            map_spec_loop_invariant env x
          ))
        | None -> R.Option None)
      in
      let v9 = (* ")" *) token env v9 in
      let v10 = map_block env v10 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10]
    )
  )

and map_triggers (env : env) (xs : CST.triggers) =
  R.List (List.map (fun (v1, v2, v3, v4) ->
    let v1 = (* "{" *) token env v1 in
    let v2 =
      (match v2 with
      | Some x -> R.Option (Some (
          map_anon_expr_rep_COMMA_expr_8e432c6 env x
        ))
      | None -> R.Option None)
    in
    let v3 =
      (match v3 with
      | Some tok -> R.Option (Some (
          (* "," *) token env tok
        ))
      | None -> R.Option None)
    in
    let v4 = (* "}" *) token env v4 in
    R.Tuple [v1; v2; v3; v4]
  ) xs)

and map_unary_expr (env : env) (x : CST.unary_expr) =
  (match x with
  | `Not_expr (v1, v2) -> R.Case ("Not_expr",
      let v1 = (* "!" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Ref_expr (v1, v2) -> R.Case ("Ref_expr",
      let v1 = (* "&" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Ref_mut_expr (v1, v2) -> R.Case ("Ref_mut_expr",
      let v1 = (* "&mut" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Deref_expr (v1, v2) -> R.Case ("Deref_expr",
      let v1 = (* "*" *) token env v1 in
      let v2 = map_unary_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Move_expr (v1, v2) -> R.Case ("Move_expr",
      let v1 = (* "move" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Copy_expr (v1, v2) -> R.Case ("Copy_expr",
      let v1 = (* "copy" *) token env v1 in
      let v2 = (* identifier *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Dot_or_index_chain x -> R.Case ("Dot_or_index_chain",
      map_dot_or_index_chain env x
    )
  )

let map_constant_decl (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.constant_decl) =
  let v1 = (* "const" *) token env v1 in
  let v2 = (* identifier *) token env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = map_type__ env v4 in
  let v5 = (* "=" *) token env v5 in
  let v6 = map_expr env v6 in
  let v7 = (* ";" *) token env v7 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7]

let map_function_decl (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) : CST.function_decl) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "inline" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 = (* "fun" *) token env v2 in
  let v3 = (* identifier *) token env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_type_params env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* "(" *) token env v5 in
  let v6 =
    (match v6 with
    | Some x -> R.Option (Some (
        map_anon_param_rep_COMMA_param_f5c95b4 env x
      ))
    | None -> R.Option None)
  in
  let v7 =
    (match v7 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v8 = (* ")" *) token env v8 in
  let v9 =
    (match v9 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = (* ":" *) token env v1 in
        let v2 = map_type__ env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v10 =
    (match v10 with
    | `Pure tok -> R.Case ("Pure",
        (* "pure" *) token env tok
      )
    | `Rep_opt_BANG_choice_acquis_access_spec_list xs -> R.Case ("Rep_opt_BANG_choice_acquis_access_spec_list",
        R.List (List.map (fun (v1, v2, v3) ->
          let v1 =
            (match v1 with
            | Some tok -> R.Option (Some (
                (* "!" *) token env tok
              ))
            | None -> R.Option None)
          in
          let v2 =
            (match v2 with
            | `Acquis tok -> R.Case ("Acquis",
                (* "acquires" *) token env tok
              )
            | `Reads tok -> R.Case ("Reads",
                (* "reads" *) token env tok
              )
            | `Writes tok -> R.Case ("Writes",
                (* "writes" *) token env tok
              )
            )
          in
          let v3 = map_access_specifier_list env v3 in
          R.Tuple [v1; v2; v3]
        ) xs)
      )
    )
  in
  let v11 = map_anon_choice_blk_9815434 env v11 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9; v10; v11]

let map_script (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.script) =
  let v1 = (* "script" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_use_decl env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_constant_decl env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 =
    (match v5 with
    | Some x -> R.Option (Some (
        map_attributes env x
      ))
    | None -> R.Option None)
  in
  let v6 =
    R.List (List.map (map_module_member_modifier env) v6)
  in
  let v7 = map_function_decl env v7 in
  let v8 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_spec_loop_invariant env v2 in
      R.Tuple [v1; v2]
    ) v8)
  in
  let v9 = (* "}" *) token env v9 in
  R.Tuple [v1; v2; v3; v4; v5; v6; v7; v8; v9]

let map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Opt_attris_choice_use_decl (v1, v2) -> R.Case ("Opt_attris_choice_use_decl",
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 =
        (match v2 with
        | `Use_decl x -> R.Case ("Use_decl",
            map_use_decl env x
          )
        | `Friend_decl x -> R.Case ("Friend_decl",
            map_friend_decl env x
          )
        | `Spec_spec_func (v1, v2) -> R.Case ("Spec_spec_func",
            let v1 = (* "spec" *) token env v1 in
            let v2 = map_spec_func env v2 in
            R.Tuple [v1; v2]
          )
        | `Spec_blk x -> R.Case ("Spec_blk",
            map_spec_loop_invariant env x
          )
        | `Spec_inva x -> R.Case ("Spec_inva",
            map_spec_invariant env x
          )
        | `Rep_module_member_modi_choice_cst_decl (v1, v2) -> R.Case ("Rep_module_member_modi_choice_cst_decl",
            let v1 =
              R.List (List.map (map_module_member_modifier env) v1)
            in
            let v2 =
              (match v2 with
              | `Cst_decl x -> R.Case ("Cst_decl",
                  map_constant_decl env x
                )
              | `Struct_decl x -> R.Case ("Struct_decl",
                  map_struct_decl env x
                )
              | `Func_decl x -> R.Case ("Func_decl",
                  map_function_decl env x
                )
              )
            in
            R.Tuple [v1; v2]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Ellips tok -> R.Case ("Ellips",
      (* "..." *) token env tok
    )
  )

let map_module_ (env : env) ((v1, v2, v3, v4, v5, v6) : CST.module_) =
  let v1 =
    (match v1 with
    | `Spec tok -> R.Case ("Spec",
        (* "spec" *) token env tok
      )
    | `Module_id tok -> R.Case ("Module_id",
        (* "module" *) token env tok
      )
    )
  in
  let v2 =
    (match v2 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_id_9d8f098 env v1 in
        let v2 = (* "::" *) token env v2 in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v3 = (* identifier *) token env v3 in
  let v4 = (* "{" *) token env v4 in
  let v5 = R.List (List.map (map_declaration env) v5) in
  let v6 = (* "}" *) token env v6 in
  R.Tuple [v1; v2; v3; v4; v5; v6]

let map_address_block (env : env) ((v1, v2, v3, v4, v5) : CST.address_block) =
  let v1 = (* "address" *) token env v1 in
  let v2 = map_anon_choice_id_9d8f098 env v2 in
  let v3 = (* "{" *) token env v3 in
  let v4 =
    R.List (List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | Some x -> R.Option (Some (
            map_attributes env x
          ))
        | None -> R.Option None)
      in
      let v2 = map_module_ env v2 in
      R.Tuple [v1; v2]
    ) v4)
  in
  let v5 = (* "}" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

let map_source_file (env : env) (x : CST.source_file) =
  (match x with
  | `Rep_opt_attris_choice_module xs -> R.Case ("Rep_opt_attris_choice_module",
      R.List (List.map (fun (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> R.Option (Some (
              map_attributes env x
            ))
          | None -> R.Option None)
        in
        let v2 =
          (match v2 with
          | `Module x -> R.Case ("Module",
              map_module_ env x
            )
          | `Script x -> R.Case ("Script",
              map_script env x
            )
          | `Addr_blk x -> R.Case ("Addr_blk",
              map_address_block env x
            )
          )
        in
        R.Tuple [v1; v2]
      ) xs)
    )
  | `Semg_exp (v1, v2) -> R.Case ("Semg_exp",
      let v1 = (* "__SEMGREP_EXPRESSION" *) token env v1 in
      let v2 = map_expr env v2 in
      R.Tuple [v1; v2]
    )
  | `Semg_stmt (v1, v2) -> R.Case ("Semg_stmt",
      let v1 = (* "__SEMGREP_STATEMENT" *) token env v1 in
      let v2 = R.List (List.map (map_sequence_item env) v2) in
      R.Tuple [v1; v2]
    )
  )

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_string
  |> print_string
