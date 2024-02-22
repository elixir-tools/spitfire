case current_token_type(parser) do
  :identifier -> &parse_identifier/1
  :do_identifier -> &parse_identifier/1
  :paren_identifier -> &parse_identifier/1
  :bracket_identifier -> &parse_identifier/1
  :op_identifier -> &parse_identifier/1
  :alias -> &parse_alias/1
  :"<<" -> &parse_bitstring/1
  :kw_identifier when is_list or is_map -> &parse_kw_identifier/1
  :kw_identifier_unsafe when is_list or is_map -> &parse_kw_identifier/1
  :kw_identifier when not is_list and not is_map -> &parse_bracketless_kw_list/1
  :kw_identifier_unsafe when not is_list and not is_map -> &parse_bracketless_kw_list/1
  :int -> &parse_int/1
  :flt -> &parse_float/1
  :atom -> &parse_atom/1
  :atom_quoted -> &parse_atom/1
  :atom_unsafe -> &parse_atom/1
  true -> &parse_boolean/1
  false -> &parse_boolean/1
  :bin_string -> &parse_string/1
  :bin_heredoc -> &parse_string/1
  :list_string -> &parse_string/1
  :list_heredoc -> &parse_string/1
  :char -> &parse_char/1
  :sigil -> &parse_sigil/1
  :fn -> &parse_anon_function/1
  :at_op -> &parse_prefix_expression/1
  :unary_op -> &parse_prefix_expression/1
  :capture_op -> &parse_prefix_expression/1
  :dual_op -> &parse_prefix_expression/1
  :capture_int -> &parse_capture_int/1
  :stab_op -> &parse_stab_expression/1
  :range_op -> &parse_range_expression/1
  :"[" -> &parse_list_literal/1
  :"(" -> &parse_grouped_expression/1
  :"{" -> &parse_tuple_literal/1
  :%{} -> &parse_map_literal/1
  :% -> &parse_struct_literal/1
  :ellipsis_op -> &parse_ellipsis_op/1
  nil -> &parse_nil_literal/1
  _ -> nil
end
