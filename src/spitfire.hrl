-define(key(M, K), maps:get(K, M)).
-define(ann(Meta), elixir_erl:get_ann(Meta)).
-define(line(Meta), elixir_utils:get_line(Meta)).
-define(generated(Meta), elixir_utils:generated(Meta)).
-define(var_context, ?MODULE).
-define(remote(Ann, Module, Function, Args), {call, Ann, {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}}, Args}).

-record(spitfire_tokenizer, {
  terminators=[],
  unescape=true,
  cursor_completion=false,
  existing_atoms_only=false,
  static_atoms_encoder=nil,
  preserve_comments=nil,
  identifier_tokenizer=spitfire_tokenizer,
  ascii_identifiers_only=true,
  indentation=0,
  column=1,
  mismatch_hints=[],
  warnings=[]
}).

