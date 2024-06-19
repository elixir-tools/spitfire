defmodule Spitfire.Env do
  @moduledoc """
  Environment querying
  """
  @env %{
    Macro.Env.prune_compile_info(__ENV__)
    | line: 0,
      file: "nofile",
      module: nil,
      function: nil,
      context_modules: []
  }
  defp env, do: @env

  @typedoc "Alias for `Macro.t()`"
  @type ast :: Macro.t()

  @typedoc "A name/arity pair"
  @type func :: {atom(), arity()}

  @typedoc "A name/arity pair"
  @type macro :: {atom(), arity()}

  @typedoc "The expansion state. Contains a total listing of local functions, macros, and module attributes."
  @type state :: %{
          functions: [func()],
          macros: [macro()],
          attrs: [String.t()]
        }
  @typedoc "The environment"
  @type env :: %{
          functions: [{module(), [func()]}],
          macros: [{module(), [macro()]}],
          attrs: [String.t()],
          variables: [atom()]
        }

  @typedoc "The environment at the cursor position"
  @type cursor_env :: env()

  @typedoc "The environment after expanding the entire syntax tree"
  @type final_env :: env()

  @doc """
  Expands the environment of the given AST.

  In order to get the environment at the given cursor position, `expand/2` expects to find a `__cursor__()` node
  inside the AST to indicate where the cursor is.

  This can be achieved by passing your code fragment (meaning, a string of your code from the top to where the cursor is) to `Spitfire.container_cursor_to_quoted/1`, and then passing that to `expand/2`.

  Please see the tests for example usage.
  """
  @spec expand(Macro.t(), String.t()) ::
          {ast :: ast(), final_state :: state(), final_env :: final_env(), cursor_env :: cursor_env()}
  def expand(ast, file) do
    env = env()

    {ast, state, env} =
      expand(
        ast,
        %{functions: %{}, macros: %{}, attrs: []},
        %{env | file: file}
      )

    {cursor_state, cursor_env} =
      Process.get(:cursor_env, {Map.new(), env()})

    cursor_env =
      Map.merge(
        Map.from_struct(cursor_env),
        %{
          functions:
            Enum.filter(Map.get(state, :functions, []), fn {m, _} -> m == cursor_env.module end) ++
              cursor_env.functions,
          macros: Enum.filter(Map.get(state, :macros, []), fn {m, _} -> m == cursor_env.module end) ++ cursor_env.macros,
          attrs: Enum.uniq(Map.get(cursor_state, :attrs, [])),
          variables: for({name, nil} <- cursor_env.versioned_vars, do: name)
        }
      )

    {ast, state, env, cursor_env}
  after
    Process.delete(:cursor_env)
  end

  defp expand({:__cursor__, _meta, _} = node, state, env) do
    Process.put(:cursor_env, {state, env})
    {node, state, env}
  end

  defp expand({:@, _, [{:__cursor__, _, _}]} = node, state, env) do
    Process.put(:cursor_env, {state, env})
    {node, state, env}
  end

  defp expand([_ | _] = list, state, env) do
    expand_list(list, state, env)
  end

  defp expand({left, right}, state, env) do
    {left, state, env} = expand(left, state, env)
    {right, state, env} = expand(right, state, env)
    {{left, right}, state, env}
  end

  defp expand({:{}, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:{}, meta, args}, state, env}
  end

  defp expand({:%{}, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:%{}, meta, args}, state, env}
  end

  defp expand({:|, meta, [left, right]}, state, env) do
    {left, state, env} = expand(left, state, env)
    {right, state, env} = expand(right, state, env)
    {{:|, meta, [left, right]}, state, env}
  end

  defp expand({:<<>>, meta, args}, state, env) do
    {args, state, env} = expand_list(args, state, env)
    {{:<<>>, meta, args}, state, env}
  end

  ## __block__

  defp expand({:__block__, _, list}, state, env) do
    expand_list(list, state, env)
  end

  ## __aliases__

  defp expand({:__aliases__, meta, [head | tail] = list}, state, env) do
    case Macro.Env.expand_alias(env, meta, list, trace: false) do
      {:alias, alias} ->
        # A compiler may want to emit a :local_function trace in here.
        # Elixir also warns on easy to confuse aliases, such as True/False/Nil.
        {alias, state, env}

      :error ->
        {head, state, env} = expand(head, state, env)

        if is_atom(head) do
          # A compiler may want to emit a :local_function trace in here.
          {Module.concat([head | tail]), state, env}
        else
          {{:__aliases__, meta, [head | tail]}, state, env}
        end
    end
  end

  ## require, alias, import
  # Those are the main special forms and they require some care.
  #
  # First of all, if __aliases__ is changed to emit traces (which a
  # custom compiler should), we should not emit traces when expanding
  # the first argument of require/alias/import.
  #
  # Second, we must never expand the alias in `:as`. This is handled
  # below.
  #
  # Finally, multi-alias/import/require, such as alias Foo.Bar.{Baz, Bat}
  # is not implemented, check elixir_expand.erl on how to implement it.

  defp expand({form, meta, [arg]}, state, env) when form in [:require, :alias, :import] do
    expand({form, meta, [arg, []]}, state, env)
  end

  defp expand({:alias, meta, [arg, opts]} = node, state, env) do
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_directive_opts(opts, state, env)

    if is_atom(arg) do
      # An actual compiler would raise if the alias fails.
      case Macro.Env.define_alias(env, meta, arg, [trace: false] ++ opts) do
        {:ok, env} -> {arg, state, env}
        {:error, _} -> {arg, state, env}
      end
    else
      {node, state, env}
    end
  end

  defp expand({:require, meta, [arg, opts]} = node, state, env) do
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_directive_opts(opts, state, env)

    if is_atom(arg) do
      # An actual compiler would raise if the module is not defined or if the require fails.
      case Macro.Env.define_require(env, meta, arg, [trace: false] ++ opts) do
        {:ok, env} -> {arg, state, env}
        {:error, _} -> {arg, state, env}
      end
    else
      {node, state, env}
    end
  end

  defp expand({:import, meta, [arg, opts]} = node, state, env) do
    {arg, state, env} = expand(arg, state, env)
    {opts, state, env} = expand_directive_opts(opts, state, env)

    if is_atom(arg) do
      # An actual compiler would raise if the module is not defined or if the import fails.
      with true <- is_atom(arg) and Code.ensure_loaded?(arg),
           {:ok, env} <- Macro.Env.define_import(env, meta, arg, [trace: false] ++ opts) do
        {arg, state, env}
      else
        _ -> {arg, state, env}
      end
    else
      {node, state, env}
    end
  end

  ## =/2
  # We include = as an example of how we could handle variables.
  # For example, if you want to store where variables are defined,
  # you would collect this information in expand_pattern/3 and
  # invoke it from all relevant places (such as case, cond, try, etc).

  defp expand({match, meta, [left, right]}, state, env) when match in [:=, :<-] do
    {left, state, env} = expand_pattern(left, state, env)
    {right, state, env} = expand(right, state, env)
    {{match, meta, [left, right]}, state, env}
  end

  ## quote/1, quote/2
  # We need to expand options and look inside unquote/unquote_splicing.
  # A custom compiler may want to raise on this special form (for example),
  # quoted expressions make no sense if you are writing a language that
  # compiles to C.

  defp expand({:quote, _, [opts]}, state, env) do
    {block, opts} = Keyword.pop(opts, :do)
    {_opts, state, env} = expand_list(opts, state, env)
    expand_quote(block, state, env)
  end

  defp expand({:quote, _, [opts, block_opts]}, state, env) do
    {_opts, state, env} = expand_list(opts, state, env)
    expand_quote(Keyword.get(block_opts, :do), state, env)
  end

  ## Pin operator
  # It only appears inside match and it disables the match behaviour.

  defp expand({:^, meta, [arg]}, state, %{context: context} = env) do
    {arg, state, env} = expand(arg, state, %{env | context: nil})
    {{:^, meta, [arg]}, state, %{env | context: context}}
  end

  defp expand({:->, _, [params, block]}, state, env) do
    {_, state, penv} =
      for p <- params, reduce: {nil, state, env} do
        {_, state, penv} ->
          expand_pattern(p, state, penv)
      end

    {res, state, _env} = expand(block, state, penv)
    {res, state, env}
  end

  ## Remote call

  defp expand({{:., dot_meta, [module, fun]}, meta, args}, state, env) when is_atom(fun) and is_list(args) do
    {module, state, env} = expand(module, state, env)
    arity = length(args)

    if is_atom(module) do
      case Macro.Env.expand_require(env, meta, module, fun, arity,
             trace: false,
             check_deprecations: false
           ) do
        {:macro, module, callback} ->
          expand_macro(meta, module, fun, args, callback, state, env)

        :error ->
          expand_remote(meta, module, fun, args, state, env)
      end
    else
      {{{:., dot_meta, [module, fun]}, meta, args}, state, env}
    end
  end

  # self calling anonymous function

  defp expand({{:., _dmeta, [func]}, _callmeta, args}, state, env) when is_list(args) do
    {res, state, _env} = expand(func, state, env)
    {res, state, env}
  end

  defp expand({:in, meta, [left, right]}, state, %{context: :match} = env) do
    {left, state, env} = expand_pattern(left, state, env)
    {{:in, meta, [left, right]}, state, env}
  end

  ## Imported or local call

  defp expand({fun, meta, args}, state, env) when is_atom(fun) and is_list(args) do
    arity = length(args)

    # For language servers, we don't want to emit traces, nor expand local macros,
    # nor print deprecation warnings. Compilers likely want those set to true.
    case Macro.Env.expand_import(env, meta, fun, arity,
           trace: false,
           allow_locals: false,
           check_deprecations: false
         ) do
      {:macro, module, callback} ->
        expand_macro(meta, module, fun, args, callback, state, env)

      {:function, module, fun} ->
        expand_remote(meta, module, fun, args, state, env)

      :error ->
        expand_local(meta, fun, args, state, env)

      {:error, :not_found} ->
        expand_local(meta, fun, args, state, env)
    end
  end

  ## __MODULE__, __DIR__, __ENV__, __CALLER__
  # A custom compiler may want to raise.

  defp expand({:__MODULE__, _, ctx}, state, env) when is_atom(ctx) do
    {env.module, state, env}
  end

  defp expand({:__DIR__, _, ctx}, state, env) when is_atom(ctx) do
    {Path.dirname(env.file), state, env}
  end

  defp expand({:__ENV__, _, ctx}, state, env) when is_atom(ctx) do
    {Macro.escape(env), state, env}
  end

  defp expand({:__CALLER__, _, ctx} = ast, state, env) when is_atom(ctx) do
    {ast, state, env}
  end

  ## var
  # For the language server, we only want to capture definitions,
  # we don't care when they are used.

  defp expand({var, meta, ctx} = ast, state, %{context: :match} = env) when is_atom(var) and is_atom(ctx) do
    ctx = Keyword.get(meta, :context, ctx)
    vv = Map.update(env.versioned_vars, var, ctx, fn _ -> ctx end)

    {ast, state, %{env | versioned_vars: vv}}
  end

  ## Fallback

  defp expand(ast, state, env) do
    {ast, state, env}
  end

  ## Macro handling

  # This is going to be the function where you will intercept expansions
  # and attach custom behaviour. As an example, we will capture the module
  # definition, fully replacing the actual implementation. You could also
  # use this to capture module attributes (optionally delegating to the actual
  # implementation), function expansion, and more.
  defp expand_macro(_meta, Kernel, :defmodule, [alias, [{_, block}]], _callback, state, env) do
    {expanded, state, env} = expand(alias, state, env)

    if is_atom(expanded) do
      {full, env} = alias_defmodule(alias, expanded, env)
      env = %{env | context_modules: [full | env.context_modules]}

      # The env inside the block is discarded.
      {result, state, _env} = expand(block, state, %{env | module: full})
      {result, state, env}
    else
      # If we don't know the module name, do we still want to expand it here?
      # Perhaps it would be useful for dealing with local functions anyway?
      # But note that __MODULE__ will return nil.
      #
      # The env inside the block is discarded.
      {result, state, _env} = expand(block, state, env)
      {result, state, env}
    end
  end

  defp expand_macro(_meta, Kernel, type, args, _callback, state, env) when type in [:def, :defmacro, :defp, :defmacrop] do
    # extract the name, params, guards, and blocks
    {name, params, guards, blocks} =
      case args do
        [{:when, _, [{name, _, params} | guards]} | maybe_blocks] ->
          {name, params, guards, maybe_blocks}

        [{name, _, params} | maybe_blocks] ->
          {name, params, [], maybe_blocks}
      end

    blocks = List.first(blocks, [])

    # collect the environment from the parameters
    # parameters are always patterns
    {_, state, penv} =
      for p <- List.wrap(params), reduce: {nil, state, env} do
        {_, state, penv} ->
          expand_pattern(p, state, penv)
      end

    # expand guards, which includes the env from params
    {_, state, _} =
      for guard <- List.wrap(guards), reduce: {nil, state, penv} do
        {_, state, env} ->
          expand(guard, state, env)
      end

    # expand the blocks, there could be `:do`, `:after`, `:catch`, etc
    {blocks, state} =
      for {type, block} <- List.wrap(blocks), reduce: {[], state} do
        {acc, state} ->
          {res, state, _env} = expand(block, state, penv)
          {[{type, res} | acc], state}
      end

    arity = length(List.wrap(params))

    # determine which key to save this function in state
    state_key =
      case type do
        type when type in [:def, :defp] -> :functions
        type when type in [:defmacro, :defmacrop] -> :macros
      end

    funcs =
      if is_atom(name) do
        Map.update(state[state_key], env.module, [{name, arity}], &Keyword.put_new(&1, name, arity))
      else
        state[state_key]
      end

    {Enum.reverse(blocks), put_in(state[state_key], funcs), env}
  end

  defp expand_macro(meta, Kernel, :@, [{name, _, p}] = args, callback, state, env) when is_list(p) do
    state = update_in(state.attrs, &[to_string(name) | &1])
    expand_macro_callback(meta, Kernel, :@, args, callback, state, env)
  end

  defp expand_macro(meta, module, fun, args, callback, state, env) do
    expand_macro_callback(meta, module, fun, args, callback, state, env)
  end

  defp expand_macro_callback(meta, module, fun, args, callback, state, env) do
    callback.(meta, args)
  catch
    :throw, other ->
      throw(other)

    :error, _error ->
      {{{:., meta, [module, fun]}, meta, args}, state, env}
  else
    ast ->
      expand(ast, state, env)
  end

  ## defmodule helpers
  # defmodule automatically defines aliases, we need to mirror this feature here.

  # defmodule Elixir.Alias
  defp alias_defmodule({:__aliases__, _, [:"Elixir", _ | _]}, module, env), do: {module, env}

  # defmodule Alias in root
  defp alias_defmodule({:__aliases__, _, _}, module, %{module: nil} = env), do: {module, env}

  # defmodule Alias nested
  defp alias_defmodule({:__aliases__, meta, [h | t]}, _module, env) when is_atom(h) do
    module = Module.concat([env.module, h])
    alias = String.to_atom("Elixir." <> Atom.to_string(h))
    {:ok, env} = Macro.Env.define_alias(env, meta, module, as: alias, trace: false)

    case t do
      [] -> {module, env}
      _ -> {String.to_atom(Enum.join([module | t], ".")), env}
    end
  end

  # defmodule _
  defp alias_defmodule(_raw, module, env) do
    {module, env}
  end

  ## Helpers

  defp expand_remote(meta, module, fun, args, state, env) do
    # A compiler may want to emit a :remote_function trace in here.
    {args, state, env} = expand_list(args, state, env)
    {{{:., meta, [module, fun]}, meta, args}, state, env}
  end

  defp expand_local(_meta, fun, args, state, env) when fun in [:for, :with] do
    {params, blocks} =
      Enum.split_while(args, fn
        [{:do, _} | _] -> false
        _ -> true
      end)

    {_, state, penv} =
      for p <- params, reduce: {nil, state, env} do
        {_, state, penv} ->
          expand_pattern(p, state, penv)
      end

    {blocks, state} =
      for {type, block} <- List.first(blocks, []), reduce: {[], state} do
        {acc, state} ->
          env =
            if type == :do do
              penv
            else
              env
            end

          {res, state, _env} = expand(block, state, env)
          {[{type, res} | acc], state}
      end

    {blocks, state, env}
  end

  defp expand_local(meta, fun, args, state, env) do
    # A compiler may want to emit a :local_function trace in here.
    {args, state, env} = expand_list(args, state, env)
    {{fun, meta, args}, state, env}
  end

  defp expand_pattern(pattern, state, %{context: context} = env) do
    {pattern, state, env} = expand(pattern, state, %{env | context: :match})
    {pattern, state, %{env | context: context}}
  end

  defp expand_directive_opts(opts, state, env) do
    opts =
      Keyword.replace_lazy(opts, :as, fn
        {:__aliases__, _, list} -> Module.concat(list)
        other -> other
      end)

    expand(opts, state, env)
  end

  defp expand_list(ast, state, env), do: expand_list(ast, state, env, [])

  defp expand_list([], state, env, acc) do
    {Enum.reverse(acc), state, env}
  end

  defp expand_list([h | t], state, env, acc) do
    {h, state, env} = expand(h, state, env)
    expand_list(t, state, env, [h | acc])
  end

  defp expand_quote(ast, state, env) do
    {_, {state, env}} =
      Macro.prewalk(ast, {state, env}, fn
        # We need to traverse inside unquotes
        {unquote, _, [expr]}, {state, env} when unquote in [:unquote, :unquote_splicing] ->
          {_expr, state, env} = expand(expr, state, env)
          {:ok, {state, env}}

        # If we find a quote inside a quote, we stop traversing it
        {:quote, _, [_]}, acc ->
          {:ok, acc}

        {:quote, _, [_, _]}, acc ->
          {:ok, acc}

        # Otherwise we go on
        node, acc ->
          {node, acc}
      end)

    {ast, state, env}
  end
end
