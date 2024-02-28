defmodule Spitfire.While2 do
  @moduledoc false
  def recurse(token, pred, callback) do
    if pred.(token) do
      case callback.(token) do
        {:filter, {_, token}} ->
          recurse(token, pred, callback)

        {item, token} ->
          [item | recurse(token, pred, callback)]
      end
    else
      Process.put(:while_token, token)
      []
    end
  end

  defmacro while2(expression, do: block) do
    {:<-, _, [condition, token]} = expression

    predicate =
      quote do
        fn unquote(token) ->
          _ = unquote(token)
          unquote(condition)
        end
      end

    callback =
      quote do
        fn unquote(token) ->
          _ = unquote(token)
          unquote(block)
        end
      end

    quote do
      items = recurse(unquote(token), unquote(predicate), unquote(callback))
      token = Process.get(:while_token)
      Process.delete(:while_token)
      {items, token}
    end
  end
end

defmodule Spitfire.While do
  @moduledoc false
  def do_while(acc, predicate) do
    {condition, wrapped_body} = predicate.(acc)

    if condition do
      acc = wrapped_body.(acc)

      do_while(acc, predicate)
    else
      acc
    end
  end

  defmacro while(expression, do: block) do
    {:<-, _, [condition, acc]} = expression

    predicate =
      quote do
        fn unquote(acc) ->
          _ = unquote(acc)
          res = unquote(condition)

          body = fn unquote(acc) ->
            _ = unquote(acc)

            unquote(block)
          end

          {res, body}
        end
      end

    quote do
      do_while(unquote(acc), unquote(predicate))
    end
  end
end
