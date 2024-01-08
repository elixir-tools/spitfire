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
