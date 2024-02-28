defmodule Spitfire.EnvTest do
  use ExUnit.Case, async: true

  # alias Spitfire.Env

  @tag :skip
  test "returns aliases at the given position" do
    code =
      Spitfire.parse!(
        """
        defmodule Foo do
          alias Foo.Bar
          alias Foo.Bar.Baz

          def run() do
            foo = 1
            
            bar = 2
            :ok
          end
        end
        """,
        literal_encoder: fn lit, meta -> {:ok, {:__literal__, meta, [lit]}} end
      )

    assert %Spitfire.Env{} = env = Spitfire.Env.new(code)

    pos = {7, 5}

    dbg(Spitfire.Env.at(env, pos))

    flunk()
  end
end
