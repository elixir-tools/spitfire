defmodule SpitfireTest do
  use ExUnit.Case
  doctest Spitfire

  test "greets the world" do
    assert Spitfire.hello() == :world
  end
end
