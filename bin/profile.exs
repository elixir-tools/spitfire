#!/usr/bin/env elixir

Mix.install([
  {:eflambe, "~> 0.3.0"},
  {:spitfire, path: "."}
])

defmodule Profiler do
  def run do
    input = File.read!(System.argv() |> List.first())
    :eflambe.apply({Spitfire, :parse!, [input]}, output_format: :brendan_gregg)
  end

  def run2 do
    input = File.read!(System.argv() |> List.first())
    :eflambe.apply({Code, :string_to_quoted!, [input, [columns: true, token_metadata: true]]}, output_format: :brendan_gregg)
  end
end

Profiler.run()
Profiler.run2()
