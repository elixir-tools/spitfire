#!/usr/bin/env elixir

Mix.install([
  {:spitfire, path: "."}
])

defmodule ParseException do
  @moduledoc false
  defexception [:message]
end

defmodule Main do
  @moduledoc false
  @ignore [
    "/lib/**/ebin/**/*",
    "/lib/**/_build/**/*",
    "/lib/**/tmp/**/*"
  ]

  def collect_files(argv) do
    root = List.first(argv)
    IO.puts(root)
    wildcard = Path.join(root, "**/{lib,test}/**/*.{ex,exs}")
    IO.puts(wildcard)
    files = Path.wildcard(wildcard)
    ignore = Enum.flat_map(@ignore, &Path.wildcard(Path.join(root, &1)))

    for file <- files, file not in ignore, {:ok, content} <- [File.read(file)] do
      {file, content}
    end
  end

  def run(files) do
    IO.puts("#{length(files)} files")

    Enum.map(files, fn {file, content} ->
      IO.puts("==> #{file}")

      {time, spitfire} = :timer.tc(fn -> Spitfire.parse(content) end, :millisecond)
      core = Code.string_to_quoted(content, columns: true, token_metadata: true, emit_warnings: false)

      if spitfire == core do
        :ok
      else
        File.mkdir_p!("tmp")
        File.write!("tmp/spitfire.ex", inspect(spitfire, pretty: true, printable_limit: :infinity, limit: :infinity))
        File.write!("tmp/core.ex", inspect(core, pretty: true, printable_limit: :infinity, limit: :infinity))

        exe = System.find_executable("delta") || "diff"

        {diff, _} = System.cmd(exe, ["tmp/spitfire.ex", "tmp/core.ex"])
        IO.puts(file)
        IO.puts(diff)
        raise ParseException, "Failed on file: #{file}"
      end

      time
    end)
  end
end

files = Main.collect_files(System.argv())

time = files |> Main.run() |> Enum.sum()

IO.puts("Parsed in #{time}ms")
