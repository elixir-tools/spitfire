#! /usr/bin/env elixir

defmodule Main do
  @moduledoc false
  require Logger

  @repo System.get_env("REPO", "elixir-tools/spitfire")
  def run do
    input = IO.read(:eof)

    results =
      case JSON.decode(input) do
        {:ok, results} ->
          results

        {:error, reason} ->
          case reason do
            {_, offset, byte} ->
              Logger.error(inspect(byte))
              Logger.error(binary_slice(input, offset, byte_size(input)))

            _ ->
              :ok
          end

          Logger.error(inspect(reason))
          raise "Malformed JSON: " <> input
      end

    failures =
      for failures <- get_in(results, ["tests", Access.all(), "failures"]), f <- failures do
        [match] = Regex.run(~r/.*Clause:.*\s+Generated:.*\n+(.*)/, f["message"], capture: :all_but_first)

        JSON.decode!(String.trim(match))
      end

    failures = Enum.uniq_by(failures, & &1["code"])

    if failures == [] do
      IO.write(:stderr, "No failures!")
    end

    for fail <- failures do
      {issues, 0} =
        gh(
          ~w(issue list --label property-failure --json title) ++
            ["--jq", ~s<. | map(. | select(.title == "#{fail["code"]}"))>]
        )

      case JSON.decode!(issues) do
        [] ->
          IO.write(:stderr, "Making new issue for #{fail["code"]}\n")

          gh(
            ~w(issue create --label property-failure) ++
              [
                "--title",
                fail["code"],
                "--body",
                """
                Type: #{fail["type"]}
                Context: #{fail["context"]}

                Code:
                ```elixir
                #{fail["code"]}
                ```

                Elixir AST:
                ```elixir
                #{fail["elixir"]}
                ```

                Spitfire AST:
                ```elixir
                #{fail["spitfire"] || "N/A"}
                ```
                """
              ]
          )

        _ ->
          IO.write(:stderr, "Issue already exists for #{fail["code"]}\n")
      end
    end
  end

  defp gh(args) do
    System.cmd("gh", args ++ ~w(--repo #{@repo}))
  end
end

Main.run()
