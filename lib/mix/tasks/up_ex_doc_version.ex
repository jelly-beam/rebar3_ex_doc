defmodule Mix.Tasks.UpExDocVersion do
  use Mix.Task

  @shortdoc "Update mix.exs' ex_doc version"
  @mix_exs "mix.exs"

  def run([new_vsn]) do
    content_up =
      @mix_exs
      |> File.read!()
      |> run(new_vsn)

    File.write!(@mix_exs, "#{content_up}\n")
  end

  defp run(mix_exs, new_vsn) do
    mix_exs
    |> Code.string_to_quoted!()
    |> Macro.prewalk(fn
      {:@, anno, [{:ex_doc_version, anno, [_cur_vsn]}]} ->
        {:@, anno, [{:ex_doc_version, anno, [new_vsn]}]}

      node ->
        node
    end)
    |> Macro.to_string()
  end
end
