defmodule UpExDocVersion do
  @mix_exs "mix.exs"

  def up(new_vsn) do
    content_up =
      @mix_exs
      |> File.read!()
      |> up(new_vsn)

    File.write!(@mix_exs, "#{content_up}\n")
  end

  defp up(mix_exs, new_vsn) do
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

[new_vsn] = System.argv()
:ok = UpExDocVersion.up(new_vsn)
