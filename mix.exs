defmodule Rebar3ExDoc.MixProject do
  use Mix.Project
  @ex_doc_version "0.30.5"
  def project do
    [
      app: :rebar3_ex_doc,
      version: "0.2.20",
      elixir: "~> 1.13",
      deps: [ex_doc: "~> #{@ex_doc_version}"],
      escript: [main_module: ExDoc.CLI, name: "ex_doc", path: "priv/ex_doc"],
      docs: [main: "readme", extras: ["README.md"]]
    ]
  end
end
