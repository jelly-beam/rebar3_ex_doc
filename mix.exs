defmodule Rebar3ExDoc.MixProject do
  use Mix.Project
  @ex_doc_version "0.31.2"
  def project do
    [
      app: :rebar3_ex_doc,
      version: "0.2.21",
      elixir: "~> 1.13",
      deps: [ex_doc: "#{@ex_doc_version}"],
      escript: [main_module: ExDoc.CLI, name: "ex_doc_otp_24", path: "priv/ex_doc_otp_24"],
      docs: [main: "readme", extras: ["README.md"]]
    ]
  end
end
