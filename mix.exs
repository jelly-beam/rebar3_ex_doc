defmodule Rebar3ExDoc.MixProject do
  use Mix.Project
  @ex_doc_version "0.32.0"
  def project do
    [
      app: :rebar3_ex_doc,
      version: "0.2.22",
      elixir: "~> 1.13",
      deps: [ex_doc: "#{@ex_doc_version}"],
      escript: [
        main_module: ExDoc.CLI,
        name: "ex_doc_otp_#{:erlang.system_info(:otp_release)}",
        path: "priv/ex_doc_otp_#{:erlang.system_info(:otp_release)}"
      ],
      docs: [main: "readme", extras: ["README.md"]]
    ]
  end
end
