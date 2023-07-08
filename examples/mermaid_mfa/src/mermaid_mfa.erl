-module(mermaid_mfa).

-export([render/2]).

render(html, "mermaid") ->
 "<script src=\"https://cdn.jsdelivr.net/npm/mermaid@10.2.4/dist/mermaid.min.js\"></script>";

render(_, _) -> "".

