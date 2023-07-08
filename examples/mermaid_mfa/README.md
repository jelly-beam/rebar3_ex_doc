mermaid_mfa
=====

Example project to show how you can use rebar3_ex_doc and ex_doc to generate mermaid graphs in your markdown docs using
the `before_closing_head_tag` and an MFA. You should also check out the [mermaid_map example](../mermaid_map/) which
demonstrates how to use `before_closing_head_tag` and a map value instead of an mfa, which allows you to gain the same
functionality, without having to place doc related code in your src code.

```mermaid
sequenceDiagram
    actor Joe
    actor Mike
    actor Robert
    Mike->>Joe: ring
    Joe->>Mike: Hello Mike.
    Mike->>Joe: Hello Joe.
    Mike->>Robert: ring
    Robert->>Mike: Hello Mike.
    Mike->>Robert: Hello Robert.
    par Mike to Joe and and Mike to Robert and Joe to Robert and Joe to Mike and Robert to Joe and Robert to Mike
        Mike->>Joe: Hello Joe.
        Joe->>Mike: Hello Mike.
        Joe->>Robert: Hello Robert.
        Robert->>Joe: Hello Joe.
        Robert->>Mike: Hello Mike.
        par Mike to Joe
        Mike->>Joe: Hello, well it worked this time.
        and Mike to Robert
        Mike->>Robert: Hello, well it worked this time.
        end
    end
```
