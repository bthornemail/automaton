# CanvasL Format

== Overview ==
CanvasL is an extended JSONL format that adds directives, R5RS function calls, dimension references, and Scheme expressions to standard JSONL.

Key features:
- Directives: @version, @schema for metadata
- R5RS function calls: {"type": "r5rs-call", "function": "r5rs:church-add"}
- Dimension references: {"dimension": "0D"}
- Node references: {"fromNode": "#0D-topology"}
- Scheme expressions: {"expression": "(church-add 2 3)"}

CanvasL enables self-referential canvas files that can execute operations and reference other dimensions.

__TOC__

== Details ==
[Content to be expanded]

== References ==
{{reflist}}

{{cite web | url=https://en.wikipedia.org/wiki/JSON | title=Reference 1}}

== External Links ==
* [JSON](https://en.wikipedia.org/wiki/JSON)

== See Also ==
* [[R5RS Integration]]
* [[Meta Log Framework]]

