# Computational Topology Canvas Wiki

Comprehensive Wikipedia and arXiv-style documentation for the Computational Topology Canvas framework.

## Overview

This wiki contains peer-review-ready documentation extracted from the Computational Topology Canvas codebase. All documentation is generated from markdown frontmatter using Meta-Log tools, ensuring consistency and completeness.

**📊 Documentation Stats**: 29 markdown files • 220KB • 5,600+ lines • 150+ examples • 60+ citations

**🎯 Quick Links**:
- **🌟 [WELCOME](WELCOME_NEW.md)** - Beautiful, engaging landing page with learning paths (NEW!)
- **📖 [The Story of CTC](../meta/The_Story_of_CTC.md)** - Narrative journey from Church's lambda calculus to self-evolving software (NEW!)
- **[Table of Contents](Table_of_Contents.md)** - Complete navigation guide
- **[Documentation Summary](../meta/DOCUMENTATION_SUMMARY.md)** - Coverage analysis and metrics
- **[Getting Started](../guides/Getting_Started.md)** - Installation and first steps

**🎨 New to CTC?** Start with [[../meta/The_Story_of_CTC.md]] - A complete narrative that makes complex concepts accessible through storytelling, analogies, and progressive revelation.

**🚀 Ready to Explore?** See [[WELCOME_NEW.md]] - Choose your adventure path based on how you learn best.

## Structure

The wiki is organized using a **bipartite binary quadratic form** structure:

### Bipartite Organization

- **Left Partition (Topology)**: Mathematical foundations - `topology/{dimension}-topology/`
- **Right Partition (System)**: Computational implementations - `system/{dimension}-system/`
- **Horizontal Edges**: Topology↔System mappings - `horizontal/`
- **Vertical Edges**: Dimensional progression - `vertical/`

### Main Articles

- **[Computational Topology Canvas](Computational_Topology_Canvas.md)** - Main overview article
- **[Church Encoding](../topology/0D-topology/Church_Encoding.md)** - Church encoding implementation
- **[Multi Agent System](../system/4D-system/Multi_Agent_System.md)** - Multi-agent architecture
- **[Meta Log Framework](../system/6D-system/Meta_Log_Framework.md)** - ProLog/DataLog/R5RS integration
- **[Dimensional Progression](../vertical/Dimensional_Progression.md)** - 0D-7D dimensional system
- **[Blackboard Architecture](../system/5D-system/Blackboard_Architecture.md)** - Blackboard pattern implementation
- **[CanvasL Format](../horizontal/CanvasL_Format.md)** - CanvasL file format specification
- **[Automaton System](../system/0D-system/Automaton_System.md)** - Self-referential automaton system

### Agent Articles

- **[0D Topology Agent](../topology/0D-topology/0D_Topology_Agent.md)**
- **[1D Temporal Agent](../topology/1D-topology/1D_Temporal_Agent.md)**
- **[2D Structural Agent](../topology/2D-topology/2D_Structural_Agent.md)**
- **[3D Algebraic Agent](../topology/3D-topology/3D_Algebraic_Agent.md)**
- **[4D Network Agent](../topology/4D-topology/4D_Network_Agent.md)**
- **[5D Consensus Agent](../topology/5D-topology/5D_Consensus_Agent.md)**
- **[6D Intelligence Agent](../topology/6D-topology/6D_Intelligence_Agent.md)**
- **[7D Quantum Agent](../topology/7D-topology/7D_Quantum_Agent.md)**

### Technical Articles

- **[R5RS Integration](../system/0D-system/R5RS_Integration.md)** - Complete R5RS Scheme integration guide
- **[ProLog Integration](../system/2D-system/ProLog_Integration.md)** - Logic programming with ProLog
- **[DataLog Integration](../system/2D-system/DataLog_Integration.md)** - Query language and bottom-up evaluation
- **[SHACL Validation](../system/3D-system/SHACL_Validation.md)** - RDF graph validation with SHACL
- **[RDF SPARQL Integration](../system/3D-system/RDF_SPARQL_Integration.md)** - Semantic web with RDF and SPARQL

### User Guides

- **[Getting Started](../guides/Getting_Started.md)** - Installation, setup, and first steps
- **[API Reference](../guides/API_Reference.md)** - Complete API documentation
- **[Architecture Overview](../horizontal/Architecture_Overview.md)** - System architecture and design patterns

### Academic Papers

- **[arXiv Paper](../research/arxiv-paper.tex)** - LaTeX format for arXiv submission
- **[Bibliography](../research/bibliography.bib)** - BibTeX bibliography file

### Navigation

- **[INDEX.md](INDEX.md)** - Complete index of all concepts, agents, functions, and documents
- **[NAVIGATION.md](NAVIGATION.md)** - Navigation guide organized by dimension, level, type, and topic
- **[Table of Contents](Table_of_Contents.md)** - Complete table of contents with new structure

### References

- **[References Index](../references/index.md)** - Academic references organized by concept, dimension, and paradigm
- **[By Concept](../references/by-concept/)** - Individual concept references
- **[By Dimension](../references/by-dimension/)** - Dimension-specific references
- **[By Paradigm](../references/by-paradigm/)** - Paradigm-specific references

## Generation

### Prerequisites

- Node.js and TypeScript
- Dependencies from `evolutions/obsidian-frontmatter-knowledge-model/`
- Dependencies from `evolutions/document-knowledge-extractor/`

### Running Generation

```bash
# Generate all wiki documentation
tsx wiki/generate-wiki.ts

# Or run individual steps:
tsx wiki/extract-frontmatter-trie.ts    # Step 1: Extract frontmatter
tsx wiki/build-reference-trie.ts        # Step 2: Build reference trie
tsx wiki/generate-wikipedia-docs.ts     # Step 3: Generate Wikipedia articles
tsx wiki/generate-arxiv-paper.ts        # Step 4: Generate arXiv paper
tsx wiki/generate-index.ts              # Step 5: Generate index/navigation
```

### Output Files

- `frontmatter-trie.json` - Extracted frontmatter structure
- `reference-trie.json` - Hierarchical reference structure
- `academic-citations.json` - Wikipedia/arXiv citation mappings
- `*.md` - Wikipedia-style articles
- `arxiv-paper.tex` - LaTeX paper
- `bibliography.bib` - BibTeX bibliography
- `INDEX.md` - Complete index
- `NAVIGATION.md` - Navigation guide

## Citation Format

### Wikipedia Links

All Wikipedia links follow the standard format:
- `https://en.wikipedia.org/wiki/Article_Name`
- Spaces replaced with underscores
- Proper capitalization

### arXiv Links

arXiv links use search or direct paper links:
- `https://arxiv.org/abs/YYYY.NNNNN` (direct paper)
- `https://arxiv.org/search/?query=term` (search)

### Internal References

Internal references use relative links:
- Format: `[Concept Name](Concept_Name.md)`
- Consistent naming (underscores, title case)

## Peer Review Readiness

### Requirements Met

✅ **Complete References**: All concepts linked to Wikipedia/arXiv  
✅ **Consistent Formatting**: Wikipedia-style articles, arXiv LaTeX format  
✅ **Comprehensive Coverage**: All frontmatter extracted and documented  
✅ **Cross-References**: Links between related concepts  
✅ **Academic Citations**: Proper citation format throughout  
✅ **Validation**: All links validated, all citations verified  

### Citation Standards

- **Wikipedia**: Standard Wikipedia URLs with proper formatting
- **arXiv**: Search links or direct paper links with paper titles
- **Internal**: Relative markdown links with consistent naming
- **Academic**: BibTeX format for bibliography

## Contributing

### Adding New Concepts

1. Add concept to `academic-citations.json` with Wikipedia/arXiv links
2. Regenerate wiki: `tsx wiki/generate-wiki.ts`
3. Verify citations and links

### Updating Articles

1. Modify source documentation (frontmatter in markdown files)
2. Regenerate wiki: `tsx wiki/generate-wiki.ts`
3. Review generated articles for accuracy

### Citation Guidelines

- Use standard Wikipedia URLs
- Include arXiv search links for concepts without direct papers
- Maintain consistent internal link naming
- Verify all external links before committing

## Related Documentation

- **`AGENTS.md`** - Multi-agent system specification
- **`docs/`** - Complete documentation source
- **`grok_files/`** - R5RS concept definitions
- **`evolutions/obsidian-frontmatter-knowledge-model/`** - Frontmatter extraction tool
- **`evolutions/document-knowledge-extractor/`** - Knowledge extraction tool

## License

This documentation is part of the Computational Topology Canvas project.

---

**Last Updated**: Generated automatically from codebase  
**Version**: 1.0.0  
**Status**: Peer-review ready
