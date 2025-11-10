# Computational Topology Canvas Wiki

Comprehensive Wikipedia and arXiv-style documentation for the Computational Topology Canvas framework.

## Overview

This wiki contains peer-review-ready documentation extracted from the Computational Topology Canvas codebase. All documentation is generated from markdown frontmatter using Meta-Log tools, ensuring consistency and completeness.

**📊 Documentation Stats**: 29 markdown files • 220KB • 5,600+ lines • 150+ examples • 60+ citations

**🎯 Quick Links**:
- **🌟 [WELCOME](WELCOME_NEW.md)** - Beautiful, engaging landing page with learning paths (NEW!)
- **📖 [The Story of CTC](The_Story_of_CTC.md)** - Narrative journey from Church's lambda calculus to self-evolving software (NEW!)
- **[Table of Contents](Table_of_Contents.md)** - Complete navigation guide
- **[Documentation Summary](DOCUMENTATION_SUMMARY.md)** - Coverage analysis and metrics
- **[Getting Started](Getting_Started.md)** - Installation and first steps

**🎨 New to CTC?** Start with [[The_Story_of_CTC]] - A complete narrative that makes complex concepts accessible through storytelling, analogies, and progressive revelation.

**🚀 Ready to Explore?** See [[WELCOME_NEW]] - Choose your adventure path based on how you learn best.

## Structure

### Main Articles

- **[Computational Topology Canvas](Computational_Topology_Canvas.md)** - Main overview article
- **[Church Encoding](Church_Encoding.md)** - Church encoding implementation
- **[Multi Agent System](Multi_Agent_System.md)** - Multi-agent architecture
- **[Meta Log Framework](Meta_Log_Framework.md)** - ProLog/DataLog/R5RS integration
- **[Dimensional Progression](Dimensional_Progression.md)** - 0D-7D dimensional system
- **[Blackboard Architecture](Blackboard_Architecture.md)** - Blackboard pattern implementation
- **[CanvasL Format](CanvasL_Format.md)** - CanvasL file format specification
- **[Automaton System](Automaton_System.md)** - Self-referential automaton system

### Agent Articles

- **[0D Topology Agent](0D_Topology_Agent.md)**
- **[1D Temporal Agent](1D_Temporal_Agent.md)**
- **[2D Structural Agent](2D_Structural_Agent.md)**
- **[3D Algebraic Agent](3D_Algebraic_Agent.md)**
- **[4D Network Agent](4D_Network_Agent.md)**
- **[5D Consensus Agent](5D_Consensus_Agent.md)**
- **[6D Intelligence Agent](6D_Intelligence_Agent.md)**
- **[7D Quantum Agent](7D_Quantum_Agent.md)**

### Technical Articles

- **[R5RS Integration](R5RS_Integration.md)** - Complete R5RS Scheme integration guide
- **[ProLog Integration](ProLog_Integration.md)** - Logic programming with ProLog
- **[DataLog Integration](DataLog_Integration.md)** - Query language and bottom-up evaluation
- **[SHACL Validation](SHACL_Validation.md)** - RDF graph validation with SHACL
- **[RDF SPARQL Integration](RDF_SPARQL_Integration.md)** - Semantic web with RDF and SPARQL

### User Guides

- **[Getting Started](Getting_Started.md)** - Installation, setup, and first steps
- **[API Reference](API_Reference.md)** - Complete API documentation
- **[Architecture Overview](Architecture_Overview.md)** - System architecture and design patterns

### Academic Papers

- **[arXiv Paper](arxiv-paper.tex)** - LaTeX format for arXiv submission
- **[Bibliography](bibliography.bib)** - BibTeX bibliography file

### Navigation

- **[INDEX.md](INDEX.md)** - Complete index of all concepts, agents, functions, and documents
- **[NAVIGATION.md](NAVIGATION.md)** - Navigation guide organized by dimension, level, type, and topic

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
