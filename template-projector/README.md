# CanvasL Semantic Slides Project

**"Living Knowledge Decks" — Browser-Native, RDF-Annotated, DBpedia-Powered, Extensible**

## Overview

The CanvasL Semantic Slides Project is a fully browser-native, offline-capable presentation system where slides are not static — they are **semantic graphs** built from CanvasL macros, enriched in real-time with DBpedia (and linked datasets), validated with SHACL, reasoned over with OWL, and annotated with RDF* for full provenance.

## Key Features

- **Browser-Native**: Full semantic reasoning stack runs in browser without backend
- **RDF-Annotated**: Every slide is a queryable RDF graph with RDF* provenance
- **DBpedia-Powered**: Live Wikipedia knowledge enrichment via SPARQL federation
- **Extensible**: Plugin architecture enables community contributions
- **Agent-Protected**: Multi-agent system ensures user consent before federating private data

## Quick Start

```bash
# Install dependencies
npm install

# Start development server
npm run dev

# Build for production
npm run build
```

## Project Structure

```
template-projector/
├── src/
│   ├── plugin/              # Plugin system
│   ├── projector/           # Core projector engine
│   └── macros/              # CanvasL macro definitions
├── templates/
│   ├── slides/              # Slide templates
│   ├── cards/               # Card components
│   └── documents/           # Document templates
├── assets/                  # Static assets
├── js/                      # JavaScript utilities
├── css/                     # Stylesheets
└── viewer.html              # Main viewer application
```

## Documentation

- **Project Specification**: `docs/26-CanvasL-Semantic-Slides-Project/01-CanvasL Semantic Slides Project.md`
- **Federated Knowledge Model**: `docs/26-CanvasL-Semantic-Slides-Project/02-Public-Private Integration with Agent Protection.md`
- **Technical Foundation**: `docs/25-Church-Encoding-Metaverse-Presentation/`

## Status

**Current Phase**: Initial Implementation
**Last Updated**: 2025-01-07

See `docs/26-CanvasL-Semantic-Slides-Project/03-STATUS.md` for detailed progress tracking.
