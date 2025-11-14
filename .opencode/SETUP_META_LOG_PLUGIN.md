# Meta-Log Plugin Setup Guide

This guide explains how to set up the `meta-log-plugin` package for use with OpenCode and the universal-life-protocol-plugin.

## Overview

The `meta-log-plugin` package provides:
- Autonomous CanvasL operations (self-regeneration, self-modification, goal negotiation, consensus, autonomous evolution)
- Geometric operations (BQF encoding, polyhedra transformations, computational mapping)
- ProLog, DataLog, and SPARQL query capabilities
- OpenCode tool integration

## Setup Steps

### 1. Build the Meta-Log Plugin

```bash
cd plugin/meta-log-plugin
npm install
npm run build:opencode
```

This creates the OpenCode-compatible build in `dist/opencode.js`.

### 2. Link the Plugin to OpenCode

```bash
# From plugin/meta-log-plugin directory
npm link

# From .opencode directory
cd ../../.opencode
npm link meta-log-plugin
```

### 3. Link the Plugin to Universal Life Protocol Plugin

```bash
# From plugin/meta-log-plugin directory (if not already linked)
cd plugin/meta-log-plugin
npm link

# From .obsidian/plugins/universal-life-protocol-plugin directory
cd .obsidian/plugins/universal-life-protocol-plugin
npm link meta-log-plugin
```

### 4. Verify Installation

```bash
# Check that the plugin is linked
cd .opencode
npm list meta-log-plugin

# Should show: meta-log-plugin@<version> -> /path/to/plugin/meta-log-plugin
```

## Usage

Once linked, the tools from `meta-log-plugin` will be available in OpenCode:

- **Autonomous Operations Tools**:
  - `regenerate` - Regenerate kernel from seed
  - `selfModify` - Self-modify CanvasL files
  - `goalNegotiate` - Negotiate goals with agents
  - `consensus` - Execute consensus votes
  - `autonomousEvolve` - Autonomous evolution

- **Geometric Operations Tools**:
  - `bqfEncode` - Encode polyhedra as BQF
  - `polyhedraTransform` - Transform polyhedra
  - `computeMapping` - Map R5RS types to polyhedra
  - `geometricValidate` - Validate geometric structures

## Troubleshooting

### Plugin Not Found

If you get "Meta-Log plugin not available" errors:

1. **Check build**: Ensure `npm run build:opencode` completed successfully
2. **Check linking**: Run `npm list meta-log-plugin` to verify link
3. **Rebuild**: Try rebuilding and relinking:
   ```bash
   cd plugin/meta-log-plugin
   npm run build:opencode
   npm link
   cd ../../.opencode
   npm link meta-log-plugin
   ```

### Import Errors

If you get import errors:

1. **Check paths**: Ensure the plugin is built in `dist/opencode.js`
2. **Check exports**: Verify `OpenCodeMetaLogPlugin` is exported from the build
3. **Clear cache**: Try clearing node_modules and reinstalling:
   ```bash
   cd .opencode
   rm -rf node_modules
   npm install
   npm link meta-log-plugin
   ```

## Related Documentation

- `plugin/meta-log-plugin/README.md` - Plugin documentation
- `docs/33-Autonomous-CanvasL/` - Autonomous CanvasL specifications
- `docs/32-Regulay-Polyhedra-Geometry/` - Geometric operations specifications

