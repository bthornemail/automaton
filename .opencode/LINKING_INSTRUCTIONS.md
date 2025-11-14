# NPM Linking Instructions for Meta-Log Plugin

## Quick Setup

Run these commands to set up npm linking:

```bash
# 1. Build the plugin
cd plugin/meta-log-plugin
npm install
npm run build:opencode

# 2. Link to OpenCode
npm link
cd ../../.opencode
npm link meta-log-plugin

# 3. Link to Universal Life Protocol Plugin
cd ../.obsidian/plugins/universal-life-protocol-plugin
npm link meta-log-plugin
```

## Verification

After linking, verify the setup:

```bash
# Check OpenCode link
cd .opencode
npm list meta-log-plugin

# Check Universal Life Protocol Plugin link
cd ../.obsidian/plugins/universal-life-protocol-plugin
npm list meta-log-plugin
```

Both should show the linked path to `plugin/meta-log-plugin`.

## Unlinking

To unlink (if needed):

```bash
# From .opencode
cd .opencode
npm unlink meta-log-plugin

# From universal-life-protocol-plugin
cd .obsidian/plugins/universal-life-protocol-plugin
npm unlink meta-log-plugin

# From plugin/meta-log-plugin
cd plugin/meta-log-plugin
npm unlink
```

