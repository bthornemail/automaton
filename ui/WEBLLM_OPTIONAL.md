# WebLLM is Now Optional

## Overview

WebLLM has been made **optional** in favor of **OpenCode** as the default LLM provider. OpenCode provides the same functionality through the OpenCode plugin without requiring large model downloads.

## Changes Made

1. **Default Provider**: Changed from `'webllm'` to `'opencode'`
2. **Provider Order**: OpenCode is now shown first in provider selection UI
3. **Lazy Loading**: WebLLM only initializes when explicitly selected
4. **Error Handling**: All WebLLM imports are wrapped with error handling for missing package

## Benefits

- ✅ **No Large Downloads**: OpenCode uses API, no GB downloads needed
- ✅ **Faster Setup**: Works immediately without model downloads
- ✅ **More Flexible**: Can use any model via OpenRouter/OpenCode API
- ✅ **Simpler Architecture**: One primary LLM path through OpenCode plugin

## Using WebLLM (Optional)

If you want to use WebLLM for privacy (local inference):

1. Install the package: `npm install @mlc-ai/web-llm`
2. Go to Settings → LLM Provider → Select "WebLLM"
3. Choose a model (first-time download will take 5-10 minutes)

## Default Configuration

- **Provider**: `opencode`
- **Model**: `openai/gpt-4o-mini` (via OpenRouter)
- **Endpoint**: `https://openrouter.ai/api/v1`

## Migration Notes

- Existing users with WebLLM config will continue to work
- New users default to OpenCode
- WebLLM can still be selected in settings if needed


