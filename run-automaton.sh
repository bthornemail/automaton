#!/bin/bash

echo "🤖 Self-Referencing JSONL Automaton Launcher"
echo "=========================================="

# Check if Node.js is available
if ! command -v node &> /dev/null; then
    echo "❌ Node.js not found. Please install Node.js first."
    exit 1
fi

# Check if TypeScript/tsx is available
if ! command -v npx &> /dev/null; then
    echo "❌ npx not found. Please install Node.js with npm."
    exit 1
fi

# Parse arguments
USE_OLLAMA=false
MODEL="llama3.2"
INTERVAL=2000
MAX_ITERATIONS=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --ollama)
            USE_OLLAMA=true
            shift
            ;;
        --model)
            MODEL="$2"
            shift 2
            ;;
        --interval)
            INTERVAL="$2"
            shift 2
            ;;
        --max)
            MAX_ITERATIONS="$2"
            shift 2
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --ollama           Use Ollama for AI control"
            echo "  --model MODEL      Ollama model (default: llama3.2)"
            echo "  --interval MS      Interval in milliseconds (default: 2000)"
            echo "  --max N           Maximum iterations (default: unlimited)"
            echo "  -h, --help        Show this help"
            echo ""
            echo "Examples:"
            echo "  $0                           # Built-in AI, 2s intervals"
            echo "  $0 --ollama                  # Ollama with llama3.2"
            echo "  $0 --ollama --model qwen2.5:3b --interval 3000"
            echo "  $0 --max 50 --interval 1000"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use -h or --help for usage information"
            exit 1
            ;;
    esac
done

# Build command arguments
ARGS=()

if [ "$USE_OLLAMA" = true ]; then
    echo "🤖 Using Ollama with model: $MODEL"
    
    # Check if Ollama is available
    if ! command -v ollama &> /dev/null; then
        echo "❌ Ollama not found. Install with:"
        echo "   curl -fsSL https://ollama.ai/install.sh | sh"
        echo "   ollama pull $MODEL"
        exit 1
    fi
    
    # Check if model is available
    if ! ollama list | grep -q "$MODEL"; then
        echo "📦 Pulling Ollama model: $MODEL"
        ollama pull "$MODEL"
    fi
    
    ARGS[0]="ollama-automaton.ts"
    ARGS[1]="$MODEL"
else
    echo "🧠 Using built-in intelligence"
    ARGS[0]="continuous-automaton.ts"
fi

ARGS[1]="$INTERVAL"

if [ -n "$MAX_ITERATIONS" ]; then
    if [ "$USE_OLLAMA" = true ]; then
        ARGS[2]="$MAX_ITERATIONS"
    else
        ARGS[1]="$INTERVAL"
        ARGS[2]="--max"
        ARGS[3]="$MAX_ITERATIONS"
    fi
fi

echo "⚡ Starting automaton..."
echo "   Interval: ${INTERVAL}ms"
if [ -n "$MAX_ITERATIONS" ]; then
    echo "   Max iterations: $MAX_ITERATIONS"
else
    echo "   Max iterations: unlimited"
fi
echo ""

# Execute the automaton
npx tsx "${ARGS[@]}"