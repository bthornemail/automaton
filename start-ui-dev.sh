#!/bin/bash

# Automaton UI Development Server Launcher

set -e

echo "🚀 Starting Automaton UI Development Environment"
echo "================================================"

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed. Please install Node.js 18+ first."
    exit 1
fi

# Check Node.js version
NODE_VERSION=$(node -v | cut -d'v' -f2 | cut -d'.' -f1)
if [ "$NODE_VERSION" -lt 18 ]; then
    echo "❌ Node.js version 18+ is required. Current version: $(node -v)"
    exit 1
fi

# Check if we're in the right directory
if [ ! -f "automaton.jsonl" ]; then
    echo "❌ automaton.jsonl not found. Please run from the automaton directory."
    exit 1
fi

# Install root dependencies if needed
if [ ! -d "node_modules" ]; then
    echo "📦 Installing root dependencies..."
    npm install
fi

# Check for ts-node-dev (preferred for auto-reload) or fallback to tsx
if [ -d "node_modules/ts-node-dev" ] || npm list ts-node-dev &>/dev/null; then
    BACKEND_CMD="ts-node-dev --respawn --transpile-only ui-server.ts"
elif [ -d "node_modules/tsx" ] || npm list tsx &>/dev/null; then
    BACKEND_CMD="tsx ui-server.ts"
    echo "⚠️  Using tsx (no auto-reload). Install ts-node-dev for auto-reload: npm install -D ts-node-dev"
else
    echo "📦 Installing backend dependencies..."
    npm install -D ts-node-dev
    BACKEND_CMD="ts-node-dev --respawn --transpile-only ui-server.ts"
fi

# Install UI dependencies if needed
if [ ! -d "ui/node_modules" ]; then
    echo "📦 Installing UI dependencies..."
    cd ui
    npm install
    cd ..
fi

# Check if ports are already in use
check_port() {
    if lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo "⚠️  Port $1 is already in use. Please stop the process using it first."
        return 1
    fi
    return 0
}

echo "🔍 Checking ports..."
if ! check_port 3000; then
    echo "   Backend port 3000 is in use"
    exit 1
fi
if ! check_port 5173; then
    echo "   UI port 5173 is in use"
    exit 1
fi

# Start the backend server
echo "🔌 Starting backend server on port 3000..."
$BACKEND_CMD > /tmp/automaton-backend.log 2>&1 &
BACKEND_PID=$!

# Wait for backend to be ready
echo "⏳ Waiting for backend to start..."
for i in {1..30}; do
    if curl -s http://localhost:3000/api/health > /dev/null 2>&1; then
        echo "✅ Backend is ready"
        break
    fi
    if [ $i -eq 30 ]; then
        echo "⚠️  Backend did not start in time, but continuing..."
    fi
    sleep 1
done

# Start the UI development server
echo "🎨 Starting UI development server on port 5173..."
cd ui
npm run dev > /tmp/automaton-ui.log 2>&1 &
UI_PID=$!
cd ..

# Wait a moment for UI to start
sleep 2

echo ""
echo "✅ Development environment started!"
echo ""
echo "📊 Service URLs:"
echo "   🎨 UI (Vite): http://localhost:5173"
echo "   🔌 Backend API: http://localhost:3000"
echo "   📡 WebSocket: ws://localhost:3000/socket.io"
echo ""
echo "📝 Logs:"
echo "   Backend: tail -f /tmp/automaton-backend.log"
echo "   UI: tail -f /tmp/automaton-ui.log"
echo ""
echo "Press Ctrl+C to stop both servers"

# Function to cleanup on exit
cleanup() {
    echo ""
    echo "🛑 Stopping servers..."
    kill $BACKEND_PID 2>/dev/null || true
    kill $UI_PID 2>/dev/null || true
    # Wait for processes to terminate
    sleep 1
    # Force kill if still running
    kill -9 $BACKEND_PID 2>/dev/null || true
    kill -9 $UI_PID 2>/dev/null || true
    echo "✅ Servers stopped"
    exit 0
}

# Set up signal handlers
trap cleanup SIGINT SIGTERM

# Wait for both processes
wait