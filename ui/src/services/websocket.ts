import { DashboardState, WebSocketMessage, RealtimeUpdates } from '@/types';

class WebSocketService {
  private ws: WebSocket | null = null;
  private reconnectAttempts = 0;
  private maxReconnectAttempts = 5;
  private reconnectDelay = 1000;
  private handlers: Partial<RealtimeUpdates> = {};

  connect(url: string = 'ws://localhost:5555') {
    try {
      this.ws = new WebSocket(url);
      
      this.ws.onopen = () => {
        console.log('WebSocket connected');
        this.reconnectAttempts = 0;
      };

      this.ws.onmessage = (event) => {
        try {
          const message: WebSocketMessage = JSON.parse(event.data);
          this.handleMessage(message);
        } catch (error) {
          console.error('Failed to parse WebSocket message:', error);
        }
      };

      this.ws.onclose = () => {
        console.log('WebSocket disconnected');
        this.attemptReconnect();
      };

      this.ws.onerror = (error) => {
        console.error('WebSocket error:', error);
        this.handlers.onError?.('WebSocket connection error');
      };

    } catch (error) {
      console.error('Failed to create WebSocket connection:', error);
      this.handlers.onError?.('Failed to connect to WebSocket');
    }
  }

  private handleMessage(message: WebSocketMessage) {
    switch (message.type) {
      case 'status':
        this.handlers.onStatusUpdate?.(message.payload as DashboardState);
        break;
      case 'dimension':
        this.handlers.onDimensionChange?.(message.payload.dimension);
        break;
      case 'action':
        this.handlers.onActionExecuted?.(message.payload.action, message.payload.result);
        break;
      case 'modification':
        this.handlers.onSelfModification?.(message.payload);
        break;
      case 'error':
        this.handlers.onError?.(message.payload);
        break;
    }
  }

  private attemptReconnect() {
    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.reconnectAttempts++;
      console.log(`Attempting to reconnect (${this.reconnectAttempts}/${this.maxReconnectAttempts})...`);
      
      setTimeout(() => {
        this.connect();
      }, this.reconnectDelay * this.reconnectAttempts);
    } else {
      console.error('Max reconnection attempts reached');
      this.handlers.onError?.('Failed to reconnect to WebSocket');
    }
  }

  // Register event handlers
  onUpdateHandlers(handlers: Partial<RealtimeUpdates>) {
    this.handlers = { ...this.handlers, ...handlers };
  }

  // Send messages to server
  sendAction(action: string, params?: any) {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify({
        type: 'action',
        payload: { action, params }
      }));
    }
  }

  sendCommand(command: string, params?: any) {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify({
        type: 'command',
        payload: { command, params }
      }));
    }
  }

  disconnect() {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  isConnected(): boolean {
    return this.ws?.readyState === WebSocket.OPEN;
  }
}

export const wsService = new WebSocketService();