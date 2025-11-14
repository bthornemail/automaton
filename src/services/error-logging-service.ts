/**
 * Error Logging Service (Backend)
 * 
 * Simple error logging service for backend use
 */

export interface ErrorContext {
  component?: string;
  service?: string;
  action?: string;
  metadata?: Record<string, any>;
  severity?: 'error' | 'warning' | 'info';
  userActions?: string[];
}

export interface ErrorLogEntry {
  id: string;
  error: Error;
  context: ErrorContext;
  timestamp: string;
  stack?: string;
}

class ErrorLoggingService {
  private history: ErrorLogEntry[] = [];
  private maxHistorySize: number;

  constructor(options: { maxHistorySize?: number } = {}) {
    this.maxHistorySize = options.maxHistorySize || 100;
  }

  logError(error: Error, context: ErrorContext = {}): void {
    const entry: ErrorLogEntry = {
      id: `error-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
      error,
      context,
      timestamp: new Date().toISOString(),
      stack: error.stack,
    };

    // Add to history
    this.history.push(entry);
    if (this.history.length > this.maxHistorySize) {
      this.history.shift();
    }

    // Log to console
    const severity = context.severity || 'error';
    const prefix = `[${severity.toUpperCase()}]`;
    const service = context.service || 'Unknown';
    const action = context.action || 'Unknown';
    
    console.error(`${prefix} ${service}.${action}:`, error.message);
    if (context.metadata) {
      console.error('  Metadata:', context.metadata);
    }
    if (error.stack) {
      console.error('  Stack:', error.stack);
    }
  }

  getHistory(): ErrorLogEntry[] {
    return [...this.history];
  }

  clearHistory(): void {
    this.history = [];
  }
}

// Export singleton instance
export const errorLoggingService = new ErrorLoggingService({
  maxHistorySize: 100
});


