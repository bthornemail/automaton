/**
 * Error Logging Service
 * 
 * Custom error logging and monitoring service with support for custom handlers
 */

export interface ErrorContext {
  component?: string;
  service?: string;
  action?: string;
  metadata?: Record<string, any>;
  severity?: 'error' | 'warning' | 'info';
  userActions?: string[];
  browserInfo?: {
    userAgent?: string;
    platform?: string;
    language?: string;
  };
}

export interface ErrorLogEntry {
  id: string;
  error: Error;
  context: ErrorContext;
  timestamp: string;
  stack?: string;
}

export interface ErrorStats {
  totalErrors: number;
  errorsByType: Record<string, number>;
  errorsByComponent: Record<string, number>;
  errorsByService: Record<string, number>;
  recentErrors: ErrorLogEntry[];
}

export interface ErrorLoggingConfig {
  enableConsoleLogging?: boolean;
  enableCustomTracking?: boolean;
  customHandler?: (error: Error, context: ErrorContext) => void;
  maxHistorySize?: number;
}

export class ErrorLoggingService {
  private errorHistory: ErrorLogEntry[] = [];
  private customHandlers: Set<(error: Error, context: ErrorContext) => void> = new Set();
  private config: Required<ErrorLoggingConfig>;

  constructor(config: ErrorLoggingConfig = {}) {
    this.config = {
      enableConsoleLogging: config.enableConsoleLogging !== false,
      enableCustomTracking: config.enableCustomTracking || false,
      maxHistorySize: config.maxHistorySize || 100,
      customHandler: config.customHandler || (() => {})
    };

    // Register initial custom handler if provided
    if (config.customHandler) {
      this.registerCustomHandler(config.customHandler);
    }

    // Check for environment variable configuration
    const envService = import.meta.env.VITE_ERROR_TRACKING_SERVICE;
    if (envService && envService !== 'console') {
      this.config.enableCustomTracking = true;
    }
  }

  /**
   * Log an error with context
   */
  logError(error: Error, context: ErrorContext = {}): void {
    const entry: ErrorLogEntry = {
      id: this.generateErrorId(),
      error,
      context: {
        ...context,
        browserInfo: context.browserInfo || this.getBrowserInfo(),
        severity: context.severity || 'error',
        timestamp: new Date().toISOString()
      },
      timestamp: new Date().toISOString(),
      stack: error.stack
    };

    // Add to history
    this.errorHistory.push(entry);
    if (this.errorHistory.length > this.config.maxHistorySize) {
      this.errorHistory.shift();
    }

    // Console logging
    if (this.config.enableConsoleLogging) {
      this.logToConsole(entry);
    }

    // Custom handlers
    if (this.config.enableCustomTracking || this.customHandlers.size > 0) {
      for (const handler of this.customHandlers) {
        try {
          handler(error, entry.context);
        } catch (handlerError) {
          console.error('Error in custom error handler:', handlerError);
        }
      }
    }
  }

  /**
   * Get error statistics
   */
  getErrorStats(): ErrorStats {
    const errorsByType: Record<string, number> = {};
    const errorsByComponent: Record<string, number> = {};
    const errorsByService: Record<string, number> = {};

    for (const entry of this.errorHistory) {
      // Count by error type
      const errorType = entry.error.name || 'UnknownError';
      errorsByType[errorType] = (errorsByType[errorType] || 0) + 1;

      // Count by component
      if (entry.context.component) {
        errorsByComponent[entry.context.component] =
          (errorsByComponent[entry.context.component] || 0) + 1;
      }

      // Count by service
      if (entry.context.service) {
        errorsByService[entry.context.service] =
          (errorsByService[entry.context.service] || 0) + 1;
      }
    }

    return {
      totalErrors: this.errorHistory.length,
      errorsByType,
      errorsByComponent,
      errorsByService,
      recentErrors: this.errorHistory.slice(-10) // Last 10 errors
    };
  }

  /**
   * Clear error history
   */
  clearErrorHistory(): void {
    this.errorHistory = [];
  }

  /**
   * Register custom error handler
   */
  registerCustomHandler(handler: (error: Error, context: ErrorContext) => void): void {
    this.customHandlers.add(handler);
  }

  /**
   * Remove custom error handler
   */
  unregisterCustomHandler(handler: (error: Error, context: ErrorContext) => void): void {
    this.customHandlers.delete(handler);
  }

  /**
   * Log to console with formatting
   */
  private logToConsole(entry: ErrorLogEntry): void {
    const { error, context } = entry;
    const prefix = `[${context.severity?.toUpperCase() || 'ERROR'}]`;
    const component = context.component ? `[${context.component}]` : '';
    const service = context.service ? `[${context.service}]` : '';
    const action = context.action ? `[${context.action}]` : '';

    console.group(`${prefix} ${component} ${service} ${action}`.trim());
    console.error('Error:', error.message);
    console.error('Type:', error.name);
    if (error.stack) {
      console.error('Stack:', error.stack);
    }
    if (context.metadata) {
      console.error('Metadata:', context.metadata);
    }
    if (context.userActions && context.userActions.length > 0) {
      console.error('User Actions:', context.userActions);
    }
    console.groupEnd();

    // Log error patterns in development mode
    if (import.meta.env.DEV || import.meta.env.MODE === 'development') {
      const stats = this.getErrorStats();
      if (stats.totalErrors % 10 === 0) {
        console.log('Error Statistics:', stats);
      }
    }
  }

  /**
   * Get browser information
   */
  private getBrowserInfo(): ErrorContext['browserInfo'] {
    if (typeof window === 'undefined') {
      return {};
    }

    return {
      userAgent: navigator.userAgent,
      platform: navigator.platform,
      language: navigator.language
    };
  }

  /**
   * Generate unique error ID
   */
  private generateErrorId(): string {
    return `error-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  }
}

// Export singleton instance
export const errorLoggingService = new ErrorLoggingService({
  enableConsoleLogging: true,
  maxHistorySize: 100
});

