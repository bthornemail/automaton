/**
 * ErrorHandler - Centralized error handling and recovery system
 * 
 * Provides structured error handling, recovery strategies, and user-friendly error messages
 */

export interface ErrorInfo {
  type: string;
  message: string;
  stack?: string | null;
  name?: string;
  code?: string;
  cause?: any;
  context?: any;
  timestamp?: string;
  id?: string;
}

export interface RecoveryContext {
  retry?: () => Promise<any>;
  fallback?: () => Promise<any>;
  [key: string]: any;
}

export interface RecoveryResult {
  error: ErrorInfo;
  recovered: boolean;
  recovery?: any;
  recoveryError?: ErrorInfo;
}

export type RecoveryStrategy = (errorInfo: ErrorInfo, context: RecoveryContext) => Promise<any>;

export class ErrorHandler {
  private errorListeners: Set<(errorInfo: ErrorInfo) => void | Promise<void>>;
  private recoveryStrategies: Map<string, RecoveryStrategy>;
  private errorHistory: ErrorInfo[];
  private maxHistorySize: number;

  constructor() {
    this.errorListeners = new Set();
    this.recoveryStrategies = new Map();
    this.errorHistory = [];
    this.maxHistorySize = 100;
  }

  /**
   * Register error listener
   */
  onError(listener: (errorInfo: ErrorInfo) => void | Promise<void>): void {
    this.errorListeners.add(listener);
  }

  /**
   * Remove error listener
   */
  offError(listener: (errorInfo: ErrorInfo) => void | Promise<void>): void {
    this.errorListeners.delete(listener);
  }

  /**
   * Register recovery strategy for error type
   */
  registerRecoveryStrategy(errorType: string, strategy: RecoveryStrategy): void {
    this.recoveryStrategies.set(errorType, strategy);
  }

  /**
   * Handle error with recovery
   */
  async handle(error: Error | string, context: RecoveryContext = {}): Promise<RecoveryResult> {
    const errorObj = this.normalizeError(error);
    const errorInfo: ErrorInfo = {
      ...errorObj,
      context,
      timestamp: new Date().toISOString(),
      id: this.generateErrorId()
    };

    // Add to history
    this.errorHistory.push(errorInfo);
    if (this.errorHistory.length > this.maxHistorySize) {
      this.errorHistory.shift();
    }

    // Notify listeners
    for (const listener of this.errorListeners) {
      try {
        await listener(errorInfo);
      } catch (listenerError) {
        console.error('Error listener failed:', listenerError);
      }
    }

    // Try recovery
    const recoveryStrategy = this.recoveryStrategies.get(errorInfo.type);
    if (recoveryStrategy) {
      try {
        const recovery = await recoveryStrategy(errorInfo, context);
        return {
          error: errorInfo,
          recovered: true,
          recovery
        };
      } catch (recoveryError: any) {
        return {
          error: errorInfo,
          recovered: false,
          recoveryError: this.normalizeError(recoveryError)
        };
      }
    }

    return {
      error: errorInfo,
      recovered: false
    };
  }

  /**
   * Normalize error to standard format
   */
  private normalizeError(error: Error | string): ErrorInfo {
    if (error instanceof Error) {
      return {
        type: this.classifyError(error),
        message: error.message,
        stack: error.stack,
        name: error.name,
        code: (error as any).code,
        cause: (error as any).cause
      };
    }

    return {
      type: 'unknown',
      message: String(error),
      stack: null,
      name: 'Error'
    };
  }

  /**
   * Classify error type
   */
  private classifyError(error: Error): string {
    const message = error.message.toLowerCase();
    const name = error.name.toLowerCase();

    // Network errors
    if (name.includes('network') || name.includes('fetch') || 
        message.includes('network') || message.includes('fetch') ||
        message.includes('timeout') || message.includes('connection')) {
      return 'network';
    }

    // Parse errors
    if (name.includes('parse') || name.includes('syntax') ||
        message.includes('parse') || message.includes('syntax') ||
        message.includes('json')) {
      return 'parse';
    }

    // Validation errors
    if (name.includes('validation') || name.includes('validate') ||
        message.includes('validation') || message.includes('invalid') ||
        message.includes('constraint')) {
      return 'validation';
    }

    // Permission errors
    if (name.includes('permission') || name.includes('access') ||
        message.includes('permission') || message.includes('access') ||
        message.includes('forbidden') || message.includes('unauthorized')) {
      return 'permission';
    }

    // Not found errors
    if (name.includes('notfound') || message.includes('not found') ||
        message.includes('404')) {
      return 'notfound';
    }

    // Rate limit errors
    if (message.includes('rate limit') || message.includes('429') ||
        message.includes('too many requests')) {
      return 'ratelimit';
    }

    return 'unknown';
  }

  /**
   * Generate unique error ID
   */
  private generateErrorId(): string {
    return `err_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }

  /**
   * Get user-friendly error message
   */
  getUserMessage(errorInfo: ErrorInfo): string {
    const messages: Record<string, string> = {
      network: 'Network connection failed. Please check your internet connection and try again.',
      parse: 'Failed to parse data. The file may be corrupted or in an unsupported format.',
      validation: 'Data validation failed. Please check your input and try again.',
      permission: 'Permission denied. You may not have access to this resource.',
      notfound: 'Resource not found. Please check the URL or file path.',
      ratelimit: 'Too many requests. Please wait a moment and try again.',
      unknown: 'An unexpected error occurred. Please try again later.'
    };

    return messages[errorInfo.type] || messages.unknown;
  }

  /**
   * Get error history
   */
  getHistory(limit: number = 10): ErrorInfo[] {
    return this.errorHistory.slice(-limit);
  }

  /**
   * Clear error history
   */
  clearHistory(): void {
    this.errorHistory = [];
  }

  /**
   * Get error statistics
   */
  getStatistics(): { total: number; byType: Record<string, number>; recent: ErrorInfo[] } {
    const stats = {
      total: this.errorHistory.length,
      byType: {} as Record<string, number>,
      recent: this.errorHistory.slice(-10)
    };

    for (const error of this.errorHistory) {
      stats.byType[error.type] = (stats.byType[error.type] || 0) + 1;
    }

    return stats;
  }
}

