/**
 * ErrorHandler - Centralized error handling and recovery system
 * 
 * Provides structured error handling, recovery strategies, and user-friendly error messages
 */

export class ErrorHandler {
  constructor() {
    this.errorListeners = new Set();
    this.recoveryStrategies = new Map();
    this.errorHistory = [];
    this.maxHistorySize = 100;
  }

  /**
   * Register error listener
   * @param {Function} listener - Error listener function
   */
  onError(listener) {
    this.errorListeners.add(listener);
  }

  /**
   * Remove error listener
   * @param {Function} listener - Error listener function
   */
  offError(listener) {
    this.errorListeners.delete(listener);
  }

  /**
   * Register recovery strategy for error type
   * @param {string} errorType - Error type (e.g., 'network', 'parse', 'validation')
   * @param {Function} strategy - Recovery function
   */
  registerRecoveryStrategy(errorType, strategy) {
    this.recoveryStrategies.set(errorType, strategy);
  }

  /**
   * Handle error with recovery
   * @param {Error|string} error - Error object or message
   * @param {Object} context - Error context
   * @returns {Promise<Object>} Recovery result
   */
  async handle(error, context = {}) {
    const errorObj = this.normalizeError(error);
    const errorInfo = {
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
      } catch (recoveryError) {
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
   * @param {Error|string} error - Error object or message
   * @returns {Object} Normalized error
   */
  normalizeError(error) {
    if (error instanceof Error) {
      return {
        type: this.classifyError(error),
        message: error.message,
        stack: error.stack,
        name: error.name,
        code: error.code,
        cause: error.cause
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
   * @param {Error} error - Error object
   * @returns {string} Error type
   */
  classifyError(error) {
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
   * @returns {string} Error ID
   */
  generateErrorId() {
    return `err_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  }

  /**
   * Get user-friendly error message
   * @param {Object} errorInfo - Error info object
   * @returns {string} User-friendly message
   */
  getUserMessage(errorInfo) {
    const messages = {
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
   * @param {number} limit - Maximum number of errors to return
   * @returns {Array} Error history
   */
  getHistory(limit = 10) {
    return this.errorHistory.slice(-limit);
  }

  /**
   * Clear error history
   */
  clearHistory() {
    this.errorHistory = [];
  }

  /**
   * Get error statistics
   * @returns {Object} Error statistics
   */
  getStatistics() {
    const stats = {
      total: this.errorHistory.length,
      byType: {},
      recent: this.errorHistory.slice(-10)
    };

    for (const error of this.errorHistory) {
      stats.byType[error.type] = (stats.byType[error.type] || 0) + 1;
    }

    return stats;
  }
}

// Default recovery strategies
ErrorHandler.defaultRecoveryStrategies = {
  network: async (errorInfo, context) => {
    // Retry with exponential backoff
    const maxRetries = 3;
    const baseDelay = 1000;

    for (let i = 0; i < maxRetries; i++) {
      await new Promise(resolve => setTimeout(resolve, baseDelay * Math.pow(2, i)));
      
      try {
        // Retry the operation if context provides retry function
        if (context.retry) {
          return await context.retry();
        }
      } catch (retryError) {
        if (i === maxRetries - 1) {
          throw retryError;
        }
      }
    }

    throw new Error('Network recovery failed after retries');
  },

  ratelimit: async (errorInfo, context) => {
    // Wait and retry
    const delay = 5000; // 5 seconds
    await new Promise(resolve => setTimeout(resolve, delay));
    
    if (context.retry) {
      return await context.retry();
    }

    throw new Error('Rate limit recovery failed');
  },

  parse: async (errorInfo, context) => {
    // Try alternative parser or fallback
    if (context.fallback) {
      return await context.fallback();
    }

    throw new Error('Parse recovery failed - no fallback available');
  }
};
