import React, { Component, ErrorInfo, ReactNode } from 'react';
import { AlertCircle, Bug } from 'lucide-react';
import { Card } from './Card';
import { Button } from './Button';
import { formatUserErrorMessage } from '../../utils/error-handling';

interface Props {
  children: ReactNode;
  fallback?: ReactNode;
}

interface State {
  hasError: boolean;
  error: Error | null;
  errorInfo: ErrorInfo | null;
}

export class ErrorBoundary extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      hasError: false,
      error: null,
      errorInfo: null,
    };
  }

  static getDerivedStateFromError(error: Error): State {
    return {
      hasError: true,
      error,
      errorInfo: null,
    };
  }

  componentDidCatch(error: Error, errorInfo: ErrorInfo) {
    console.error('ErrorBoundary caught an error:', error, errorInfo);
    this.setState({
      error,
      errorInfo,
    });

    // Log error to error tracking service
    if (typeof window !== 'undefined' && (window as any).errorLoggingService) {
      (window as any).errorLoggingService.logError(error, {
        component: 'ErrorBoundary',
        action: 'componentDidCatch',
        severity: 'error',
        metadata: {
          componentStack: errorInfo.componentStack,
          timestamp: new Date().toISOString()
        }
      });
    }
  }

  handleReset = () => {
    this.setState({
      hasError: false,
      error: null,
      errorInfo: null,
    });
  };

  render() {
    if (this.state.hasError) {
      if (this.props.fallback) {
        return this.props.fallback;
      }

      // Get user-friendly error message
      const userMessage = this.state.error 
        ? formatUserErrorMessage(this.state.error)
        : 'An unexpected error occurred';

      // Get error code and description
      const errorCode = this.getErrorCode(this.state.error);
      const errorDescription = this.getErrorDescription(this.state.error);

      return (
        <div className="min-h-screen flex items-center justify-center p-4 bg-gray-900">
          <Card className="max-w-2xl" variant="outlined">
            <div className="flex items-start gap-4">
              <AlertCircle className="w-8 h-8 text-red-500 flex-shrink-0" />
              <div className="flex-1">
                <h2 className="text-xl font-bold text-white mb-2">Something went wrong</h2>
                {errorCode && (
                  <p className="text-sm text-gray-500 mb-2">
                    Error Code: <span className="font-mono">{errorCode}</span>
                  </p>
                )}
                <p className="text-gray-400 mb-2">
                  {userMessage}
                </p>
                {errorDescription && (
                  <p className="text-sm text-gray-500 mb-4">
                    {errorDescription}
                  </p>
                )}
                {(import.meta.env.MODE === 'development' || import.meta.env.DEV) && this.state.errorInfo && (
                  <details className="mb-4">
                    <summary className="text-sm text-gray-500 cursor-pointer mb-2">
                      Error Details
                    </summary>
                    <pre className="text-xs text-gray-600 bg-gray-900 p-3 rounded overflow-auto max-h-60">
                      {this.state.error?.stack}
                      {'\n\n'}
                      {this.state.errorInfo.componentStack}
                    </pre>
                  </details>
                )}
                <div className="flex gap-3">
                  <Button onClick={this.handleReset} variant="primary">
                    Try Again
                  </Button>
                  <Button
                    onClick={() => window.location.reload()}
                    variant="secondary"
                  >
                    Reload Page
                  </Button>
                  {(import.meta.env.MODE === 'development' || import.meta.env.DEV) && (
                    <Button
                      onClick={() => {
                        // Copy error details to clipboard
                        const errorDetails = {
                          error: this.state.error?.message,
                          stack: this.state.error?.stack,
                          componentStack: this.state.errorInfo?.componentStack,
                          timestamp: new Date().toISOString()
                        };
                        navigator.clipboard.writeText(JSON.stringify(errorDetails, null, 2));
                      }}
                      variant="outline"
                      size="sm"
                    >
                      <Bug className="w-4 h-4 mr-2" />
                      Copy Error
                    </Button>
                  )}
                </div>
              </div>
            </div>
          </Card>
        </div>
      );
    }

    return this.props.children;
  }

  private getErrorCode(error: Error | null): string | null {
    if (!error) return null;
    
    // Map error types to error codes
    const errorCodeMap: Record<string, string> = {
      'TimeoutError': 'ERR_TIMEOUT',
      'NetworkError': 'ERR_NETWORK',
      'ValidationError': 'ERR_VALIDATION',
      'WorkerError': 'ERR_WORKER',
      'FileSystemError': 'ERR_FILESYSTEM'
    };

    return errorCodeMap[error.name] || 'ERR_UNKNOWN';
  }

  private getErrorDescription(error: Error | null): string | null {
    if (!error) return null;

    const errorName = error.name;
    const errorMessage = error.message;

    // Provide actionable guidance based on error type
    if (errorName === 'TimeoutError') {
      return 'The operation took longer than expected. This might be due to a slow network connection or server load.';
    }

    if (errorName === 'NetworkError' || errorMessage.includes('network')) {
      return 'Please check your internet connection and try again. If the problem persists, the server might be temporarily unavailable.';
    }

    if (errorName === 'ValidationError') {
      return 'The data provided is invalid. Please check your input and ensure all required fields are filled correctly.';
    }

    if (errorName === 'WorkerError' || errorMessage.includes('worker')) {
      return '3D rendering is not available. The application will use 2D visualization instead.';
    }

    if (errorName === 'FileSystemError') {
      return 'File operation failed. Please check file permissions and available disk space.';
    }

    return null;
  }
}
