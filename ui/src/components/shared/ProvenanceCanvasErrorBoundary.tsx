/**
 * Provenance Canvas Error Boundary
 * 
 * Error boundary specifically for UnifiedProvenanceCanvas component.
 * Provides user-friendly error messages and retry functionality.
 */

import React, { Component, ErrorInfo, ReactNode } from 'react';
import { AlertCircle, RefreshCw } from 'lucide-react';
import { Card } from './Card';
import { Button } from './Button';

interface Props {
  children: ReactNode;
  onRetry?: () => void;
  fallback?: ReactNode;
}

interface State {
  hasError: boolean;
  error: Error | null;
  errorInfo: ErrorInfo | null;
}

export class ProvenanceCanvasErrorBoundary extends Component<Props, State> {
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
    this.setState({
      error,
      errorInfo,
    });

    // Log error
    if (typeof window !== 'undefined' && (window as any).errorLoggingService) {
      (window as any).errorLoggingService.logError(error, {
        component: 'ProvenanceCanvasErrorBoundary',
        action: 'componentDidCatch',
        severity: 'error',
      });
    }
  }

  handleReset = () => {
    this.setState({
      hasError: false,
      error: null,
      errorInfo: null,
    });
    
    if (this.props.onRetry) {
      this.props.onRetry();
    }
  };

  render() {
    if (this.state.hasError) {
      if (this.props.fallback) {
        return this.props.fallback;
      }

      // Get user-friendly error message
      const errorMessage = this.getUserFriendlyMessage(this.state.error);

      return (
        <div className="w-full h-full flex items-center justify-center bg-gray-900 p-4">
          <Card className="max-w-2xl" variant="outlined">
            <div className="flex items-start gap-4">
              <AlertCircle className="w-8 h-8 text-red-500 flex-shrink-0" />
              <div className="flex-1">
                <h2 className="text-xl font-bold text-white mb-2">Provenance Canvas Error</h2>
                <p className="text-gray-400 mb-4">
                  {errorMessage}
                </p>
                {(import.meta.env.MODE === 'development' || import.meta.env.DEV) && this.state.errorInfo && (
                  <details className="mb-4">
                    <summary className="text-sm text-gray-500 cursor-pointer mb-2">
                      Error Details
                    </summary>
                    <pre className="text-xs text-gray-600 bg-gray-900 p-3 rounded overflow-auto max-h-40">
                      {this.state.error?.stack}
                      {'\n\n'}
                      {this.state.errorInfo.componentStack}
                    </pre>
                  </details>
                )}
                <div className="flex gap-3">
                  <Button onClick={this.handleReset} variant="primary" size="sm">
                    <RefreshCw className="w-4 h-4 mr-2" />
                    Try Again
                  </Button>
                  <Button
                    onClick={() => window.location.reload()}
                    variant="secondary"
                    size="sm"
                  >
                    Reload Page
                  </Button>
                </div>
              </div>
            </div>
          </Card>
        </div>
      );
    }

    return this.props.children;
  }

  private getUserFriendlyMessage(error: Error | null): string {
    if (!error) {
      return 'An unexpected error occurred while loading the provenance canvas.';
    }

    const errorName = error.name;
    const errorMessage = error.message;

    // Map common errors to user-friendly messages
    if (errorName === 'TimeoutError' || errorMessage.includes('timeout')) {
      return 'The operation took too long to complete. Please try again.';
    }

    if (errorName === 'NetworkError' || errorMessage.includes('network') || errorMessage.includes('fetch')) {
      return 'Network connection failed. Please check your internet connection and try again.';
    }

    if (errorName === 'ValidationError' || errorMessage.includes('validation')) {
      return 'Invalid data provided. Please check your input and try again.';
    }

    if (errorName === 'WorkerError' || errorMessage.includes('worker')) {
      return '3D rendering is not available. The canvas will show 2D visualization instead.';
    }

    if (errorMessage.includes('OffscreenCanvas')) {
      return '3D rendering is not supported in this browser. Showing 2D view instead.';
    }

    // Fallback to generic message
    return 'An error occurred while loading the provenance canvas. Please try again or contact support if the problem persists.';
  }
}

