/**
 * Worker Error Boundary
 * 
 * Error boundary specifically for worker-related errors.
 * Displays 2D visualization fallback when worker fails.
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

export class WorkerErrorBoundary extends Component<Props, State> {
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
        component: 'WorkerErrorBoundary',
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

      return (
        <div className="w-full h-full flex items-center justify-center bg-gray-900 p-4">
          <Card className="max-w-md" variant="outlined">
            <div className="flex items-start gap-4">
              <AlertCircle className="w-8 h-8 text-yellow-500 flex-shrink-0" />
              <div className="flex-1">
                <h2 className="text-xl font-bold text-white mb-2">3D Rendering Unavailable</h2>
                <p className="text-gray-400 mb-4">
                  The 3D worker failed to initialize. Showing 2D visualization instead.
                </p>
                {this.state.error && (
                  <p className="text-sm text-gray-500 mb-4">
                    {this.state.error.message}
                  </p>
                )}
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
                    Retry Worker
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
}

