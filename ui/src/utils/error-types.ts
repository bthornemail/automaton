/**
 * Custom Error Types for Error Handling
 */

export class TimeoutError extends Error {
  constructor(
    message: string,
    public readonly timeout: number,
    public readonly operation?: string
  ) {
    super(message);
    this.name = 'TimeoutError';
    Object.setPrototypeOf(this, TimeoutError.prototype);
  }
}

export class ValidationError extends Error {
  constructor(
    message: string,
    public readonly fieldErrors: Record<string, string[]> = {},
    public readonly object?: string
  ) {
    super(message);
    this.name = 'ValidationError';
    Object.setPrototypeOf(this, ValidationError.prototype);
  }

  /**
   * Get all field errors as a formatted string
   */
  getFieldErrorsString(): string {
    const errors: string[] = [];
    for (const [field, fieldErrors] of Object.entries(this.fieldErrors)) {
      errors.push(`${field}: ${fieldErrors.join(', ')}`);
    }
    return errors.join('; ');
  }
}

export class WorkerError extends Error {
  constructor(
    message: string,
    public readonly workerContext?: {
      workerId?: string;
      operation?: string;
      fallbackMode?: string;
    }
  ) {
    super(message);
    this.name = 'WorkerError';
    Object.setPrototypeOf(this, WorkerError.prototype);
  }
}

export class FileSystemError extends Error {
  constructor(
    message: string,
    public readonly filePath: string,
    public readonly operation: 'read' | 'write' | 'delete' | 'move',
    public readonly originalError?: Error
  ) {
    super(message);
    this.name = 'FileSystemError';
    Object.setPrototypeOf(this, FileSystemError.prototype);
  }
}

