/**
 * useMetaLogDb Hook
 * 
 * React hook for MetaLogDbBrowser initialization and usage
 * Provides loading state and error handling
 */

import { useState, useEffect, useCallback } from 'react';
import { getMetaLogBrowserAdapter, MetaLogBrowserAdapter } from '../services/meta-log-browser-adapter';

export interface UseMetaLogDbResult {
  adapter: MetaLogBrowserAdapter | null;
  initialized: boolean;
  loading: boolean;
  error: Error | null;
  initialize: () => Promise<void>;
}

/**
 * Hook to access MetaLogDbBrowser adapter
 * Automatically initializes on mount
 */
export function useMetaLogDb(): UseMetaLogDbResult {
  const [adapter, setAdapter] = useState<MetaLogBrowserAdapter | null>(null);
  const [initialized, setInitialized] = useState(false);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);

  const initialize = useCallback(async () => {
    try {
      setLoading(true);
      setError(null);
      
      const adapterInstance = getMetaLogBrowserAdapter();
      await adapterInstance.init();
      
      setAdapter(adapterInstance);
      setInitialized(adapterInstance.isInitialized());
    } catch (err) {
      const error = err instanceof Error ? err : new Error(String(err));
      setError(error);
      console.error('Failed to initialize MetaLogDbBrowser:', error);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    initialize();
  }, [initialize]);

  return {
    adapter,
    initialized,
    loading,
    error,
    initialize
  };
}

