import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';
import './index.css';
import { getMetaLogBrowserAdapter } from './services/meta-log-browser-adapter';

// Initialize MetaLogDbBrowser on app startup (non-blocking)
getMetaLogBrowserAdapter()
  .init()
  .then(() => {
    console.log('✓ MetaLogDbBrowser initialized on app startup');
  })
  .catch((error) => {
    console.warn('⚠ MetaLogDbBrowser initialization failed on startup (will retry on first use):', error);
  });

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
);