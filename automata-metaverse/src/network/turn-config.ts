/**
 * TURN Server Configuration
 * 
 * Loads TURN server configuration from environment variables
 * Supports coturn integration
 */

export interface TURNConfig {
  urls: string | string[];
  username?: string;
  credential?: string;
}

export interface ICEConfig {
  iceServers: RTCIceServer[];
}

/**
 * Load TURN configuration from environment variables
 */
export function loadTURNConfig(): TURNConfig | null {
  // Browser environment: import.meta.env.VITE_*
  // Node.js environment: process.env.*
  const turnUrl = typeof window !== 'undefined'
    ? (import.meta as any).env?.VITE_TURN_SERVER_URL
    : process.env.TURN_SERVER_URL;

  const turnUsername = typeof window !== 'undefined'
    ? (import.meta as any).env?.VITE_TURN_USERNAME
    : process.env.TURN_USERNAME;

  const turnPassword = typeof window !== 'undefined'
    ? (import.meta as any).env?.VITE_TURN_PASSWORD
    : process.env.TURN_PASSWORD;

  if (!turnUrl) {
    return null;
  }

  const config: TURNConfig = {
    urls: turnUrl
  };

  if (turnUsername && turnPassword) {
    config.username = turnUsername;
    config.credential = turnPassword;
  }

  return config;
}

/**
 * Build ICE servers configuration
 * Includes STUN fallback and TURN server if configured
 */
export function buildICEServers(): RTCIceServer[] {
  const servers: RTCIceServer[] = [];

  // Always include Google STUN server as fallback
  servers.push({ urls: 'stun:stun.l.google.com:19302' });

  // Add TURN server if configured
  const turnConfig = loadTURNConfig();
  if (turnConfig) {
    servers.push(turnConfig);
  }

  return servers;
}

/**
 * Build ICE configuration for RTCPeerConnection
 */
export function buildICEConfig(): ICEConfig {
  return {
    iceServers: buildICEServers()
  };
}

