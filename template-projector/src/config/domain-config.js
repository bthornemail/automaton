/**
 * Domain Configuration for universallifeprotocol.online
 * 
 * Centralized configuration for domain, IP addresses, and DNS settings
 */

export const domainConfig = {
  domain: 'universallifeprotocol.online',
  ipv4: '172.238.45.134',
  ipv6: '2600:3c0a::2000:96ff:fef2:9b19',
  
  subdomains: {
    www: {
      ipv4: '172.238.45.134',
      ipv6: '2600:3c0a::2000:96ff:fef2:9b19'
    },
    mail: {
      ipv4: '172.238.45.134',
      ipv6: '2600:3c0a::2000:96ff:fef2:9b19'
    },
    templates: {
      ipv4: '172.238.45.134',
      ipv6: '2600:3c0a::2000:96ff:fef2:9b19'
    }
  },
  
  dns: {
    nameservers: [
      'ns1.linode.com',
      'ns2.linode.com',
      'ns3.linode.com',
      'ns4.linode.com',
      'ns5.linode.com'
    ],
    mx: {
      priority: 10,
      host: 'mail.universallifeprotocol.online'
    }
  },
  
  ports: {
    http: 80,
    https: 443,
    dev: 3003
  },
  
  /**
   * Get the base URL for the application
   * @param {boolean} useHttps - Whether to use HTTPS (default: true in production)
   * @param {string} subdomain - Optional subdomain (www, mail, etc.)
   * @returns {string} Base URL
   */
  getBaseUrl(useHttps = null, subdomain = null) {
    const isProduction = process.env.NODE_ENV === 'production';
    const protocol = useHttps !== null 
      ? (useHttps ? 'https' : 'http')
      : (isProduction ? 'https' : 'http');
    
    const host = subdomain 
      ? `${subdomain}.${this.domain}`
      : this.domain;
    
    const port = isProduction ? '' : `:${this.ports.dev}`;
    
    return `${protocol}://${host}${port}`;
  },
  
  /**
   * Get the IPv4 URL
   * @param {boolean} useHttps - Whether to use HTTPS
   * @param {number} port - Port number (default: dev port)
   * @returns {string} IPv4 URL
   */
  getIPv4Url(useHttps = false, port = null) {
    const protocol = useHttps ? 'https' : 'http';
    const portStr = port ? `:${port}` : `:${this.ports.dev}`;
    return `${protocol}://${this.ipv4}${portStr}`;
  },
  
  /**
   * Get the IPv6 URL
   * @param {boolean} useHttps - Whether to use HTTPS
   * @param {number} port - Port number (default: dev port)
   * @returns {string} IPv6 URL (with brackets)
   */
  getIPv6Url(useHttps = false, port = null) {
    const protocol = useHttps ? 'https' : 'http';
    const portStr = port ? `:${port}` : `:${this.ports.dev}`;
    return `${protocol}://[${this.ipv6}]${portStr}`;
  },
  
  /**
   * Check if the current hostname matches the configured domain
   * @returns {boolean} True if hostname matches domain or subdomain
   */
  isCurrentDomain() {
    if (typeof window === 'undefined') return false;
    const hostname = window.location.hostname;
    return hostname === this.domain 
      || hostname === `www.${this.domain}`
      || hostname === `mail.${this.domain}`
      || hostname === `templates.${this.domain}`
      || hostname === this.ipv4
      || hostname === `[${this.ipv6}]`;
  },
  
  /**
   * Get allowed origins for CORS
   * @returns {string[]} Array of allowed origins
   */
  getAllowedOrigins() {
    return [
      `https://${this.domain}`,
      `https://www.${this.domain}`,
      `https://templates.${this.domain}`,
      `http://${this.domain}`,
      `http://www.${this.domain}`,
      `http://templates.${this.domain}`,
      `http://${this.ipv4}`,
      `http://[${this.ipv6}]`,
      'http://localhost:3003',
      'http://127.0.0.1:3003'
    ];
  }
};

// Export default for convenience
export default domainConfig;

