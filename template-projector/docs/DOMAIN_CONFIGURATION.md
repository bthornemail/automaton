# Domain Configuration for universallifeprotocol.online

This document describes how the template-projector app is configured to use the `universallifeprotocol.online` domain and IP addresses.

## Domain Information

- **Domain**: `universallifeprotocol.online`
- **IPv4**: `172.238.45.134`
- **IPv6**: `2600:3c0a::2000:96ff:fef2:9b19`

### Subdomains

- **www**: `www.universallifeprotocol.online` → `172.238.45.134`
- **mail**: `mail.universallifeprotocol.online` → `172.238.45.134`

### DNS Nameservers

- `ns1.linode.com`
- `ns2.linode.com`
- `ns3.linode.com`
- `ns4.linode.com`
- `ns5.linode.com`

### Mail Exchange (MX)

- Priority: 10
- Host: `mail.universallifeprotocol.online`

## Configuration Files

### 1. Domain Configuration Module

**File**: `src/config/domain-config.js`

Centralized configuration module that exports:
- Domain and IP addresses
- Helper methods for generating URLs
- CORS allowed origins
- Domain detection utilities

**Usage**:
```javascript
import { domainConfig } from './src/config/domain-config.js';

// Get base URL
const baseUrl = domainConfig.getBaseUrl(); // https://universallifeprotocol.online (production)
const devUrl = domainConfig.getBaseUrl(false); // http://universallifeprotocol.online:3003

// Get IP URLs
const ipv4Url = domainConfig.getIPv4Url(); // http://172.238.45.134:3003
const ipv6Url = domainConfig.getIPv6Url(); // http://[2600:3c0a::2000:96ff:fef2:9b19]:3003

// Check if current hostname matches domain
if (domainConfig.isCurrentDomain()) {
  console.log('Running on universallifeprotocol.online');
}

// Get allowed origins for CORS
const origins = domainConfig.getAllowedOrigins();
```

### 2. Vite Configuration

**File**: `vite.config.js`

Updated to:
- Import domain configuration
- Configure server `allowedHosts` with domain and IP addresses
- Set CORS origins based on domain configuration
- Make domain config available in client code via `__DOMAIN_CONFIG__`

**Server Configuration**:
- **Host**: `0.0.0.0` (listens on all interfaces)
- **Port**: `3003` (configurable via `TEMPLATE_PROJECTOR_PORT` env var)
- **Allowed Hosts**: 
  - `universallifeprotocol.online`
  - `www.universallifeprotocol.online`
  - `mail.universallifeprotocol.online`
  - `localhost`
  - `127.0.0.1`
  - `172.238.45.134` (IPv4)
  - `[2600:3c0a::2000:96ff:fef2:9b19]` (IPv6)

**CORS Configuration**:
- Allows requests from all domain variations (HTTP/HTTPS, www/non-www, IP addresses)
- Includes localhost for development
- Credentials enabled

### 3. Client-Side Access

Domain configuration is available in client code via `__DOMAIN_CONFIG__`:

```javascript
// Access domain config in browser
const domainConfig = window.__DOMAIN_CONFIG__ || {};
console.log(domainConfig.domain); // universallifeprotocol.online
console.log(domainConfig.ipv4); // 172.238.45.134
console.log(domainConfig.baseUrl); // http://universallifeprotocol.online:3003
```

## Environment Variables

- `TEMPLATE_PROJECTOR_HOST`: Server host (default: `0.0.0.0`)
- `TEMPLATE_PROJECTOR_PORT`: Server port (default: `3003`)
- `NODE_ENV`: Environment mode (`development` or `production`)

## Deployment

### Development

```bash
npm run dev
```

Server will be accessible at:
- `http://localhost:3003`
- `http://172.238.45.134:3003`
- `http://universallifeprotocol.online:3003` (if DNS is configured)

### Production

```bash
npm run build
npm run preview
```

For production deployment:
1. Configure reverse proxy (nginx/Apache) to forward requests to port 3003
2. Set up SSL/TLS certificates for HTTPS
3. Configure DNS A/AAAA records to point to `172.238.45.134` / `2600:3c0a::2000:96ff:fef2:9b19`

## DNS Configuration

The DNS zone file is located at:
- `docs/universallifeprotocol.online.txt`

This file contains the complete DNS zone configuration including:
- SOA record
- NS records
- A records (IPv4)
- AAAA records (IPv6)
- MX record

## Testing

To test domain configuration:

1. **Local Development**:
   ```bash
   npm run dev
   # Access at http://localhost:3003
   ```

2. **IP Address Access**:
   ```bash
   # Access at http://172.238.45.134:3003
   ```

3. **Domain Access** (requires DNS configuration):
   ```bash
   # Access at http://universallifeprotocol.online:3003
   ```

## Notes

- The app listens on all interfaces (`0.0.0.0`) to allow access via domain, IP, or localhost
- CORS is configured to allow requests from all domain variations
- Domain configuration is centralized in `src/config/domain-config.js` for easy maintenance
- IPv6 addresses must be bracketed in URLs: `http://[2600:3c0a::2000:96ff:fef2:9b19]:3003`

