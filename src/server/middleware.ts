/**
 * Server Middleware Setup
 * 
 * Configures Express middleware for security, CORS, compression, etc.
 */

import express from 'express';
import cors from 'cors';
import helmet from 'helmet';
import compression from 'compression';
import morgan from 'morgan';
import { securityConfig } from '../config/security';
import { rateLimiters } from '../middleware/rate-limit';

/**
 * Setup Express middleware
 */
export function setupMiddleware(app: express.Application): void {
  // Security middleware
  app.use(helmet({
    contentSecurityPolicy: {
      directives: {
        defaultSrc: ["'self'"],
        scriptSrc: ["'self'", "'unsafe-inline'", "'unsafe-eval'"], // Needed for Vite
        styleSrc: ["'self'", "'unsafe-inline'"],
        imgSrc: ["'self'", "data:", "https:"],
        connectSrc: ["'self'", "ws:", "wss:"],
      },
    },
    crossOriginEmbedderPolicy: false, // Needed for WebGL
  }));

  // CORS with restricted origins - must be before other middleware
  app.use(cors({
    origin: (origin, callback) => {
      // Allow requests with no origin (mobile apps, Postman, etc.)
      if (!origin) {
        return callback(null, true);
      }
      
      // Normalize origin for comparison (handle both localhost and 127.0.0.1)
      const normalizedOrigin = origin.toLowerCase();
      const normalizedAllowed = securityConfig.cors.allowedOrigins.map(o => o.toLowerCase());
      
      if (normalizedAllowed.includes(normalizedOrigin)) {
        callback(null, true);
      } else {
        console.warn(`CORS blocked origin: ${origin} (allowed: ${securityConfig.cors.allowedOrigins.join(', ')})`);
        callback(new Error('Not allowed by CORS'));
      }
    },
    credentials: securityConfig.cors.credentials,
    methods: securityConfig.cors.methods,
    allowedHeaders: [...securityConfig.cors.allowedHeaders, 'X-Requested-With', 'Accept', 'Accept-Language', 'Content-Language'],
    exposedHeaders: ['Content-Length', 'Content-Type'],
    maxAge: 86400, // 24 hours
    preflightContinue: false,
    optionsSuccessStatus: 204,
  }));

  // Compression
  app.use(compression());

  // Body parsing
  app.use(express.json({ limit: '10mb' }));
  app.use(express.urlencoded({ extended: true, limit: '10mb' }));

  // Logging
  app.use(morgan('combined'));

  // Rate limiting for all routes
  app.use(rateLimiters.api);
}
