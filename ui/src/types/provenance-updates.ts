/**
 * Types for real-time provenance chain updates
 */

import { ProvenanceNode, ProvenanceEdge, ProvenanceChain, Slide, Card } from '../services/provenance-slide-service';
import type { VectorClock } from './vector-clock';

/**
 * Chain update types
 */
export type ChainUpdateType = 
  | 'node:added' 
  | 'node:updated' 
  | 'node:removed' 
  | 'edge:added' 
  | 'edge:removed'
  | 'chain:rebuilt';

/**
 * Chain update message
 */
export interface ChainUpdate {
  type: ChainUpdateType;
  evolutionPath: string;
  timestamp: number;
  clientId: string;
  vectorClock?: VectorClock;
  data: {
    node?: ProvenanceNode;
    edge?: ProvenanceEdge;
    nodeId?: string;
    edgeId?: string;
    chain?: ProvenanceChain;
  };
}

/**
 * Slide update message
 */
export interface SlideUpdate {
  slideId: string;
  evolutionPath: string;
  updates: Partial<Slide>;
  timestamp: number;
  clientId: string;
  vectorClock?: VectorClock;
}

/**
 * Card update message
 */
export interface CardUpdate {
  cardId: string;
  slideId: string;
  evolutionPath: string;
  updates: Partial<Card>;
  timestamp: number;
  clientId: string;
  vectorClock?: VectorClock;
}

/**
 * Conflict information
 */
export interface Conflict {
  type: 'node' | 'edge' | 'slide' | 'card';
  id: string;
  evolutionPath: string;
  updates: Array<ChainUpdate | SlideUpdate | CardUpdate>;
  resolution: 'pending' | 'resolved' | 'last-write-wins';
  resolvedBy?: string;
  resolvedAt?: number;
}

/**
 * Update handler interface
 */
export interface UpdateHandler {
  onChainUpdate?: (update: ChainUpdate) => void;
  onSlideUpdate?: (update: SlideUpdate) => void;
  onCardUpdate?: (update: CardUpdate) => void;
  onConflict?: (conflict: Conflict) => void;
  onError?: (error: Error) => void;
}

/**
 * Subscription information
 */
export interface ProvenanceSubscription {
  evolutionPath: string;
  clientId: string;
  subscribedAt: number;
}

