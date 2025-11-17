/**
 * A₁₀: MQTT Herald Automaton
 * 
 * Role: Peer Discovery via MQTT
 * Uses MQTTClient for discovery and announcements
 */

import { BaseAutomaton } from './types.js';
import type { AutomatonId, SwarmContext, AutomatonMessage } from './types.js';
import { MQTTClient, type MQTTClientConfig } from '../network/mqtt-client.js';

export interface A10MQTTHeraldState {
  discoveredPeers: Map<string, any>; // peerId -> peer info
  announced: boolean;
  initialized: boolean;
}

/**
 * A₁₀: MQTT Herald Automaton
 * 
 * Manages peer discovery via MQTT
 */
export class A10_MQTTHerald extends BaseAutomaton {
  readonly id: 10 = 10;
  readonly name = 'A₁₀ MQTT Herald';
  readonly role = 'Peer Discovery via MQTT';

  state: A10MQTTHeraldState = {
    discoveredPeers: new Map(),
    announced: false,
    initialized: false
  };

  private mqttClient?: MQTTClient;
  private discoveryTopic: string = 'automata/discovery';
  private announcementTopic: string = 'automata/announcements';
  private localPeerId: string;

  constructor(config?: MQTTClientConfig) {
    super();
    
    this.localPeerId = config?.clientId || `peer-${Date.now()}`;
    
    this.mqttClient = new MQTTClient({
      ...config,
      onMessage: (topic, message) => {
        this.handleMQTTMessage(topic, message);
      },
      onConnect: () => {
        this.handleMQTTConnect();
      },
      onDisconnect: () => {
        this.handleMQTTDisconnect();
      }
    });
  }

  async tick(swarm: SwarmContext): Promise<void> {
    if (!this.state.initialized) {
      await this.initialize(swarm);
    }

    // Re-announce presence periodically
    if (this.mqttClient?.isConnected() && !this.state.announced) {
      await this.announcePresence();
    }
  }

  private async initialize(swarm: SwarmContext): Promise<void> {
    try {
      // Connect to MQTT broker
      await this.mqttClient?.connect();

      // Subscribe to discovery topics
      if (this.mqttClient?.isConnected()) {
        await this.mqttClient.subscribe(this.discoveryTopic);
        await this.mqttClient.subscribe(this.announcementTopic);
      }

      this.state.initialized = true;
      console.log('A₁₀: MQTT Herald initialized');
    } catch (error) {
      console.error('A₁₀: Error initializing MQTT client:', error);
      // Continue without MQTT if not available
      this.state.initialized = true;
    }
  }

  /**
   * Announce presence to MQTT broker
   */
  async announcePresence(): Promise<void> {
    if (!this.mqttClient?.isConnected()) {
      return;
    }

    try {
      const announcement = {
        peerId: this.localPeerId,
        timestamp: Date.now(),
        type: 'presence'
      };

      await this.mqttClient.publish(this.announcementTopic, JSON.stringify(announcement));
      this.state.announced = true;
      console.log('A₁₀: Announced presence');
    } catch (error) {
      console.error('A₁₀: Error announcing presence:', error);
    }
  }

  /**
   * Handle MQTT message
   */
  private handleMQTTMessage(topic: string, message: Buffer): void {
    try {
      const data = JSON.parse(message.toString());

      if (topic === this.discoveryTopic || topic === this.announcementTopic) {
        if (data.peerId && data.peerId !== this.localPeerId) {
          // Discovered a new peer
          this.state.discoveredPeers.set(data.peerId, {
            peerId: data.peerId,
            timestamp: data.timestamp || Date.now(),
            ...data
          });

          console.log(`A₁₀: Discovered peer: ${data.peerId}`);

          // Notify swarm about discovered peer
          // In a real implementation, this would trigger WebRTC connection via A₉
        }
      }
    } catch (error) {
      console.error('A₁₀: Error handling MQTT message:', error);
    }
  }

  /**
   * Handle MQTT connect
   */
  private handleMQTTConnect(): void {
    console.log('A₁₀: Connected to MQTT broker');
    // Subscribe to topics on connect
    if (this.mqttClient) {
      this.mqttClient.subscribe(this.discoveryTopic).catch(console.error);
      this.mqttClient.subscribe(this.announcementTopic).catch(console.error);
    }
  }

  /**
   * Handle MQTT disconnect
   */
  private handleMQTTDisconnect(): void {
    console.log('A₁₀: Disconnected from MQTT broker');
    this.state.announced = false;
  }

  /**
   * Publish discovery request
   */
  async requestDiscovery(): Promise<void> {
    if (!this.mqttClient?.isConnected()) {
      return;
    }

    try {
      const request = {
        peerId: this.localPeerId,
        timestamp: Date.now(),
        type: 'discovery-request'
      };

      await this.mqttClient.publish(this.discoveryTopic, JSON.stringify(request));
    } catch (error) {
      console.error('A₁₀: Error requesting discovery:', error);
    }
  }

  async receive(from: AutomatonId, message: AutomatonMessage): Promise<void> {
    if (message.type === 'request-discovery') {
      await this.requestDiscovery();
    } else if (message.type === 'announce-presence') {
      await this.announcePresence();
    }
  }

  /**
   * Get discovered peers
   */
  getDiscoveredPeers(): Map<string, any> {
    return this.state.discoveredPeers;
  }
}

