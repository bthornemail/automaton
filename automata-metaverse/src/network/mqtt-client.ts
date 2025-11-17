/**
 * MQTT Client Wrapper
 * 
 * Wrapper for MQTT client for peer discovery and messaging
 * Uses mqtt package (peer dependency)
 */

export interface MQTTClientConfig {
  brokerUrl?: string;
  clientId?: string;
  username?: string;
  password?: string;
  onMessage?: (topic: string, message: Buffer) => void;
  onConnect?: () => void;
  onDisconnect?: () => void;
  onError?: (error: Error) => void;
}

/**
 * MQTT Client Wrapper
 * 
 * Manages MQTT connection for peer discovery
 */
export class MQTTClient {
  private client: any; // mqtt.MqttClient
  private config: MQTTClientConfig;
  private connected: boolean = false;
  private subscriptions: Set<string> = new Set();

  constructor(config: MQTTClientConfig = {}) {
    this.config = config;
  }

  /**
   * Connect to MQTT broker
   */
  async connect(): Promise<void> {
    // Dynamic import of mqtt package (peer dependency)
    let mqtt: any;
    try {
      // @ts-ignore - mqtt is a peer dependency, may not be installed
      mqtt = await import('mqtt');
    } catch (error) {
      throw new Error('MQTT package not installed. Install with: npm install mqtt');
    }

    const brokerUrl = this.config.brokerUrl || process.env.MQTT_BROKER_URL || 'mqtt://localhost:1883';
    const clientId = this.config.clientId || `automata-${Date.now()}`;

    const options: any = {
      clientId,
      clean: true,
      reconnectPeriod: 1000,
      connectTimeout: 30000
    };

    if (this.config.username) {
      options.username = this.config.username;
    }

    if (this.config.password) {
      options.password = this.config.password;
    }

    this.client = mqtt.connect(brokerUrl, options);

    // Set up event handlers
    this.client.on('connect', () => {
      this.connected = true;
      console.log('MQTT: Connected to broker');
      if (this.config.onConnect) {
        this.config.onConnect();
      }
    });

    this.client.on('message', (topic: string, message: Buffer) => {
      if (this.config.onMessage) {
        this.config.onMessage(topic, message);
      }
    });

    this.client.on('error', (error: Error) => {
      console.error('MQTT: Error:', error);
      if (this.config.onError) {
        this.config.onError(error);
      }
    });

    this.client.on('close', () => {
      this.connected = false;
      console.log('MQTT: Disconnected from broker');
      if (this.config.onDisconnect) {
        this.config.onDisconnect();
      }
    });

    // Wait for connection
    return new Promise<void>((resolve, reject) => {
      this.client.once('connect', () => {
        resolve();
      });
      this.client.once('error', (error: Error) => {
        reject(error);
      });
    });
  }

  /**
   * Disconnect from MQTT broker
   */
  disconnect(): void {
    if (this.client) {
      this.client.end();
      this.client = null;
      this.connected = false;
      this.subscriptions.clear();
    }
  }

  /**
   * Subscribe to topic
   */
  subscribe(topic: string): Promise<void> {
    if (!this.client || !this.connected) {
      throw new Error('MQTT client not connected');
    }

    return new Promise<void>((resolve, reject) => {
      this.client.subscribe(topic, (error: Error | null) => {
        if (error) {
          reject(error);
        } else {
          this.subscriptions.add(topic);
          console.log(`MQTT: Subscribed to topic: ${topic}`);
          resolve();
        }
      });
    });
  }

  /**
   * Unsubscribe from topic
   */
  unsubscribe(topic: string): Promise<void> {
    if (!this.client || !this.connected) {
      throw new Error('MQTT client not connected');
    }

    return new Promise<void>((resolve, reject) => {
      this.client.unsubscribe(topic, (error: Error | null) => {
        if (error) {
          reject(error);
        } else {
          this.subscriptions.delete(topic);
          console.log(`MQTT: Unsubscribed from topic: ${topic}`);
          resolve();
        }
      });
    });
  }

  /**
   * Publish message to topic
   */
  publish(topic: string, message: string | Buffer, options?: any): Promise<void> {
    if (!this.client || !this.connected) {
      throw new Error('MQTT client not connected');
    }

    return new Promise<void>((resolve, reject) => {
      this.client.publish(topic, message, options || {}, (error: Error | null) => {
        if (error) {
          reject(error);
        } else {
          resolve();
        }
      });
    });
  }

  /**
   * Check if connected
   */
  isConnected(): boolean {
    return this.connected && this.client?.connected === true;
  }

  /**
   * Get subscriptions
   */
  getSubscriptions(): string[] {
    return Array.from(this.subscriptions);
  }
}

