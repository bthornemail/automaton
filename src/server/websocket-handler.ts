/**
 * WebSocket Handler
 * 
 * Handles Socket.IO WebSocket connections and events
 */

import { Server as SocketIOServer, Socket } from 'socket.io';
import { verifySession } from '../auth/session';
import { securityConfig } from '../config/security';
import { AutomatonController } from './automaton-controller';
import WordNetIntegration from '../services/wordnet';
import { simpleOpenCodeService } from '../services/simple-opencode';
import OpenCodeIntegration from '../../opencode-integration';
import { ProvenanceUpdateHandler } from './provenance-update-handler';

interface ChatParticipant {
  userId: string;
  userName: string;
  type: 'human' | 'agent';
  socketId: string;
}

export class WebSocketHandler {
  private io: SocketIOServer;
  private automatonController: AutomatonController;
  private wordNet: WordNetIntegration;
  private chatParticipants: Map<string, ChatParticipant>;
  private provenanceUpdateHandler: ProvenanceUpdateHandler;

  constructor(
    io: SocketIOServer,
    automatonController: AutomatonController,
    wordNet: WordNetIntegration
  ) {
    this.io = io;
    this.automatonController = automatonController;
    this.wordNet = wordNet;
    this.chatParticipants = new Map();
    this.provenanceUpdateHandler = new ProvenanceUpdateHandler(io);
    this.setupSocketHandlers();
  }

  private setupSocketHandlers(): void {
    this.io.on('connection', (socket: Socket) => {
      console.log('🔌 Client connected to WebSocket:', socket.id);

      // Send initial status
      const state = this.automatonController.getState();
      const automaton = this.automatonController.getAutomaton();
      socket.emit('status', {
        isRunning: state.isRunning,
        currentDimension: (automaton as any).currentDimension,
        iterationCount: (automaton as any).executionHistory?.length || 0,
        selfModificationCount: (automaton as any).selfModificationCount,
        totalObjects: (automaton as any).objects?.length || 0,
        executionMode: 'builtin',
        status: state.isRunning ? 'running' : 'idle',
      });

      // Handle command events
      socket.on('command', async (data: any) => {
        await this.handleCommand(socket, data);
      });

      // Handle chat events
      socket.on('chat:join', (data: any) => {
        this.handleChatJoin(socket, data);
      });

      socket.on('chat:broadcast', (data: any) => {
        this.handleChatBroadcast(socket, data);
      });

      socket.on('chat:direct', (data: any) => {
        this.handleChatDirect(socket, data);
      });

      socket.on('chat:agent', (data: any) => {
        this.handleChatAgent(socket, data);
      });

      // Handle provenance chain events
      socket.on('command', async (data: any) => {
        if (data.command === 'provenance:subscribe') {
          await this.handleProvenanceSubscribe(socket, data.params);
        } else if (data.command === 'provenance:unsubscribe') {
          this.handleProvenanceUnsubscribe(socket, data.params);
        } else if (data.command === 'provenance:update') {
          await this.handleProvenanceUpdate(socket, data.params);
        } else {
          await this.handleCommand(socket, data);
        }
      });

      // Handle disconnect
      socket.on('disconnect', () => {
        this.handleDisconnect(socket);
      });
    });
  }

  private async handleCommand(socket: Socket, data: any): Promise<void> {
    if (data.command === 'chat:broadcast') {
      const message = data.params;
      if (message) {
        this.io.emit('chat:broadcast', message);
        console.log(`📢 Broadcast: ${message.from} → ${message.content.substring(0, 50)}...`);
      }
    } else if (data.command === 'chat:direct') {
      const message = data.params;
      if (message && message.to) {
        const recipient = Array.from(this.chatParticipants.values())
          .find(p => p.userId === message.to);
        if (recipient) {
          this.io.to(recipient.socketId).emit('chat:direct', message);
          socket.emit('chat:direct', message); // Also send back to sender
          console.log(`💬 Direct: ${message.from} → ${message.to}: ${message.content.substring(0, 50)}...`);
        }
      }
    } else if (data.command === 'chat:agent') {
      const message = data.params;
      if (message) {
        this.io.emit('chat:agent', message);
        this.io.emit('chat:direct', message); // Also send as direct to recipient
        console.log(`🤖 Agent: ${message.from} → ${message.to || 'all'}: ${message.content.substring(0, 50)}...`);
      }
    }
  }

  private handleChatJoin(socket: Socket, data: any): void {
    const { userId, userName, type } = data;
    if (userId && userName) {
      this.chatParticipants.set(userId, {
        userId,
        userName,
        type: type || 'human',
        socketId: socket.id,
      });
      
      this.io.emit('chat:participant-joined', {
        userId,
        userName,
        type: type || 'human',
      });
      
      console.log(`👤 User joined chat: ${userName} (${userId})`);
    }
  }

  private handleChatBroadcast(socket: Socket, data: any): void {
    this.io.emit('chat:broadcast', data);
    console.log(`📢 Broadcast: ${data.from} → ${data.content?.substring(0, 50)}...`);
  }

  private handleChatDirect(socket: Socket, data: any): void {
    if (data.to) {
      const recipient = Array.from(this.chatParticipants.values())
        .find(p => p.userId === data.to);
      if (recipient) {
        this.io.to(recipient.socketId).emit('chat:direct', data);
        socket.emit('chat:direct', data); // Also send back to sender
        console.log(`💬 Direct: ${data.from} → ${data.to}: ${data.content?.substring(0, 50)}...`);
      }
    }
  }

  private handleChatAgent(socket: Socket, data: any): void {
    this.io.emit('chat:agent', data);
    this.io.emit('chat:direct', data); // Also send as direct to recipient
    console.log(`🤖 Agent: ${data.from} → ${data.to || 'all'}: ${data.content?.substring(0, 50)}...`);
  }

  private handleDisconnect(socket: Socket): void {
    console.log('🔌 Client disconnected from WebSocket');
    
    // Remove participant
    const participant = Array.from(this.chatParticipants.entries())
      .find(([_, p]) => p.socketId === socket.id);
    if (participant) {
      const [userId] = participant;
      this.chatParticipants.delete(userId);
      
      // Notify all clients
      this.io.emit('chat:participant-left', userId);
      console.log(`👤 User left chat: ${userId}`);
    }
  }

  /**
   * Get chat participants
   */
  getChatParticipants(): ChatParticipant[] {
    return Array.from(this.chatParticipants.values());
  }

  /**
   * Handle provenance chain subscription.
   */
  private async handleProvenanceSubscribe(socket: Socket, params: any): Promise<void> {
    const { evolutionPath } = params;
    if (!evolutionPath) {
      socket.emit('provenance:subscription-error', { message: 'Missing evolutionPath' });
      return;
    }

    try {
      // Start watching if not already watching
      await this.provenanceUpdateHandler.watchEvolutionDirectory(evolutionPath);
      
      // Subscribe client
      this.provenanceUpdateHandler.subscribeClient(evolutionPath, socket.id);
      
      // Send confirmation
      socket.emit('provenance:subscribed', { evolutionPath });
      console.log(`📁 Client ${socket.id} subscribed to ${evolutionPath}`);
    } catch (error) {
      socket.emit('provenance:subscription-error', {
        message: error instanceof Error ? error.message : String(error)
      });
      console.error(`Failed to subscribe client ${socket.id} to ${evolutionPath}:`, error);
    }
  }

  /**
   * Handle provenance chain unsubscription.
   */
  private handleProvenanceUnsubscribe(socket: Socket, params: any): void {
    const { evolutionPath } = params;
    if (!evolutionPath) {
      return;
    }

    this.provenanceUpdateHandler.unsubscribeClient(evolutionPath, socket.id);
    console.log(`📁 Client ${socket.id} unsubscribed from ${evolutionPath}`);
  }

  /**
   * Handle provenance chain update from client.
   */
  private async handleProvenanceUpdate(socket: Socket, update: any): Promise<void> {
    const { evolutionPath, clientId, type } = update;
    if (!evolutionPath || !clientId || !type) {
      return;
    }

    // Broadcast to other clients (conflict resolution)
    // TODO: Implement conflict detection and resolution
    this.provenanceUpdateHandler.broadcastUpdate(evolutionPath, update);
    console.log(`📤 Client ${clientId} sent update for ${evolutionPath}: ${type}`);
  }

  /**
   * Get provenance update handler.
   */
  getProvenanceUpdateHandler(): ProvenanceUpdateHandler {
    return this.provenanceUpdateHandler;
  }
}
