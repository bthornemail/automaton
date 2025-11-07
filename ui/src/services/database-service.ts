/**
 * Database Service - Frontend
 * 
 * Database-agnostic service layer for frontend
 * Works with any backend database adapter
 */

import { apiService } from './api';

export interface DatabaseService {
  // JSONL operations
  readJSONL(file: string): Promise<any[]>;
  writeJSONL(file: string, data: any[]): Promise<void>;
  appendJSONL(file: string, data: any): Promise<void>;
  queryJSONL(file: string, predicate: (item: any) => boolean): Promise<any[]>;

  // R5RS Function operations
  getR5RSFunction(name: string): Promise<any>;
  listR5RSFunctions(pattern?: string): Promise<string[]>;
  invokeR5RSFunction(name: string, args: any[], context?: any): Promise<any>;
  registerR5RSFunction(name: string, definition: any): Promise<void>;

  // Generic CRUD operations
  create(collection: string, data: any): Promise<string>;
  read(collection: string, id: string): Promise<any>;
  update(collection: string, id: string, data: any): Promise<void>;
  delete(collection: string, id: string): Promise<void>;
  query(collection: string, filter?: any, options?: QueryOptions): Promise<any[]>;
}

export interface QueryOptions {
  limit?: number;
  offset?: number;
  sort?: { field: string; direction: 'asc' | 'desc' }[];
  projection?: string[];
}

class DatabaseServiceImpl implements DatabaseService {
  async readJSONL(file: string): Promise<any[]> {
    const response = await apiService.request(`/jsonl/${file}`);
    return response.success ? response.data : [];
  }

  async writeJSONL(file: string, data: any[]): Promise<void> {
    await apiService.request(`/jsonl/${file}`, {
      method: 'POST',
      body: JSON.stringify({ data })
    });
  }

  async appendJSONL(file: string, data: any): Promise<void> {
    await apiService.request(`/jsonl/${file}/append`, {
      method: 'POST',
      body: JSON.stringify(data)
    });
  }

  async queryJSONL(file: string, predicate: (item: any) => boolean): Promise<any[]> {
    const data = await this.readJSONL(file);
    return data.filter(predicate);
  }

  async getR5RSFunction(name: string): Promise<any> {
    const response = await apiService.request(`/r5rs/functions/${name}`);
    return response.success ? response.data : null;
  }

  async listR5RSFunctions(pattern?: string): Promise<string[]> {
    const url = pattern 
      ? `/r5rs/functions?pattern=${encodeURIComponent(pattern)}`
      : '/r5rs/functions';
    const response = await apiService.request(url);
    return response.success ? response.data : [];
  }

  async invokeR5RSFunction(name: string, args: any[], context?: any): Promise<any> {
    const response = await apiService.request(`/r5rs/functions/${name}/invoke`, {
      method: 'POST',
      body: JSON.stringify({ args, context })
    });
    return response.success ? response.data : null;
  }

  async registerR5RSFunction(name: string, definition: any): Promise<void> {
    await apiService.request(`/r5rs/functions/${name}/register`, {
      method: 'POST',
      body: JSON.stringify(definition)
    });
  }

  async create(collection: string, data: any): Promise<string> {
    const response = await apiService.request(`/${collection}`, {
      method: 'POST',
      body: JSON.stringify(data)
    });
    return response.success ? response.data.id : '';
  }

  async read(collection: string, id: string): Promise<any> {
    const response = await apiService.request(`/${collection}/${id}`);
    return response.success ? response.data : null;
  }

  async update(collection: string, id: string, data: any): Promise<void> {
    await apiService.request(`/${collection}/${id}`, {
      method: 'PUT',
      body: JSON.stringify(data)
    });
  }

  async delete(collection: string, id: string): Promise<void> {
    await apiService.request(`/${collection}/${id}`, {
      method: 'DELETE'
    });
  }

  async query(collection: string, filter?: any, options?: QueryOptions): Promise<any[]> {
    const params = new URLSearchParams();
    if (filter) params.append('filter', JSON.stringify(filter));
    if (options?.limit) params.append('limit', options.limit.toString());
    if (options?.offset) params.append('offset', options.offset.toString());
    if (options?.sort) params.append('sort', JSON.stringify(options.sort));
    if (options?.projection) params.append('projection', options.projection.join(','));

    const url = `/${collection}${params.toString() ? '?' + params.toString() : ''}`;
    const response = await apiService.request(url);
    return response.success ? response.data : [];
  }
}

export const databaseService: DatabaseService = new DatabaseServiceImpl();
