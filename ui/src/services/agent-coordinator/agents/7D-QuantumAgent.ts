import { DimensionalAgent } from '../DimensionalAgent';
import { ContentLoader } from '../ContentLoader';

export class QuantumAgent7D extends DimensionalAgent {
  constructor(contentLoader: ContentLoader) {
    super('7D', contentLoader);
    this.name = '7D-Quantum-Agent';
  }
}

