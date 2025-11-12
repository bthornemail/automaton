import { DimensionalAgent } from '../DimensionalAgent';
import { ContentLoader } from '../ContentLoader';

export class ConsensusAgent5D extends DimensionalAgent {
  constructor(contentLoader: ContentLoader) {
    super('5D', contentLoader);
    this.name = '5D-Consensus-Agent';
  }
}

