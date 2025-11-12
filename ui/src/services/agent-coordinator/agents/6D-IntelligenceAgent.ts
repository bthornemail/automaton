import { DimensionalAgent } from '../DimensionalAgent';
import { ContentLoader } from '../ContentLoader';

export class IntelligenceAgent6D extends DimensionalAgent {
  constructor(contentLoader: ContentLoader) {
    super('6D', contentLoader);
    this.name = '6D-Intelligence-Agent';
  }
}

