import { DimensionalAgent } from '../DimensionalAgent';
import { ContentLoader } from '../ContentLoader';

export class TemporalAgent1D extends DimensionalAgent {
  constructor(contentLoader: ContentLoader) {
    super('1D', contentLoader);
    this.name = '1D-Temporal-Agent';
  }
}

