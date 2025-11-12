import { DimensionalAgent } from '../DimensionalAgent';
import { ContentLoader } from '../ContentLoader';

export class NetworkAgent4D extends DimensionalAgent {
  constructor(contentLoader: ContentLoader) {
    super('4D', contentLoader);
    this.name = '4D-Network-Agent';
  }
}

