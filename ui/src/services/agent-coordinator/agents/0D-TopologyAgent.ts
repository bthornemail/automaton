import { DimensionalAgent } from '../DimensionalAgent';
import { ContentLoader } from '../ContentLoader';

export class TopologyAgent0D extends DimensionalAgent {
  constructor(contentLoader: ContentLoader) {
    super('0D', contentLoader);
    this.name = '0D-Topology-Agent';
  }
}

