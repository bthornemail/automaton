import { DimensionalAgent } from '../DimensionalAgent';
import { ContentLoader } from '../ContentLoader';

export class StructuralAgent2D extends DimensionalAgent {
  constructor(contentLoader: ContentLoader) {
    super('2D', contentLoader);
    this.name = '2D-Structural-Agent';
  }
}

