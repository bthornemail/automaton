import { DimensionalAgent } from '../DimensionalAgent';
import { ContentLoader } from '../ContentLoader';

export class AlgebraicAgent3D extends DimensionalAgent {
  constructor(contentLoader: ContentLoader) {
    super('3D', contentLoader);
    this.name = '3D-Algebraic-Agent';
  }
}

