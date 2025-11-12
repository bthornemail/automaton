// evaluation-vertex-shader.glsl
// Vertex shader for evaluation trace animations

uniform float evaluationTime;
uniform vec3 evaluationPosition;
uniform float evaluationScale;

attribute vec3 position;
attribute vec3 normal;

varying vec3 vPosition;
varying vec3 vNormal;

void main() {
  // Transform position based on evaluation time
  vec3 transformedPosition = position;
  
  // Add evaluation-based animation
  transformedPosition += sin(evaluationTime) * normal * 0.1;
  transformedPosition *= evaluationScale;
  transformedPosition += evaluationPosition;
  
  vPosition = transformedPosition;
  vNormal = normal;
  
  gl_Position = projectionMatrix * modelViewMatrix * vec4(transformedPosition, 1.0);
}

