// polynomial-fragment-shader.glsl
// Fragment shader for polynomial visualization with rings and combinator fields

uniform vec3 monadCoords;
uniform vec3 functorCoords; 
uniform vec3 perceptronCoords;
uniform float evaluationTime;
uniform vec3 yCombinator;
uniform vec3 zCombinator;
uniform float opacity;

varying vec3 vPosition;
varying vec3 vNormal;

void main() {
  // Polynomial rings as concentric spheres
  float monadRing = sin(length(vPosition - monadCoords) * 10.0 - evaluationTime);
  float functorRing = sin(length(vPosition - functorCoords) * 15.0 - evaluationTime * 1.5);
  float perceptronRing = sin(length(vPosition - perceptronCoords) * 20.0 - evaluationTime * 2.0);
  
  // Combinator field effects
  vec3 yField = normalize(vPosition - yCombinator);
  vec3 zField = normalize(vPosition - zCombinator);
  float combinatorEffect = dot(yField, zField);
  
  // Final color combines polynomial structure
  vec3 color = vec3(
    monadRing * 0.8 + combinatorEffect * 0.2,
    functorRing * 0.6 + combinatorEffect * 0.4, 
    perceptronRing * 0.7 + combinatorEffect * 0.3
  );
  
  // Normalize color
  color = normalize(color) * 0.5 + 0.5;
  
  gl_FragColor = vec4(color, opacity);
}

