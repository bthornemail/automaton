// combinator-shader.glsl
// Shader for combinator field visualization

uniform vec3 yCombinator;
uniform vec3 zCombinator;
uniform vec3 mCombinator;
uniform vec3 sCombinator;
uniform float fieldStrength;

varying vec3 vPosition;
varying vec3 vNormal;

void main() {
  // Calculate distances to combinators
  float distY = length(vPosition - yCombinator);
  float distZ = length(vPosition - zCombinator);
  float distM = length(vPosition - mCombinator);
  float distS = length(vPosition - sCombinator);
  
  // Field effects (inverse square law)
  float fieldY = fieldStrength / (distY * distY + 1.0);
  float fieldZ = fieldStrength / (distZ * distZ + 1.0);
  float fieldM = fieldStrength / (distM * distM + 1.0);
  float fieldS = fieldStrength / (distS * distS + 1.0);
  
  // Combine fields
  float totalField = fieldY + fieldZ + fieldM + fieldS;
  
  // Color based on field strength
  vec3 color = vec3(
    fieldY * 0.5 + fieldM * 0.3,
    fieldZ * 0.5 + fieldS * 0.3,
    totalField * 0.2
  );
  
  // Normalize and apply
  color = normalize(color) * min(1.0, totalField);
  
  gl_FragColor = vec4(color, 0.8);
}

