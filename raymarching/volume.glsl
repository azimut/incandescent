#version 130

//pull the ray in from the vertex shader
inout vec3 rayOrigin;
inout vec3 rayDirection;

//uniforms needed to run
uniform float voxelResolution = 128.0; //number of voxels in any dimension
uniform vec3 boxMin = {-1.0, -1.0, -1.0};
uniform vec3 boxMax = {1.0, 1.0, 1.0};
uniform sampler3D textureVolume; // our bound texture
uniform float textureDensity = 0.5; //the density

vec3 rayDirNorm; //normalized ray direction

//our outs
out vec4 out_Color;

bool CheckBoxIntersection(vec3 boxmin,
                          vec3 boxmax,
                          out float tnear,
                          out float tfar) {

  vec3 invR = 1.0 / rayDirNorm;
  vec3 tbot = invR * (boxmin.xyz - rayOrigin);
  vec3 ttop = invR * (boxmax.xyz - rayOrigin);

  //re-order intersections to find smallest and largest on each axis
  vec3 tmin = min(ttop, tbot);
  vec3 tmax = max(ttop, tbot);

  //find the largest tmin and the smallest tmax
  vec2 t0 = max(tmin.xx, tmin.yz);
  float largest_tmin = max(t0.x, t0.y);
  t0 = min(tmax.xx, tmax.yz);
  float smallest_tmax = min(t0.x, t0.y);

  //check for a hit
  bool hit;
  if((largest_tmin > smallest_tmax)) {
    hit = false;
  }
  else {
    hit = true;
  }
  tnear = largest_tmin;
  tfar = smallest_tmax;
  return hit;
}



void main()

{
  //stepsize = half the size of a voxel
  float stepSize = 1.0 / (voxelResolution*2);

  //normalize our ray direction now that it's been interpolated
  rayDirNorm = normalize(rayDirection);

  //calculate ray intersection with bounding box
  float tnear, tfar;
  bool hit = CheckBoxIntersection(boxMin,
                                  boxMax,
                                  tnear,
                                  tfar);
  if(!hit) discard;
  if(tnear < 0.0) tnear = 0.0;

  //calculate intersection points
  vec3 Pnear = rayOrigin + rayDirNorm*tnear;
  vec3 Pfar = rayOrigin + rayDirNorm*tfar;

  //convert to texture coords
  Pnear = Pnear * 0.5 + 0.5;
  Pfar = Pfar * 0.5 + 0.5;

  //march the ray
  vec4 colorAcc = vec4(0,0,0,0);

  //using front to back rendering
  vec3 P = Pnear;
  vec3 Pstep = rayDirNorm * stepSize;

  for(int i = 0; i < voxelResolution*2; i++) {
    vec4 s = texture(textureVolume, P);
    s = abs(s);
    s.a = clamp(s.a, 0, 1);
    s.a *= textureDensity;
    s.rgb *= s.a; //premultiply the alpha
    colorAcc = (1.0 - colorAcc.a)*s + colorAcc;
    //early exit if it's opaque
    if(colorAcc.a > 0.99) {
      break;
    }
    P += Pstep;
  }
  out_Color = colorAcc;
}


