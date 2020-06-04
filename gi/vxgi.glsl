float traceShadowCone(vec3 normal,
                      vec3 from,
                      vec3 to){
  const float aperture = tan(radians(5.0)),
    doubledAperture = max(VOXEL_INVERSE, 2.0 * aperture),
    s = 0.33333;
  from += normal * VOXEL_INVERSE * 2.0;
  vec3 direction = to - from;

  float maxDistance = length(direction),
    dist = 2.5 * VOXEL_INVERSE,
    accumulator = 0.0;
  direction /= maxDistance;
  maxDistance = min(maxDistance, 1.41421356237);
  dist += voxelJitterNoise(vec4(from.xyz + to.xyz + normal.xyz, tc.x)).x * s * VOXEL_INVERSE;
  vec3 position = from + (direction * dist);
  while((accumulator < 1.0) &&
        (dist < maxDistance) &&
        isInsideCube(position, 0.0)){

    float diameter = max(VOXEL_INVERSE * 0.5, doubledAperture * dist),
      mipMapLevel = max(0.0, log2((diameter * float(voxelVolumeSize)) + 1.0));

    accumulator += (1.0 - accumulator) *
      clamp(textureLod(VoxelVolumeTexture6,
                       position,
                       mipMapLevel).w * 1.0,
            0.0,
            1.0);
    dist += max(diameter, VOXEL_INVERSE) * s;
    position = from + (direction * dist);
  }
  return clamp(1.0 - accumulator, 0.0, 1.0);
}
