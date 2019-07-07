
Code for https://www.youtube.com/watch?v=8ryjWDBnnJA

### Notes
- even disabled things get into a callback if they collide, so better put them appart when offscreen
- floor has body nil
- the logic put in defcallback kind of calls for a real event system... even so to have 1 thread that updates the global var. Instead of wanting to rely on svref/atomic
