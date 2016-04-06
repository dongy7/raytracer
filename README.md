# raytracer

Basic ray tracer implementing the Phong shading model with box filtering.

## Running
```
git clone https://github.com/dongy7/raytracer.git
cd raytracer
cabal run
```

## Performance
Testing done on a 2.3GHz Core i7 system

|Antialias| Time|
|---------|-----|
| 1x      | < 1s |
| 4x      | < 2s |
| 16x     | < 5s |
| 64x     | < 20s|
