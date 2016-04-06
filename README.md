# raytracer

Basic ray tracer implementing the Phong shading model with box filtering.

## Running
Make sure your GHCI version is at least `7.10.2`.

```
git clone https://github.com/dongy7/raytracer.git
cd raytracer
cabal run
```

## Performance

###Runtime Environment
- OSX v10.10.5
- GHCI v7.10.3
- GLUT v2.7.0.3
- 2.3GHz Core i7 

|Antialias| Time|
|---------|-----|
| 1x      | < 1s |
| 4x      | < 2s |
| 16x     | < 5s |
| 64x     | < 20s|

## Result
<img src="/out/screenshot.png" width="400px" height="400px" />
