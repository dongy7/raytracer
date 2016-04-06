module Math.SceneParams where
import Geometry.Vector

width::Int
width = 512

height::Int
height = 512

u :: Vector
u = (1, 0, 0)

v :: Vector
v = (0, 1, 0)

w :: Vector
w = (0, 0, 1)

e :: Vector
e = (0, 0, 0)

d :: Scalar
d = 0.1

l :: Scalar
l = -0.1

r :: Scalar
r = 0.1

b :: Scalar
b = -0.1

t :: Scalar
t = 0.1

intensity :: Scalar
intensity = 1.0

lightSource :: Vector
lightSource = (-4, 4, -3)

epsilon :: Scalar
epsilon = 0.0001

gamma :: Scalar
gamma = 2.2
