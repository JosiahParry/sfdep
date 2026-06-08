# Graph based neighbors

Create graph based neighbors on a set of points.

## Usage

``` r
st_nb_delaunay(geometry, .id = NULL)

st_nb_gabriel(geometry, .nnmult = 3)

st_nb_relative(geometry, .nnmult = 3)
```

## Arguments

- geometry:

  an object of class sfc. If polygons are used, points are generated
  using
  [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

- .id:

  default `NULL`. Passed as `spdep::tri2nb(x, row.names = .id)` to
  `spdep`.

- .nnmult:

  default 3. Used for memory scalling. See
  [`spdep::gabrielneigh()`](https://r-spatial.github.io/spdep/reference/graphneigh.html)
  for more.

## Value

an object of class nb

## Details

- `st_nb_delaunay()` uses
  [`spdep::tri2nb()`](https://r-spatial.github.io/spdep/reference/tri2nb.html)

- `st_nb_gabriel()` uses
  [`spdep::gabrielneigh()`](https://r-spatial.github.io/spdep/reference/graphneigh.html)
  and
  [`spdep::graph2nb()`](https://r-spatial.github.io/spdep/reference/graphneigh.html)

- `st_nb_relative()` uses
  [`spdep::relativeneigh()`](https://r-spatial.github.io/spdep/reference/graphneigh.html)
  and
  [`spdep::graph2nb()`](https://r-spatial.github.io/spdep/reference/graphneigh.html)

`st_nb_delaunay()` implements Delaunay triangulation via `spdep` and
thus via `deldir`. Delaunay triangulation creates a mesh of triangles
that connects all points in a set. It ensures that no point is in in the
circumcircle of an triangle in the triangulation. As a result, Delaunay
triangulation maximizes the minimum angle in each triangle consequently
avoiding skinny triangles.

The Gabriel graph is a subgraph of the Delaunay triangulation. Edges are
created when the closed disc between two points p, and q, contain no
other points besides themselves.

The relative neighborhood graph (RNG) is based on the Delaunay
triangulation. It connects two points when there are no other closer
points to each of them. The RNG is a subgraph of the Delaunay
triangulation.

Note that Delaunay triangulation assumes a plane and thus uses Euclidean
distances.

See
[`spdep::gabrielneigh()`](https://r-spatial.github.io/spdep/reference/graphneigh.html)
for further descriptions of the graph neighbor implementations.

## Examples

``` r
geometry <- sf::st_centroid(sf::st_geometry(guerry))
st_nb_delaunay(geometry)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 488 
#> Percentage nonzero weights: 6.754325 
#> Average number of links: 5.741176 
st_nb_gabriel(geometry)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 194 
#> Percentage nonzero weights: 2.685121 
#> Average number of links: 2.282353 
#> 16 regions with no links:
#> 46, 53, 54, 61, 63, 64, 69, 70, 74, 76, 78, 80, 81, 83, 84, 85
#> Non-symmetric neighbours list
st_nb_relative(geometry)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 122 
#> Percentage nonzero weights: 1.688581 
#> Average number of links: 1.435294 
#> 23 regions with no links:
#> 27, 32, 36, 37, 46, 48, 53, 54, 61, 63, 64, 69, 70, 73, 74, 76, 78, 79,
#> 80, 81, 83, 84, 85
#> Non-symmetric neighbours list
```
