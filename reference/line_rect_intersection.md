# Find intersection point between a line and axis-aligned rectangle

Computes the intersection point(s) between a line segment (defined by
two endpoints) and an axis-aligned rectangle. Returns the intersection
point closest to the starting point of the line.

## Usage

``` r
line_rect_intersection(x0, y0, x1, y1, half_w, half_h, gap = 0)
```

## Arguments

- x0:

  Numeric. X-coordinate of the line's starting point.

- y0:

  Numeric. Y-coordinate of the line's starting point.

- x1:

  Numeric. X-coordinate of the line's ending point.

- y1:

  Numeric. Y-coordinate of the line's ending point.

- half_w:

  Numeric. Half-width of the rectangle (distance from center to
  left/right edge).

- half_h:

  Numeric. Half-height of the rectangle (distance from center to
  top/bottom edge).

- gap:

  Controls the gap between the line and target / source box. Positive
  values increases the line lenght, while negative shorten the line
  lenght. Zero does nothing (leaves the line as-is). Default = 0.

## Value

A numeric vector of length 2 with names "x" and "y", containing the
coordinates of the intersection point closest to the line's start point
(x0, y0). If no valid intersection is found (rare edge case), returns
the rectangle center (x1, y1).

## Details

The rectangle is specified by its center coordinates and half-width and
half-height dimensions. The algorithm checks intersections with all four
sides of the rectangle and dplyr::selects the point nearest to the
line's origin.

This function is commonly used in visualization tasks such as drawing
arrows or connecting lines between nodes that terminate at the boundary
of node bounding boxes rather than their centers.
