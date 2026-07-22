# Adjust edge start and end so they touch the box border

Modifies edge coordinate data so that connecting lines/arrows terminate
at the boundaries of node boxes rather than at their centers. This
prevents overlap between edge ends and node labels, improving visual
clarity in DAG (Directed Acyclic Graph) plots.

## Usage

``` r
adjust_edges_by_box(edges_df, nodes_df)
```

## Arguments

- edges_df:

  A data frame containing edge information with at minimum:

  from

  :   Character or integer identifier of the source node

  to

  :   Character or integer identifier of the target node

  curvature

  :   Numeric indicator; 1 for curved edges, 0 for straight

  Additional columns are preserved in the output.

- nodes_df:

  A data frame containing node information with required columns:

  node_id

  :   Unique identifier matching edges\$from and edges\$to

  x

  :   X-coordinate of node center

  y

  :   Y-coordinate of node center

  half_w

  :   Half-width of the node bounding box

  half_h

  :   Half-height of the node bounding box

  These values typically come from
  [`compute_node_boxes`](https://labasim.github.io/lmisc/reference/compute_node_boxes.md).

## Value

A data frame with the same columns as \`edges_df\` plus four new
columns:

- xstart_adj:

  Adjusted X-coordinate for edge start point

- ystart_adj:

  Adjusted Y-coordinate for edge start point

- xend_adj:

  Adjusted X-coordinate for edge end point

- yend_adj:

  Adjusted Y-coordinate for edge end point

- is_horizontal:

  Logical flag indicating nearly-horizontal lines

- is_vertical:

  Logical flag indicating nearly-vertical lines

Rows are reordered with regular straight lines first, followed by
horizontal, then curved edges.

## Details

The function performs several adjustments:

1.  Joins node position and box dimension data to each edge

2.  Uses geometric intersection to calculate precise edge-to-box
    connection points

3.  Applies special offsets for horizontal lines (reduces overlap)

4.  Applies different offsets for curved edges to improve visual
    aesthetics

This is a critical preprocessing step before rendering DAG plots, as it
ensures arrows visually connect to the perimeter of node boxes without
penetrating them. The function leverages
[`line_rect_intersection`](https://labasim.github.io/lmisc/reference/line_rect_intersection.md)
for accurate intersection calculations.

## Algorithm Details

\*\*General Case\*\*: Uses \`line_rect_intersection\` to find where the
line from node center to node center intersects the perimeter of each
node's bounding box.

\*\*Horizontal Lines\*\*: For efficiency and visual consistency, applies
a simplified offset (0.5 \* half-width) instead of full intersection
calculation. This keeps horizontal arrows visibly separated from both
nodes.

\*\*Curved Lines\*\*: Applies custom offsets (quarter width, full
height) to ensure curved edges emerge from appropriate positions on node
boundaries.

## Dependencies

This function requires:

- dplyr for data manipulation (left_join, mutate, dplyr::filter,
  bind_rows)

- [`line_rect_intersection`](https://labasim.github.io/lmisc/reference/line_rect_intersection.md)
  for geometric calculations

## See also

- [`compute_node_boxes`](https://labasim.github.io/lmisc/reference/compute_node_boxes.md) -
  Calculate node bounding box dimensions

- [`line_rect_intersection`](https://labasim.github.io/lmisc/reference/line_rect_intersection.md) -
  Find intersection of line and rectangle
