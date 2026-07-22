# Preprocess Edges DataFrame Ensures required edge attributes have default values when not provided.

Preprocess Edges DataFrame Ensures required edge attributes have default
values when not provided.

## Usage

``` r
preprocess_edges_df(edges_df)
```

## Arguments

- edges_df:

  A data frame containing edge information Expected columns include:

  from

  :   Node ID of the source node

  to

  :   Node ID of the target node

  curvature

  :   Curvature flag (0 for straight lines, 1 for curved)

  gap

  :   Optional gap offset from node borders (default: 0)

  pvalue

  :   Statistical significance value for styling

  est

  :   Effect estimate displayed on edge labels

  ci.lower

  :   Lower bound of confidence interval

  ci.upper

  :   Upper bound of confidence interval

## Value

A data frame identical to the input, but with missing optional columns
populated with sensible defaults. This ensures downstream plotting
functions receive complete edge data.

## Details

The following defaults are applied:

- `label_position`: Default 0.5 centers text on the edge.
