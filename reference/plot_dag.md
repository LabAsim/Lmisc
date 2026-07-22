# Plots the dag based on the nodes and edges passed.

Plots the dag based on the nodes and edges passed.

## Usage

``` r
plot_dag(
  nodes,
  edges,
  label_size = 16.9,
  label_size_unit = "pt",
  label_border_size = 1,
  text_size = 7,
  xlim = c(0, 14),
  ylim = c(0, 8),
  plot_width_cm = 20,
  plot_height_cm = 12,
  footnote = NULL,
  footnote_size = 12
)
```

## Arguments

- nodes:

  A dataframe containing data from tidy_dagitty(): node_id, x, y, label

- edges:

  A dataframe containing data from tidy_dagitty()

- label_size:

  Set the label size of the text inside the nodes

- label_size_unit:

  Set the unit of the \`label_size\`

- label_border_size:

  Sets the boxes' border size

- text_size:

  Controls the text's size on top of the paths

- xlim:

  A vector of integers. Controls the limits of the plot

- ylim:

  A vector of integers. Controls the limits of the plot

- plot_width_cm:

  The width of the plot in cm

- plot_height_cm:

  The height of the plot in cm

- footnote:

  Sets the footnote

- footnote_size:

  Controls the size of the footnote text
