# Compute bounding boxes for plot nodes based on text labels

This function measures label box sizes and convert to data units

## Usage

``` r
compute_node_boxes(
  nodes,
  label_col = "label",
  text_size = 16.9,
  size.unit = "pt",
  fontface = "bold",
  padding_cm = 0.3,
  xlim,
  ylim,
  plot_width_cm = 20,
  plot_height_cm = 12
)
```

## Arguments

- nodes:

  A data frame containing the nodes to be processed. Must include the
  column specified by \`label_col\`.

- label_col:

  Character string specifying the name of the column containing text
  labels for which boxes should be computed. Default is "label".

- text_size:

  Numeric value specifying the text size in points (or mm if
  \`size.unit\` is "mm"). Default is 16.9.

- size.unit:

  Character string specifying the unit for \`text_size\`. Supported
  values are "pt" (points) or "mm" (millimeters). Default is "pt".

- fontface:

  Character string specifying the font face. Default is "bold". Can also
  be "plain", "italic", or "bold.italic".

- padding_cm:

  Numeric value specifying the horizontal and vertical padding around
  the text in centimeters. Default is 0.3.

- xlim:

  Numeric vector of length 2 specifying the x-axis limits of the plot.

- ylim:

  Numeric vector of length 2 specifying the y-axis limits of the plot.

- plot_width_cm:

  Numeric value specifying the total plot width in centimeters. Default
  is 20.

- plot_height_cm:

  Numeric value specifying the total plot height in centimeters. Default
  is 12.

## Value

A data frame containing the original \`nodes\` data with additional
columns:

- box_w_cm:

  Box width in centimeters

- box_h_cm:

  Box height in centimeters

- half_w:

  Half box width in data units (for centering)

- half_h:

  Half box height in data units (for centering)
