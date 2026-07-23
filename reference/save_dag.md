# Save a DAG plot to file

The function opens the appropriate graphics device based on the file
extension (.tiff, .tif, or .png), renders the plot, and closes the
device to ensure the file is properly written. Resolution is fixed at
300 DPI for publication quality output, and dimensions are specified in
centimeters.

## Usage

``` r
save_dag(path, plot, width = 50, height = 25)
```

## Arguments

- path:

  Character string specifying the output file path, including the
  filename and extension (e.g., "figure.tiff" or "output.png").

- plot:

  A plot object to save. Must support printing via \`print()\` method
  (typically ggplot2 objects or grid grobs).

- width:

  Numeric value specifying the plot width. Default is 50.

- height:

  Numeric value specifying the plot height. Default is 25.

## Details

This is a convenience wrapper around R's base graphics devices (tiff and
png) that standardizes resolution settings to 300 DPI, which is suitable
for publication-quality figures. Dimensions are specified in centimeters
for consistency with typical figure sizing conventions.

## Examples

``` r
# Basic usage - save a ggplot as TIFF (extension determines format)
library(ggplot2)
p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
save_dag("my_plot.tiff", p, width = 20, height = 15)
#> agg_record_1aef32063771 
#>                       2 

# Save as PNG automatically from extension
save_dag("my_plot.png", p, width = 20, height = 15)
#> agg_record_1aef32063771 
#>                       2 
```
