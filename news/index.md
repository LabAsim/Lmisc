# Changelog

## Lmisc 0.1.3

- `scale_factor` replaces `gap`. You can now shrink the length of the
  curved or straight line by using scale_factor with values from 0 to 1.
  `scale_factor=1` means that the end and start points would be at the
  borders of the boxes. `scale_factor=0` means that the end & start
  points would be at the centers of the boxes.

## Lmisc 0.1.2

- `modify_parameter_estimates` converts to numeric wherever it can

## Lmisc 0.1.1

- Added function `modify_parameter_estimates`
- Added a vignette to present how to use `plot_dag` with `Lavaan` ouput
