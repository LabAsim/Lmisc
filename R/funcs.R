# Resolve R CMD check NOTE: no visible binding for global variable
utils::globalVariables(
  c(
    "x", "y", "label", "half_w", "half_h",
    "x_from", "y_from", "x_to", "y_to",
    "half_w_from", "half_h_from", "half_w_to", "half_h_to",
    "is_vertical", "is_horizontal",
    "goes_up", "goes_down", "goes_right", "goes_left",
    "box_w_cm", "box_h_cm",
    "curvature", "curvature_amount",
    "pvalue", "est", "ci.lower", "ci.upper",
    "hjust", "vjust", "label_position",
    "xstart_adj", "ystart_adj", "xend_adj", "yend_adj",
    "row_id", "gap"
  )
)


#' Save a DAG plot to file
#'
#' The function opens the appropriate graphics device based on the file extension
#' (.tiff, .tif, or .png), renders the plot, and closes the device to ensure
#' the file is properly written. Resolution is fixed at 300 DPI for publication
#' quality output, and dimensions are specified in centimeters.
#'
#' This is a convenience wrapper around R's base graphics devices (tiff and png)
#' that standardizes resolution settings to 300 DPI, which is suitable for
#' publication-quality figures. Dimensions are specified in centimeters for
#' consistency with typical figure sizing conventions.
#' @param path Character string specifying the output file path, including the
#'   filename and extension (e.g., "figure.tiff" or "output.png").
#' @param plot A plot object to save. Must support printing via `print()` method
#'   (typically ggplot2 objects or grid grobs).
#' @param width Numeric value specifying the plot width. Default is 50.
#' @param height Numeric value specifying the plot height. Default is 25.
#' @examples
#' # Basic usage - save a ggplot as TIFF (extension determines format)
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' save_dag("my_plot.tiff", p, width = 20, height = 15)
#'
#' # Save as PNG automatically from extension
#' save_dag("my_plot.png", p, width = 20, height = 15)
#'
#' @importFrom grDevices dev.off png tiff
#' @export
save_dag <- function(path, plot, width = 50, height = 25) {
  # Validation to check that the path matches the specified type
  ext <- tolower(tools::file_ext(path))
  if (ext == "tiff") {
    type <- "tiff"
  } else if (ext == "png") {
    type <- "png"
  } else {
    stop("Unsupported file extension '", ext, "'. Use .tiff or .png")
  }
  if (type == "tiff") {
    # Alternative agg_tiff() for better graphics
    tiff(
      path,
      width = width,
      height = height,
      units = "cm",
      res = 300
    )
  } else if (type == "png") {
    png(
      path,
      width = width,
      height = height,
      units = "cm",
      res = 300
    )
  } else {
    stop("Type not supported")
  }
  print(plot)
  dev.off()
}


# -------------------------------------------------
# Measure label box sizes and convert to data units
# -------------------------------------------------
#' Compute bounding boxes for plot nodes based on text labels
#'
#' This function measures label box sizes and convert to data units
#' @param nodes A data frame containing the nodes to be processed. Must include
#'   the column specified by `label_col`.
#' @param label_col Character string specifying the name of the column containing
#'   text labels for which boxes should be computed. Default is "label".
#' @param text_size Numeric value specifying the text size in points (or mm if
#'   `size.unit` is "mm"). Default is 16.9.
#' @param size.unit Character string specifying the unit for `text_size`. Supported
#'   values are "pt" (points) or "mm" (millimeters). Default is "pt".
#' @param fontface Character string specifying the font face. Default is "bold".
#'   Can also be "plain", "italic", or "bold.italic".
#' @param padding_cm Numeric value specifying the horizontal and vertical padding
#'   around the text in centimeters. Default is 0.3.
#' @param xlim Numeric vector of length 2 specifying the x-axis limits of the plot.
#' @param ylim Numeric vector of length 2 specifying the y-axis limits of the plot.
#' @param plot_width_cm Numeric value specifying the total plot width in centimeters.
#'   Default is 20.
#' @param plot_height_cm Numeric value specifying the total plot height in centimeters.
#'   Default is 12.
#'
#' @return A data frame containing the original `nodes` data with additional columns:
#'   \describe{
#'     \item{box_w_cm}{Box width in centimeters}
#'     \item{box_h_cm}{Box height in centimeters}
#'     \item{half_w}{Half box width in data units (for centering)}
#'     \item{half_h}{Half box height in data units (for centering)}
#'   }
#'
#' @importFrom dplyr bind_rows bind_cols mutate
#' @importFrom grid textGrob grobWidth grobHeight convertWidth convertHeight gpar
#'
#' @export
compute_node_boxes <- function(
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
) {
  fontsize_pt <- if (size.unit == "pt") {
    text_size
  } else if (size.unit == "mm") {
    text_size / (25.4 / 72) # 1 pt = 25.4/72 ≈ 0.3528 mm
  } else {
    stop("Only 'pt' and 'mm' are supported.")
  }

  dims <- lapply(nodes[[label_col]], function(lbl) {
    tg <- grid::textGrob(
      lbl,
      gp = grid::gpar(fontsize = fontsize_pt, fontface = fontface)
    )

    text_w_cm <- grid::convertWidth(grid::grobWidth(tg), "cm", valueOnly = TRUE)
    text_h_cm <- grid::convertHeight(
      grid::grobHeight(tg),
      "cm",
      valueOnly = TRUE
    )

    data.frame(
      box_w_cm = text_w_cm + 2 * padding_cm,
      box_h_cm = text_h_cm + 2 * padding_cm
    )
  })

  dims <- dplyr::bind_rows(dims)

  x_data_per_cm <- diff(xlim) / plot_width_cm
  y_data_per_cm <- diff(ylim) / plot_height_cm

  nodes |>
    dplyr::bind_cols(dims) |>
    dplyr::mutate(
      half_w = (box_w_cm * x_data_per_cm) / 2,
      half_h = (box_h_cm * y_data_per_cm) / 2
    )
}

#' Find intersection point between a line and axis-aligned rectangle
#'
#' Computes the intersection point(s) between a line segment (defined by two
#' endpoints) and an axis-aligned rectangle. Returns the intersection point
#' closest to the starting point of the line.
#'
#' The rectangle is specified by its center coordinates and half-width
#' and half-height dimensions. The algorithm checks intersections with all four
#' sides of the rectangle and dplyr::selects the point nearest to the line's origin.
#'
#' This function is commonly used in visualization tasks such as drawing arrows
#' or connecting lines between nodes that terminate at the boundary of node
#' bounding boxes rather than their centers.
#'
#' @param x0 Numeric. X-coordinate of the line's starting point.
#' @param y0 Numeric. Y-coordinate of the line's starting point.
#' @param x1 Numeric. X-coordinate of the line's ending point.
#' @param y1 Numeric. Y-coordinate of the line's ending point.
#' @param half_w Numeric. Half-width of the rectangle (distance from center to
#'   left/right edge).
#' @param half_h Numeric. Half-height of the rectangle (distance from center to
#'   top/bottom edge).
#' @param gap Controls the gap between the line and target / source box.
#'   Positive values increases the line lenght, while negative shorten the line
#'   lenght. Zero does nothing (leaves the line as-is). Default = 0.
#'
#' @return A numeric vector of length 2 with names "x" and "y", containing the
#'   coordinates of the intersection point closest to the line's start point
#'   (x0, y0). If no valid intersection is found (rare edge case), returns the
#'   rectangle center (x1, y1).
#'
line_rect_intersection <- function(
    x0, y0, x1, y1, half_w, half_h, gap = 0
    ) {
  # Rectangle bounds
  xmin <- x1 - half_w
  xmax <- x1 + half_w
  ymin <- y1 - half_h
  ymax <- y1 + half_h
  # Line directions
  dx <- x1 - x0
  dy <- y1 - y0
  line_length <- sqrt(dx^2 + dy^2)

  # Normalized direction vector (unit vector)
  if (line_length > 0) {
    ux <- dx / line_length
    uy <- dy / line_length
  } else {
    ux <- 0
    uy <- 0
  }

  candidates <- list()

  # Iersect with vertical sides: x = xmin, x = xmax
  if (dx != 0) {
    # Left side
    t_left <- (xmin - x0) / dx
    y_left <- y0 + t_left * dy
    if (y_left >= ymin && y_left <= ymax) {
      candidates[[length(candidates) + 1]] <- c(x = xmin, y = y_left)
    }

    # R side
    t_right <- (xmax - x0) / dx
    y_right <- y0 + t_right * dy
    if (y_right >= ymin && y_right <= ymax) {
      candidates[[length(candidates) + 1]] <- c(x = xmax, y = y_right)
    }
  }

  # Interse with horizontal sides: y = ymin, y = ymax
  if (dy != 0) {
    # Bottom
    t_bottom <- (ymin - y0) / dy
    x_bottom <- x0 + t_bottom * dx
    if (x_bottom >= xmin && x_bottom <= xmax) {
      candidates[[length(candidates) + 1]] <- c(x = x_bottom, y = ymin)
    }

    # Top
    t_top <- (ymax - y0) / dy
    x_top <- x0 + t_top * dx
    if (x_top >= xmin && x_top <= xmax) {
      candidates[[length(candidates) + 1]] <- c(x = x_top, y = ymax)
    }
  }

  # If no interseions found (shouldn't really happen), return center
  if (length(candidates) == 0) {
    return(c(x = x1, y = y1))
  }

  # Pick the interstion closest to (x0, y0)
  dists <- sapply(candidates, function(pt) {
    (pt["x"] - x0)^2 + (pt["y"] - y0)^2
  })

  pt <- candidates[[which.min(dists)]]

  # Apply gap offset alg the line direction
  # Positive gap: moves FORWARD along line  το source and target boxes)
  # Negative gap: moves away from source and target boxes
  pt["x"] <- pt["x"] + ux #* gap
  pt["y"] <- pt["y"] + uy #* gap

  # Vertical lines
  # if (dy != 0 && dx==0){
  #   if (dy > 0){
  #     print(paste(dx, ux, dy, uy))
  #     pt["x"] <- pt["x"] - dx * gap
  #     pt["y"] <- pt["y"] - uy * gap
  #   } else {
  #     print(paste(dx, ux, dy, uy))
  #     pt["x"] <- pt["x"] + dx * gap
  #     pt["y"] <- pt["y"] + uy * gap
  #   }
  # }
  # attr(pt, "dx") <- dx
  # attr(pt, "dy") <- dx
  # attr(pt, "ux") <- ux
  # attr(pt, "uy") <- uy
  return(pt)
}

#' Adjust edge start and end so they touch the box border
#'
#' Modifies edge coordinate data so that connecting lines/arrows terminate at
#' the boundaries of node boxes rather than at their centers. This prevents
#' overlap between edge ends and node labels, improving visual clarity in DAG
#' (Directed Acyclic Graph) plots.
#'
#' The function performs several adjustments:
#' \enumerate{
#'   \item Joins node position and box dimension data to each edge
#'   \item Uses geometric intersection to calculate precise edge-to-box connection points
#'   \item Applies special offsets for horizontal lines (reduces overlap)
#'   \item Applies different offsets for curved edges to improve visual aesthetics
#' }
#'
#' This is a critical preprocessing step before rendering DAG plots, as it ensures
#' arrows visually connect to the perimeter of node boxes without penetrating
#' them. The function leverages \code{\link{line_rect_intersection}} for accurate
#' intersection calculations.
#'
#' @param edges_df A data frame containing edge information with at minimum:
#'   \describe{
#'     \item{from}{Character or integer identifier of the source node}
#'     \item{to}{Character or integer identifier of the target node}
#'     \item{curvature}{Numeric indicator; 1 for curved edges, ≠ 1 for straight}
#'   }
#'   Additional columns are preserved in the output.
#' @param nodes_df A data frame containing node information with required columns:
#'   \describe{
#'     \item{node_id}{Unique identifier matching edges$from and edges$to}
#'     \item{x}{X-coordinate of node center}
#'     \item{y}{Y-coordinate of node center}
#'     \item{half_w}{Half-width of the node bounding box}
#'     \item{half_h}{Half-height of the node bounding box}
#'   }
#'   These values typically come from \code{\link{compute_node_boxes}}.
#'
#' @return A data frame with the same columns as `edges_df` plus four new columns:
#'   \describe{
#'     \item{xstart_adj}{Adjusted X-coordinate for edge start point}
#'     \item{ystart_adj}{Adjusted Y-coordinate for edge start point}
#'     \item{xend_adj}{Adjusted X-coordinate for edge end point}
#'     \item{yend_adj}{Adjusted Y-coordinate for edge end point}
#'     \item{is_horizontal}{Logical flag indicating nearly-horizontal lines}
#'     \item{is_vertical}{Logical flag indicating nearly-vertical lines}
#'   }
#'   Rows are reordered with regular straight lines first, followed by horizontal,
#'   then curved edges.
#'
#' @section Algorithm Details:
#'
#' **General Case**: Uses `line_rect_intersection` to find where the line from node
#' center to node center intersects the perimeter of each node's bounding box.
#'
#' **Horizontal Lines**: For efficiency and visual consistency, applies a simplified
#' offset (0.5 × half-width) instead of full intersection calculation. This keeps
#' horizontal arrows visibly separated from both nodes.
#'
#' **Curved Lines**: Applies custom offsets (quarter width, full height) to ensure
#' curved edges emerge from appropriate positions on node boundaries.
#'
#' @section Dependencies:
#' This function requires:
#' \itemize{
#'   \item \pkg{dplyr} for data manipulation (left_join, mutate, dplyr::filter, bind_rows)
#'   \item \code{\link{line_rect_intersection}} for geometric calculations
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{compute_node_boxes}} - Calculate node bounding box dimensions
#'   \item \code{\link{line_rect_intersection}} - Find intersection of line and rectangle
#' }
#'
#' @importFrom dplyr left_join
#'

adjust_edges_by_box <- function(edges_df, nodes_df) {
  # Check required columns exist
  required_nodes_cols <- c("node_id", "x", "y", "half_w", "half_h")
  if (!all(required_nodes_cols %in% names(nodes_df))) {
    stop(
      "nodes_df missing required columns: ",
      paste(setdiff(required_nodes_cols, names(nodes_df)), collapse = ", ")
    )
  }

  required_edges_cols <- c("from", "to", "curvature")
  if (!all(required_edges_cols %in% names(edges_df))) {
    stop(
      "edges_df missing required columns: ",
      paste(setdiff(required_edges_cols, names(edges_df)), collapse = ", ")
    )
  }

  out <- edges_df |>
    left_join(
      nodes_df |>
        dplyr::select(
          all_of(
            c("node_id", "x", "y", "half_w", "half_h")
          )
        )|>
        dplyr::rename(
          x_from = x,
          y_from = y,
          half_w_from = half_w,
          half_h_from = half_h
        ),
      by = c("from" = "node_id")
    ) |>
    left_join(
      nodes_df |>
        dplyr::select(
          all_of(
            c("node_id", "x", "y", "half_w", "half_h")
          )
        )|>
        dplyr::rename(
          x_to = x,
          y_to = y,
          half_w_to = half_w,
          half_h_to = half_h
        ),
      by = c("to" = "node_id")
    )

  # Find horizontal and vertical lines for all edges
  out <- out |>
    dplyr::mutate(
      is_horizontal = abs(y_from - y_to) < 1e-1,
      is_vertical = abs(x_from - x_to) < 1e-1,
      goes_up = y_to > y_from,
      goes_down = y_to < y_from,
      goes_right = x_to > x_from,
      goes_left = x_to < x_from
    )

  # At the start, preserve original row indices
  out$row_id <- seq_len(nrow(out))

  # Separate curved and straight edges
  curved_idx <- which(out$curvature == 1)
  straight_idx <- which(out$curvature == 0)

  # Initialize adjusted coordinates with baseline
  xstart_adj <- rep(NA_real_, nrow(out))
  ystart_adj <- rep(NA_real_, nrow(out))
  xend_adj <- rep(NA_real_, nrow(out))
  yend_adj <- rep(NA_real_, nrow(out))

  ##################
  # Straight paths #
  ##################
  if (length(straight_idx) > 0) {
    # Vertical
    vertical_straight <- out[straight_idx, ] |>
      dplyr::filter(is_vertical == T, is_horizontal == F)

    if (nrow(vertical_straight) > 0) {
      for (i in seq_len(nrow(vertical_straight))) {
        row <- vertical_straight[i, ]
        idx <- row$row_id
        # gap_val <- ifelse(is.na(row$gap), 1, row$gap)  # Default to 1 if NA

        if (row$gap == 1) {
          # Do nothing - use border-to-border (current behavior)
          start_list <- mapply(
            FUN = line_rect_intersection,
            x0 = row$x_to,
            y0 = row$y_to,
            x1 = row$x_from,
            y1 = row$y_from,
            half_w = row$half_w_from,
            half_h = row$half_h_from,
            gap = 0,  # No extra gap from border
            SIMPLIFY = FALSE
          )
          start_pts <- do.call(rbind, start_list)

          end_list <- mapply(
            FUN = line_rect_intersection,
            x0 = row$x_from,
            y0 = row$y_from,
            x1 = row$x_to,
            y1 = row$y_to,
            half_w = row$half_w_to,
            half_h = row$half_h_to,
            gap = 0,  # No extra gap from border
            SIMPLIFY = FALSE
          )
          end_pts <- do.call(rbind, end_list)
          xstart_adj[idx] <- start_pts[, "x"]
          ystart_adj[idx] <- start_pts[, "y"]
          xend_adj[idx] <- end_pts[, "x"]
          yend_adj[idx] <- end_pts[, "y"]
        } else if (row$gap == 0) {
          # Start and end from boxes' centers
          xstart_adj[idx] <- row$x_from
          ystart_adj[idx] <- row$y_from
          xend_adj[idx] <- row$x_to
          yend_adj[idx] <- row$y_to
        } else if (row$gap < 0) {
          # Shrink from start by starting box's half_height
          # Shrink from end by ending box's half_height
          xstart_adj[idx] <- row$x_from
          ystart_adj[idx] <- row$y_from + sign(row$y_to - row$y_from) * row$half_h_from * abs(row$gap)
          xend_adj[idx] <- row$x_to
          yend_adj[idx] <- row$y_to + sign(row$y_from - row$y_to) * row$half_h_to * abs(row$gap)
        }
      }
    }

    # Horizontal
    horizontal_straight <- out[straight_idx, ] |>
      dplyr::filter(is_horizontal == T, is_vertical == F)

    if (nrow(horizontal_straight) > 0) {
      for (i in seq_len(nrow(horizontal_straight))) {
        row <- horizontal_straight[i, ]
        idx <- row$row_id
        xstart_adj[idx] <- row$x_from + sign(row$x_to - row$x_from) * row$half_w_from * abs(row$gap)
        ystart_adj[idx] <- row$y_from
        xend_adj[idx] <- row$x_to + sign(row$x_from - row$x_to) * row$half_w_to * abs(row$gap)
        yend_adj[idx] <- row$y_to
        # if (row$goes_right) {
        #   xstart_adj[idx] <- row$x_from + sign(row$x_to - row$x_from) * row$half_w_from * abs(row$gap)
        #   ystart_adj[idx] <- row$y_from
        #   xend_adj[idx] <- row$x_to + sign(row$x_from - row$x_to) * row$half_w_to * abs(row$gap)
        #   yend_adj[idx] <- row$y_to
        # } else if (row$goes_left) {
        #   xstart_adj[idx] <- row$x_from + sign(row$x_to - row$x_from) * row$half_w_from * abs(row$gap)
        #   ystart_adj[idx] <- row$y_from
        #   xend_adj[idx] <- row$x_to + sign(row$x_from - row$x_to) * row$half_w_to * abs(row$gap)
        #   yend_adj[idx] <- row$y_to
        # }
      }
    }

    # Diagonal
    diagonal_straight <- out[straight_idx, ] |>
      dplyr::filter(is_horizontal == F, is_vertical == F)

    if (nrow(diagonal_straight) > 0) {
      for (i in seq_len(nrow(diagonal_straight))) {
        row <- diagonal_straight[i, ]
        idx <- row$row_id

        # Calculate half-diagonal for each box
        diag_half_from <- sqrt(row$half_w_from^2 + row$half_h_from^2)
        diag_half_to <- sqrt(row$half_w_to^2 + row$half_h_to^2)

        # Calculate direction unit vector
        dx <- row$x_to - row$x_from
        dy <- row$y_to - row$y_from
        line_length <- sqrt(dx^2 + dy^2)
        if (line_length > 0) {
          ux <- dx / line_length
          uy <- dy / line_length
        } else {
          ux <- 0
          uy <- 0
        }
        if (row$gap == 1) {
          # Do nothing - use border-to-border (current behavior)
          start_list <- mapply(
            FUN = line_rect_intersection,
            x0 = row$x_to,
            y0 = row$y_to,
            x1 = row$x_from,
            y1 = row$y_from,
            half_w = row$half_w_from,
            half_h = row$half_h_from,
            gap = 0,
            SIMPLIFY = FALSE
          )
          start_pts <- do.call(rbind, start_list)

          end_list <- mapply(
            FUN = line_rect_intersection,
            x0 = row$x_from,
            y0 = row$y_from,
            x1 = row$x_to,
            y1 = row$y_to,
            half_w = row$half_w_to,
            half_h = row$half_h_to,
            gap = 0,
            SIMPLIFY = FALSE
          )
          end_pts <- do.call(rbind, end_list)
          xstart_adj[idx] <- start_pts[, "x"]
          ystart_adj[idx] <- start_pts[, "y"]
          xend_adj[idx] <- end_pts[, "x"]
          yend_adj[idx] <- end_pts[, "y"]
        } else if (row$gap == 0) {
          # Start and end from boxes' centers
          xstart_adj[idx] <- row$x_from
          ystart_adj[idx] <- row$y_from
          xend_adj[idx] <- row$x_to
          yend_adj[idx] <- row$y_to
        } else if (row$gap < 0) {
          # Shrink from start by starting box's half_diagonal
          # Shrink from end by ending box's half_diagonal
          xstart_adj[idx] <- row$x_from + ux * diag_half_from * abs(row$gap)
          ystart_adj[idx] <- row$y_from + uy * diag_half_from * abs(row$gap)
          xend_adj[idx] <- row$x_to - ux * diag_half_to * abs(row$gap)
          yend_adj[idx] <- row$y_to - uy * diag_half_to * abs(row$gap)
        }
      }
    }
  }

  ################
  # Curved paths #
  ################
  if (length(curved_idx) > 0) {
    curved <- out[curved_idx, ]

    for (i in seq_len(nrow(curved))) {
      row <- curved[i, ]

      # Positive curvature
      if (row$curvature_amount > 0) {
        # Vertical alignment (same X)
        if (row$is_vertical && row$goes_down) {
          row$xstart_adj <- row$x_from - row$half_w_from # Left side
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to - row$half_w_to # Left side
          row$yend_adj <- row$y_to
        }
        if (row$is_vertical && row$goes_up) {
          # Vertical  + up
          row$xstart_adj <- row$x_from + row$half_w_from # right side
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to + row$half_w_to # right side
          row$yend_adj <- row$y_to
          # Horizontal alignment (same Y)
        } else if (row$is_horizontal && row$goes_right) {
          row$xstart_adj <- row$x_from
          row$ystart_adj <- row$y_from - row$half_h_from # Top of source
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to - row$half_h_to # Top of target
        } else if (row$is_horizontal && row$goes_left) {
          row$xstart_adj <- row$x_from
          row$ystart_adj <- row$y_from + row$half_h_from # Top of source
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to + row$half_h_to # Top of target

          # Upwards + Right (↗️)
        } else if (row$goes_up && row$goes_right) {
          row$xstart_adj <- row$x_from + row$half_w_from # Right of source
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to - row$half_h_to # Bottom of target

          # Upwards + Left (↖️)
        } else if (row$goes_up && row$goes_left) {
          row$xstart_adj <- row$x_from - row$half_w_from # Left of source
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to - row$half_h_to # Bottom of target

          # Downwards + Right (↘️)
        } else if (row$goes_down && row$goes_right) {
          row$xstart_adj <- row$x_from
          row$ystart_adj <- row$y_from - row$half_h_from # Bottom of source
          row$xend_adj <- row$x_to - row$half_w_to # left of source
          row$yend_adj <- row$y_to
          # Downwards + Left (↙️)
        } else if (row$goes_down && row$goes_left) {
          row$xstart_adj <- row$x_from - row$half_w_from # left of source
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to + row$half_h_to # Top of target
        }
      } else {
        ######################
        # Negative curvature #
        ######################

        # Vertical alignment (same X)
        if (row$is_vertical && row$goes_down) {
          row$xstart_adj <- row$x_from + row$half_w_from # Right side
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to + row$half_w_to # Right side
          row$yend_adj <- row$y_to
        } else if (row$is_vertical && row$goes_up) {
          row$xstart_adj <- row$x_from - row$half_w_from # left side
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to - row$half_w_to # left side
          row$yend_adj <- row$y_to
        } else if (row$is_horizontal && row$goes_left) {
          # Horizontal alignment (same Y) + left
          row$xstart_adj <- row$x_from
          row$ystart_adj <- row$y_from - row$half_h_from # Bottom of source
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to - row$half_h_to # bottom of target
        } else if (row$is_horizontal && row$goes_right) {
          # Horizontal and right
          row$xstart_adj <- row$x_from
          row$ystart_adj <- row$y_from + row$half_h_from # Top of source
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to + row$half_h_to # top of target

          # Upwards + Right (↗️)
        } else if (row$goes_up && row$goes_right) {
          row$xstart_adj <- row$x_from + row$half_w_from # Right of source
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to - row$half_h_to # Bottom of target

          # Upwards + Left (↖️)
        } else if (row$goes_up && row$goes_left) {
          row$xstart_adj <- row$x_from - row$half_w_from # Left of source
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to - row$half_h_to # Bottom of target

          # Downwards + Right (↘️)
        } else if (row$goes_down && row$goes_right) {
          row$xstart_adj <- row$x_from + row$half_w_from # Right of source
          row$ystart_adj <- row$y_from
          row$xend_adj <- row$x_to
          row$yend_adj <- row$y_to + row$half_h_to # Top of target

          # Downwards + Left (↙️)
        } else if (row$goes_down && row$goes_left) {
          row$xstart_adj <- row$x_from
          row$ystart_adj <- row$y_from - row$half_h_from # Bottom of source
          row$xend_adj <- row$x_to + row$half_w_to # Right of target
          row$yend_adj <- row$y_to
        }
      }

      # Store results back
      curved_idx_i <- curved_idx[i]
      xstart_adj[curved_idx_i] <- row$xstart_adj
      ystart_adj[curved_idx_i] <- row$ystart_adj
      xend_adj[curved_idx_i] <- row$xend_adj
      yend_adj[curved_idx_i] <- row$yend_adj
    }
  }

  # Add adjusted coordinates to output
  out <- out |>
    mutate(
      xstart_adj = xstart_adj,
      ystart_adj = ystart_adj,
      xend_adj = xend_adj,
      yend_adj = yend_adj
    )

  return(out)
}

#' Preprocess Edges DataFrame
#' Ensures required edge attributes have default values when not provided.
#' @param edges_df A data frame containing edge information
#'   Expected columns include:
#'   \describe{
#'     \item{from}{Node ID of the source node}
#'     \item{to}{Node ID of the target node}
#'     \item{curvature}{Curvature flag (0 for straight lines, 1 for curved)}
#'     \item{gap}{Optional gap offset from node borders (default: 0)}
#'     \item{pvalue}{Statistical significance value for styling}
#'     \item{est}{Effect estimate displayed on edge labels}
#'     \item{ci.lower}{Lower bound of confidence interval}
#'     \item{ci.upper}{Upper bound of confidence interval}
#'   }
#'
#' @return A data frame identical to the input, but with missing optional
#'   columns populated with sensible defaults. This ensures downstream
#'   plotting functions receive complete edge data.
#'
#' @details
#' The following defaults are applied:
#' \itemize{
#'   \item \code{label_position}: Default 0.5 centers text on the edge.
#'   }
preprocess_edges_df <- function(edges_df){
  # Add this after loading edges_df
  if (!"label_position" %in% names(edges_df)) {
    # Default is 0.5 which centers the text on top of the line
    edges_df$label_position <- 0.5
  }
  if (!"gap" %in% names(edges_df)) {

    edges_df$gap <- 1
  }
  return(edges_df)
}

#' Plots the dag based on the nodes and edges passed.
#' @param edges A dataframe containing data from tidy_dagitty()
#' @param nodes A dataframe containing data from tidy_dagitty(): node_id, x, y, label
#' @param label_size Set the label size of the text inside the nodes
#' @param label_size_unit Set the unit of the `label_size`
#' @param label_border_size Sets the boxes' border size
#' @param text_size Controls the text's size on top of the paths
#' @param xlim A vector of integers. Controls the limits of the plot
#' @param ylim A vector of integers. Controls the limits of the plot
#' @param plot_width_cm The width of the plot in cm
#' @param plot_height_cm The height of the plot in cm
#' @param footnote Sets the footnote
#' @param footnote_size Controls the size of the footnote text
#' @import ggplot2 dplyr
#' @importFrom purrr map
#' @importFrom geomtextpath geom_textsegment geom_textcurve
#' @export
plot_dag <- function(
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
) {
  # First build the nodes
  nodes_boxes <- compute_node_boxes(
    nodes = nodes,
    label_col = "label",
    text_size = label_size,
    size.unit = label_size_unit,
    fontface = "bold",
    padding_cm = 0.3,
    xlim = xlim,
    ylim = ylim,
    plot_width_cm = plot_width_cm,
    plot_height_cm = plot_height_cm
  )
  edges_adj <- preprocess_edges_df(edges)  |>
    adjust_edges_by_box(nodes_df =  nodes_boxes)

  to_return <- ggplot() +
    geom_label(
      data = nodes_boxes,
      aes(x = x, y = y, label = label),
      size = label_size,
      linewidth = label_border_size,
      size.unit = label_size_unit,
      fontface = "bold"
    ) +

    ##################################
    # Statistically significant lines#
    ##################################
    geomtextpath::geom_textsegment(
      data = edges_adj |>
        dplyr::filter(
          curvature == 0, pvalue <= 0.05),
      aes(
        x = xstart_adj,
        y = ystart_adj,
        xend = xend_adj,
        yend = yend_adj,
        label = paste0(
          est,
          "\n(",
          ci.lower,
          " — ",
          ci.upper,
          ")"
        ),
        hjust = hjust,
        vjust = vjust
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1,
      linetype = 1,
      size = text_size
    ) +
    #############################################
    # Non-vertical non-horizontal non-sig lines #
    #############################################
    geomtextpath::geom_textsegment(
      data = edges_adj |>
        dplyr::filter(
          curvature == 0, pvalue > 0.05, !is_vertical, !is_horizontal
        ),
      aes(
        x = xstart_adj,
        y = ystart_adj,
        xend = xend_adj,
        yend = yend_adj,
        label = paste0(
          est, # "β=",
          "\n(",
          ci.lower,
          " — ",
          ci.upper,
          ")"
        ),
        hjust = hjust,
        vjust = vjust
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1,
      linetype = 2,
      size = text_size
    ) +
    ###############################################
    # Vertical non-sig lines are drawn separately #
    ###############################################
    geom_segment(
      data = edges_adj |> dplyr::filter(curvature == 0, pvalue > 0.05, is_vertical),
      aes(
        x = xstart_adj,
        y = ystart_adj,
        xend = xend_adj,
        yend = yend_adj,
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1,
      linetype = 2,
      lineend = "butt",
      linejoin = "bevel"
    ) +
    geom_label(
      data = edges_adj |>
        dplyr::filter(curvature == 0, pvalue > 0.05, is_vertical),
      aes(
        # label_position spans from 0 to 1. It controls where
        # the text will be put.
        # 0.5 means in the middle of the line. 1 at the end
        # 0 at the start
        x = xstart_adj + (xend_adj - xstart_adj) * label_position,
        y = ystart_adj + (yend_adj - ystart_adj) * label_position,
        label = paste0(
          est,
          "\n(",
          ci.lower,
          " — ",
          ci.upper,
          ")"
        ),
        hjust = hjust,
        vjust = vjust
      ),
      angle = 0,
      linewidth = 0, # removes border line
      fill = "white", # background color
      size = text_size
    ) +
    #############################
    # Horizontal non-sign paths #
    #############################
    geom_segment(
      data = edges_adj |> dplyr::filter(curvature == 0, pvalue > 0.05, is_horizontal),
      aes(
        x = xstart_adj,
        y = ystart_adj,
        xend = xend_adj,
        yend = yend_adj,
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      linewidth = 1,
      linetype = 2,
      lineend = "butt",
      linejoin = "bevel"
    ) +
    # Horizontal non-sign paths
    geom_label(
      data = edges_adj |>
        dplyr::filter(curvature == 0, pvalue > 0.05, is_horizontal),
      aes(
        # label_position spans from 0 to 1. It controls where
        # the text will be put.
        # 0.5 means in the middle of the line. 1 at the end
        # 0 at the start
        x = xstart_adj + (xend_adj - xstart_adj) * label_position,
        y = ystart_adj + (yend_adj - ystart_adj) * label_position,
        label = paste0(
          est,
          "\n(",
          ci.lower,
          " — ",
          ci.upper,
          ")"
        ),
        hjust = hjust,
        vjust = vjust
      ),
      angle = 0,
      linewidth = 0, # removes border line
      fill = "white", # background color
      size = text_size
    )
  ################################
  # Curved non-significant paths #
  ################################
  geom_layers <- edges_adj |>
    filter(curvature == 1, pvalue > 0.05) |>
    pull(curvature_amount) |>
    purrr::map(
      function(crv) {
        geomtextpath::geom_textcurve(
          data = edges_adj  |>
            dplyr::filter(
              curvature == 1, pvalue > 0.05, curvature_amount == crv
            ),
          aes(
            x = xstart_adj, y = ystart_adj, xend = xend_adj, yend = yend_adj,
            label = paste0(est, "\n(", ci.lower, " — ", ci.upper, ")"),
            hjust = hjust, vjust = vjust
          ),
          curvature = crv,
          arrow = arrow(length = unit(3, "mm"), type = "closed"),
          linewidth = 1, linetype = 2, lineend = "butt", size = text_size
        )
      }
    )
  ############################
  # Curved significant paths #
  ############################
  geom_layers_curved_sign_paths <- edges_adj |>
    filter(curvature == 1, pvalue <= 0.05) |>
    pull(curvature_amount) |>
    purrr::map(
      function(crv) {
        geomtextpath::geom_textcurve(
          data = edges_adj |>
            dplyr::filter(
              curvature == 1, pvalue <= 0.05, curvature_amount == crv
            ),
          aes(
            x = xstart_adj, y = ystart_adj, xend = xend_adj, yend = yend_adj,
            label = paste0(est, "\n(", ci.lower, " — ", ci.upper, ")"),
            hjust = hjust, vjust = vjust
          ),
          curvature = crv,
          arrow = arrow(length = unit(3, "mm"), type = "closed"),
          linewidth = 1, linetype = 1, size = text_size
        )
      }
    )
  to_return <- to_return + geom_layers +
    geom_layers_curved_sign_paths +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_void()

  if (is.null(footnote) == F) {
    to_return <- to_return +
      labs(caption = footnote) +
      theme(plot.caption = element_text(hjust = 0.5, size = footnote_size))
  }

  attr(to_return, "edges_adj") <- edges_adj
  attr(to_return, "nodes_boxes") <- nodes_boxes
  return(to_return)
}
