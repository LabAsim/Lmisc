############
# save_dag #
############

describe("save_dag auto-detects file type from extension", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  it("successfully saves with automatic format detection for .tiff, .tif, and .png", {
    # Test all three supported extensions
    tiff_path <- tempfile(fileext = ".tiff")
    png_path <- tempfile(fileext = ".png")

    # Each should save without error and create a valid file
    expect_no_error({
      save_dag(tiff_path, test_plot, width = 10, height = 8)
      save_dag(png_path, test_plot, width = 10, height = 8)
    })

    # Verify all files were created
    expect_true(file.exists(tiff_path))
    expect_true(file.exists(png_path))

    # Verify files have non-zero size
    expect_gt(file.info(tiff_path)$size, 0)
    expect_gt(file.info(png_path)$size, 0)

    # Cleanup
    unlink(c(tiff_path, png_path))
  })
})


test_that("saves TIFF file successfully", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  temp_path <- tempfile(fileext = ".tiff")

  expect_no_error({
    save_dag(temp_path, test_plot, width = 10, height = 8)
  })

  expect_true(file.exists(temp_path))
  expect_gt(file.info(temp_path)$size, 0)

  unlink(temp_path)
})

test_that("saves PNG file successfully", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  temp_path <- tempfile(fileext = ".png")

  expect_no_error({
    save_dag(temp_path, test_plot, width = 10, height = 8)
  })

  expect_true(file.exists(temp_path))
  expect_gt(file.info(temp_path)$size, 0)

  unlink(temp_path)
})

test_that("throws error for unsupported type", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  temp_path <- tempfile(fileext = "sdsds.jpg")

  expect_error(
    save_dag(temp_path, test_plot)
  )
})

test_that("output file size reflects specified dimensions", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  small_path <- tempfile(fileext = ".tiff")
  large_path <- tempfile(fileext = ".tiff")

  save_dag(small_path, test_plot, width = 5, height = 5)
  save_dag(large_path, test_plot, width = 20, height = 20)

  # Larger dimensions should produce larger file (generally)
  expect_lte(file.info(small_path)$size, file.info(large_path)$size)

  unlink(small_path)
  unlink(large_path)
})

test_that("works with different ggplot objects", {
  bar_plot <- ggplot(mtcars, aes(x = factor(cyl))) +
    geom_bar()
  line_plot <- ggplot(mtcars, aes(x = hp, y = wt)) +
    geom_line()

  temp_bar <- tempfile(fileext = ".png")
  temp_line <- tempfile(fileext = ".png")

  expect_no_error(save_dag(temp_bar, bar_plot))
  expect_no_error(save_dag(temp_line, line_plot))

  expect_true(file.exists(temp_bar))
  expect_true(file.exists(temp_line))

  unlink(temp_bar)
  unlink(temp_line)
})

test_that("default type and dimensions work", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  temp_path <- tempfile(fileext = ".tiff")

  # Uses default type="tiff", width=50, height=25
  expect_no_error({
    save_dag(temp_path, test_plot)
  })

  expect_true(file.exists(temp_path))
  expect_gt(file.info(temp_path)$size, 0)

  unlink(temp_path)
})

test_that("handles special characters in filename", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  temp_path <- tempfile(pattern = "test_plot-", fileext = ".tiff")

  expect_no_error({
    save_dag(temp_path, test_plot)
  })

  expect_true(file.exists(temp_path))
  unlink(temp_path)
})

test_that("full directory paths work", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  temp_dir <- tempdir()
  temp_path <- file.path(temp_dir, "subfolder_test.tiff")

  expect_no_error({
    save_dag(temp_path, test_plot)
  })

  expect_true(file.exists(temp_path))
  unlink(temp_path)
})

test_that("can overwrite existing file", {
  test_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  temp_path <- tempfile(fileext = ".tiff")

  save_dag(temp_path, test_plot, width = 10, height = 8)
  first_size <- file.info(temp_path)$size

  save_dag(temp_path, test_plot, width = 20, height = 20)
  second_size <- file.info(temp_path)$size

  expect_true(file.exists(temp_path))
  expect_gt(second_size, first_size)

  unlink(temp_path)
})

######################
# compute_node_boxes #
######################

test_that("returns data frame with expected columns", {
  nodes <- data.frame(
    label = c("A", "B", "C"),
    x = c(1, 5, 10),
    y = c(1, 2, 3)
  )

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    xlim = c(0, 12),
    ylim = c(0, 4)
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("label", "x", "y", "box_w_cm", "box_h_cm", "half_w", "half_h") %in% names(result)))
})

test_that("preserves number of rows", {
  nodes <- data.frame(
    label = c("Node1", "Node2", "Node3", "Node4", "Node5"),
    x = 1:5,
    y = 1:5
  )

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  expect_equal(nrow(result), nrow(nodes))
})

test_that("longer text produces larger boxes", {
  nodes <- data.frame(
    label = c("A", "VeryLongLabel", "Short"),
    x = c(1, 2, 3),
    y = c(1, 2, 3)
  )

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    xlim = c(0, 5),
    ylim = c(0, 5)
  )

  expect_true(result$box_w_cm[2] > result$box_w_cm[1])
  expect_true(result$box_h_cm[2] >= result$box_h_cm[1])
})

test_that("mm unit converts correctly to pt", {
  nodes <- data.frame(label = "Test")

  result_pt <- compute_node_boxes(
    nodes,
    label_col = "label",
    text_size = 18,
    size.unit = "pt",
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  result_mm <- compute_node_boxes(
    nodes,
    label_col = "label",
    text_size = 6,
    size.unit = "mm",
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  expect_equal(nrow(result_pt), nrow(result_mm))
})

test_that("invalid size.unit throws error", {
  nodes <- data.frame(label = "Test")

  expect_error(
    compute_node_boxes(
      nodes,
      label_col = "label",
      size.unit = "inches",
      xlim = c(0, 10),
      ylim = c(0, 10)
    ),
    "Only 'pt' and 'mm' are supported"
  )
})

test_that("padding adds to both width and height", {
  nodes <- data.frame(label = "X")

  result_no_padding <- compute_node_boxes(
    nodes,
    label_col = "label",
    padding_cm = 0,
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  result_with_padding <- compute_node_boxes(
    nodes,
    label_col = "label",
    padding_cm = 1,
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  expect_true(result_with_padding$box_w_cm > result_no_padding$box_w_cm)
  expect_true(result_with_padding$box_h_cm > result_no_padding$box_h_cm)
})

test_that("bold fontface affects text dimensions", {
  nodes <- data.frame(label = "BoldText")

  result_bold <- compute_node_boxes(
    nodes,
    label_col = "label",
    fontface = "bold",
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  result_plain <- compute_node_boxes(
    nodes,
    label_col = "label",
    fontface = "plain",
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  expect_equal(nrow(result_bold), nrow(result_plain))
  # Bold typically slightly wider or same width
  expect_gte(result_bold$box_w_cm, result_plain$box_w_cm - 0.1) # Allow small tolerance
})

test_that("half dimensions are half of box dimensions scaled to data units", {
  nodes <- data.frame(label = "Test")

  xlim <- c(0, 10)
  ylim <- c(0, 5)
  plot_width_cm <- 20
  plot_height_cm <- 10

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    xlim = xlim,
    ylim = ylim,
    plot_width_cm = plot_width_cm,
    plot_height_cm = plot_height_cm
  )

  x_data_per_cm <- diff(xlim) / plot_width_cm
  y_data_per_cm <- diff(ylim) / plot_height_cm

  expected_half_w <- (result$box_w_cm[1] * x_data_per_cm) / 2
  expected_half_h <- (result$box_h_cm[1] * y_data_per_cm) / 2

  expect_equal(result$half_w[1], expected_half_w, tolerance = 0.01)
  expect_equal(result$half_h[1], expected_half_h, tolerance = 0.01)
})

test_that("empty labels produce minimal boxes", {
  nodes <- data.frame(
    label = c("", "Text", ""),
    x = c(1, 2, 3),
    y = c(1, 2, 3)
  )

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  expect_equal(nrow(result), 3)
  expect_true(all(result$box_w_cm >= 0))
  expect_true(all(result$box_h_cm >= 0))
})


test_that("plot dimensions scale box_to_data units correctly", {
  nodes <- data.frame(label = "ScaleTest")

  result_narrow <- compute_node_boxes(
    nodes,
    label_col = "label",
    plot_width_cm = 10,
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  result_wide <- compute_node_boxes(
    nodes,
    label_col = "label",
    plot_width_cm = 40, # Wider
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  expect_equal(result_narrow$box_w_cm, result_wide$box_w_cm)
  expect_true(result_narrow$half_w != result_wide$half_w)
})

test_that("original node columns are preserved", {
  nodes <- data.frame(
    id = c(100, 200, 300),
    name = c("Alpha", "Beta", "Gamma"),
    value = c(10, 20, 30),
    label = c("A", "B", "C")
  )

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  expect_true("id" %in% names(result))
  expect_true("name" %in% names(result))
  expect_true("value" %in% names(result))
  expect_equal(result$id, nodes$id)
  expect_equal(result$name, nodes$name)
})

test_that("single node is handled correctly", {
  nodes <- data.frame(label = "Single", x = 5, y = 5)

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    xlim = c(0, 10),
    ylim = c(0, 10)
  )

  expect_equal(nrow(result), 1)
  expect_s3_class(result, "data.frame")
})

test_that("missing label column throws error", {
  nodes <- data.frame(name = c("A", "B"))

  expect_error(
    compute_node_boxes(
      nodes,
      label_col = "nonexistent",
      xlim = c(0, 10),
      ylim = c(0, 10)
    )
  )
})

test_that("large text sizes don't cause errors", {
  nodes <- data.frame(label = c("Small", "Huge"))

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    text_size = 72, # Large font
    xlim = c(0, 100),
    ylim = c(0, 100)
  )

  expect_equal(nrow(result), 2)
})

test_that("integration test with realistic network plot nodes", {
  nodes <- data.frame(
    id = 1:10,
    label = paste0("Node_", LETTERS[1:10]),
    x = runif(10, 0, 100),
    y = runif(10, 0, 50),
    degree = sample(1:5, 10, replace = TRUE)
  )

  result <- compute_node_boxes(
    nodes,
    label_col = "label",
    text_size = 14,
    size.unit = "pt",
    padding_cm = 0.25,
    xlim = c(0, 100),
    ylim = c(0, 50),
    plot_width_cm = 25,
    plot_height_cm = 15
  )

  expect_equal(nrow(result), 10)
  expect_true(all(result$box_w_cm > 0))
  expect_true(all(result$box_h_cm > 0))
  expect_true(all(result$half_w > 0))
  expect_true(all(result$half_h > 0))
})

##########################
# line_rect_intersection #
##########################

test_that("returns named vector with x and y coordinates", {
  result <- line_rect_intersection(
    x0 = 0, y0 = 5,
    x1 = 10, y1 = 5,
    half_w = 2, half_h = 1.5
  )

  expect_equal(class(result), "numeric")
  expect_length(result, 2)
  expect_equal(names(result), c("x", "y"))
  expect_equal(result, c(x = 9, y = 5))
})

test_that("horizontal line from left hits right side", {
  result <- line_rect_intersection(
    x0 = 0, y0 = 5,
    x1 = 10, y1 = 5,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 9), tolerance = 1e-10)
  expect_equal(result["y"], c(y = 5), tolerance = 1e-10)
})

test_that("horizontal line from right hits left side", {
  result <- line_rect_intersection(
    x0 = 10, y0 = 5,
    x1 = 0, y1 = 5,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 1), tolerance = 1e-10)
  expect_equal(result["y"], c(y = 5), tolerance = 1e-10)
})

test_that("vertical line from bottom hits top side", {
  result <- line_rect_intersection(
    x0 = 5, y0 = 0,
    x1 = 5, y1 = 10,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 5), tolerance = 1e-10)
  expect_equal(result["y"], c(y = 9.5), tolerance = 1e-10)
})

test_that("vertical line from top hits bottom side", {
  result <- line_rect_intersection(
    x0 = 5, y0 = 10,
    x1 = 5, y1 = 0,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 5), tolerance = 1e-10)
  expect_equal(result["y"], c(y = 0.5), tolerance = 1e-10)
})

test_that("line from outside to center hits near-side boundary", {

  result <- line_rect_intersection(
    x0 = -10, y0 = 5,
    x1 = 5, y1 = 5,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 4), tolerance = 1e-10) # left edge
  expect_equal(result["y"], c(y = 5), tolerance = 1e-10)
})

test_that("when multiple intersections exist, closest to start is chosen", {

  result <- line_rect_intersection(
    x0 = 0, y0 = 5,
    x1 = 20, y1 = 5,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 19.0), tolerance = 1e-1)
  expect_equal(result["y"], c(y = 5), tolerance = 1e-10)
})

test_that("line aimed at corner returns corner point", {
  result <- line_rect_intersection(
    x0 = 0, y0 = 0,
    x1 = 7, y1 = 6.5,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 6.12), tolerance = 1e-1)
  expect_equal(result["y"], c(y = 5.68), tolerance = 1e-2)
})

test_that("handles very large rectangles correctly", {
  result <- line_rect_intersection(
    x0 = 0, y0 = 0,
    x1 = 100, y1 = 100,
    half_w = 100, half_h = 100
  )

  expect_true(!is.na(result["x"]))
  expect_true(!is.na(result["y"]))
})

test_that("near-horizontal line handled correctly", {
  result <- line_rect_intersection(
    x0 = 0, y0 = 5.1,
    x1 = 10, y1 = 5.2,
    half_w = 2, half_h = 1.5
  )

  expect_true(!is.na(result["x"]))
  expect_true(!is.na(result["y"]))
})

test_that("near-vertical line handled correctly", {
  result <- line_rect_intersection(
    x0 = 5.1, y0 = 0,
    x1 = 5.2, y1 = 10,
    half_w = 2, half_h = 1.5
  )

  expect_true(!is.na(result["x"]))
  expect_true(!is.na(result["y"]))
})

test_that("degenerate line (zero length) returns center", {
  result <- line_rect_intersection(
    x0 = 0, y0 = 0,
    x1 = 0, y1 = 0,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 0), tolerance = 1e-10)
  expect_equal(result["y"], c(y = 0), tolerance = 1e-10)
})


test_that("return values are numeric type", {
  result <- line_rect_intersection(
    x0 = 0, y0 = 5,
    x1 = 10, y1 = 5,
    half_w = 2, half_h = 1.5
  )

  expect_type(result, "double")
})

test_that("line perpendicular to edge hits that edge directly", {

  result <- line_rect_intersection(
    x0 = 10, y0 = 5,
    x1 = 0, y1 = 5,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 1), tolerance = 1e-10)
  expect_equal(result["y"], c(y = 5), tolerance = 1e-10)
})

test_that("integration: simulates arrow endpoint calculation", {

  arrow_start_x <- 10
  arrow_start_y <- 10
  target_node_center_x <- 50
  target_node_center_y <- 50
  target_half_width <- 2
  target_half_height <- 2

  result <- line_rect_intersection(
    x0 = arrow_start_x, y0 = arrow_start_y,
    x1 = target_node_center_x, y1 = target_node_center_y,
    half_w = target_half_width, half_h = target_half_height
  )

  expect_equal(
    result["x"],
    c(x = target_node_center_x - target_half_width),
    tolerance = 1
  )
  expect_equal(result["y"], c(y = 48.71), tolerance = 1e-1)
})

test_that("correctly identifies closest intersection by distance", {
  result <- line_rect_intersection(
    x0 = -100, y0 = 5,
    x1 = 200, y1 = 5,
    half_w = 2, half_h = 1.5
  )

  expect_equal(result["x"], c(x = 199), tolerance = 1e-10)
})

test_that("results are suitable for plotting arrows", {

  results <- list()

  for (i in 1:10) {
    start_x <- runif(1, -50, 0)
    start_y <- runif(1, -50, 50)

    r <- line_rect_intersection(
      x0 = start_x, y0 = start_y,
      x1 = 50, y1 = 25,
      half_w = 5, half_h = 5
    )

    expect_true(r["x"] >= 45 && r["x"] <= 55)
    expect_true(r["y"] >= 20 && r["y"] <= 30)
  }
})

#######################
# adjust_edges_by_box #
#######################

it("throws informative error when nodes_df missing required columns", {
  nodes <- data.frame(x = c(0, 5), y = c(0, 3)) # Missing node_id, half_w, half_h
  edges <- data.frame(from = c("A"), to = c("B"), curvature = c(0))

  err_msg <- expect_error(
    adjust_edges_by_box(edges, nodes),
    "nodes_df missing required columns",
    fixed = TRUE
  )

  expect_match(err_msg$message, "node_id", fixed = TRUE)
  expect_match(err_msg$message, "half_w", fixed = TRUE)
  expect_match(err_msg$message, "half_h", fixed = TRUE)
})

it("throws informative error when edges_df missing required columns", {
  nodes <- data.frame(
    node_id = c("A", "B"),
    x = c(0, 5),
    y = c(0, 3),
    half_w = c(1.5, 2.0),
    half_h = c(1.0, 1.5)
  )
  edges <- data.frame(to = c("B"), pvalue = c(0.01)) #

  err_msg <- expect_error(
    adjust_edges_by_box(edges, nodes),
    "edges_df missing required columns",
    fixed = TRUE
  )

  expect_match(err_msg$message, "from", fixed = TRUE)
  expect_match(err_msg$message, "curvature", fixed = TRUE)
})


create_test_nodes <- function() {
  data.frame(
    node_id = c("A", "B", "C", "D"),
    x = c(0, 5, 10, 2),
    y = c(0, 3, 0, 5),
    half_w = c(1.5, 2.0, 1.5, 1.0),
    half_h = c(1.0, 1.5, 1.0, 1.5)
  )
}

create_test_edges <- function() {
  data.frame(
    from = c("A", "A", "B", "C", "D"),
    to = c("B", "C", "C", "D", "A"),
    curvature = c(0, 1, 0, 0, 0),
    pvalue = c(0.01, 0.30, 0.05, 0.001, 0.1),
    est = c(0.5, 0.2, -0.3, 0.8, -0.1),
    curvature_amount = c(0, 0.15, 0, 0, 0)
  )
}

describe("adjust_edges_by_box output structure", {
  it("returns data frame with expected columns", {
    edges <- create_test_edges() |> preprocess_edges_df()
    nodes <- create_test_nodes()
    result <- adjust_edges_by_box(edges, nodes)

    expect_s3_class(result, "data.frame")
    expect_true(all(c(
      "from", "to", "curvature", "pvalue", "est",
      "xstart_adj", "ystart_adj", "xend_adj", "yend_adj",
      "is_horizontal", "is_vertical"
    ) %in% names(result)))
  })

  it("preserves number of rows from input", {
    edges <- create_test_edges() |> preprocess_edges_df()
    nodes <- create_test_nodes()

    result <- adjust_edges_by_box(edges, nodes)

    expect_equal(nrow(result), nrow(edges))
  })
})

describe("adjust_edges_by_box coordinate adjustment", {
  it("start point lies on source node boundary", {
    edges <- create_test_edges() |> preprocess_edges_df()
    nodes <- create_test_nodes()

    result <- adjust_edges_by_box(edges, nodes)

    start_x <- result$xstart_adj[1]
    start_y <- result$ystart_adj[1]

    expect_lte(start_x, 1.6)
    expect_gte(start_x, -1.6)
    expect_lte(start_y, 1.1)
    expect_gte(start_y, -1.1)
  })

  it("end point lies on target node boundary", {
    edges <- create_test_edges()  |> preprocess_edges_df()
    nodes <- create_test_nodes()

    result <- adjust_edges_by_box(edges, nodes)

    end_x <- result$xend_adj[1]
    end_y <- result$yend_adj[1]

    expect_lte(end_x, 7.1)
    expect_gte(end_x, 2.9)
    expect_lte(end_y, 4.6)
    expect_gte(end_y, 1.4)
  })
})

describe("adjust_edges_by_box vertical lines", {
  it("detects perfectly vertical lines", {
    nodes <- data.frame(
      node_id = c("U", "V"),
      x = c(5, 5),
      y = c(0, 10),
      half_w = c(1.0, 1.0),
      half_h = c(1.0, 1.0)
    )

    edges <- data.frame(
      from = c("U"),
      to = c("V"),
      curvature = c(0),
      pvalue = c(0.01),
      est = c(0.5)
    ) |> preprocess_edges_df()

    result <- adjust_edges_by_box(edges, nodes)
    expect_true(result$is_vertical[1])
  })
})

describe("adjust_edges_by_box integration", {
  it("produces coordinates suitable for draw_dag", {

    edges <- create_test_edges() |> preprocess_edges_df()
    nodes <- create_test_nodes()

    result <- adjust_edges_by_box(edges, nodes)

    required_for_draw <- c(
      "xstart_adj", "ystart_adj", "xend_adj", "yend_adj",
      "curvature", "pvalue", "est", "ci.lower", "ci.upper"
    )

    # Check that key columns exist
    expect_true(all(c("xstart_adj", "ystart_adj", "xend_adj", "yend_adj") %in% names(result)))
  })
})

describe("adjust_edges_by_box numerical precision", {
  it("handles large coordinate values", {
    nodes <- data.frame(
      node_id = c("FAR1", "FAR2"),
      x = c(-1000, 1000),
      y = c(-500, 500),
      half_w = c(50, 50),
      half_h = c(25, 25)
    )

    edges <- data.frame(
      from = c("FAR1"),
      to = c("FAR2"),
      curvature = c(0),
      pvalue = c(0.01),
      est = c(0.5)
    ) |> preprocess_edges_df()

    expect_no_error({
      result <- adjust_edges_by_box(edges, nodes)
    })
  })


  it("all values are finite (no Inf or NaN)", {
    edges <- create_test_edges() |> preprocess_edges_df()
    nodes <- create_test_nodes()

    result <- adjust_edges_by_box(edges, nodes)

    expect_true(all(is.finite(result$xstart_adj)))
    expect_true(all(is.finite(result$ystart_adj)))
    expect_true(all(is.finite(result$xend_adj)))
    expect_true(all(is.finite(result$yend_adj)))
  })
})

############
# plot_dag #
###########

create_test_nodes2 <- function() {
  data.frame(
    node_id = c("Input", "Hidden", "Output", "InputVertical"),
    label = c("Input", "Hidden", "Output", "InputVertical"),
    x = c(0, 14, 14, 0),
    y = c(0, 3, 0, 3)
  )
}

create_test_edges2 <- function() {
  data.frame(
    from = c("Input", "Hidden"),
    to = c("Hidden", "Output"),
    curvature = c(0, 0),
    pvalue = c(0.01, 0.30),
    est = c(0.8, 0.5),
    ci.lower = c(0.4, 0.1),
    ci.upper = c(1.2, 0.9),
    hjust = c(0.33, 0.28),
    vjust = c(0.5, 0.5),
    label_position = c(1, 1),
    curvature_amount = c(
      0.2, 0.15, -0.25, -0.3,
      -0.35, -0.4, -0.4, 0.2
    ),
    gap = c(0, 0)
  )
}



create_test_edges_curved <- function() {
  data.frame(
    from = c(
      "Input", "Hidden", "Output", "InputVertical",
      "InputVertical", "InputVertical", "Output", "Hidden"
    ),
    to = c(
      "Hidden", "Output", "Input", "Input",
      "Output", "Hidden", "InputVertical", "Input"
    ),
    curvature = c(1, 1, 1, 1, 1, 1, 1, 1),
    pvalue = c(0.1, 0.30, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    est = c(
      0.2, 0.15, -0.25, -0.3,
      -0.35, -0.4, -0.4, 0.2
    ),
    ci.lower = c(0.4, 0.1, -0.6, -0.3, -0.3, -0.3, -0.3, -0.3),
    ci.upper = c(1.2, 0.9, 0.0, -0.3, -0.3, -0.3, -0.3, -0.3),
    hjust = c(0.5, 0.50, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    vjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    curvature_amount = c(
      0.2, 0.15, -0.25, -0.3,
      -0.35, -0.4, -0.4, 0.2
    )
  )
}
create_test_edges_vertical <- function() {
  data.frame(
    from = c("Bottom", "Top"),
    to = c("Top", "Middle"),
    curvature = c(0, 0),
    pvalue = c(0.01, 0.30),
    est = c(0.7, 0.2),
    ci.lower = c(0.3, -0.1),
    ci.upper = c(1.1, 0.5),
    hjust = c(0.5, 0.5),
    vjust = c(0.5, 0.5),
    x = c(5, 10),
    y = c(0, 5),
    is_vertical = c(F, T),
    label_position = c(0.5, 0.5)
  )
}

describe("plot_dag basic functionality", {
  it("returns a ggplot object", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result <- plot_dag(nodes, edges)

    expect_s3_class(result, "ggplot")
    expect_s3_class(result, "gg")
  })

  it("contains expected number of layers", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result <- plot_dag(nodes, edges)

    expect_gte(length(result$layers), 5)
  })
})

describe("plot_dag footnote handling", {
  it("includes footnote caption when provided", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result <- plot_dag(
      nodes,
      edges,
      footnote = "Sample size: N = 500"
    )

    expect_true(!is.null(result$labels$caption))
    expect_equal(result$labels$caption, "Sample size: N = 500")
  })

  it("excludes caption when footnote is NULL", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result <- plot_dag(nodes, edges, footnote = NULL)

    expect_null(result$labels$caption)
  })

  it("uses custom footnote_size", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result <- plot_dag(
      nodes,
      edges,
      footnote = "Test footnote",
      footnote_size = 20
    )

    expect_true(!is.null(result$labels$caption))
    expect_gte(length(result$layers), 5)
  })

  it("handles empty string footnote", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result <- plot_dag(nodes, edges, footnote = "")

    expect_false(is.null(result$labels$caption))
    expect_equal(result$labels$caption, "")
  })
})

describe("plot_dag label customization", {
  it("applies label_size_unit parameter", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result_pt <- plot_dag(
      nodes,
      edges,
      label_size = 16.9,
      label_size_unit = "pt"
    )

    result_mm <- plot_dag(
      nodes,
      edges,
      label_size = 6,
      label_size_unit = "mm"
    )

    expect_s3_class(result_pt, "ggplot")
    expect_s3_class(result_mm, "ggplot")
  })

  it("applies text_size parameter to edge labels", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result <- plot_dag(nodes, edges, text_size = 10)

    expect_s3_class(result, "ggplot")
  })

  it("handles label_border_size parameter", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result_thin <- plot_dag(nodes, edges, label_border_size = 0.5)
    result_thick <- plot_dag(nodes, edges, label_border_size = 2)

    expect_s3_class(result_thin, "ggplot")
    expect_s3_class(result_thick, "ggplot")
  })
})

withr::with_seed(
  seed=123,
  code = {
  describe("plot_dag edge cases", {
    it("handles when single node", {
      if (identical(Sys.getenv("VDIFR_SKIP_CHECK"), "true")) {
        skip("Skipping visual tests during devtools::check")
      }
      nodes <- data.frame(
        label = c("Single"),
        x = c(7),
        y = c(4),
        node_id = c("Single")
      )

      edges <- data.frame(
        from = character(1),
        to = character(1),
        curvature = numeric(1),
        pvalue = numeric(1),
        est = numeric(1),
        ci.lower = numeric(1),
        ci.upper = numeric(1),
        hjust = numeric(1),
        vjust = numeric(1),
        curvature = c(1),
        curvature_amount = c(0),
        label_position = c(1)
      )
      p <- plot_dag(nodes, edges)
      suppressWarnings(
        vdiffr::expect_doppelganger(
          "handles when single node",
          p
        )
      )
    })

    it("handles duplicate edges (same endpoints)", {
      nodes <- data.frame(
        label = c("A", "B"),
        x = c(0, 10),
        y = c(0, 0),
        node_id = c("A", "B")
      )

      edges <- data.frame(
        from = c("A", "A"),
        to = c("B", "B"),
        curvature = c(0, 1),
        pvalue = c(0.01, 0.30),
        est = c(0.8, 0.2),
        ci.lower = c(0.4, -0.1),
        ci.upper = c(1.2, 0.5),
        hjust = c(0.33, 0.50),
        vjust = c(0.5, 0.5),
        curvature = c(1, 1),
        curvature_amount = c(0, 0),
        gap = c(0,0)
      )

      result <- plot_dag(nodes, edges)

      expect_s3_class(result, "ggplot")
    })
  })
  }
)

describe("plot_dag rendering verification", {

  it("plot renders with ggplot2 methods", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result <- plot_dag(nodes, edges)


    expect_no_error({
      result_plus <- result + labs(title = "Test Title")
    })
  })

  it("creates consistent output for same inputs", {
    nodes <- create_test_nodes2()
    edges <- create_test_edges2()

    result1 <- plot_dag(nodes, edges)
    result2 <- plot_dag(nodes, edges)

    # Both should be valid ggplot objects
    expect_s3_class(result1, "ggplot")
    expect_s3_class(result2, "ggplot")

    # Should have same number of layers
    expect_equal(result1$layers, result2$layers)
  })
})


it("complete workflow: plot and save", {
  nodes <- create_test_nodes2()
  edges <- create_test_edges2()
  dag_plot <- plot_dag(nodes, edges, footnote = "Test")

  temp_path <- tempfile(fileext = ".tiff")
  expect_no_error(save_dag(temp_path, dag_plot))
  expect_true(file.exists(temp_path))
  unlink(temp_path)
})

withr::with_seed(
  seed=123,
  code = {
    it(
      "Curved non-significant paths",
      {
        # Skip during check if VDIFR_SKIP_CHECK is set
        if (identical(Sys.getenv("VDIFR_SKIP_CHECK"), "true")) {
          skip("Skipping visual tests during devtools::check")
        }
        nodes <- create_test_nodes2()
        edges <- create_test_edges_curved()
        edges$label_position <- c(1, 1, 1, 1, 1, 1, 1, 1)
        p <- plot_dag(nodes, edges, ylim = c(-2, 7), xlim = c(-4, 16), text_size = 3)
        vdiffr::expect_doppelganger(
          "Curved non-significant paths",
          p
        )
      }
    )
  }
)
withr::with_seed(
  seed=123,
  code = {
    it(
      "Curved significant and non-significant paths",
      {

        # Skip during check if VDIFR_SKIP_CHECK is set
        if (identical(Sys.getenv("VDIFR_SKIP_CHECK"), "true")) {
          skip("Skipping visual tests during devtools::check")
        }
        nodes <- create_test_nodes2()
        edges <- data.frame(
          from = c(
            "Input", "Hidden", "Output", "InputVertical",
            "InputVertical", "InputVertical", "Output", "Hidden"
          ),
          to = c(
            "Hidden", "Output", "Input", "Input",
            "Output", "Hidden", "InputVertical", "Input"
          ),
          curvature = c(1, 1, 1, 1, 1, 1, 1, 1),
          pvalue = c(0.01, 0.30, 0.005, 0.5, 0.5, 0.5, 0.005, 0.5),
          est = c(
            0.2, 0.15, -0.25, -0.3,
            -0.35, -0.4, -0.4, 0.2
          ),
          ci.lower = c(0.4, 0.1, -0.6, -0.3, -0.3, -0.3, -0.3, -0.3),
          ci.upper = c(1.2, 0.9, 0.0, -0.3, -0.3, -0.3, -0.3, -0.3),
          hjust = c(0.5, 0.50, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
          vjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
          curvature_amount = c(
            0.2, 0.15, -0.25, -0.3,
            -0.35, -0.4, -0.4, 0.2
          )
        )
        edges$label_position <- c(1, 1, 1, 1, 1, 1, 1, 1)
        p <- plot_dag(nodes, edges, ylim = c(-2, 7), xlim = c(-2, 16), text_size = 3)
        vdiffr::expect_doppelganger(
          "Curved significant and non-significant paths",
          p
        )

      }
    )
  }
)

withr::with_seed(
  seed=123,
  code = {
    it(
      "Mixed straight Curved sig non-sig paths",
      {
        # Skip during check if VDIFR_SKIP_CHECK is set
        if (identical(Sys.getenv("VDIFR_SKIP_CHECK"), "true")) {
          skip("Skipping visual tests during devtools::check")
        }
            nodes <- create_test_nodes2()
            edges <- data.frame(
              from = c(
                "Input", "Hidden", "Output", "InputVertical",
                "InputVertical", "InputVertical", "Output", "Hidden"
              ),
              to = c(
                "Hidden", "Output", "Input", "Input",
                "Output", "Hidden", "InputVertical", "Input"
              ),
              curvature = c(0, 0, 1, 1, 1, 1, 1, 1),
              pvalue = c(0.01, 0.30, 0.005, 0.5, 0.5, 0.5, 0.005, 0.5),
              est = c(
                0.2, 0.15, -0.25, -0.3,
                -0.35, -0.4, -0.4, 0.2
              ),
              ci.lower = c(0.4, 0.1, -0.6, -0.3, -0.3, -0.3, -0.3, -0.3),
              ci.upper = c(1.2, 0.9, 0.0, -0.3, -0.3, -0.3, -0.3, -0.3),
              hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
              vjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
              curvature_amount = c(
                0, 0, -0.25, -0.3,
                -0.35, -0.4, -0.4, 0.2
              )
            )
            edges$label_position <- c(1, 0.5, 1, 1, 1, 1, 1, 1)
            p <- plot_dag(nodes, edges, ylim = c(-2, 7), xlim = c(-2, 16), text_size = 3)
            # p
            vdiffr::expect_doppelganger(
              "Mixed straight Curved sig non-sig paths",
              p
            )

      }
    )
  }
)



withr::with_seed(
  seed=123,
  code = {
    it(
      "Curved paths positive curvature",
      {
        # Skip during check if VDIFR_SKIP_CHECK is set
        if (identical(Sys.getenv("VDIFR_SKIP_CHECK"), "true")) {
          skip("Skipping visual tests during devtools::check")
        }
        nodes <- create_test_nodes2()
        edges <- data.frame(
          from = c(
            "Input", "Hidden", "Output", "InputVertical",
            "InputVertical", "Hidden", "Output", "Hidden"
          ),
          to = c(
            "Hidden", "Output", "Hidden", "Input",
            "Output", "InputVertical", "InputVertical", "Input"
          ),
          curvature = c(1, 1, 1, 1, 1, 1, 1, 1),
          pvalue = c(0.01, 0.30, 0.005, 0.5, 0.5, 0.5, 0.005, 0.5),
          est = c(
            0.2, 0.15, -0.25, -0.3,
            -0.35, -0.4, -0.4, 0.2
          ),
          ci.lower = c(0.4, 0.1, -0.6, -0.3, -0.3, -0.3, -0.3, -0.3),
          ci.upper = c(1.2, 0.9, 0.0, -0.3, -0.3, -0.3, -0.3, -0.3),
          hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
          vjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
          curvature_amount = c(
            0.2, 0.2, 0.25, 0.3,
            0.35, 0.4, 0.4, 0.2
          )
        )
        edges$label_position <- c(1, 0.5, 1, 1, 1, 1, 1, 1)
        p <- plot_dag(nodes, edges, ylim = c(-2, 7), xlim = c(-2, 16), text_size = 3)
        # p
        vdiffr::expect_doppelganger(
          "Curved paths positive curvature",
          p
        )

      }
    )
  }
)


withr::with_seed(
  seed=123,
  code = {
    it(
      "Curved paths negative curvature",
      {
        # Skip during check if VDIFR_SKIP_CHECK is set
        if (identical(Sys.getenv("VDIFR_SKIP_CHECK"), "true")) {
          skip("Skipping visual tests during devtools::check")
        }
        nodes <- create_test_nodes2()
        edges <- data.frame(
          from = c(
            "Input", "Hidden", "Output", "InputVertical",
            "InputVertical", "Hidden", "Output", "Hidden"
          ),
          to = c(
            "Hidden", "Output", "Hidden", "Input",
            "Output", "InputVertical", "InputVertical", "Input"
          ),
          curvature = c(1, 1, 1, 1, 1, 1, 1, 1),
          pvalue = c(0.01, 0.30, 0.005, 0.5, 0.5, 0.5, 0.005, 0.5),
          est = c(
            0.2, 0.15, -0.25, -0.3,
            -0.35, -0.4, -0.4, 0.2
          ),
          ci.lower = c(0.4, 0.1, -0.6, -0.3, -0.3, -0.3, -0.3, -0.3),
          ci.upper = c(1.2, 0.9, 0.0, -0.3, -0.3, -0.3, -0.3, -0.3),
          hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
          vjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
          curvature_amount = c(
            -0.2, -0.2, -0.25, -0.3,
            -0.35, -0.4, -0.4, -0.2
          )
        )
        edges$label_position <- c(1, 0.5, 1, 1, 1, 1, 1, 1)
        p <- plot_dag(nodes, edges, ylim = c(-2, 7), xlim = c(-2, 16), text_size = 3)
        # p
        vdiffr::expect_doppelganger(
          "Curved paths negative curvature",
          p
        )

      }
    )
  }
)



withr::with_seed(
  seed=123,
  code = {
    it(
      "horizontal lines2",
      {
        # Skip during check if VDIFR_SKIP_CHECK is set
        if (identical(Sys.getenv("VDIFR_SKIP_CHECK"), "true")) {
          skip("Skipping visual tests during devtools::check")
        }
            nodes <- create_test_nodes2()
            edges <- data.frame(
              from = c(
                "Input", "Output", "Output", "InputVertical", "Hidden"
              ),
              to = c(
                "Output", "Hidden", "InputVertical", "Input", "InputVertical"
              ),
              curvature = c(0, 0, 0, 0,0),
              pvalue = c(0.1, 0.30, 0.5, 0.5, 0.005),
              est = c(
                0.2, 0.15, -0.25, -0.3, -0.5
              ),
              ci.lower = c(0.4, 0.1, -0.6, -0.3, -0.3),
              ci.upper = c(1.2, 0.9, 0.0, -0.3, -0.3),
              hjust = c(0.5, 0.5, 0.25, 0.5, 0.5),
              vjust = c(0.5, 0.5, 0.50, 0.5, 0.5),
              curvature_amount = c(
                0, 0, 0, 0, 0
              ),
              gap = c(0.75, 0, 0.2, 0, 0.5)
            )
            edges$label_position <- c(0.5, 0.5, 1, 0.8, 0.5)
            p <- plot_dag(nodes, edges, ylim = c(-2, 4), xlim = c(-2, 16), text_size = 3)
            # p
            vdiffr::expect_doppelganger(
              "horizontal lines2",
              p
            )

      }
    )
}
    )


withr::with_seed(
  seed=123,
  code = {
    it(
      "diagonal lines",
      {
        # Skip during check if VDIFR_SKIP_CHECK is set
        if (identical(Sys.getenv("VDIFR_SKIP_CHECK"), "true")) {
          skip("Skipping visual tests during devtools::check")
        }
        nodes <- data.frame(
          node_id = c("A", "C", "B", "D"),
          label = c("A", "C", "B", "D"),
          x = c(0, 14, 14, 0),
          y = c(0, 3, 0, 3)
        )
        edges <- data.frame(
          from = c(
            "A", "C", "B", "D", "B", "C"
          ),
          to = c(
            "B", "D", "C", "A", "D", "A"
          ),
          curvature = c(0, 0, 0, 0, 0, 0),
          pvalue = c(0.1, 0.30, 0.5, 0.5, 0.5, 0.005),
          est = c(
            0.2, 0.15, -0.25, -0.3, -0.35, -0.4
          ),
          ci.lower = c(0.4, 0.1, -0.6, -0.3, -0.3, -0.3),
          ci.upper = c(1.2, 0.9, 0.0, -0.3, -0.3, -0.3),
          hjust = c(0.5, 0.5, 0.5, 0.5, 0.3, 0.8),
          vjust = c(0.5, 0.5, 0.50, 0.5, 0, 0),
          curvature_amount = c(
            0, 0, 0, 0,0,0
          )
        )
        # Set the gap values
        edges$gap <- c(-1, -3, -1, -3, -1,-3)
        p <- plot_dag(nodes, edges, ylim = c(-2, 4), xlim = c(-2, 16), text_size = 3)
        # p
        vdiffr::expect_doppelganger(
          "diagonal lines",
          p
        )

      }
    )
  }
)



