#' Modifies and formats parameter estimates in a dataframe
#'
#' Rounds numeric columns to specified precision and creates formatted p-value
#' strings. Non-numeric columns are left unchanged. A new column 'pvalue_str'
#' is added containing formatted p-values.
#'
#' @param df A dataframe containing parameter estimates, including at least a
#'   `pvalue` column for formatting
#' @param round_digits Number of decimal places to round numeric values to
#'   (default: 3)
#' @param add_equal_sign Logical; whether to prepend "=" to p-value strings
#'   (default: TRUE)
#'
#' @import dplyr
#' @importFrom glue glue
#' @export
modify_parameter_estimates <- function(
  df,
  round_digits = 3,
  add_equal_sign = T
) {
  df[] <- lapply(
    X = df[],
    FUN = function(x) {
      if (is.character(x) == F) {
        x <- round(x, digits = round_digits)
        return(x)
      } else {
        # Try to convert it to numeric
        # If x="asdsad" => x_num=NA
        # if x="1" => x_num=1

        x_num <- suppressWarnings(as.numeric(x))

        return(
          unlist(
            lapply(
              # Iterate over each value
              seq_along(x),
              function(i) {
                if (is.na(x_num[i])) x[i] else x_num[i]
              }
            )
          )
        )
      }
    }
  )
  df <- df %>%
    mutate(
      pvalue_str = case_when(
        .data[["pvalue"]] < 0.001 ~ "<0.001",
        .default = if (add_equal_sign == F) {
          as.character(
            round(.data[["pvalue"]], digits = round_digits)
          )
        } else {
          as.character(
            glue::glue(
              "={as.character(round(.data[['pvalue']], digits = round_digits))}"
            )
          )
        }
      )
    )
  return(df)
}

shrink_endpoints <- function(x1, y1, x2, y2, factor = 0.9) {
  stopifnot(factor >= 0, factor <= 1)

  # Direction from start to end
  dx <- x2 - x1
  dy <- y2 - y1

  # Move each endpoint inward by the same fraction
  x1_new <- x1 + ((1 - (factor)) / 2) * dx
  y1_new <- y1 + ((1 - (factor)) / 2) * dy

  x2_new <- x2 - ((1 - (factor)) / 2) * dx
  y2_new <- y2 - ((1 - (factor)) / 2) * dy

  data.frame(
    x1 = x1_new,
    y1 = y1_new,
    x2 = x2_new,
    y2 = y2_new
  )
}
