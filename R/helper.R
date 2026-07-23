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
        return(as.numeric(x))
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
