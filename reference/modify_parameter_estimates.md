# Modifies and formats parameter estimates in a dataframe

Rounds numeric columns to specified precision and creates formatted
p-value strings. Non-numeric columns are left unchanged. A new column
'pvalue_str' is added containing formatted p-values.

## Usage

``` r
modify_parameter_estimates(df, round_digits = 3, add_equal_sign = T)
```

## Arguments

- df:

  A dataframe containing parameter estimates, including at least a
  \`pvalue\` column for formatting

- round_digits:

  Number of decimal places to round numeric values to (default: 3)

- add_equal_sign:

  Logical; whether to prepend "=" to p-value strings (default: TRUE)
