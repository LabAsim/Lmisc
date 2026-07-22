# How to use it with Lavaan

    library(Lmisc)
    library(dplyr)
    library(ggplot2)
    library(lavaan)

## Specify the model

``` r

# Load the dataset. This dataset was taken from lavaan 6.2.1. Versions >0.7 have removed it.
data("FacialBurns", package = "Lmisc")
```

We specify the model first. This is just a toy model for illustrative
purposes.

``` r

model <- "
HADS ~ Age
HADS ~ Sex
HADS ~ TBSA
TBSA ~ Age
TBSA ~ Sex
"
```

``` r

fit_model <- sem(
  model = model,
  data = FacialBurns,
  missing = "fiml"
)
summary(fit_model)
#> lavaan 0.7-2 ended normally after 1 iteration
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         9
#> 
#>   Number of observations                            77
#>   Number of missing patterns                         1
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Observed
#>   Observed information based on                Hessian
#> 
#> Regressions:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   HADS ~                                              
#>     Age               0.043    0.055    0.779    0.436
#>     Sex               4.413    1.848    2.388    0.017
#>     TBSA              0.220    0.107    2.066    0.039
#>   TBSA ~                                              
#>     Age               0.073    0.058    1.255    0.209
#>     Sex               3.088    1.945    1.588    0.112
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .HADS             -0.435    3.071   -0.142    0.887
#>    .TBSA              2.973    3.267    0.910    0.363
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .HADS             43.473    7.006    6.205    0.000
#>    .TBSA             49.727    8.014    6.205    0.000
```

## Dagify the model

``` r

dag <- ggdag::dagify(
  HADS ~ Age,
  HADS ~ Sex,
  HADS ~ TBSA,
  TBSA ~ Age,
  TBSA ~ Sex,
  outcome = "TBSA",
  coords = list(
    x = c(
      TBSA = 0,
      HADS = 4,
      Age = 2,
      Sex = 2
    ),
    y = c(
      HADS = 0,
      TBSA = 0,
      Age = 1,
      Sex = -1
    )
  ),
  labels = c(
    # If you need to change the names, specify the labels here
    HADS = "HADS",
    TBSA = "TBSA",
    Age = "Age",
    Sex = "Sex"
  )
)


tidy_dag <- ggdag::tidy_dagitty(dag)
```

## Create nodes and edges dataframes

``` r


nodes <- tidy_dag$data %>%
  filter(!duplicated(name)) %>%
  dplyr::transmute(
    node_id = name,
    x = x,
    y = y,
    label = label
  ) %>%
  mutate(node_id = label)

# If you need curved paths
edges <- tidy_dag$data %>%
  mutate(
    curvature = case_when(
      name == "Sex" & to == "HADS" ~ 1,
      TRUE ~ 0 # default: straight
    ) 
  ) |> 
  mutate(
    curvature_amount = case_when(
      name == "Sex" & to == "HADS" ~ 0.3,
      TRUE ~ 0 
    ) 
  )
```

## Extract the parameters

``` r

parameters_fit_model <- modify_parameter_estimates(
  df = parameterestimates(
    fit_model,
    standardized = F
  ),
  round_digits = 2
)


temp <- data.frame(
  # Extract only the regression paths
  parameters_fit_model[
    parameters_fit_model$op == "~", 
    ]
)


edges <- left_join(
  x = edges,
  y = temp,
  by = join_by(
    # rhs =  right hand side = where the path starts
    # lhs = left hand side = where the path ends
    name == rhs, 
    to == lhs
  )
)

# Rename and keep only what we need
edges <- edges %>%
  filter(!is.na(to)) %>%
  transmute(
    from = name,
    to = to,
    curvature = curvature,
    curvature_amount = curvature_amount,
    pvalue = pvalue,
    est = est,
    ci.lower = ci.lower,
    ci.upper = ci.upper
  )

edges <- edges |> 
  mutate(
    hjust = case_when(
      from == "Sex" & to == "HADS" ~ 0.5,
      .default = 0.5
    )
  ) |> 
  mutate(
    vjust = case_when(
      from == "Sex" & to == "HADS" ~ 0.5,
      .default = 0.5
    )
  ) |> 
  mutate(
    gap = case_when(
      from == "Sex" & to == "HADS" ~ -0.5,
      .default = -1
    )
  )
```

## Plot the estimates

``` r

plot_dag(
  nodes = nodes,
  edges = edges,
  label_size = 10,
  label_size_unit = "pt",
  text_size = 4,
  xlim = c(-0.5, 4.4),
  ylim = c(-1.5, 1.5)
)
#> Warning: The text offset exceeds the curvature in one or more paths. This will result in
#> displaced letters. Consider reducing the vjust or text size, or use the hjust
#> parameter to move the string to a different point on the path.
```

![](How_to_use_it_with_Lavaan_files/figure-html/unnamed-chunk-8-1.png)
