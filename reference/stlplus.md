# Title

Perform an STL like (based on Loess) decomposition on any periodicity

## Usage

``` r
stlplus(
  y,
  period,
  multiplicative = TRUE,
  swindow = 7,
  twindow = 0,
  lwindow = 0,
  sdegree = 0,
  tdegree = 1,
  ldegree = 1,
  sjump = 0,
  tjump = 0,
  ljump = 0,
  ninnerloop = 1,
  nouterloop = 15,
  weight.threshold = 0.001,
  weight.function = c("BIWEIGHT", "UNIFORM", "TRIANGULAR", "EPANECHNIKOV", "TRICUBE",
    "TRIWEIGHT"),
  legacy = FALSE
)
```

## Arguments

- y:

  input time series.

- period:

  period, any positive real number.

- multiplicative:

  Boolean indicating if the decomposition mode is multiplicative (TRUE).

- swindow:

  length of the seasonal filter.

- twindow:

  length of the trend filter.

- lwindow:

  length of the filter used to remove the trend of the seasonal

- sdegree:

  degree of the seasonal local polynomial (0 or 1)

- tdegree:

  degree of the trend local polynomial (0 or 1)

- ldegree:

  degree of the low-pass local polynomial (0 or 1)

- sjump:

  number of jumps in the computation of the seasonal

- tjump:

  number of jumps in the computation of the trend

- ljump:

  number of jumps in the computation of the trend in the seasonal

- ninnerloop:

  Number of inner loops

- nouterloop:

  Number of outer loops (computation of robust weights)

- weight.threshold:

  Weights threshold (in \[0, 0.3\])

- weight.function:

  weights function

- legacy:

  use of the (bugged) legacy MAD

## Value

A matrix with the following series: y, sa, t, s, i, fit, weights

## Examples

``` r
q<-rjd3stl::stlplus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
decomp<-q$decomposition
```
