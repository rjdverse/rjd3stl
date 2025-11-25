# Title

Title

## Usage

``` r
mstl(
  y,
  periods,
  multiplicative = TRUE,
  swindows = NULL,
  twindow = 0,
  ninnerloop = 1,
  nouterloop = 15,
  nojump = FALSE,
  weight.threshold = 0.001,
  weight.function = c("BIWEIGHT", "UNIFORM", "TRIANGULAR", "EPANECHNIKOV", "TRICUBE",
    "TRIWEIGHT")
)
```

## Arguments

- weight.function:

## Examples

``` r
q<-rjd3stl::mstl(rjd3toolkit::ABS$X0.2.09.10.M, c(12, 25))
decomp<-q$decomposition
```
