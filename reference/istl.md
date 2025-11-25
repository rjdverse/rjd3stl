# Title

Title

## Usage

``` r
istl(
  y,
  periods,
  multiplicative = TRUE,
  swindows = NULL,
  twindows = NULL,
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
q<-rjd3stl::istl(rjd3toolkit::ABS$X0.2.09.10.M, c(12, 25))
decomp<-q$decomposition
matplot(decomp[,c(1:3)], type='l')
```
