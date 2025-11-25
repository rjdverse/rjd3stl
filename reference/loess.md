# Fit a Loess regression.

Fit a Loess regression.

## Usage

``` r
loess(y, window, degree = 1, jump = 0)
```

## Arguments

- y:

  input time series.

- jump:

## Examples

``` r
q<-rjd3stl::stlplus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
decomp<-q$decomposition
t<-decomp[,'t']
matplot(cbind(t,loess(t, 121)), type='l')
```
