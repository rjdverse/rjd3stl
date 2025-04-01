
#' Title
#'
#' Perform an STL like (based on Loess) decomposition on any periodicity
#'
#' @param y input time series.
#' @param period period, any positive real number.
#' @param multiplicative Boolean indicating if the decomposition mode is multiplicative (TRUE).
#' @param swindow length of the seasonal filter.
#' @param twindow length of the trend filter.
#' @param lwindow length of the filter used to remove the trend of the seasonal
#' @param sdegree degree of the seasonal local polynomial (0 or 1)
#' @param tdegree degree of the trend local polynomial (0 or 1)
#' @param ldegree degree of the low-pass local polynomial (0 or 1)
#' @param sjump number of jumps in the computation of the seasonal
#' @param tjump number of jumps in the computation of the trend
#' @param ljump number of jumps in the computation of the trend in the seasonal
#' @param ninnerloop Number of inner loops
#' @param nouterloop Number of outer loops (computation of robust weights)
#' @param weight.threshold Weights threshold (in [0, 0.3])
#' @param weight.function weights function
#' @param legacy use of the (bugged) legacy MAD
#'
#' @return A matrix with the following series: y, sa, t, s, i, fit, weights
#' @export
#' @examples
#' q<-rjd3stl::stlplus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#' decomp<-q$decomposition

stlplus<-function(y, period, multiplicative=TRUE, swindow=7, twindow=0, lwindow=0, sdegree=0, tdegree=1, ldegree=1, sjump=0, tjump=0, ljump=0, ninnerloop=1, nouterloop=15, weight.threshold=0.001,
    weight.function=c('BIWEIGHT', 'UNIFORM', 'TRIANGULAR', 'EPANECHNIKOV', 'TRICUBE', 'TRIWEIGHT'), legacy=FALSE){
  weight.function <- match.arg(weight.function)

  jrslt<-.jcall("jdplus/stl/base/r/StlDecomposition", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "stl",
                as.numeric(y), as.integer(period), as.logical(multiplicative),
                as.integer(swindow), as.integer(twindow), as.integer(lwindow),
                as.integer(sdegree), as.integer(tdegree), as.integer(ldegree),
                as.integer(sjump), as.integer(tjump), as.integer(ljump),
                as.integer(ninnerloop), as.integer(nouterloop),
                as.numeric(weight.threshold), as.character(weight.function),
                as.logical(legacy))
  m<-rjd3toolkit::.jd2r_matrix(jrslt)
  colnames(m)<-c('y', 'sa', 't', 's', 'i', 'fit', 'weights')
  parameters<-list(
    multiplicative=multiplicative,
    swindow=swindow,
    twindow=twindow,
    ninnerloop=ninnerloop,
    nouterloop=nouterloop,
    weight.threshold=weight.threshold,
    weight.function=weight.function
  )

  return(structure(list(
    decomposition=m,
    parameters=parameters),
    class="JD3_STL"))
}

#' Title
#'
#' @param y
#' @param periods
#' @param multiplicative
#' @param swindows
#' @param twindow
#' @param ninnerloop
#' @param nouterloop
#' @param nojump
#' @param weight.threshold
#' @param weight.function
#'
#' @return
#' @export
#'
#' @examples
#' q<-rjd3stl::mstl(rjd3toolkit::ABS$X0.2.09.10.M, c(12, 25))
#' decomp<-q$decomposition
mstl<-function(y, periods, multiplicative=TRUE, swindows=NULL, twindow=0, ninnerloop=1, nouterloop=15, nojump=FALSE, weight.threshold=0.001,
              weight.function=c('BIWEIGHT', 'UNIFORM', 'TRIANGULAR', 'EPANECHNIKOV', 'TRICUBE', 'TRIWEIGHT')){
  weight.function <- match.arg(weight.function)

  if (is.null(swindows))
    swin<-.jnull("[I")
  else
    swin<-.jarray(as.integer(swindows))

  jrslt<-.jcall("jdplus/stl/base/r/StlDecomposition", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "mstl", as.numeric(y), .jarray(as.integer(periods)), as.logical(multiplicative), swin, as.integer(twindow),
                as.integer(ninnerloop), as.integer(nouterloop), as.logical(nojump), as.numeric(weight.threshold), as.character(weight.function))
  m<-rjd3toolkit::.jd2r_matrix(jrslt)

  snames<-paste('s', as.integer(periods), sep='')
  colnames(m)<-c('y', 'sa', 't', snames, 'i', 'fit', 'weights')
  parameters<-list(
    multiplicative=multiplicative,
    swindow=swindows,
    twindow=twindow,
    ninnerloop=ninnerloop,
    nouterloop=nouterloop,
    weight.threshold=weight.threshold,
    weight.function=weight.function
  )

  return(structure(list(
    decomposition=m,
    parameters=parameters),
    class="JD3_STL"))
}

#' Title
#'
#' @param y
#' @param periods
#' @param multiplicative
#' @param swindows
#' @param twindows
#' @param ninnerloop
#' @param nouterloop
#' @param nojump
#' @param weight.threshold
#' @param weight.function
#'
#' @return
#' @export
#'
#' @examples
#' q<-rjd3stl::istl(rjd3toolkit::ABS$X0.2.09.10.M, c(12, 25))
#' decomp<-q$decomposition
#' matplot(decomp[,c(1:3)], type='l')
istl<-function(y, periods, multiplicative=TRUE, swindows=NULL, twindows=NULL, ninnerloop=1, nouterloop=15, nojump=FALSE, weight.threshold=0.001,
              weight.function=c('BIWEIGHT', 'UNIFORM', 'TRIANGULAR', 'EPANECHNIKOV', 'TRICUBE', 'TRIWEIGHT')){
  weight.function <- match.arg(weight.function)
  if (is.null(swindows))
    swin<-.jnull("[I")
  else
    swin<-.jarray(as.integer(swindows))
  if (is.null(twindows))
    twin<-.jnull("[I")
  else
    twin<-.jarray(as.integer(twindows))

  jrslt<-.jcall("jdplus/stl/base/r/StlDecomposition", "Ljdplus/toolkit/base/api/math/matrices/Matrix;", "istl", as.numeric(y), .jarray(as.integer(periods)), as.logical(multiplicative), swin, twin,
                as.integer(ninnerloop), as.integer(nouterloop), as.logical(nojump), as.numeric(weight.threshold), as.character(weight.function))
  m<-rjd3toolkit::.jd2r_matrix(jrslt)
  snames<-paste('s', as.integer(periods), sep='')
  colnames(m)<-c('y', 'sa', 't', snames, 'i', 'fit', 'weights')
  parameters<-list(
    multiplicative=multiplicative,
    swindows=swindows,
    twindows=twindows,
    ninnerloop=ninnerloop,
    nouterloop=nouterloop,
    weight.threshold=weight.threshold,
    weight.function=weight.function
  )

  return(structure(list(
    decomposition=m,
    parameters=parameters),
    class="JD3_STL"))
}



#' Fit a Loess regression.
#'
#' @param y input time series.
#' @param window
#' @param degree
#' @param jump
#'
#' @return
#' @export
#'
#' @examples
#' q<-rjd3stl::stlplus(rjd3toolkit::ABS$X0.2.09.10.M, 12)
#' decomp<-q$decomposition
#' t<-decomp[,'t']
#' matplot(cbind(t,loess(t, 121)), type='l')
loess<-function(y, window, degree=1, jump=0){
  if (degree != 0 && degree != 1)
    stop("Unsupported degree")
  if (jump <0)
    stop("jump should be positive")
  return(.jcall("jdplus/stl/base/r/StlDecomposition", "[D", "loess", as.numeric(y), as.integer(window), as.integer(degree), as.integer(jump)))
}
