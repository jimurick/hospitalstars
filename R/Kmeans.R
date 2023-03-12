


# Based on the function here:
#     https://github.com/cran/amap/blob/master/R/Kmeans.R
# except using the `kmeans_cpp()` function implemented here, and


#' Approximate translation of the SAS procedure "fastclus" to R
#'
#' @return the means
#' @export
sas_proc_fastclus <- function(x, centers, iter.max = 10, strict = Inf) {

  x <- as.matrix(x)
  m <- nrow(x)
  if (missing(centers)) stop("'centers' must be a number or a matrix")

  if (length(centers) == 1) {
    k <- centers
    centers <- x[sample(1:m, k), , drop = FALSE]
  } else {
    centers <- as.matrix(centers)
    if (any(duplicated(centers))) stop("initial centers are not distinct")
    cn <- NULL
    k <- nrow(centers)
    if (m < k) stop("more cluster centers than data points")
  }

  if(iter.max < 1) stop("'iter.max' must be positive")
  if(ncol(x) != ncol(centers))
    stop("must have same number of columns in 'x' and 'centers'")

  Z <- kmeans_cpp(x, centers, iter.max, strict)

  if (Z$iter > iter.max)
    warning("did not converge in ", iter.max, " iterations", call.=FALSE)
  if (any(Z$nc == 0))
    warning("empty cluster(s): try a better set of initial centers", call.=FALSE)

  centers <- matrix(Z$centers, k)
  dimnames(centers) <- list(1:k, dimnames(x)[[2]])
  cluster <- Z$cluster
  if(!is.null(rn <- rownames(x)))
    names(cluster) <- rn

  out <- list(cluster = cluster, centers = centers, withinss = Z$wss,
              size = Z$nc)
  class(out) <- "kmeans"
  out
}

