

#include <R.h>
#include <Rcpp.h>
#include <cmath>

using namespace std;
using namespace Rcpp;

// Slight variation on the k-means algorithm found here:
//    https://github.com/cran/amap/blob/master/src/kmeans.cpp
// This version agrees with the SAS k-means

//' my k-means function
//'
//' @param x the data to be clustered
//' @param centers initial centers
//' @param maxiter the max iter
//' @param strict how strict?
//' @export
// [[Rcpp::export]]
List kmeans_cpp(NumericVector x, NumericVector centers,
                int maxiter, double strict) {
  int n = x.length();
  int k = centers.length();
  IntegerVector cl(n);
  IntegerVector nc(k);
  NumericVector wss(k);

  int iter, i, j, it, inew = 0;
  double best, dd;
  Rboolean updated;

  for (iter = 0; iter < maxiter; iter++) {
    updated = FALSE;
    for (i = 0; i < n; i++) {
      best = R_PosInf;
      for (j = 0; j < k; j++) {
        dd = abs(x[i] - centers[j]);
        if (dd < best) {
          best = dd;
          if (best <= strict) {
            inew = j+1;
          } else {
            inew = -(j+1);
          }
        }
      }
      if (cl[i] != inew) {
        updated = TRUE;
        cl[i] = inew;
      }
    }
    if (!updated)
      break;

    for (j = 0; j < k; j++) centers[j] = 0.0;
    for (j = 0; j < k; j++) nc[j]      = 0;

    for (i = 0; i < n; i++) {
      if (cl[i] > 0) {
        it = cl[i] - 1;
        nc[it]++;
        centers[it] += x[i];
      }
    }
    for(j = 0; j < k; j++) centers[j] /= nc[j];
  }

  for (i = 0; i < n; i++) {
    it = abs(cl[i]) - 1;
    wss[it] += (x[i] - centers[it]) * (x[i] - centers[it]);
  }

  return List::create(
    _["cluster"] = cl, _["centers"] = centers,
    _["iter"] = iter, _["wss"] = wss, _["nc"] = nc
  );
}


