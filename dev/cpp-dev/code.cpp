#include <Rcpp.h>
#include <vector>
using namespace Rcpp;
using namespace std;


// from https://coolbutuseless.github.io/2018/09/17/intersection-of-multiple-vectors/
// [[Rcpp::export]]
std::vector<int> intersection_rcpp_core(IntegerVector v1, IntegerVector v2) {
  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());
  std::vector<int> v_intersection;

  std::set_intersection(v1.begin(), v1.end(),
                        v2.begin(), v2.end(),
                        std::back_inserter(v_intersection));

  return v_intersection;
}


// [[Rcpp::export]]
IntegerVector shuffle_nbs(int i, int n, int card) {
  IntegerVector x = Rcpp::seq(1, n);
  IntegerVector no_i = x[x != i];

  IntegerVector index = Rcpp::sample(no_i.size(), card);

  IntegerVector res = no_i[index - 1];
  return res;


}

//' Conditional Permutation
// [[Rcpp::export]]
List cond_permute(IntegerVector lens, int n, IntegerVector cards) {
  List res(n);
  for (int i = 0; i < n; i++) {
    res[i] = shuffle_nbs(i + 1, n, cards[i]);
  }
  return res;
}


// [[Rcpp::export]]
double lm_bv(double x, NumericVector yj, NumericVector wt) {
  double prod = Rcpp::sum(yj * wt);
  return x * prod;
}

// This is 75% faster
// [[Rcpp::export]]
std::vector<double> lm_bv_calc(NumericVector x, List yj, List wt) {
  int iter = x.size();
  std::vector<double> res;
  for (int i = 0; i < iter; i++) {
    double iter_res = lm_bv(x[i], yj[i], wt[i]);
    res.push_back(iter_res);
  }

  return res;
}
