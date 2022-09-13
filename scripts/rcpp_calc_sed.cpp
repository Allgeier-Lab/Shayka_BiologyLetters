// [[Rcpp::depends(RcppProgress)]]

#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <chrono>

using namespace Rcpp;

// [[Rcpp::export]]
double rcpp_calc_sed(double n, double norm_mean, double norm_sd) {
  
  // get starting time
  auto start = std::chrono::system_clock::now();

  // convert n to long integer
  long nn = (long) n;
  
  // print converted n to check
  if (nn > 2147483647) {
    
    Rcout << "n > .Machine$integer.max. Converting n to long n=" << nn << std::endl;
    
  } else {
    
    Rcout << "Converting n to long n=" << nn << std::endl;
    
  }

  // init double to store final results
  double result = 0.0;
  
  // init object to save last i counter
  long i_check = 1;

  // setup progress bar
  Progress progress(nn, TRUE);

  for (long i = 1; i <= nn; i++) {

    // check abort of function
    if (Progress::check_abort()) {

      Rcpp::stop("Stopped by user.");

    }

    // pull from random norm
    double random_norm = R::rnorm(norm_mean, norm_sd);

    // cumulative add to result
    result += random_norm;

    // check if integer overflow happened
    if (i < 0) {

      progress.cleanup();

      Rcout << "Current i = " << i << std::endl;

      Rcpp::stop("NOOOOO! Integer overflow...");

    }

    // save last i for check sums
    if (i == nn) {

      i_check = i;

    }

    // update progress bar
    progress.increment();

  }

  // print check sums
  Rcout << "Checks: i=" << i_check << " n=" << nn << " -> Diff=" << nn - i_check << std::endl;
  
  // get starting time
  auto end = std::chrono::system_clock::now();

  std::chrono::duration<double> elapsed_min = (end - start) / 60;
  
  // print starting time
  Rcout << "Total time: " << elapsed_min.count() << " minutes" << std::endl;
  
  // return result
  return result;
  
}
