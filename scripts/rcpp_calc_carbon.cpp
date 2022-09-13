// [[Rcpp::depends(RcppDist, RcppProgress)]]

#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <truncnorm.h>
#include <chrono>


using namespace Rcpp;

// Truncated normal distribution from: J.B. Duck-Mayr (2018). RcppDist: 'Rcpp'
// Integration of Additional Probability Distributions. R package version 0.1.1.
// <https://CRAN.R-project.org/package=RcppDist>

// [[Rcpp::export]]
double rcpp_calc_carbon(double n, double trunc_mean, double trunc_sd,
                        double trunc_min, double trunc_max,
                        double norm_mean, double norm_sd) {
  
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

  for (int i = 1; i <= nn; i++) {

    // check abort of function
    if (Progress::check_abort()) {

      Rcpp::stop("Stopped by user.");

    }

    // pull from truncated random norm
    double random_trunc = r_truncnorm(trunc_mean, trunc_sd, trunc_min, trunc_max);

    // pull from random norm
    double random_norm = R::rnorm(norm_mean, norm_sd);

    // multiply the two numbers and cumulative add to result
    result += (random_trunc * random_norm);
    
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
