// [[Rcpp::depends(RcppProgress)]]

// include headers
#include <Rcpp.h>
#include <progress.hpp>
#include <progress_bar.hpp>
#include <chrono>
#include <random>

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// https://stackoverflow.com/questions/7560114/random-number-c-in-some-range

// [[Rcpp::export]]
double rcpp_sample_sum(Rcpp::NumericVector x, int n) {
  
  // get starting time
  auto start = std::chrono::system_clock::now();
  
  // convert n to long integer
  long nn = (long) n;
  
  double sum = 0.0;
  
  std::random_device random; // obtain a random number from hardware
  std::mt19937 generator(random()); // seed the generator
  std::uniform_int_distribution<> random_uniform(0, x.length() - 1); // define the range

  // print converted n to check
  if (nn > 2147483647) {
    
    Rcout << "n > .Machine$integer.max" << std::endl;
    
  }
  
  Rcout << "Converted counter nn=" << nn << std::endl;
  
  // setup progress bar
  Progress progress(nn, TRUE);
  
  for (int i = 0; i < nn; i++) {
    
    // check abort of function
    if (Progress::check_abort()) {
      
      Rcpp::stop("Stopped by user.");
      
    }
    
    int iterator_temp = random_uniform(generator);

    sum += x(iterator_temp);

    // update progress bar
    progress.increment();

  }
  
  // get starting time
  auto end = std::chrono::system_clock::now();
  
  std::chrono::duration<double> elapsed_min = (end - start) / 60;
  
  // print starting time
  Rcout << "Total time: " << elapsed_min.count() << " minutes" << std::endl;
  
  return sum;

}

/*** R
rcpp_sample_sum(c(1:10), n = 100)
*/
