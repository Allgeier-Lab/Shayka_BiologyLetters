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

// biomass: Numeric vector with biomass values
// carbon: Numeric vector with carbon values
// n: Double with total number of cells
// verbose: Logical if progress report shoud be printed 

// https://stackoverflow.com/questions/7560114/random-number-c-in-some-range

// [[Rcpp::export]]
double rcpp_sample_carbon(Rcpp::NumericVector biomass, Rcpp::NumericVector carbon, double n, bool verbose) {
  
  // get starting time
  auto start = std::chrono::system_clock::now();
  
  // convert n to long integer
  long nn = (long) n;
  if (verbose) Rcout << "Converted counter nn=" << nn << std::endl;
  
  // init double to store cumulative value
  double sum = 0.0;
  
  // obtain a random number from hardware and seed the generator
  std::random_device random;
  std::mt19937 generator(random());
  
  // define ranges for random numbers for biomass and carbon
  std::uniform_int_distribution<> random_biomass(0, biomass.length() - 1);
  std::uniform_int_distribution<> random_carbon(0, carbon.length() - 1);

  // setup progress bar
  Progress progress(nn, verbose);
  
  for (int i = 0; i < nn; i++) {
    
    // check abort of function
    if (Progress::check_abort()) Rcpp::stop("Stopped by user");
      
    // get random iterator of vector
    int itr_b = random_biomass(generator);
    int itr_c = random_carbon(generator);
    
    // get random values
    double biomass_temp = biomass(itr_b);
    double carbon_temp = carbon(itr_c);
    
    // calculate cumulative sum
    sum += (biomass_temp * carbon_temp);
    
    // update progress bar
    progress.increment();
  
  }
  
  // get ending time
  auto end = std::chrono::system_clock::now();
  
  // calculate total time
  std::chrono::duration<double> elapsed_min = (end - start) / 60;
  
  // print total time
  if (verbose) Rcout << "Total time: " << elapsed_min.count() << " minutes" << std::endl;
  
  return sum;

}
