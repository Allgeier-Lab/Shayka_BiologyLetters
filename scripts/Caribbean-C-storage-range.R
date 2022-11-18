library(truncnorm) # remotes::install_github("olafmersmann/truncnorm")
library(ggplot2)

set.seed(42)

agb_mean <- 47.84 #mean aboveground (ag) biomass (gDW m-2)
agb_sd <- 28.98 #standard deviation of mean ag biomass

bgb_mean <- 191.47 #mean belowground (bg) biomass (gDW m-2)
bgb_sd <- 135.67 #standard deviation of mean bg biomass

n_sample <- 100

# create normal distribution values using mean and sd
df_range <- purrr::map_dfr(list(ag = c(agb_mean, agb_sd), bg = c(bgb_mean, bgb_sd)), function(i) {
  
  samples <- truncnorm::rtruncnorm(n = n_sample, mean = i[[1]], sd = i[[2]], a = 0.0)
  
  # get lower and higher 10% and 90% quantiles
  lower <- quantile(x = samples, probs = 0.1)
  upper <- quantile(x = samples, probs = 0.9)
  
  # sample values from lower and upper for each cell, we might need to to this in Rcpp
  n_cells <- 14340940
  
  total_lower <- sum(sample(x = samples[samples < lower], size = n_cells, replace = TRUE))
  
  total_higher <- sum(sample(x =  samples[samples > upper], size = n_cells, replace = TRUE))
  
  data.frame(lower = total_lower, higher = total_higher)
  
}, .id = "part")


# tidyr::pivot_longer(df_range, -part, names_to = "range") |> 
ggplot(df_range, aes(x = factor(part), ymin = lower, ymax = higher)) + 
  geom_errorbar(linewidth = 1, width = 0.25) + 
  theme_classic()
