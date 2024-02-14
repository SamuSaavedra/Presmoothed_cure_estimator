# Packages
package_name <- "survPresmooth"
if (!require(package_name, character.only = TRUE)) {
  install.packages(package_name)
  library(package_name, character.only = TRUE)
} else {
  library(package_name, character.only = TRUE)
}

# Path
n <- 50  # Sampling size
directory <- "~/Samu2024/"
folder <- paste0("n_", n)
setwd(directory)
if (!dir.exists(file.path(directory, folder))) {
  # If the directory does not exist, it is created
  dir.create(file.path(directory, folder))
  cat("Folder created!\n")
} else {
  cat("Folder already exists.\n")
}
setwd(file.path(directory, folder))

# Main program
plotting = FALSE  # set to TRUE to plot the data
seed_number = 1:1000
for (seed in seed_number) {
  start_time <- Sys.time()
  set.seed(seed)
  results <- list()
  y <- rexp(n, rate = 1)  # RNG with exponential distribution
  p <- seq(0.2, 0.8, by = 0.1)  # Probability of NOT being cured
  for (pnbc in p) {
    c_rates = seq(0.1, 2, by = 0.1)
    for (c_rate in c_rates) {
      c <- rexp(n, rate = c_rate)  # Censoring values
      u <- runif(n)  # RNG with uniform distribution (between 0 and 1)
      t <- ifelse(u < pnbc, pmin(y, c), c)  # Observed times
      d <- ifelse(u < pnbc, ifelse(y < c, 1, 0), 0)  # Uncensoring indicator
      # d = 0 means censored, d = 1 means uncensored
      data <- data.frame(t = t, d = d)
      
      # Variable names
      name_wo <- paste("wo_presm", seed, "c", c_rate, "p", pnbc, sep = "_")
      name_w <- paste("w_presm", seed, "c", c_rate, "p", pnbc, sep = "_")
      
      # Without presmoothing
      wo_presm = presmooth(t, d, dataset = data, estimand = "S", 
                           presmoothing = FALSE)
      title <- paste("Without presmoothing. Exp. Rate: ", c_rate, 
                     "Curation prob.: ", 1 - pnbc, "MIN: ", 
                     round(min(wo_presm$estimate), digits = 2))
      if (plotting == TRUE) {
        plot(wo_presm$x.est, wo_presm$estimate, main = title)
      }
      
      # With presmoothing
      w_presm = presmooth(t, d, dataset = data, estimand = "S", 
                          presmoothing = TRUE, bw.selec = "bootstrap", control = 
                          control.presmooth(n.boot = c(1000, 200)))
      # bw.select is the window selection method
      if (plotting == TRUE) {
        title <- paste("With presmoothing. n:", n, "Exp. Rate: ", c_rate, 
                     "Curation prob.: ", 1 - pnbc, "MIN: ", 
                     round(min(w_presm$estimate), digits = 2))
        plot(w_presm$x.est, w_presm$estimate, main = title)
      }
      
      # Adding data to results
      results[[name_wo]] <- wo_presm
      results[[name_w]] <- w_presm
    }
  }
  # Saving the results
  name_results <- paste0("results_", seed, ".rds")
  saveRDS(results, file = name_results)
  
  # Time spent on execution
  end_time <- Sys.time()
  time_exec <- difftime(end_time, start_time, units = "mins")
  cat(sprintf("Execution %d completed! (%d/%d; %.1f%%) Time spent: %.2f minutes
              \n", seed, seed, max(seed_number), seed/max(seed_number) * 100, 
              round(time_exec, digits = 2)))
}
