# Load parallel package
package_name <- c("survPresmooth", "parallel")
for (package in package_name) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Time of the start
start_time <- Sys.time()

# Number of cores
num_cores <- detectCores()

# Path
n <- 500  # Sampling size
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

# Function
generate_data <- function(seed, n) {
  library("survPresmooth")  # Load required library
  
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
      data <- data.frame(t = t, d = d)
      
      name_wo <- paste("wo_presm", seed, "c", c_rate, "p", pnbc, sep = "_")
      name_w <- paste("w_presm", seed, "c", c_rate, "p", pnbc, sep = "_")
      
      wo_presm = presmooth(t, d, dataset = data, estimand = "S", 
                           presmoothing = FALSE)
      
      w_presm = presmooth(t, d, dataset = data, estimand = "S", 
                          presmoothing = TRUE, bw.selec = "bootstrap", control = 
                            control.presmooth(n.boot = c(1000, 200)))
      
      results[[name_wo]] <- wo_presm
      results[[name_w]] <- w_presm
    }
  }
  # Saving results
  name_results <- paste0("results_", seed, ".rds")
  saveRDS(results, file = name_results)
}

# Run iterations in parallel
seed_number <- 1:1000
cl <- makeCluster(num_cores)
# Export required functions
clusterExport(cl, c("n", "seed_number", "presmooth"))
clusterMap(cl, generate_data, seed_number, n)
stopCluster(cl)  # Closes the cluster

# Total program execution time
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")
cat(paste0("Program successfully executed! Total execution time: ", 
           round(total_time, 2), " minutes."))
