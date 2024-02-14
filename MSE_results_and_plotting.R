#################### MSE Processing ####################

# Static variables
n <- 50
setwd(paste0("~/Samu2024/n_", n))
SEEDS <- 1:1000
PNBCS <- seq(0.2, 0.8, by = 0.1)
C_RATES <- seq(0.1, 2, by = 0.1)
WWOS <- c("w", "wo")


# Creating empty vectors to store results
for (seed in SEEDS) {
  for (pnbc in PNBCS) {
    for (c_rate in C_RATES) {
      for (wwo in WWOS) {
        name_vector <- paste0("vector_", wwo, "_c_", c_rate, "_p_", pnbc)
        assign(name_vector, vector())
      }
    }
  }
}

# Main loop to get and store the Squared Error
for (seed in SEEDS) {
  # Load data
  file_name <- paste0("results_", seed, ".rds")
  assign(paste0("results_", seed), readRDS(file_name))
  for (pnbc in PNBCS) {
    pbc <- 1 - pnbc  # Probability of being cured
    for (c_rate in C_RATES) {
      for (wwo in WWOS) {
        # Min of $estimate
        name_wwo_prediction <- paste0(wwo, "_prediction")
        assign(name_wwo_prediction,
               min(eval(parse(text = paste0("results_", seed, "$", wwo, 
                                            "_presm_", seed, "_c_", c_rate, 
                                            "_p_", pnbc, "$estimate")))))
        
        # Square error calculation
        name_wwo_result <- paste0(wwo, "_result")
        assign(name_wwo_result, (pbc - get(name_wwo_prediction))^2)
        
        # Adding the result to the vector
        name_vector <- paste0("vector_", wwo, "_c_", c_rate, "_p_", pnbc)
        assign(name_vector, c(get(name_vector), get(name_wwo_result)))  
      }
    }
  }
  rm(list = paste0("results_", seed))  # Delete already used object from memory
}

# Calculation of MSE
for (seed in SEEDS) {
  for (pnbc in PNBCS) {
    for (c_rate in C_RATES) {
      for (wwo in WWOS) {
        name_vector <- paste0("vector_", wwo, "_c_", c_rate, "_p_", pnbc)
        name_mean <- paste0(wwo, "_mean_c_",  c_rate, "_p_", pnbc)
        assign(name_mean, mean(get(name_vector)))  # MSE
      }
    }
  }
}

# MSE Visualization
variables <- ls(pattern = "_mean_")  # All means

# Removing NA variables
for (variable in variables) {
  if (is.na(get(variable))) {
    # Removes NA variable from the vector
    index_na_var <- grep(variable, variables)
    variables <- variables[-index_na_var]
    # Removes NA variable from the environment
    rm(list = variable)
  }
}

# Creation of empty matrices
w_matrix <- matrix(nrow = length(C_RATES), ncol = length(PNBCS))
wo_matrix <- matrix(nrow = length(C_RATES), ncol = length(PNBCS))

# Adding values to matrices
for (variable in variables) {
  splitted <- strsplit(variable, "_")  # Splitting by "_"
  wwo <- splitted[[1]][1]  # WO or W?
  c <- which(C_RATES == splitted[[1]][4])  # Index of C
  p <- which(PNBCS == splitted[[1]][6])  # Index of P
  if (wwo == "w") {  # The data is written in w_matrix
    w_matrix[c, p] <- get(variable)
  } else {  # The data is written in wo_matrix
    wo_matrix[c, p] <- get(variable)
  }
}

# Plotting
for (c_rate in seq_along(C_RATES)) {
  w_data <- w_matrix[c_rate, ]
  wo_data <- wo_matrix[c_rate, ]
  title <- paste0("n = ", n, "Exp. rate: ", format(C_RATES[c_rate], nsmall = 1))
  # Save as PNG
  png(filename = paste0("n_", n, "_exp_rate_", 
                        format(C_RATES[c_rate], nsmall = 1), ".png"),
      width = 1280, height = 720)
  plot(PNBCS, w_data, type = "l", col = "blue", 
       ylim = range(na.omit(c(w_data, wo_data))), 
       xlab = "Probability of not being cured", ylab = "MSE",
       main = title)
  lines(PNBCS, wo_data, col = "red")
  legend("topleft", legend=c("Without presmoothing", "With presmoothing"),
         col=c("red", "blue"), lty = 1:1, cex = 1)
  dev.off()  # Close the PNG
}

for (pnbc in seq_along(PNBCS)) {
  w_data <- w_matrix[ , pnbc]
  wo_data <- wo_matrix[ , pnbc]
  title <- paste0("n = ", n, " Probability of not being cured: ", PNBCS[pnbc])
  # Save as PNG
  png(filename = paste0("n_", n, "_p_", format(PNBCS[pnbc], nsmall = 1), 
                        ".png"), width = 1280, height = 720)
  plot(C_RATES, w_data, type = "l", col = "blue", 
       ylim = range(na.omit(c(w_data, wo_data))), 
       xlab = "Exp. rate", ylab = "MSE",
       main = title)
  lines(C_RATES, wo_data, col = "red")
  legend("topleft", legend=c("Without presmoothing", "With presmoothing"),
         col=c("red", "blue"), lty = 1:1, cex = 1)
  dev.off()  # Close the PNG
}
