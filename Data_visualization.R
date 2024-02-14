# MSE Visualization
variables <- ls(pattern = "_mean_")  # All means

for (variable in variables) {
  if (is.na(get(variable))) {
    # Removes NA variable from the vector
    index_na_var <- grep(variable, variables)
    variables <- variables[-index_na_var]
    # Removes NA variable from the environment
    rm(list = variable)
  }
}

pnbc_values <- new.env()
c_rate_values <- new.env()
PNBCS <- seq(0.2, 0.8, by = 0.1)
C_RATES <- seq(0.1, 2, by = 0.1)
n <- 100

# Empty vectors to store data
for (pnbc in PNBCS) {
  name_vector <- paste0("p_", pnbc)
  assign(name_vector, vector())
}

for (c_rate in C_RATES) {
  name_vector <- paste0("c_", c_rate)
  assign(name_vector, vector())
}

# Storing data in the vectors
for (variable in variables) {
  for (pnbc in PNBCS) {
    # String we're looking for (and name of the variable)
    lf_pnbc <- paste0("p_", pnbc)
    if (grepl(lf_pnbc, variable)) {
      pnbc_values[[lf_pnbc]][variable] = get(variable)
    }
  }
  for (c_rate in C_RATES) {
    # String we're looking for (and name of the variable)
    lf_c_rate <- paste0("c_", c_rate, "_")
    if (grepl(lf_c_rate, variable)) {
      c_rate_values[[lf_c_rate]][variable] = get(variable)
    }
  }
}
