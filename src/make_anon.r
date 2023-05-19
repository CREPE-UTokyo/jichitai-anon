# sdcMicro and dplyr packages are required for this script.
if (!require(sdcMicro)) install.packages("sdcMicro")
if (!require(dplyr)) install.packages("dplyr")

# Function for k-anonymization
k_anonymize <- function(header, k, filenames) {
  for (filename in filenames) {
    data <- read.csv(filename) # Load file
    if (!header %in% colnames(data)) stop(paste("Header", header, "not found in file", filename)) # Check header
    # K-anonymization using sdcMicro package
    sdc <- createSdcObj(dat=data, keyVars=header, numVars=header)
    sdc <- kAnon(sdc, k=k) # Execute k-anonymization
    write.csv(sdc@orgData, paste0("k_anonymized_", filename)) # Write result to CSV
  }
}

# Function for random sampling
random_sampling <- function(n, filenames) {
  for (filename in filenames) {
    data <- read.csv(filename) # Load file
    sampled_data <- data %>% sample_n(n) # Perform random sampling
    write.csv(sampled_data, paste0("random_sampling_", filename)) # Write result to CSV
  }
}
