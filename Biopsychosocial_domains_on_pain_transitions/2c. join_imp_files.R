library(mice)
library(purrr)

# Folder containing the .rda files
imp_dir <- "path/to/your/imputation/files/"

# Exact filenames
files <- file.path(
  imp_dir,
  sprintf("mult_pain_imp_%d.rda", 1:100)
)

# Check all files exist
stopifnot(all(file.exists(files)))

# Function to load one .rda file safely
load_imp <- function(file) {
  e <- new.env()
  load(file, envir = e)
  
  if (!"imp" %in% ls(e)) {
    stop("No object called 'imp' found in: ", file)
  }
  
  e$imp
}

# Load all 100 mids objects into a list
imp_list <- lapply(files, load_imp)

# Check each has m = 1
sapply(imp_list, function(x) x$m)

# Combine mids objects
imp_all <- Reduce(mice::ibind, imp_list)

# Check m
imp_all$m

# Save
save(imp_all, file="mult_pain_imp_all.rda")