# Code according to https://github.com/zumbov2/swissparl

# Install package 'purrr'
# install.packages("purrr")

# Load package 'purrr'
library(purrr)

# Get Session IDs
sessions50 <- swissparl::get_data("Session", Language = "DE", LegislativePeriodNumber = 50)
sessions50$ID <- as.character(sessions50$ID)

# Define Function
get_voting_buffered <- function(id) {
  
  # Create folder
  folder <- "data/data_raw/speeches50"
  if(!dir.exists(folder)) dir.create(folder)
  
  # Download
  dt <- swissparl::get_data(table = "Transcript", Language = "DE", IdSession = id)
  
  # Save
  saveRDS(dt, paste0(folder, "/", id, ".rds"))
  
}

# Apply Function to Session IDs
purrr::walk(sessions50$ID, get_voting_buffered)

# Combine to One Dataset
speeches50 <- purrr::map_dfr(list.files("data/data_raw/speeches50", full.names = T), readRDS)

# Save dataset
save(speeches50, file = 'data/data_raw/speeches_raw.Rdata') 