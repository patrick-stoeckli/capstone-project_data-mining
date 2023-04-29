# subject <- swissparl::get_data(
#   table = "SubjectBusiness",
#   BusinessShortNumber = "05.057",
#   Language = "DE"
# )
# 
# test <- swissparl::get_data(
#   table = "Transcript",
#   IdSubject = as.numeric(subject$IdSubject),
#   Language = "DE",
#   IdSession = "4713"
# )
# 
# dt <- swissparl::get_data("Transcript", Language = "DE", IdSession = "4713")
# 
# install.packages("purrr")
# library(purrr)
# 
# # Get Session IDs
# sessions50 <- swissparl::get_data("Session", Language = "DE", LegislativePeriodNumber = 50)
# 
# # Define Function
# get_voting_buffered <- function(id) {
#   
#   # Create folder
#   folder <- "voting50"
#   if(!dir.exists(folder)) dir.create(folder)
#   
#   # Download
#   dt <- swissparl::get_data("Transcript", Language = "DE", IdSubject = as.numeric(subject$IdSubject), IdSession = id)
#   
#   # Save
#   saveRDS(dt, paste0(folder, "/", id, ".rds"))
#   
# }
# 
# # Apply Function to Session IDs
# purrr::walk(sessions50$ID, get_voting_buffered)
# 
# # Combine to One Dataset
# v50 <- purrr::map_dfr(list.files("voting50", full.names = T), readRDS)
# 
# df4 <- swissparl::get_data(
#   table = "Transcript", 
#   IdSubject = as.numeric(df3$IdSubject),
#   Language = "DE"
# )
# 
# df5 <- df4 %>% 
#   filter(IdSession >= 4901 & IdSession <= 4920)



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
  
  #dt <- swissparl::get_data("Voting", Language = "DE", IdSession = id)
  
  # Save
  saveRDS(dt, paste0(folder, "/", id, ".rds"))
  
}

# Apply Function to Session IDs
purrr::walk(sessions50$ID, get_voting_buffered)

# Combine to One Dataset
speeches50 <- purrr::map_dfr(list.files("data/data_raw/speeches50", full.names = T), readRDS)

save(speeches50, file = 'data/data_raw/speeches_raw.Rdata') 


# # Get Session IDs
# sessions50 <- swissparl::get_data("Session", Language = "DE", LegislativePeriodNumber = 50)
# sessions50$ID <- as.character(sessions50$ID)
# 
# # Define Function
# get_voting_buffered <- function(id) {
# 
#   # Create folder
#   folder <- "data/data_raw/speeches50"
#   if(!dir.exists(folder)) dir.create(folder)
# 
#   # Download
#   dt <- swissparl::get_data(table = "Transcript", Language = "DE", IdSession = id, IdSubject = as.numeric(df3$IdSubject))
# 
#   #dt <- swissparl::get_data("Voting", Language = "DE", IdSession = id)
# 
#   # Save
#   saveRDS(dt, paste0(folder, "/", id, ".rds"))
# 
# }
# 
# # Apply Function to Session IDs
# purrr::walk(sessions50$ID, get_voting_buffered)
# 
# # Combine to One Dataset
# speeches50 <- purrr::map_dfr(list.files("data/data_raw/speeches50", full.names = T), readRDS)
# 
# save(speeches50, file = 'data/data_raw/speeches_raw.Rdata')

