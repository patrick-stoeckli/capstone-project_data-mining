#Load necessary packages
library(tidyverse)
library(xml2)
library(httr) 
library(rjson)

#Load basic dataset created in .r_file '01_sort_basic_data'
load(file = 'data/data_raw/basic_data.Rdata') 
basic_data <- basic_data %>% 
  select(IdSubject, TitleDE, FirstName, LastName, ParlGroupName) 

#Load speeches dataset created in .r_file '02_download_speeches_data'
load(file = 'data/data_raw/speeches_raw.Rdata') 

#########################################################################
#########################################################################

#Filter out releveant speeches - National Council 
speeches_NR <- speeches50 %>% 
  filter(IdSubject %in% Basic_data_raw$IdSubject) %>%
  filter(SpeakerFunction == "Mit-M" | SpeakerFunction == "Mit-F") %>%
  filter(CouncilName == "Nationalrat") %>%
  select(IdSubject, PersonNumber, SpeakerFullName, ParlGroupName, Text) %>%
  rename(SpeakerPersonNumber = PersonNumber) %>%
  rename(SpeakerParlGroupName = ParlGroupName) %>%
  rename(SpeakerText = Text) 

#Remove formatting-notation
for (i in 1:length(speeches_NR$SpeakerText)) {
  # Umwandlung des XML-Texts in normalen Text
  speeches_NR$SpeakerText[i] <- xml_text(read_xml(speeches_NR$SpeakerText[i]), "//p")
}

#Merge dataset with basic data and save it
speeches_NR <- merge(speeches_NR, basic_data, by = "IdSubject", all.x = TRUE) 
save(speeches_NR, file = 'data/data_processed/speeches_NR.Rdata') 

#########################################################################

#Filter out releveant speeches - Council of States
speeches_SR <- speeches50 %>% 
  filter(IdSubject %in% Basic_data_raw$IdSubject) %>%
  filter(SpeakerFunction == "Mit-M" | SpeakerFunction == "Mit-F") %>%
  filter(CouncilName == "Ständerat") %>% 
  select(IdSubject, PersonNumber, SpeakerFullName, ParlGroupName, Text) %>%
  rename(SpeakerPersonNumber = PersonNumber) %>%
  rename(SpeakerParlGroupName = ParlGroupName) %>%
  rename(SpeakerText = Text) 

#Remove formatting-notation
for (i in 1:length(speeches_SR$SpeakerText)) {
  # Umwandlung des XML-Texts in normalen Text
  speeches_SR$SpeakerText[i] <- xml_text(read_xml(speeches_SR$SpeakerText[i]), "//p")
}

#Merge dataset with basic data and save it
speeches_SR <- merge(speeches_SR, basic_data, by = "IdSubject", all.x = TRUE) 
save(speeches_SR, file = 'data/data_processed/speeches_SR.Rdata')

#########################################################################
#########################################################################

######RQ1: Positive or negative - National Council###### 

load(file = 'data/data_processed/speeches_NR.Rdata')

assign("speeches_NR_rq1", speeches_NR)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "What is the sentiment of the following text using a number between -1 and 1? -1 is very negative, 1 is very positive. Please only print the number itself: \nText:"

speeches_NR_rq1$npvalue <- rep(0, nrow(speeches_NR_rq1))

# Call the OpenAI API to generate output text based on the combined text
for (i in 1201:nrow(speeches_NR_rq1)){
  input_text <- speeches_NR_rq1$SpeakerText[i]            # Define the input text for sentiment analysis
  combined_text <- paste(prompt, input_text)        # Combine the prompt and input text
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user",
        content = combined_text
      ))
    )
  )
  parsed_response <- fromJSON(httr::content(response, as = "text"))
  speeches_NR_rq1$npvalue[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

speeches_NR_rq1 <- speeches_NR_rq1 %>% 
  mutate(npcopy = npvalue) %>%
  mutate(npvalue = gsub("[^0-9.-]+", "", npvalue)) %>%
  mutate(npvalue = as.numeric(npvalue)) 

# missing_obs <- speeches_NR_rq1[is.na(speeches_NR_rq1$npvalue), ]
# missing_obs

speeches_NR_rq1$npvalue[1068] <- 0

save(speeches_NR_rq1, file = 'data/data_processed/speeches_NR_rq1.Rdata')

#########################################################################

######RQ1: Positive or negative - Council of States ###### 

load(file = 'data/data_processed/speeches_SR.Rdata')

assign("speeches_SR_rq1", speeches_SR)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "What is the sentiment of the following text using a number between -1 and 1? -1 is very negative, 1 is very positive. Please only print the number itself: \nText:"

speeches_SR_rq1$npvalue <- rep(0, nrow(speeches_SR_rq1))

# Call the OpenAI API to generate output text based on the combined text
for (i in 1:nrow(speeches_SR_rq1)){
  input_text <- speeches_SR_rq1$SpeakerText[i]            # Define the input text for sentiment analysis
  combined_text <- paste(prompt, input_text)        # Combine the prompt and input text
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user",
        content = combined_text
      ))
    )
  )
  parsed_response <- fromJSON(httr::content(response, as = "text"))
  speeches_SR_rq1$npvalue[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

speeches_SR_rq1 <- speeches_SR_rq1 %>% 
  mutate(npcopy = npvalue) %>%
  mutate(npvalue = gsub("[^0-9.-]+", "", npvalue)) %>%
  mutate(npvalue = as.numeric(npvalue)) 

# missing_obs <- speeches_SR_rq1[is.na(speeches_SR_rq1$npvalue), ]
# missing_obs

save(speeches_SR_rq1, file = 'data/data_processed/speeches_SR_rq1.Rdata')

#########################################################################
#########################################################################

######RQ2: Left or right and conservative or liberal - National Council###### 

load(file = 'data/data_processed/speeches_NR.Rdata')

assign("speeches_NR_rq2", speeches_NR)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "Analyse the following text regarding its political bias. Give me two numbers between -1 and 1. The first one should reflect if the text represents a politicially left (-1) or a polticially right (1) statement. The second one should reflect if the text represents a politicially conservative (-1) or a polticially liberal (1) statement. Please round the numbers to one decimal place first. Then only print the two numbers separated by a comma without any additional explanation.  It is very important that you only print the numbers. I do not want any sort of explanation at all. This is the text:"
speeches_NR_rq2$lrclvalue <- rep(0, nrow(speeches_NR_rq2))

# Call the OpenAI API to generate output text based on the combined text
for (i in 1:nrow(speeches_NR_rq2)){
  input_text <- speeches_NR_rq2$SpeakerText[i]            # Define the input text for sentiment analysis
  combined_text <- paste(prompt, input_text)        # Combine the prompt and input text
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user",
        content = combined_text
      ))
    )
  )
  parsed_response <- fromJSON(httr::content(response, as = "text"))
  #ifelse(length(parsed_response$choices[[1]]$message$content)==0,speeches_NR_rq2$lrclvalue[i] <- "Error",speeches_NR_rq2$lrclvalue[i] <- parsed_response$choices[[1]]$message$content)
  speeches_NR_rq2$lrclvalue[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

speeches_NR_rq2 <- speeches_NR_rq2 %>% 
  mutate(lrclcopy = lrclvalue) %>%
  separate(col = lrclvalue, into = c("lrvalue", "clvalue"), sep = ",") %>%
  mutate(lrvalue = as.numeric(lrvalue)) %>%
  mutate(clvalue = as.numeric(clvalue)) 

# missing_obs <- speeches_NR_rq2[is.na(speeches_NR_rq2$lrvalue), ]
# missing_obs
# 
# missing_obs2 <- speeches_NR_rq2[is.na(speeches_NR_rq2$clvalue), ]
# missing_obs2

speeches_NR_rq2$clvalue[139] <- 0.9
speeches_NR_rq2$clvalue[1468] <- 0.1

save(speeches_NR_rq2, file = 'data/data_processed/speeches_NR_rq2.Rdata')

#########################################################################

######RQ2: Left or right and conservative or liberal - Council of States ###### 

load(file = 'data/data_processed/speeches_SR.Rdata')

assign("speeches_SR_rq2", speeches_SR)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "Analyse the following text regarding its political bias. Give me two numbers between -1 and 1. The first one should reflect if the text represents a politicially left (-1) or a polticially right (1) statement. The second one should reflect if the text represents a politicially conservative (-1) or a polticially liberal (1) statement. Please round the numbers to one decimal place first. Then only print the two numbers separated by a comma without any additional explanation.  It is very important that you only print the numbers. I do not want any sort of explanation at all. This is the text:"
speeches_SR_rq2$lrclvalue <- rep(0, nrow(speeches_SR_rq2))

# Call the OpenAI API to generate output text based on the combined text
for (i in 1:nrow(speeches_SR_rq2)){
  input_text <- speeches_SR_rq2$SpeakerText[i]            # Define the input text for sentiment analysis
  combined_text <- paste(prompt, input_text)        # Combine the prompt and input text
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user",
        content = combined_text
      ))
    )
  )
  parsed_response <- fromJSON(httr::content(response, as = "text"))
  speeches_SR_rq2$lrclvalue[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

speeches_SR_rq2 <- speeches_SR_rq2 %>% 
  mutate(lrclcopy = lrclvalue) %>%
  separate(col = lrclvalue, into = c("lrvalue", "clvalue"), sep = ",") %>%
  mutate(lrvalue = as.numeric(lrvalue)) %>%
  mutate(clvalue = as.numeric(clvalue)) 

# missing_obs <- speeches_SR_rq2[is.na(speeches_SR_rq2$lrvalue), ]
# missing_obs
# 
# missing_obs2 <- speeches_SR_rq2[is.na(speeches_SR_rq2$clvalue), ]
# missing_obs2

speeches_SR_rq2$clvalue[376] <- 0.6
speeches_SR_rq2$clvalue[694] <- 0.0

# 311 and 700 are too long
speeches_SR_rq2 <- speeches_SR_rq2[-c(376,694),]

save(speeches_SR_rq2, file = 'data/data_processed/speeches_SR_rq2.Rdata')

#########################################################################
#########################################################################

######RQ3: Complexity and formality - National Council###### 

load(file = 'data/data_processed/speeches_NR.Rdata')

assign("speeches_NR_rq3", speeches_NR)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "Analyze the following text regarding its complexity and formality. Give me for both factors a number between 0 and 10. The first number should reflect if the text is complex, the second number should reflect if the text is formal. Print only the two integer numbers separated by a comma without any additional explanation or other words. It is very important that you only print the numbers separated by a comma. I do not want any sort of explanation at all. This is the text:"
speeches_NR_rq3$cfvalue <- rep(0, nrow(speeches_NR_rq3))

# Call the OpenAI API to generate output text based on the combined text
for (i in 311:nrow(speeches_NR_rq3)){
  input_text <- speeches_NR_rq3$SpeakerText[i]            # Define the input text for sentiment analysis
  combined_text <- paste(prompt, input_text)        # Combine the prompt and input text
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user",
        content = combined_text
      ))
    )
  )
  parsed_response <- fromJSON(httr::content(response, as = "text"))
  speeches_NR_rq3$cfvalue[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

for (i in 1:nrow(speeches_NR_rq3)){
  speeches_NR_rq3$fvalue[i] <- substr(speeches_NR_rq3$cfvalue[i], nchar(speeches_NR_rq3$cfvalue[i]), nchar(speeches_NR_rq3$cfvalue[i]))
  speeches_NR_rq3$cvalue[i] <- (as.numeric(substr(speeches_NR_rq3$cfvalue[i], nchar(1), nchar(1))))
}

na_indices <- which(is.na(speeches_NR_rq3$cvalue))

for (i in na_indices){
  value <- sub(".*Complexity: ([0-9]+).*", "\\1", speeches_NR_rq3$cfvalue[i])
  numeric_value <- as.numeric(value)
  speeches_NR_rq3$cvalue[i] <- value
}  

speeches_NR_rq3 <- speeches_NR_rq3 %>% 
  mutate(cvalue = as.numeric(cvalue)) %>%
  mutate(fvalue = as.numeric(fvalue)) 

missing_obs <- speeches_NR_rq3[is.na(speeches_NR_rq3$cvalue), ]
missing_obs

speeches_NR_rq3$cvalue[39] <- 5
speeches_NR_rq3$cvalue[365] <- 7

missing_obs2 <- speeches_NR_rq3[is.na(speeches_NR_rq3$fvalue), ]
missing_obs2

speeches_NR_rq3$fvalue[39] <- 8
speeches_NR_rq3$fvalue[267] <- 8 
speeches_NR_rq3$fvalue[335] <- 8 
speeches_NR_rq3$fvalue[365] <- 9
speeches_NR_rq3$fvalue[771] <- 7 

save(speeches_NR_rq3, file = 'data/data_processed/speeches_NR_rq3.Rdata')

#########################################################################

######RQ3: Complexity and formality - Council of States ###### 

load(file = 'data/data_processed/speeches_SR.Rdata')

assign("speeches_SR_rq3", speeches_SR)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "Analyze the following text regarding its complexity and formality. Give me for both factors a number between 0 and 10. The first number should reflect if the text is complex, the second number should reflect if the text is formal. Print only the two integer numbers separated by a comma without any additional explanation or other words. It is very important that you only print the numbers separated by a comma. I do not want any sort of explanation at all. This is the text:"
speeches_SR_rq3$cfvalue <- rep(0, nrow(speeches_SR_rq3))

# Call the OpenAI API to generate output text based on the combined text
for (i in 311:nrow(speeches_SR_rq3)){
  input_text <- speeches_SR_rq3$SpeakerText[i]            # Define the input text for sentiment analysis
  combined_text <- paste(prompt, input_text)        # Combine the prompt and input text
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user",
        content = combined_text
      ))
    )
  )
  parsed_response <- fromJSON(httr::content(response, as = "text"))
  ifelse(length(parsed_response$choices[[1]]$message$content)==0,speeches_SR_rq3$cfvalue[i] <- "Error",speeches_SR_rq3$cfvalue[i] <- parsed_response$choices[[1]]$message$content)
  #speeches_SR_rq3$cfvalue[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

for (i in 1:nrow(speeches_SR_rq3)){
  speeches_SR_rq3$fvalue[i] <- substr(speeches_SR_rq3$cfvalue[i], nchar(speeches_SR_rq3$cfvalue[i]), nchar(speeches_SR_rq3$cfvalue[i]))
  speeches_SR_rq3$cvalue[i] <- (as.numeric(substr(speeches_SR_rq3$cfvalue[i], nchar(1), nchar(1))))
}

na_indices <- which(is.na(speeches_SR_rq3$cvalue))

for (i in na_indices){
  value <- sub(".*Complexity: ([0-9]+).*", "\\1", speeches_SR_rq3$cfvalue[i])
  numeric_value <- as.numeric(value)
  speeches_SR_rq3$cvalue[i] <- value
}  

speeches_SR_rq3 <- speeches_SR_rq3 %>% 
  mutate(cvalue = as.numeric(cvalue)) %>%
  mutate(fvalue = as.numeric(fvalue)) 

missing_obs <- speeches_SR_rq3[is.na(speeches_SR_rq3$cvalue), ]
missing_obs

speeches_SR_rq3$cvalue[844] <- 5
speeches_SR_rq3$cvalue[594] <- 5
speeches_SR_rq3$cvalue[1145] <- 3 

missing_obs2 <- speeches_SR_rq3[is.na(speeches_SR_rq3$fvalue), ]
missing_obs2

speeches_SR_rq3$fvalue[844] <- 8
speeches_SR_rq3$fvalue[905] <- 9
speeches_SR_rq3$fvalue[1052] <- 7 
speeches_SR_rq3$fvalue[1086] <- 8 
speeches_SR_rq3$fvalue[1145] <- 8 

speeches_SR_rq3 <- speeches_SR_rq3[-c(311,700),]

save(speeches_SR_rq3, file = 'data/data_processed/speeches_SR_rq3.Rdata')