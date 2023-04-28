library(httr) 
library(rjson)
library(xml2)
library(tidyverse)

load(file = 'data/data_raw/Basic_data_raw.Rdata') 

Basic <- Basic_data_raw %>% 
  select(IdSubject, TitleDE, FirstName, LastName, ParlGroupName) 

load(file = 'data/data_raw/speeches_raw.Rdata') 

speeches_NR <- speeches50 %>% 
  filter(IdSubject %in% Basic_data_raw$IdSubject) %>%
  filter(SpeakerFunction == "Mit-M" | SpeakerFunction == "Mit-F") %>%
  filter(CouncilName == "Nationalrat") %>%
  select(IdSubject, PersonNumber, SpeakerFullName, ParlGroupName, Text) %>%
  rename(SpeakerPersonNumber = PersonNumber) %>%
  rename(SpeakerParlGroupName = ParlGroupName) %>%
  rename(SpeakerText= Text) 

for (i in 1:length(speeches_NR$SpeakerText)) {
  # Umwandlung des XML-Texts in normalen Text
  speeches_NR$SpeakerText[i] <- xml_text(read_xml(speeches_NR$SpeakerText[i]), "//p")
}

save(speeches_NR, file = 'data/data_raw/speeches_NR.Rdata') 

speeches_NR_merged <- merge(speeches_NR, Basic, by = "IdSubject", all.x = TRUE) 

save(speeches_NR_merged, file = 'data/data_raw/speeches_NR_merged.Rdata') 

# NA
# "P-F" "P-M"                  
# "1VP-F" "1VP-M"
# "2VP-F" "2VP-M"
# "BR-F"
# "VPBR-F"   
# "BPR-F"

# load(file = 'data/data_raw/speeches_NR_merged.Rdata') 
# 
# assign("speeches_NR_final", speeches_NR_merged)
# 
# # Set your OpenAI API key
# api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)
# 
# # Define the sentiment-related prompt
# prompt <- "What is the sentiment of the following text using a number between -1 and 1? -1 is very negative, 1 is very positive. Please only print the number itself: \nText:"
# 
# speeches_NR_final$value <- rep(0, nrow(speeches_NR_final))
# 
# # Call the OpenAI API to generate output text based on the combined text
# for (i in 1201:nrow(speeches_NR_final)){
#   input_text <- speeches_NR_final$SpeakerText[i]            # Define the input text for sentiment analysis
#   combined_text <- paste(prompt, input_text)        # Combine the prompt and input text
#   response <- httr::POST(
#     url = "https://api.openai.com/v1/chat/completions", 
#     httr::add_headers(Authorization = paste("Bearer", api_key)),
#     content_type_json(),
#     encode = "json",
#     body = list(
#       model = "gpt-3.5-turbo", 
#       messages = list(list(
#         role = "user", 
#         content = combined_text 
#       ))
#     )
#   )
#   parsed_response <- fromJSON(httr::content(response, as = "text"))
#   speeches_NR_final$value[i] <- parsed_response$choices[[1]]$message$content
#   print(i)
#   Sys.sleep(20)
# }
# 
# save(speeches_NR_final, file = 'data/data_processed/speeches_NR_final.Rdata') 
# 
#   
# 



load(file = 'data/data_raw/speeches_NR_merged.Rdata')

assign("speeches_NR_final_2", speeches_NR_merged)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "What is the sentiment of the following text using a number between -1 and 1? -1 is very negative, 1 is very positive. Please only print the number itself: \nText:"

speeches_NR_final_2$value <- rep(0, nrow(speeches_NR_final_2))

# Call the OpenAI API to generate output text based on the combined text
for (i in 1201:nrow(speeches_NR_final_2)){
  input_text <- speeches_NR_final_2$SpeakerText[i]            # Define the input text for sentiment analysis
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
  speeches_NR_final_2$value[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

save(speeches_NR_final_2, file = 'data/data_processed/speeches_NR_final_2.Rdata')
# 
#   
# 

speeches_SR <- speeches50 %>% 
  filter(IdSubject %in% Basic_data_raw$IdSubject) %>%
  filter(SpeakerFunction == "Mit-M" | SpeakerFunction == "Mit-F") %>%
  filter(CouncilName == "Ständerat") %>% 
  select(IdSubject, PersonNumber, SpeakerFullName, ParlGroupName, Text) %>%
  rename(SpeakerPersonNumber = PersonNumber) %>%
  rename(SpeakerParlGroupName = ParlGroupName) %>%
  rename(SpeakerText= Text) 

for (i in 1:length(speeches_SR$SpeakerText)) {
  # Umwandlung des XML-Texts in normalen Text
  speeches_SR$SpeakerText[i] <- xml_text(read_xml(speeches_SR$SpeakerText[i]), "//p")
}

save(speeches_SR, file = 'data/data_raw/speeches_SR.Rdata') 

speeches_SR_merged <- merge(speeches_SR, Basic, by = "IdSubject", all.x = TRUE) 

save(speeches_SR_merged, file = 'data/data_raw/speeches_SR_merged.Rdata')

load(file = 'data/data_raw/speeches_SR_merged.Rdata')

assign("speeches_SR_final", speeches_SR_merged)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "What is the sentiment of the following text using a number between -1 and 1? -1 is very negative, 1 is very positive. Please only print the number itself: \nText:"

speeches_SR_final$value <- rep(0, nrow(speeches_SR_final))

# Call the OpenAI API to generate output text based on the combined text
# for (i in 1:nrow(speeches_SR_final)){
#   input_text <- speeches_SR_final$SpeakerText[i]            # Define the input text for sentiment analysis
#   combined_text <- paste(prompt, input_text)        # Combine the prompt and input text
#   response <- httr::POST(
#     url = "https://api.openai.com/v1/chat/completions",
#     httr::add_headers(Authorization = paste("Bearer", api_key)),
#     content_type_json(),
#     encode = "json",
#     body = list(
#       model = "gpt-3.5-turbo",
#       messages = list(list(
#         role = "user",
#         content = combined_text
#       ))
#     )
#   )
#   parsed_response <- fromJSON(httr::content(response, as = "text"))
#   speeches_SR_final$value[i] <- parsed_response$choices[[1]]$message$content
#   print(i)
#   Sys.sleep(20)
# }
# 
# save(speeches_SR_final, file = 'data/data_processed/speeches_SR_final.Rdata')


load(file = 'data/data_raw/speeches_SR_merged.Rdata')

assign("speeches_SR_final_2", speeches_SR_merged)

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "What is the sentiment of the following text using a number between -1 and 1? -1 is very negative, 1 is very positive. Please only print the number itself: \nText:"

speeches_SR_final_2$value <- rep(0, nrow(speeches_SR_final_2))

# Call the OpenAI API to generate output text based on the combined text
for (i in 701:nrow(speeches_SR_final_2)){
  input_text <- speeches_SR_final_2$SpeakerText[i]            # Define the input text for sentiment analysis
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
  speeches_SR_final_2$value[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

save(speeches_SR_final_2, file = 'data/data_processed/speeches_SR_final_2.Rdata')

