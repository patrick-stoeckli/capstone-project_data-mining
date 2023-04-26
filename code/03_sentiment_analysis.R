library(httr) 
library(rjson)
library(xml2)
library(tidyverse)

load(file = 'data/data_raw/Basic_raw.Rdata') 

Basic <- Basic_raw %>% 
  select(IdSubject, TitleDE, FirstName, LastName, ParlGroupName) 

load(file = 'data/data_raw/speeches_raw.Rdata') 

speeches_NR <- v50 %>% 
  filter(IdSubject %in% Basic_raw$IdSubject) %>%
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

first_try <- merge(speeches_NR, Basic, by = "IdSubject", all.x = TRUE) 

speeches_SR <- v50 %>% 
  filter(IdSubject %in% Basic_raw$IdSubject) %>%
  filter(SpeakerFunction == "Mit-M" | SpeakerFunction == "Mit-F") %>%
  filter(CouncilName == "Ständerat")       

for (i in 1:length(speeches_SR$Text)) {
  # Umwandlung des XML-Texts in normalen Text
  speeches_SR$Text[i] <- xml_text(read_xml(speeches_SR$Text[i]), "//p")
}

# NA
# "P-F" "P-M"                  
# "1VP-F" "1VP-M"
# "2VP-F" "2VP-M"
# "BR-F"
# "VPBR-F"   
# "BPR-F"

# Set your OpenAI API key
api_key <- readLines("credentials/openAI_api-key.txt", n = 1, warn = FALSE)

# Define the sentiment-related prompt
prompt <- "What is the sentiment of the following text using a number between -1 and 1? -1 is very negative, 1 is very positive. Please only print the number itself: \nText:"

first_try$value <- rep(0, 301)

# Call the OpenAI API to generate output text based on the combined text
for (i in 1:nrow(first_try)){
  input_text <- first_try$SpeakerText[i]            # Define the input text for sentiment analysis
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
  first_try$value[i] <- parsed_response$choices[[1]]$message$content
  print(i)
  Sys.sleep(20)
}

save(first_try, file = 'data/data_raw/first_try.Rdata') 

  

