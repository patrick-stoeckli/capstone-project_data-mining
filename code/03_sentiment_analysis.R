library(httr) 
library(rjson)
library(xml2)

load(file = 'data/data_raw/speeches_raw.Rdata') 
load(file = 'data/data_raw/Basic_raw.Rdata') 

speeches_NR <- v50 %>% 
  filter(IdSubject %in% df3$IdSubject) %>%
  filter(SpeakerFunction == "Mit-M" | SpeakerFunction == "Mit-F") %>%
  filter(CouncilName == "Nationalrat")

speeches_SR <- v50 %>% 
  filter(IdSubject %in% df3$IdSubject) %>%
  filter(SpeakerFunction == "Mit-M" | SpeakerFunction == "Mit-F") %>%
  filter(CouncilName == "Ständerat")       

for (i in 1:length(speeches_NR$Text)) {
  # Umwandlung des XML-Texts in normalen Text
  speeches_NR$Text[i] <- xml_text(read_xml(speeches_NR$Text[i]), "//p")
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
prompt <- "What is the sentiment of the following text using just one word?\nText:"

# Call the OpenAI API to generate output text based on the combined text
for (i in 1:nrow(df)){
  input_text <- df$sentence[i]                      # Define the input text for sentiment analysis
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
  df$output_text[i] <- parsed_response$choices$message$content
  Sys.sleep(20)
}