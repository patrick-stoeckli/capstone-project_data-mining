# Set the seed for reproducibility
set.seed(123)

# Define the number of observations
n_obs <- 5

# Generate random sentences
sentences <- c(
  "The quick brown fox jumps over the lazy dog.",
  "A watched pot never boils.",
  "Actions speak louder than words.",
  "All is fair in love and war.",
  "Better late than never.",
  "Curiosity killed the cat.",
  "Don't count your chickens before they hatch."
)

# Generate random numbers
numbers <- sample(1:100, n_obs, replace = TRUE)

# Combine the sentences and numbers into a data frame
df <- data.frame(
  sentence = sample(sentences, n_obs, replace = TRUE),
  number = numbers
)

# Print the data frame
print(df)

library(httr) # for curl requests
library(rjson)

# Set your OpenAI API key
api_key <- "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"

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