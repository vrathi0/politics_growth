install.packages("jsonlite")
install.packages("httr")
install.packages("reticulate")


library(jsonlite)

# Load the JSON data
json_data <- fromJSON("/Users/vaibhavrathi/Dropbox/chatgpt_export/conversations.json", flatten = TRUE)

# Check the structure of the JSON
str(json_data)


count_tokens <- function(text) {
  words <- strsplit(text, "\\s+")[[1]]  # Split text into words
  return(length(words) * 1.3)  # Approximate tokens (each word ~1.3 tokens)
}

# Extract messages
messages <- unlist(json_data$messages$content)

# Apply token counting function
token_counts <- sapply(messages, count_tokens)

# Total token estimate
total_tokens <- sum(token_counts)
print(paste("Estimated total tokens:", total_tokens))


input_tokens <- total_tokens / 2
output_tokens <- total_tokens / 2

input_cost <- (input_tokens / 1000) * 0.01
output_cost <- (output_tokens / 1000) * 0.03
total_cost <- input_cost + output_cost

print(paste("Estimated API cost: $", round(total_cost, 2)))
