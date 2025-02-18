library(rvest)

# Load the HTML content from the file
html_file <- "code/0 DOCS/test.txt"
html_content <- read_html(html_file)

# Extract the dropdown list items
dropdown_items <- html_content %>%
  html_nodes(".selectize-dropdown-content .option") %>%
  html_text(trim = TRUE)

# Display the dropdown list
print(dropdown_items)

# Save the dropdown list to a CSV file (optional)
write.csv(dropdown_items, "dropdown_list.csv", row.names = FALSE)
