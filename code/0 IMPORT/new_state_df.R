# Load necessary library
library(tibble)

# Create the data frame with state creation events
new_state_df <- tibble(
  state_name = c(
    "Madhya Pradesh", "Chhattisgarh",
    "Bihar", "Jharkhand",
    "Uttar Pradesh", "Uttarakhand",
    "Andhra Pradesh", "Telangana"
  ),
  Year = c(
    "2000", "2000",
    "2000", "2000",
    "2000", "2000",
    "2014", "2014"
  )
)

# Print the table
new_state_df=new_state_df %>% clean_names() %>% 
  mutate(new_state=1)

new_state_df
