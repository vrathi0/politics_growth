from selenium import webdriver
from selenium.webdriver.support.ui import Select
import time

# Initialize the webdriver (example with Chrome)
driver = webdriver.Chrome()

# Open the webpage
driver.get("https://dop.rajasthan.gov.in/forms/CivilType_history.aspx")

# Allow some time for the page to load
time.sleep(3)

# Example: Select an option from the service type dropdown
service_type_dropdown = Select(driver.find_element_by_id('your_dropdown_id_here'))
service_type_dropdown.select_by_visible_text('Service Type Example') # Replace with actual text

# Similar steps for other dropdowns or inputs...

# Allow time for the page to update based on selection
time.sleep(3)

# Now scrape the required data
# data = driver.find_element_by_xpath('your_xpath_here').text

# Process and save your data...

# Close the browser
driver.quit()
