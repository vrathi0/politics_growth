from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

class TestTest():
    def setup_method(self, method):
        self.driver = webdriver.Chrome()
        self.vars = {}
    
    def teardown_method(self, method):
        self.driver.quit()
    
    def test_test(self):
        self.driver.get("https://ananthapuramu.ap.gov.in/")
        self.driver.set_window_size(1440, 425)

        # Wait and close any web-based modal dialogs if they appear
        WebDriverWait(self.driver, 10).until(EC.visibility_of_element_located((By.CSS_SELECTOR, "selector-of-modal-dialog")))
        close_modal = self.driver.find_element(By.CSS_SELECTOR, "button-to-close-modal")
        close_modal.click()

        # Handle any new window or tab pop-ups
        original_window = self.driver.current_window_handle
        WebDriverWait(self.driver, 10).until(EC.number_of_windows_to_be(2))  # Adjust the expected number of windows if needed

        for window_handle in self.driver.window_handles:
            if window_handle != original_window:
                self.driver.switch_to.window(window_handle)
                break

        # Interact with elements in the new window
        # ...

        # Close the new window and switch back to the original window
        self.driver.close()
        self.driver.switch_to.window(original_window)

        # Continue with the rest of your test
        WebDriverWait(self.driver, 10).until(
            lambda d: d.find_element(By.ID, "s3wassModalpopupClose").is_displayed()
        )
        self.driver.find_element(By.ID, "s3wassModalpopupClose").click()
        self.driver.find_element(By.CSS_SELECTOR, "#menu-item-24559 > a").click()

        # Extract the table HTML
        WebDriverWait(self.driver, 10).until(
            lambda d: d.find_element(By.CLASS_NAME, "table-responsive").is_displayed()
        )
        table = self.driver.find_element(By.CLASS_NAME, "table-responsive")
        table_html = table.get_attribute('outerHTML')

        # Save the HTML to a file
        with open('collectors_table.html', 'w') as file:
            file.write(table_html)

        print("Table HTML saved to 'collectors_table.html'.")

# Create an instance of the TestTest class
test_instance = TestTest()

# Setup the test environment
test_instance.setup_method(None)

# Run the test
try:
    test_instance.test_test()
finally:
    # Teardown the test environment regardless of test success or failure
    test_instance.teardown_method(None)
