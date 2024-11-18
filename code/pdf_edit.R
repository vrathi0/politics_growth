

## FUNCTION/CODE TO MODIFY PDFs

rm(list=ls())
source("code/functions.R")

# Load necessary libraries
library(pdftools)
library(qpdf)

# Function to extract specific pages from a PDF
extract_pdf_pages <- function(input_pdf, output_pdf, start_page, end_page) {
  # Ensure the pages are valid
  if (start_page < 1 || end_page < start_page) {
    stop("Invalid page numbers")
  }
  
  # Use qpdf to extract pages
  qpdf::pdf_subset(input_pdf, pages = start_page:end_page, output = output_pdf)
  
  message(paste("Pages", start_page, "to", end_page, "have been saved to", output_pdf))
}

# Example of how to use the function
input_pdf <- "/Users/vaibhavrathi/Dropbox/books (Selective Sync Conflict)/Llosa, Mario Vargas - A Writer's Reality.pdf"
output_pdf <- "/Users/vaibhavrathi/Dropbox/books (Selective Sync Conflict)/Llosa_Ch7.pdf"
start_page <- 146
end_page <- 164

extract_pdf_pages(input_pdf, output_pdf, start_page, end_page)
