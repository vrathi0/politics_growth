rm(list=ls())
source("code/functions.R")


library(pdftools)
library(RefManageR)

library(pdftools)
library(RefManageR)

# Set the path to the folder containing the PDF files
pdf_folder <- "papers"

# Get the list of PDF files in the folder
pdf_files <- list.files(pdf_folder, pattern = "*.pdf", full.names = TRUE)

# Initialize an empty list to store the extracted references
references <- list()

# Loop through each PDF file
for (pdf_file in p) {
  # Read the PDF file
  pdf_text <- pdf_text(pdf_files[p])
  
  # Combine all the pages into a single character vector
  full_text <- paste(pdf_text, collapse = " ")
  
  # Extract the reference section using regular expressions
  reference_text <- regmatches(full_text, regexpr("References.*", full_text, ignore.case = TRUE))
  
  # Store the extracted references
  references[[p]] <- reference_text
}

# Combine all the references into a single character vector
all_references <- unlist(references)

# Create a list to store the BibTeX entries
bib_entries <- list()

# Generate unique keys for each reference
keys <- paste0("key_", 1:length(separated_references[[1]]))

dd=separated_references[[1]]

bib_entries <- lapply(dd, function(reference) {
  bib_entry <- ref_manager$new.entry(type = "misc")
  bib_entry$set("title", reference)
  bib_entry
})

# Combine the BibEntry objects into a BibEntryList
bib_entry_list <- BibEntryList$new(bib_entries)


# Loop through each reference and create a BibTeX entry
for (i in 1:33) {
  bib_entry_o <- BibEntry$new(bibtype = "misc", key = keys[i])
  bib_entry$setField("note", separated_references[[1]][i])
  bib_entries[[i]] <- bib_entry
}

# Create a BibTeX file
write.bibtex(bib_entries, file = "references.bib")


##########################
##########################
##########################
##########################

# Function to convert a reference to BibTeX format
formatReferenceToBibTeX <- function(title, authors, journal, volume, number, 
                                    pages, year, publisher = NULL, note = NULL) {
  bib <- "@"
  
  # Determine the type of reference based on available information
  if (!is.null(journal)) {
    bib <- paste0(bib, "article")
  } else if (!is.null(publisher)) {
    bib <- paste0(bib, "book")
  } else {
    bib <- paste0(bib, "unpublished")
  }
  
  # Add reference key
  bib <- paste0(bib, "{", gsub("[^A-Za-z0-9]+", "", tolower(paste0(authors, year))), ",\n")
  
  # Add reference fields
  bib <- paste0(bib, "  title = {", title, "},\n")
  bib <- paste0(bib, "  author = {", authors, "},\n")
  bib <- paste0(bib, "  journal = {", journal, "},\n")  # Only for article type
  bib <- paste0(bib, "  volume = {", volume, "},\n")  # Only for article type
  bib <- paste0(bib, "  number = {", number, "},\n")  # Only for article type
  bib <- paste0(bib, "  pages = {", pages, "},\n")  # Only for article type
  bib <- paste0(bib, "  year = {", year, "},\n")
  
  # Add optional fields
  if (!is.null(publisher)) {
    bib <- paste0(bib, "  publisher = {", publisher, "},\n")
  }
  
  if (!is.null(note)) {
    bib <- paste0(bib, "  note = {", note, "},\n")
  }
  
  # Remove trailing comma and newline, add closing curly brace
  bib <- paste0(substr(bib, 1, nchar(bib) - 2), "\n}\n")
  
  return(bib)
}

# Function to convert a list of references to BibTeX format
convertReferencesToBibTeX <- function(references) {
  bibtex <- ""
  
  for (ref in references) {
    bibtex <- paste0(bibtex, formatReferenceToBibTeX(ref$title, ref$authors, ref$journal, ref$volume, ref$number, ref$pages, ref$year, ref$publisher, ref$note))
  }
  
  return(bibtex)
}

# Example usage
references <- list(
  list(
    title = "A theory of misgovernance",
    authors = "Banerjee, A.V.",
    journal = "Quarterly Journal of Economics",
    volume = "112",
    pages = "1289–1332",
    year = "1997"
  ),
  list(
    title = "Corruption and development: a review of issues",
    authors = "Bardhan, P.",
    journal = "Journal of Economic Literature",
    volume = "35",
    pages = "1320–1346",
    year = "1997"
  ),
  # Add more references as needed
)

bibtexOutput <- convertReferencesToBibTeX(references)
cat(bibtexOutput)



extractFieldValue <- function(text, pattern) {
  match <- regmatches(text, gregexpr(pattern, text))
  if (length(match) > 0) {
    return(gsub(pattern, "\\1", match[[1]]))
  } else {
    return(NA)
  }
}

# Function to convert a reference text to a reference object
convertTextToReference <- function(text) {
  title <- sub("^(.*?)\\.\\s.*", "\\1", text)
  authors <- sub("^(.*?),.*", "\\1", text)
  journal <- sub("^.*?\\.\\s([A-Z][a-z]+(?:\\s[A-Z][a-z]+)*).*", "\\1", text)
  volume <- sub("^.*?\\s([0-9]{1,3}).*", "\\1", text)
  number <- sub("^.*?\\s[0-9]{1,3}(?:,\\s([0-9]{1,3})).*", "\\1", text)
  pages <- sub("^.*?\\s[0-9]{1,3}(?:,\\s[0-9]{1,3})\\s(.*).", "\\1", text)
  year <- sub("^.*?([0-9]{4})\\..*", "\\1", text)
  
  reference <- list(
    title = title,
    authors = authors,
    journal = journal,
    volume = volume,
    number = number,
    pages = pages,
    year = year
  )
  
  return(reference)
}

referenceText <- "Banerjee, A.V., 1997. A theory of misgovernance. Quarterly Journal of Economics 112, 1289–1332."

# Convert the reference text to a reference object
reference <- convertTextToReference(referenceText)

