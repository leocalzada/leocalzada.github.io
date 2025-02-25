library(RefManageR)
library(stringr)
library(yaml)
library(knitr)

# Function to create folder structure and index.qmd files
create_folder_structure <- function(bib_file, research_folder) {
  # Set BibOptions
  BibOptions(check.entries = FALSE, style = "markdown", bib.style = "authoryear")
  
  # Read the BibTeX file
  bib <- ReadBib(bib_file)
  
  # Create the research folder if it doesn't exist
  dir.create(research_folder, showWarnings = FALSE)
  
  # Iterate through each entry in the BibTeX file
  for (i in seq_along(bib)) {
    entry <- bib[[i]]
    
    # Get the entry type and convert to lowercase
    entry_type <- tolower(entry$bibtype)
    
    # Get the BibTeX key
    bib_key <- names(bib)[i]
    
    # Create type folder
    type_folder <- file.path(research_folder, entry_type)
    dir.create(type_folder, showWarnings = FALSE)
    
    # Create bibkey folder
    bibkey_folder <- file.path(type_folder, bib_key)
    if (dir.exists(bibkey_folder)) {
      cat("Skipping existing folder:", bibkey_folder, "\n")
      next
    }
    dir.create(bibkey_folder, showWarnings = FALSE)
    
    # Create index.qmd file
    index_file <- file.path(bibkey_folder, "index.qmd")
    if (file.exists(index_file)) {
      cat("Skipping existing file:", index_file, "\n")
      next
    }
    
    # Generate citation in markdown format
    citation <- Cite(bib[bib_key], .opts = list(style = "markdown", bib.style = "authoryear"))
    
    # Prepare YAML front matter
    yaml_data <- list(
      title = entry$title,
      date = paste0(entry$year, "-01-01"),
      author = list(
        list(
          name = paste(entry$author[[1]]$given, entry$author[[1]]$family),
          url = "",
          orcid = "",
          affiliation = ""
        )
      ),
      categories = sapply(entry$keywords, function(x) paste0("- ", x)),
      `pub-info` = list(
        reference = as.character(citation),
        links = list(
          list(
            name = "Final version",
            url = ifelse(!is.null(entry$url), entry$url, ""),
            icon = "fa-solid fa-scroll"
          )
        )
      ),
      haiku = list("", "", "")
    )
    
    # Write YAML front matter to index.qmd file
    writeLines(c("---", yaml::as.yaml(yaml_data), "---"), index_file)
    cat("Created:", index_file, "\n")
  }
}

# Usage example
bib_file <- here::here("files/bib/Paperpile-Curriculum Vitae.bib")
research_folder <- "research"
create_folder_structure(bib_file, research_folder)
