library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)

sc_dir <- "data/raw/south_carolina"
sc_files <- list.files(sc_dir, pattern = "\\.xlsx$", full.names = TRUE)

clean_sc_file <- function(path) {
  # Skip non-valid xlsx files (e.g. 404 HTML error files < 100KB)
  if (file.info(path)$size < 100000) {
    cat("Skipping non-Excel file:", basename(path), "\n")
    return(NULL)
  }
  

  sheets <- excel_sheets(path)
  if (!"District" %in% sheets) {
    cat("No 'District' sheet in:", basename(path), "\n")
    return(NULL)
  }
  
  # Extract year from filename (e.g. scready_2024.xlsx)
  yr <- as.integer(stringr::str_extract(basename(path), "\\d{4}"))
  
  df_raw <- read_excel(path, sheet = "District")
  
  df_raw |>
    rename_with(tolower) |>
    filter(!is.na(distcode)) |>
    mutate(
      year = yr,
      period = if_else(year <= 2024, "pre_ban", "post_ban"),
      period = factor(period, levels = c("pre_ban", "post_ban"))
    )
}

sc_list <- map(sc_files, clean_sc_file) |> compact()

if (length(sc_list) > 0) {
  sc_combined <- bind_rows(sc_list)
  write_csv(sc_combined, "data/clean/sc_district_clean.csv")
  cat("Successfully updated data/clean/sc_district_clean.csv with", nrow(sc_combined), "rows.\n")
} else {
  cat("No valid SC Excel files found.\n")
}
