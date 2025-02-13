# Install packages if not already installed
if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("readr")) install.packages("readr")

library(httr)
library(jsonlite)
library(readr)
library(dplyr)


# ----- Step 1: Read the TSV file directly from the URL -----

tsv_url <- "https://omim.org/phenotypicSeries/PS115200?format=tsv"
# read_tsv automatically uses tab as the delimiter.
dcm_phenotypic_series <- read_tsv(tsv_url, skip = 6)

# ----- Step 2: Extract the Phenotype MIM Number column -----
# (Inspect the column names to ensure you have the correct one)
#print(names(df))
# In this example we assume the column is named "Phenotype MIM Number".
# Use backticks since the column name contains spaces.
#omim_numbers <- as.character(dcm_phenotypic_series$`Phenotype MIM number`)
#omim_numbers <- omim_numbers[!is.na(omim_numbers)]


# ----- Step 2: Get HPO for each OMIM number -----

hpo_url <- "https://github.com/obophenotype/human-phenotype-ontology/releases/download/v2025-01-16/genes_to_phenotype.txt"

# Read the tab-delimited file (adjust the file path as needed)
data <- read.delim(hpo_url, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# If your file contains both OMIM and other IDs (e.g., ORPHA), filter for OMIM entries:
omim_data <- data %>%
  dplyr::filter(grepl("^OMIM:", disease_id)) %>%
  dplyr::mutate(disease_id = sub("^OMIM:", "", disease_id))

# Option 1: If you want HPO terms for all OMIM disease IDs:
#result_all <- omim_data %>%
#  group_by(disease_id) %>%
#  dplyr::summarize(
#    hpo_ids   = paste(unique(hpo_id), collapse = ", "),
#    hpo_names = paste(unique(hpo_name), collapse = ", ")
#  )

#print(result_all)

# Option 2: If you have a specific list of OMIM disease IDs:
desired_ids <- as.list(dcm_phenotypic_series$`Phenotype MIM number`)

result_subset <- omim_data %>%
  dplyr::filter(disease_id %in% desired_ids) %>%
  group_by(disease_id) %>%
  dplyr::summarize(
    hpo_ids   = paste(unique(hpo_id), collapse = ", "),
    hpo_names = paste(unique(hpo_name), collapse = ", ")
  ) #%>%  ungroup()

#result_subset2 <- omim_data %>%
 # dplyr::filter(disease_id %in% desired_ids) %>%
  #left_join(result_subset1, by = "disease_id")


# Now, expand the summarized HPO terms so each HPO becomes its own row
result_expanded <- result_subset %>%
  separate_rows(hpo_ids, hpo_names, sep = ",\\s*")

print(result_expanded)


unique_hpos <- result_expanded %>%
  dplyr::select(hpo_ids, hpo_names) %>%
  distinct()



# ----- Step 3: Create a data frame with the results and write to CSV -----

dcm_omim_to_hpo <- "data/dcm_omim_to_hpo.csv"
write.csv(result_expanded, dcm_omim_to_hpo, row.names = FALSE)

cat("CSV file", dcm_omim_to_hpo, "created successfully.\n")


dcm_omim_to_hpo_unique <- "data/dcm_omim_to_hpo_unique.csv"
write.csv(unique_hpos, dcm_omim_to_hpo_unique, row.names = FALSE)

cat("CSV file", dcm_omim_to_hpo_unique, "created successfully.\n")
