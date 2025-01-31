# install / load libraries ------------------------------------------------

if (!require("ontologyIndex")) install.packages("ontologyIndex")
library("ontologyIndex")

if (!require("dplyr")) install.packages("dplyr")
library("dplyr")

if (!require("tidyr")) install.packages("tidyr")
library("tidyr")

if (!require("Hmisc")) install.packages("Hmisc")
library("Hmisc")

# get ontology ------------------------------------------------------------

hpo_url <- "https://github.com/obophenotype/human-phenotype-ontology/releases/download/2025-01-16/hp-base.obo"
hpo <- get_ontology(hpo_url, extract_tags = "everything")

# get parent cardiovascular terms -------------------------------------------
parent_terms <- c("HP:0030956", "HP:0001627")

# Function to get all descendants for a given term
get_all_descendants <- function(ontology, term_id) {
  descendants <- ontology$ancestors[[term_id]]
  # In ontologyIndex, 'ancestors' contains all ancestors, so to get descendants,
  # we need to find all terms where 'term_id' is an ancestor.
  descendants <- names(which(sapply(ontology$ancestors, function(anc) term_id %in% anc)))
  return(descendants)
}

# Initialize a vector to store all selected term IDs
selected_terms <- parent_terms

# Loop through each parent term and get its descendants
for (parent in parent_terms) {
  descendants <- get_all_descendants(hpo, parent)
  selected_terms <- unique(c(selected_terms, descendants))
}

# Optional: Verify the number of selected terms
cat("Total selected terms:", length(selected_terms), "\n")


# Subset the ontology to include only the selected terms
subset_hpo <- list()

# Subset basic ontology properties
subset_hpo$id <- selected_terms
subset_hpo$name <- hpo$name[selected_terms]
subset_hpo$namespace <- hpo$namespace[selected_terms]
subset_hpo$definition <- hpo$definition[selected_terms]
subset_hpo$parents <- lapply(selected_terms, function(term) {
  intersect(hpo$parents[[term]], selected_terms)
})
names(subset_hpo$parents) <- selected_terms
subset_hpo$ancestors <- lapply(selected_terms, function(term) {
  intersect(hpo$ancestors[[term]], selected_terms)
})
names(subset_hpo$ancestors) <- selected_terms

# Include other necessary fields as needed (e.g., synonyms, xrefs)
# Here's an example for synonyms:
if (!is.null(hpo$synonyms)) {
  subset_hpo$synonyms <- hpo$synonyms[selected_terms]
}

# Convert the list to an ontologyIndex object
class(subset_hpo) <- "ontologyIndex"


# Define the output OBO file path
output_obo <- "hp0030956_and_hp0001627_descendants.obo"

# Open a connection to the output file
con <- file(output_obo, "w")

# Write the OBO header
writeLines("format-version: 1.2", con)
writeLines("data-version: HP:0030956 and HP:0001627 descendants", con)
writeLines("", con)

# Function to write each term in OBO format
write_term <- function(term_id, ontology) {
  writeLines("[Term]", con)
  writeLines(paste0("id: ", term_id), con)
  writeLines(paste0("name: ", ontology$name[term_id]), con)
  if (!is.null(ontology$namespace)) {
    writeLines(paste0("namespace: ", ontology$namespace[term_id]), con)
  }
  if (!is.null(ontology$definition[[term_id]])) {
    writeLines(paste0("def: \"", ontology$definition[[term_id]], "\""), con)
  }
  # Write synonyms if available
  if (!is.null(ontology$synonyms[[term_id]])) {
    for (syn in ontology$synonyms[[term_id]]) {
      writeLines(paste0("synonym: \"", syn, "\""), con)
    }
  }
  # Write parent relationships
  if (length(ontology$parents[[term_id]]) > 0) {
    for (parent in ontology$parents[[term_id]]) {
      writeLines(paste0("is_a: ", parent, " ! ", ontology$name[parent]), con)
    }
  }
  writeLines("", con)  # Add a blank line between terms
}

# Loop through each selected term and write it to the OBO file
for (term in selected_terms) {
  write_term(term, subset_hpo)
}

# Close the connection
close(con)

cat("Subset ontology saved to", output_obo, "\n")



# Creating the csv of the HPO terms ----------------------------------------




# Function to perform pre-order depth-first traversal
traverse_depth_first <- function(ontology, roots) {
  traversal_order <- data.frame(
    Term_ID = character(),
    Term_Name = character(),
    Parent_IDs = character(),
    Level = integer(),
    stringsAsFactors = FALSE
  )

  # Recursive helper function
  traverse <- function(term, level) {
    # Get term details
    term_name <- ontology$name[[term]]
    term_parents <- paste(ontology$parents[[term]], collapse = "; ")

    # Add the current term to traversal_order
    traversal_order <<- rbind(traversal_order, data.frame(
      Term_ID = term,
      Term_Name = term_name,
      Parent_IDs = term_parents,
      Level = level,
      stringsAsFactors = FALSE
    ))

    # Get children of the current term
    children <- names(which(sapply(ontology$parents, function(parents) term %in% parents)))

    # Sort children alphabetically by Term_Name for consistent ordering
    children <- children[order(ontology$name[children])]

    # Recursively traverse each child
    for (child in children) {
      traverse(child, level + 1)
    }
  }

  # Start traversal from each root term
  for (root in roots) {
    traverse(root, 0)
  }

  return(traversal_order)
}

# Perform depth-first traversal
traversal_result <- traverse_depth_first(subset_hpo, parent_terms)

# View the traversal result
print(traversal_result)


# Add indentation to Term_Name based on Level for better visualization
traversal_result <- traversal_result %>%
  mutate(Indented_Name = paste0(strrep("  ", Level), Term_Name))

# Optionally, add an Order column to reflect traversal sequence
traversal_result <- traversal_result %>%
  mutate(Order = row_number())

# Reorder columns for clarity
traversal_df <- traversal_result %>%
  select(Order, Term_ID, Indented_Name, Parent_IDs, Level)


# Define the output CSV file path
output_csv <- "hp0030956_and_hp0001627_descendants_ordered.csv"

# Write the data frame to CSV
write.csv(traversal_df, file = output_csv, row.names = FALSE)

cat("Ordered hierarchy CSV saved to", output_csv, "\n")
