################################################################################
# Joshua C. Fjelstul, Ph.D.
# evoeu R package
################################################################################

##################################################
# read in data
##################################################

nodes <- read.csv("data-raw/evoeu_nodes.csv", stringsAsFactors = FALSE)
edges <- read.csv("data-raw/evoeu_edges.csv", stringsAsFactors = FALSE)

##################################################
# clean node data
##################################################

# rename variables
nodes <- dplyr::rename(
  nodes,
  key_id = key_ID,
  celex = CELEX_number,
  date = document_date,
  author = document_author
)

# node type ID
nodes$node_type_id <- stringr::str_extract(nodes$node_type, "[0-9]+")
nodes$node_type_id <- as.numeric(nodes$node_type_id)

# node type
nodes$node_type <- stringr::str_remove(nodes$node_type, "^N[0-9]+: ")

# date
nodes$date[nodes$date == "Not available"] <- NA
nodes$date <- lubridate::ymd(nodes$date)

# year
nodes$year <- lubridate::year(nodes$date)

# month
nodes$month <- lubridate::month(nodes$date)

# day
nodes$day <- lubridate::day(nodes$date)

# organize variables
nodes <- dplyr::select(
  nodes,
  key_id, celex, node_type_id, node_type,
  author, date, year, month, day
)

##################################################
# clean edges data
##################################################

# rename variables
edges <- dplyr::rename(
  edges,
  key_id = key_ID,
  outgoing_celex = CELEX_outgoing,
  outgoing_node_type = node_type_outgoing,
  incoming_celex = CELEX_incoming,
  incoming_node_type = node_type_incoming
)

# outgoing node type ID
edges$outgoing_node_type_id <- stringr::str_extract(edges$outgoing_node_type, "[0-9]+")
edges$outgoing_node_type_id <- as.numeric(edges$outgoing_node_type_id)

# outgoing node type
edges$outgoing_node_type <- stringr::str_remove(edges$outgoing_node_type, "^N[0-9]+: ")

# incoming node type ID
edges$incoming_node_type_id <- stringr::str_extract(edges$incoming_node_type, "[0-9]+")
edges$incoming_node_type_id <- as.numeric(edges$incoming_node_type_id)

# incoming node type
edges$incoming_node_type <- stringr::str_remove(edges$incoming_node_type, "^N[0-9]+: ")

# edge type ID
edges$edge_type_id <- stringr::str_extract(edges$edge_type, "[0-9]+")
edges$edge_type_id <- as.numeric(edges$edge_type_id)

# edge type
edges$edge_type <- stringr::str_remove(edges$edge_type, "^E[0-9]+: ")

# merge in date
edges <- dplyr::left_join(
  edges,
  dplyr::select(nodes, celex, date, year),
  by = c("outgoing_celex" = "celex")
)

# organize variables
edges <- dplyr::select(
  edges,
  key_id,
  outgoing_celex, outgoing_node_type_id, outgoing_node_type,
  edge_type_id, edge_type,
  incoming_celex, incoming_node_type_id, incoming_node_type,
)

##################################################
# save data
##################################################

save(nodes, file = "data/nodes.RData")
save(edges, file = "data/edges.RData")

##################################################
# codebook
##################################################

# read in data
variables <- read.csv("data-raw/documentation/evoeu_variables.csv", stringsAsFactors = FALSE)

# convert to a tibble
variables <- dplyr::as_tibble(variables)

# save
save(variables, file = "data/variables.RData")

##################################################
# datasets
##################################################

# read in data
datasets <- read.csv("data-raw/documentation/evoeu_datasets.csv", stringsAsFactors = FALSE)

# convert to a tibble
datasets <- dplyr::as_tibble(datasets)

# save
save(datasets, file = "data/datasets.RData")

##################################################
# documentation
##################################################

# documentation
load("data/variables.RData")
load("data/datasets.RData")

# document data
codebookr::document_data(
  file_path = "R/",
  variables_input = variables,
  datasets_input = datasets,
  include_variable_type = TRUE,
  author = "Joshua C. Fjelstul, Ph.D.",
  package = "evoeu"
)

##################################################
# codebook
##################################################

# create a codebook
codebookr::create_codebook(
  file_path = "codebook/evoeu_codebook.tex",
  datasets_input = datasets,
  variables_input = variables,
  title_text = "The Evolution of European Union Law \\\\ (EvoEU) Database",
  version_text = "1.0",
  footer_text = "The EvoEU Database Codebook \\hspace{5pt} | \\hspace{5pt} Joshua C. Fjelstul, Ph.D.",
  author_names = "Joshua C. Fjelstul, Ph.D.",
  theme_color = "#4B94E6",
  heading_font_size = 30,
  subheading_font_size = 10,
  title_font_size = 16,
  table_of_contents = TRUE,
  include_variable_type = TRUE
)

##################################################
# load data
##################################################

load("data/nodes.RData")
load("data/edges.RData")
load("data/variables.RData")
load("data/datasets.RData")

##################################################
# build
##################################################

write.csv(nodes, "build/evoeu_nodes.csv", row.names = FALSE, quote = TRUE)
write.csv(edges, "build/evoeu_edges.csv", row.names = FALSE, quote = TRUE)
write.csv(variables, "build/evoeu_variables.csv", row.names = FALSE, quote = TRUE)
write.csv(datasets, "build/evoeu_datasets.csv", row.names = FALSE, quote = TRUE)

################################################################################
# end R script
################################################################################
