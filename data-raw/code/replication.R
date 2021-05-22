###########################################################################
# Joshua C. Fjelstul, Ph.D.
# evoeu R package
###########################################################################

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

# organize variables
edges <- dplyr::select(
  edges,
  key_id,
  outgoing_celex, outgoing_node_type_id, outgoing_node_type,
  edge_type_id, edge_type,
  incoming_celex, incoming_node_type_id, incoming_node_type
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
codebook <- read.csv("data-raw/codebook/codebook.csv", stringsAsFactors = FALSE)

# convert to a tibble
codebook <- dplyr::as_tibble(codebook)

# save
save(codebook, file = "data/codebook.RData")

# documentation
codebookr::document_data(
  path = "R/",
  codebook_file = "data-raw/codebook/codebook.csv",
  markdown_file = "data-raw/codebook/descriptions.txt",
  author = "Joshua C. Fjelstul, Ph.D.",
  package = "evoeu"
)

##################################################
# load data
##################################################

load("data/nodes.RData")
load("data/edges.RData")
load("data/codebook.RData")

##################################################
# build
##################################################

write.csv(nodes, "build/evoeu_nodes.csv", row.names = FALSE, quote = TRUE)
write.csv(edges, "build/evoeu_edges.csv", row.names = FALSE, quote = TRUE)
write.csv(codebook, "build/evoeu_codebook.csv", row.names = FALSE, quote = TRUE)

##################################################
# server
##################################################

write.csv(nodes, "server/evoeu_nodes.csv", row.names = FALSE, quote = TRUE, na = "\\N")
write.csv(edges, "server/evoeu_edges.csv", row.names = FALSE, quote = TRUE, na = "\\N")
write.csv(codebook, "server/evoeu_codebook.csv", row.names = FALSE, quote = TRUE, na = "\\N")

###########################################################################
# end R script
###########################################################################
