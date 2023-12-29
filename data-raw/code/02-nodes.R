# Joshua C. Fjelstul, Ph.D.
# evoeu R package

# load data --------------------------------------------------------------------

# load data
load(file = "data-raw/eurlex-cellar-data/nodes_1_raw.RData")
load(file = "data-raw/eurlex-cellar-data/nodes_2_raw.RData")
load(file = "data-raw/eurlex-cellar-data/nodes_3_raw.RData")
load(file = "data-raw/eurlex-cellar-data/nodes_4_raw.RData")
load(file = "data-raw/eurlex-cellar-data/nodes_5_raw.RData")
load(file = "data-raw/eurlex-cellar-data/nodes_6_raw.RData")
load(file = "data-raw/eurlex-cellar-data/nodes_7_raw.RData")
load(file = "data-raw/eurlex-cellar-data/nodes_8_raw.RData")
load(file = "data-raw/eurlex-cellar-data/nodes_9_raw.RData")

# stack data
documents_raw <- dplyr::bind_rows(
  nodes_1_raw,
  nodes_2_raw,
  nodes_3_raw,
  nodes_4_raw,
  nodes_5_raw,
  nodes_6_raw,
  nodes_7_raw,
  nodes_8_raw,
  nodes_9_raw
)

# clean environment
rm(
  nodes_1_raw, nodes_2_raw, nodes_3_raw,
  nodes_4_raw, nodes_5_raw, nodes_6_raw,
  nodes_7_raw, nodes_8_raw, nodes_9_raw
)

# keep unique values
documents <- documents_raw |>
  dplyr::select(
    sector, celex, date, title, author, url
  ) |>
  dplyr::mutate(
    sector = sector |>
      as.numeric()
  ) |>
  dplyr::distinct()

# get a list of documents with more than one entry
list_duplicates <- documents |>
  dplyr::mutate(
    duplicate = duplicated(celex)
  ) |>
  dplyr::filter(duplicate) |>
  dplyr::pull(celex)

# collapse documents with more than one entry
duplicates <- documents |>
  dplyr::filter(celex %in% list_duplicates) |>
  dplyr::arrange(-nchar(title)) |>
  dplyr::group_by(sector, celex) |>
  dplyr::summarize(
    count = dplyr::n(),
    date = stringr::str_c(unique(date), collapse = ", "),
    author = stringr::str_c(unique(author), collapse = " BREAK "),
    title = dplyr::first(title),
    url = dplyr::first(url)
  )

# remove documents with more than one entry
documents <- documents |>
  dplyr::filter(!celex %in% duplicates$celex)

# add in collapsed data for documents with more than one entry
documents <- dplyr::bind_rows(
  documents,
  duplicates |>
    dplyr::select(-count)
)

# validation checks ------------------------------------------------------------

# check that the number of dropped rows is correct
nrow(documents_raw) - sum(duplicates$count - 1) == nrow(documents)

# check that the CELEX number is unique
length(unique(documents$celex)) == nrow(documents)

# check that the URL is unique
length(unique(documents$url)) == nrow(documents)

# clean dates ------------------------------------------------------------------

# check for missing dates
table(documents$date == "")

# correct out of range dates
documents <- documents |>
  dplyr::mutate(
    date = dplyr::case_when(
      date <= "1003-03-03" ~ "not available",
      TRUE ~ date
    )
  )

# check for missing dates
table(documents$date == "not available")

# code author ------------------------------------------------------------------

# get a list of documents without an author
missing_author <- documents |>
  dplyr::filter(author == "")

# code missing authors
documents <- documents |>
  dplyr::mutate(
    author = dplyr::case_when(
      sector == 8 ~ "national court",
      sector == 6 & stringr::str_detect(celex, "CC") ~ "Court of Justice",
      TRUE ~ author
    )
  )

# check the number of documents without an author after corrections
sum(documents$author == "")

# code descriptors -------------------------------------------------------------

# extract the descriptor from the CELEX number
documents <- documents |>
  dplyr::mutate(
    descriptor = celex |>
      stringr::str_extract("[A-Z]+"),
  )

# make a table of CELEX descriptors
descriptors <- documents |>
  dplyr::group_by(sector, descriptor) |>
  dplyr::summarize(
    count = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(sector, -count)

# code document type according to the CELEX number
documents <- documents |>
  dplyr::mutate(
    celex_type = dplyr::case_when(

      # sector 1: treaties
      sector == 1 & descriptor == "A" ~ "Treaty Establishing the European Atomic Energy Community",
      sector == 1 & descriptor == "AN" ~ "Treaty Establishing the European Atomic Energy Community",
      sector == 1 & descriptor == "AR" ~ "Treaty Establishing the European Atomic Energy Community",
      sector == 1 & descriptor == "B" ~ "Accession Treaty of 1972",
      sector == 1 & descriptor == "BN" ~ "Accession Treaty of 1972",
      sector == 1 & descriptor == "C" ~ "Treaty of Nice",
      sector == 1 & descriptor == "D" ~ "Treaty of Amsterdam",
      sector == 1 & descriptor == "DNA" ~ "Treaty of Amsterdam",
      sector == 1 & descriptor == "DNB" ~ "Treaty of Amsterdam",
      sector == 1 & descriptor == "E" ~ "Treaty on the Functioning of the European Union",
      sector == 1 & descriptor == "EN" ~ "Treaty on the Functioning of the European Union",
      sector == 1 & descriptor == "F" ~ "Merger Treaty",
      sector == 1 & descriptor == "G" ~ "Greenland Treaty of 1985",
      sector == 1 & descriptor == "H" ~ "Accession Treaty of 1979",
      sector == 1 & descriptor == "HN" ~ "Accession Treaty of 1979",
      sector == 1 & descriptor == "I" ~ "Accession Treaty of 1985",
      sector == 1 & descriptor == "IN" ~ "Accession Treaty of 1985",
      sector == 1 & descriptor == "J" ~ "Accession Treaty of 2012",
      sector == 1 & descriptor == "JN" ~ "Accession Treaty of 2012",
      sector == 1 & descriptor == "K" ~ "Treaty Establishing the European Coal and Steel Community",
      sector == 1 & descriptor == "KN" ~ "Treaty Establishing the European Coal and Steel Community",
      sector == 1 & descriptor == "L" ~ "Treaty of Lisbon",
      sector == 1 & descriptor == "LN" ~ "Treaty of Lisbon",
      sector == 1 & descriptor == "LR" ~ "Treaty of Lisbon",
      sector == 1 & descriptor == "M" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MA" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MB" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MC" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MD" ~ "Treaty on European Union",
      sector == 1 & descriptor == "ME" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MF" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MG" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MH" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MI" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MJ" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MK" ~ "Treaty on European Union",
      sector == 1 & descriptor == "ML" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MM" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MN" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MO" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MP" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MQ" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MR" ~ "Treaty on European Union",
      sector == 1 & descriptor == "MS" ~ "Treaty on European Union",
      sector == 1 & descriptor == "N" ~ "Accession Treaty of 1994",
      sector == 1 & descriptor == "NN" ~ "Accession Treaty of 1994",
      sector == 1 & descriptor == "P" ~ "Charter of Fundamental Rights of the European Union",
      sector == 1 & descriptor == "R" ~ "Treaty Amending Certain Financial Provisions",
      sector == 1 & descriptor == "S" ~ "Accession Treaty of 2005",
      sector == 1 & descriptor == "SA" ~ "Accession Treaty of 2005",
      sector == 1 & descriptor == "SAFI" ~ "Accession Treaty of 2005",
      sector == 1 & descriptor == "SPR" ~ "Accession Treaty of 2005",
      sector == 1 & descriptor == "SAN" ~ "Accession Treaty of 2005",
      sector == 1 & descriptor == "SP" ~ "Accession Treaty of 2005",
      sector == 1 & descriptor == "SPN" ~ "Accession Treaty of 2005",
      sector == 1 & descriptor == "T" ~ "Accession Treaty of 2003",
      sector == 1 & descriptor == "TN" ~ "Accession Treaty of 2003",
      sector == 1 & descriptor == "TR" ~ "Accession Treaty of 2003",
      sector == 1 & descriptor == "U" ~ "Single European Act",
      sector == 1 & descriptor == "V" ~ "Treaty Establishing a Constitution for Europe",
      sector == 1 & descriptor == "VN" ~ "Treaty Establishing a Constitution for Europe",
      sector == 1 & descriptor == "W" ~ "United Kingdom Withdrawal Agreement",
      sector == 1 & descriptor == "X" ~ "Treaty Amending Protocol on the Statute of the European Investment Bank",
      sector == 1 & descriptor == "XA" ~ "Treaty Amending Protocol on the Statute of the European Investment Bank",
      sector == 1 & descriptor == "XB" ~ "Treaty Amending Protocol on the Statute of the European Investment Bank",
      sector == 1 & descriptor == "XC" ~ "Treaty Amending Protocol on the Statute of the European Investment Bank",

      # sector 2: international agreements
      sector == 2 & descriptor == "A" ~ "agreement with member states, non-member states, or an international organization",
      sector == 2 & descriptor == "D" ~ "act of a body created by an international agreement",
      sector == 2 & descriptor == "P" ~ "act of a body created by an international agreement",
      sector == 2 & descriptor == "X" ~ "other international agreement",
      sector == 2 & descriptor == "XC" ~ "other international agreement",

      # sector 3: legislation
      sector == 3 & descriptor == "A" ~ "opinion",
      sector == 3 & descriptor == "B" ~ "budget",
      sector == 3 & descriptor == "C" ~ "declaration",
      sector == 3 & descriptor == "D" ~ "decision",
      sector == 3 & descriptor == "E" ~ "legislative document (Common and Foreign Security Policy)",
      sector == 3 & descriptor == "F" ~ "legislative document (Policy and Judicial Cooperation in Criminal Matters)",
      sector == 3 & descriptor == "G" ~ "resolution",
      sector == 3 & descriptor == "H" ~ "recommendation",
      sector == 3 & descriptor == "J" ~ "non-opposition to a notified joint venture",
      sector == 3 & descriptor == "K" ~ "recommendation (European Coal and Steel Community)",
      sector == 3 & descriptor == "L" ~ "directive",
      sector == 3 & descriptor == "M" ~ "decision (Commission)",
      sector == 3 & descriptor == "O" ~ "guidelines (European Central Bank)",
      sector == 3 & descriptor == "Q" ~ "institutional arrangement",
      sector == 3 & descriptor == "R" ~ "regulation",
      sector == 3 & descriptor == "S" ~ "decision (European Coal and Steel Community)",
      sector == 3 & descriptor == "X" ~ "other legislation",
      sector == 3 & descriptor == "Y" ~ "other legislation",

      # sector 4: complementary legislation
      sector == 4 & descriptor == "A" ~ "agreements between member states",
      sector == 4 & descriptor == "D" ~ "decisions of the representatives of the member states",
      sector == 4 & descriptor == "X" ~ "other complementary legislation",
      sector == 4 & descriptor == "Y" ~ "other complementary legislation",
      sector == 4 & descriptor == "Z" ~ "other complementary legislation",

      # sector 5: working documents
      sector == 5 & descriptor == "AA" ~ "opinion (Court of Auditors)",
      sector == 5 & descriptor == "AB" ~ "opinion (European Central Bank)",
      sector == 5 & descriptor == "AC" ~ "opinion (Economic and Social Committee)",
      sector == 5 & descriptor == "AE" ~ "opinion (European Economic and Social Committee)",
      sector == 5 & descriptor == "AG" ~ "position (Council of the European Union)",
      sector == 5 & descriptor == "AK" ~ "opinion (ECSC Consultative Committee)",
      sector == 5 & descriptor == "AP" ~ "legislative resolution (European Parliament)",
      sector == 5 & descriptor == "AR" ~ "opinion (Committee of the Regions)",
      sector == 5 & descriptor == "AS" ~ "state aid document (European Commission)",
      sector == 5 & descriptor == "AT" ~ "antitrust document (European Commission)",
      sector == 5 & descriptor == "BC" ~ "draft budget (European Commission)",
      sector == 5 & descriptor == "BP" ~ "budget (European Parliament)",
      sector == 5 & descriptor == "DC" ~ "other working document (European Commission)",
      sector == 5 & descriptor == "DMA" ~ "decision summary (European Commission)",
      sector == 5 & descriptor == "DP" ~ "internal decision (European Parliament)",
      sector == 5 & descriptor == "EC" ~ "proposal of codified versions of regulations (European Commission)",
      sector == 5 & descriptor == "FC" ~ "proposal of codified versions of directives (European Commission)",
      sector == 5 & descriptor == "GC" ~ "proposal of codified versions of decisions (European Commission)",
      sector == 5 & descriptor == "HB" ~ "recommendation (European Central Bank)",
      sector == 5 & descriptor == "IE" ~ "own-initiative opinion (European Economic and Social Committee)",
      sector == 5 & descriptor == "IG" ~ "initiative from a member state (Council of the European Union)",
      sector == 5 & descriptor == "IP" ~ "other resolution (European Parliament)",
      sector == 5 & descriptor == "IR" ~ "own-initiative opinion (Committee of the Regions)",
      sector == 5 & descriptor == "JC" ~ "JOIN document (European Commission)",
      sector == 5 & descriptor == "KG" ~ "Council assent (Council of the European Union)",
      sector == 5 & descriptor == "M" ~ "merger control document (European Commission)",
      sector == 5 & descriptor == "PC" ~ "COM document (European Commission)",
      sector == 5 & descriptor == "SA" ~ "special report (Court of Auditors)",
      sector == 5 & descriptor == "SC" ~ "SEC or SWD document (European Commission)",
      sector == 5 & descriptor == "TA" ~ "report (Court of Auditors)",
      sector == 5 & descriptor == "XA" ~ "other working document (Court of Auditors)",
      sector == 5 & descriptor == "XB" ~ "other working document (European Central Bank)",
      sector == 5 & descriptor == "XC" ~ "other working document (European Commission)",
      sector == 5 & descriptor == "XE" ~ "other working document (European Economic and Social Committee)",
      sector == 5 & descriptor == "XG" ~ "other working document (Council of the European Union)",
      sector == 5 & descriptor == "XK" ~ "other working document (ECSC Consultative Committee)",
      sector == 5 & descriptor == "XP" ~ "other working document (European Parliament)",
      sector == 5 & descriptor == "XR" ~ "other working document (Committee of the Regions)",
      sector == 5 & descriptor == "XX" ~ "other working document",

      # sector 6: case law
      sector == 6 & descriptor == "CA" ~ "communication: judgment (Court of Justice)",
      sector == 6 & descriptor == "CB" ~ "communication: order (Court of Justice)",
      sector == 6 & descriptor == "CC" ~ "Advocate General opinion (Court of Justice)",
      sector == 6 & descriptor == "CD" ~ "decision (Court of Justice)",
      sector == 6 & descriptor == "CG" ~ "communication: opinion (Court of Justice)",
      sector == 6 & descriptor == "CJ" ~ "judgment (Court of Justice)",
      sector == 6 & descriptor == "CN" ~ "communication: new case (Court of Justice)",
      sector == 6 & descriptor == "CO" ~ "order (Court of Justice)",
      sector == 6 & descriptor == "CP" ~ "view (Court of Justice)",
      sector == 6 & descriptor == "CS" ~ "attachment order (Court of Justice)",
      sector == 6 & descriptor == "CT" ~ "third-party proceeding (Court of Justice)",
      sector == 6 & descriptor == "CU" ~ "communication: request for an opinion (Court of Justice)",
      sector == 6 & descriptor == "CV" ~ "opinion (Court of Justice)",
      sector == 6 & descriptor == "CX" ~ "ruling (Court of Justice)",
      sector == 6 & descriptor == "FA" ~ "communication: judgment (Civil Service Tribunal)",
      sector == 6 & descriptor == "FB" ~ "communication: order (Civil Service Tribunal)",
      sector == 6 & descriptor == "FJ" ~ "judgment (Civil Service Tribunal)",
      sector == 6 & descriptor == "FN" ~ "communication: new case (Civil Service Tribunal)",
      sector == 6 & descriptor == "FO" ~ "order (Civil Service Tribunal)",
      sector == 6 & descriptor == "TA" ~ "communication: judgment (General Court)",
      sector == 6 & descriptor == "TB" ~ "communication: order (General Court)",
      sector == 6 & descriptor == "TC" ~ "Advocate General opinion (General Court)",
      sector == 6 & descriptor == "TJ" ~ "judgment (General Court)",
      sector == 6 & descriptor == "TN" ~ "communication: new case (General Court)",
      sector == 6 & descriptor == "TO" ~ "order (General Court)",
      sector == 6 & descriptor == "TT" ~ "third-party proceeding (General Court)",

      # sector 7: national transposition measures
      sector == 7 & descriptor == "D" ~ "national implementation measure for a decision",
      sector == 7 & descriptor == "F" ~ "national implementation measure for a framework decision",
      sector == 7 & descriptor == "L" ~ "national implementation measure for a directive",
      sector == 7 & descriptor == "R" ~ "national implementation measure for a regulation",

      # sector 8 references to national case law concerning the EU
      sector == 8 ~ "national case law",

      # sector 9: parliamentary questions
      sector == 9 & descriptor == "H" ~ "question at question time (European Parliament)",
      sector == 9 & descriptor == "E" ~ "written question (European Parliament)",
      sector == 9 & descriptor == "O" ~ "oral question (European Parliament)",

      # missing
      TRUE ~ "missing"
    )
  )

# check the number of missing CELEX descriptors
sum(documents$celex_type == "missing")

# code categories --------------------------------------------------------------

# code categories
documents <- documents |>
  dplyr::mutate(
    category = dplyr::case_when(
      sector == 1 ~ "primary law",
      sector == 2 ~ "international law",
      sector == 3 ~ "secondary law",
      sector == 4 ~ "complementary law",
      sector == 5 ~ "working documents",
      sector == 6 ~ "case law",
      sector == 7 ~ "national transposition measures",
      sector == 8 ~ "national case law",
      sector == 9 ~ "parliamentary questions",
      TRUE ~ "missing"
    )
  )

# check values
table(documents$category)

# code subcategories
documents <- documents |>
  dplyr::mutate(
    subcategory = dplyr::case_when(

      # sector 1: treaties
      sector == 1 & descriptor == "A" ~ "treaties",
      sector == 1 & descriptor == "AN" ~ "treaties",
      sector == 1 & descriptor == "AR" ~ "treaties",
      sector == 1 & descriptor == "B" ~ "accession treaties",
      sector == 1 & descriptor == "BN" ~ "accession treaties",
      sector == 1 & descriptor == "C" ~ "treaties",
      sector == 1 & descriptor == "D" ~ "treaties",
      sector == 1 & descriptor == "DNA" ~ "treaties",
      sector == 1 & descriptor == "DNB" ~ "treaties",
      sector == 1 & descriptor == "E" ~ "treaties",
      sector == 1 & descriptor == "EN" ~ "treaties",
      sector == 1 & descriptor == "F" ~ "treaties",
      sector == 1 & descriptor == "G" ~ "treaties",
      sector == 1 & descriptor == "H" ~ "accession treaties",
      sector == 1 & descriptor == "HN" ~ "accession treaties",
      sector == 1 & descriptor == "I" ~ "accession treaties",
      sector == 1 & descriptor == "IN" ~ "accession treaties",
      sector == 1 & descriptor == "J" ~ "accession treaties",
      sector == 1 & descriptor == "JN" ~ "accession treaties",
      sector == 1 & descriptor == "K" ~ "treaties",
      sector == 1 & descriptor == "KN" ~ "treaties",
      sector == 1 & descriptor == "L" ~ "treaties",
      sector == 1 & descriptor == "LN" ~ "treaties",
      sector == 1 & descriptor == "LR" ~ "treaties",
      sector == 1 & descriptor == "M" ~ "treaties",
      sector == 1 & descriptor == "MA" ~ "treaties",
      sector == 1 & descriptor == "MB" ~ "treaties",
      sector == 1 & descriptor == "MC" ~ "treaties",
      sector == 1 & descriptor == "MD" ~ "treaties",
      sector == 1 & descriptor == "ME" ~ "treaties",
      sector == 1 & descriptor == "MF" ~ "treaties",
      sector == 1 & descriptor == "MG" ~ "treaties",
      sector == 1 & descriptor == "MH" ~ "treaties",
      sector == 1 & descriptor == "MI" ~ "treaties",
      sector == 1 & descriptor == "MJ" ~ "treaties",
      sector == 1 & descriptor == "MK" ~ "treaties",
      sector == 1 & descriptor == "ML" ~ "treaties",
      sector == 1 & descriptor == "MM" ~ "treaties",
      sector == 1 & descriptor == "MN" ~ "treaties",
      sector == 1 & descriptor == "MO" ~ "treaties",
      sector == 1 & descriptor == "MP" ~ "treaties",
      sector == 1 & descriptor == "MQ" ~ "treaties",
      sector == 1 & descriptor == "MR" ~ "treaties",
      sector == 1 & descriptor == "MS" ~ "treaties",
      sector == 1 & descriptor == "N" ~ "accession treaties",
      sector == 1 & descriptor == "NN" ~ "accession treaties",
      sector == 1 & descriptor == "P" ~ "treaties",
      sector == 1 & descriptor == "R" ~ "treaties",
      sector == 1 & descriptor == "S" ~ "accession treaties",
      sector == 1 & descriptor == "SA" ~ "accession treaties",
      sector == 1 & descriptor == "SAFI" ~ "accession treaties",
      sector == 1 & descriptor == "SPR" ~ "accession treaties",
      sector == 1 & descriptor == "SAN" ~ "accession treaties",
      sector == 1 & descriptor == "SP" ~ "accession treaties",
      sector == 1 & descriptor == "SPN" ~ "accession treaties",
      sector == 1 & descriptor == "T" ~ "accession treaties",
      sector == 1 & descriptor == "TN" ~ "accession treaties",
      sector == 1 & descriptor == "TR" ~ "accession treaties",
      sector == 1 & descriptor == "U" ~ "treaties",
      sector == 1 & descriptor == "V" ~ "treaties",
      sector == 1 & descriptor == "VN" ~ "treaties",
      sector == 1 & descriptor == "W" ~ "withdrawal agreements",
      sector == 1 & descriptor == "X" ~ "treaties",
      sector == 1 & descriptor == "XA" ~ "treaties",
      sector == 1 & descriptor == "XB" ~ "treaties",
      sector == 1 & descriptor == "XC" ~ "treaties",

      # sector 2: international treaties
      sector == 2 & descriptor == "A" ~ "international agreements",
      sector == 2 & descriptor == "D" ~ "acts of international organizations",
      sector == 2 & descriptor == "P" ~ "acts of an international organizations",
      sector == 2 & descriptor == "X" ~ "other international law",
      sector == 2 & descriptor == "XC" ~ "other international law",

      # sector 3: legislation
      sector == 3 & descriptor == "A" ~ "opinions",
      sector == 3 & descriptor == "B" ~ "budgets",
      sector == 3 & descriptor == "C" ~ "declarations",
      sector == 3 & descriptor == "D" ~ "decisions",
      sector == 3 & descriptor == "E" ~ "other secondary law",
      sector == 3 & descriptor == "F" ~ "other secondary law",
      sector == 3 & descriptor == "G" ~ "resolutions",
      sector == 3 & descriptor == "H" ~ "recommendations",
      sector == 3 & descriptor == "J" ~ "decisions",
      sector == 3 & descriptor == "K" ~ "recommendations",
      sector == 3 & descriptor == "L" ~ "directives",
      sector == 3 & descriptor == "M" ~ "decisions",
      sector == 3 & descriptor == "O" ~ "guidelines",
      sector == 3 & descriptor == "Q" ~ "institutional arrangements",
      sector == 3 & descriptor == "R" ~ "regulations",
      sector == 3 & descriptor == "S" ~ "decisions",
      sector == 3 & descriptor == "X" ~ "other secondary law",
      sector == 3 & descriptor == "Y" ~ "other secondary law",

      # sector 4: complementary law
      sector == 4 & descriptor == "A" ~ "agreements between member states",
      sector == 4 & descriptor == "D" ~ "decisions of the representatives of the governments of the member states",
      sector == 4 & descriptor == "X" ~ "other complementary law",
      sector == 4 & descriptor == "Y" ~ "other complementary law",
      sector == 4 & descriptor == "Z" ~ "other complementary law",

      # sector 5: working documents
      sector == 5 & descriptor == "AA" ~ "opinions",
      sector == 5 & descriptor == "AB" ~ "opinions",
      sector == 5 & descriptor == "AC" ~ "opinions",
      sector == 5 & descriptor == "AE" ~ "opinions",
      sector == 5 & descriptor == "AG" ~ "positions",
      sector == 5 & descriptor == "AK" ~ "opinions",
      sector == 5 & descriptor == "AP" ~ "legislative resolutions",
      sector == 5 & descriptor == "AR" ~ "opinions",
      sector == 5 & descriptor == "AS" ~ "state aid documents",
      sector == 5 & descriptor == "AT" ~ "antitrust documents",
      sector == 5 & descriptor == "BC" ~ "draft budgets",
      sector == 5 & descriptor == "BP" ~ "budgets",
      sector == 5 & descriptor == "DC" ~ "COM documents",
      sector == 5 & descriptor == "DMA" ~ "other working documents",
      sector == 5 & descriptor == "DP" ~ "internal decisions",
      sector == 5 & descriptor == "EC" ~ "proposals of codified versions of regulations",
      sector == 5 & descriptor == "FC" ~ "proposals of codified versions of directives",
      sector == 5 & descriptor == "GC" ~ "proposals of codified versions of decisions",
      sector == 5 & descriptor == "HB" ~ "recommendations",
      sector == 5 & descriptor == "IE" ~ "own-initiative opinions",
      sector == 5 & descriptor == "IG" ~ "initiatives from member states",
      sector == 5 & descriptor == "IP" ~ "non-legislative resolutions",
      sector == 5 & descriptor == "IR" ~ "own-initiative opinions",
      sector == 5 & descriptor == "JC" ~ "JOIN documents",
      sector == 5 & descriptor == "KG" ~ "Council assents",
      sector == 5 & descriptor == "M" ~ "merger control documents",
      sector == 5 & descriptor == "PC" ~ "COM documents",
      sector == 5 & descriptor == "SA" ~ "special reports",
      sector == 5 & descriptor == "SC" ~ "SWD documents",
      sector == 5 & descriptor == "TA" ~ "reports",
      sector == 5 & descriptor == "XA" ~ "other working documents",
      sector == 5 & descriptor == "XB" ~ "other working documents",
      sector == 5 & descriptor == "XC" ~ "other working documents",
      sector == 5 & descriptor == "XE" ~ "other working documents",
      sector == 5 & descriptor == "XG" ~ "other working documents",
      sector == 5 & descriptor == "XK" ~ "other working documents",
      sector == 5 & descriptor == "XP" ~ "other working documents",
      sector == 5 & descriptor == "XR" ~ "other working documents",
      sector == 5 & descriptor == "XX" ~ "other working documents",

      # sector 6: case law
      sector == 6 & descriptor == "CA" ~ "communications",
      sector == 6 & descriptor == "CB" ~ "communications",
      sector == 6 & descriptor == "CC" ~ "Advocate General opinions",
      sector == 6 & descriptor == "CD" ~ "decisions",
      sector == 6 & descriptor == "CG" ~ "communications",
      sector == 6 & descriptor == "CJ" ~ "judgments",
      sector == 6 & descriptor == "CN" ~ "communications",
      sector == 6 & descriptor == "CO" ~ "orders",
      sector == 6 & descriptor == "CP" ~ "Advocate General views",
      sector == 6 & descriptor == "CS" ~ "attachment orders",
      sector == 6 & descriptor == "CT" ~ "third-party proceedings",
      sector == 6 & descriptor == "CU" ~ "communications",
      sector == 6 & descriptor == "CV" ~ "opinions",
      sector == 6 & descriptor == "CX" ~ "rulings",
      sector == 6 & descriptor == "FA" ~ "communications",
      sector == 6 & descriptor == "FB" ~ "communications",
      sector == 6 & descriptor == "FJ" ~ "judgments",
      sector == 6 & descriptor == "FN" ~ "communications",
      sector == 6 & descriptor == "FO" ~ "orders",
      sector == 6 & descriptor == "TA" ~ "communications",
      sector == 6 & descriptor == "TB" ~ "communications",
      sector == 6 & descriptor == "TC" ~ "Advocate General opinions",
      sector == 6 & descriptor == "TJ" ~ "judgments",
      sector == 6 & descriptor == "TN" ~ "communications",
      sector == 6 & descriptor == "TO" ~ "orders",
      sector == 6 & descriptor == "TT" ~ "third-party proceedings",

      # sector 7: national transposition measures
      sector == 7 & descriptor == "D" ~ "national implementation measures for decisions",
      sector == 7 & descriptor == "F" ~ "national implementation measures for decisions",
      sector == 7 & descriptor == "L" ~ "national implementation measures for directives",
      sector == 7 & descriptor == "R" ~ "national implementation measures for regulations",

      # sector 8: national case law
      sector == 8 ~ "national case law",

      # sector 9: parliamentary questions
      sector == 9 & descriptor == "H" ~ "questions at question time",
      sector == 9 & descriptor == "E" ~ "written questions",
      sector == 9 & descriptor == "O" ~ "oral questions",

      # missing
      TRUE ~ "missing"
    )
  )


# clean transposition data -----------------------------------------------------

edges_7 <- nodes_7_raw |>
  dplyr::mutate(
    from_celex = celex |>
      stringr::str_replace("[0-9]{4}[A-Z][0-9]{4}", "*") |>
      stringr::str_squish(),
    to_celex = celex |>
      stringr::str_extract("7[0-9]{4}[A-Z][0-9]{4}") |>
      stringr::str_replace("^7", "3") |>
      stringr::str_squish(),
    edge_type = "transposes"
  ) |>
  dplyr::select(
    from_celex, edge_type, to_celex
  ) |>
  dplyr::distinct()

nodes_7 <- nodes_7_raw |>
  dplyr::mutate(
    celex = celex |>
      stringr::str_replace("[0-9]{4}[A-Z][0-9]{4}", "*") |>
      stringr::str_squish(),
  ) |>
  dplyr::select(
    celex, date
  ) |>
  dplyr::distinct()

