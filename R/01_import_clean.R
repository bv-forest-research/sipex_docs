rm(list = ls())

library(data.table)
library(stringr)
library(dplyr)

#setwd("Specify your working directory here")
#setwd("D:/BVRC/R/SIPex_upload/sipex_docs-main/sipex_docs") 
setwd("D:\\BVRC\\R\\Github\\sipex_docs") #This is Rover's directory, change to your own

#######################
#### Main settings ####
#######################

upload_collection <- "downloads_081126.csv" #specify the collection to upload
in_dir <- "cleaned"
out_dir <- "../sipex_upload"
script_dir <- "R"
ref_results <- "ref_results"
ref_df <- "ref_source"

#switch for cross referencing the controlled taglist. Set to TRUE to run the audit, FALSE to skip it.
run_tag_audit <- TRUE

#switch for descriptive location spelling check. Set to TRUE to run the audit, FALSE to skip it.
check_loc_spelling <- TRUE

#######################
#######################
#load and pre process raw collection csv
raw_collection <- fread(file.path(in_dir, upload_collection))
current_collection <- raw_collection %>% 
  slice(-1) %>%
  filter(!str_detect(
    `Tagging Complete?`,
    fixed("No - do not upload to SIPex")
  )) %>%
  select(
    -`Tagging Complete?`,
    -`Uploaded to SIPex?`,
    -DOI,
    -`License Disclaimer`,
    -Latitude,
    -Longitude
  )

write.csv(current_collection, file.path(in_dir, "current_collection.csv"), row.names = FALSE)

# View(current_collection)

#######################
#######################
# Title and Descriptions accept parenthesis, punctuations, apostrophes and hyphens.
# Tags don't accept special characters but accepts dashes, spaces, underscores and capitalization.
#No slashes or ampersands
# License - refer to this for specific names and descriptions https://opendefinition.org/licenses/
# Groups/Categories - these are already added so the value for this is the slug/url 
#of the categories
# comma separated for multiple categories

# data associated with LTR
# batch uploads at the end of Septemeber:
d1 <- current_collection
# d1 <- fread(file.path(in_dir, "current_collection.csv"))
# update to include additional training entries:
# d1 <- fread(file.path(in_dir,"downloads_061025.csv"))

# update to include more from C3:
# d1 <- fread(file.path(in_dir,"downloads_071025.csv"))


#d1 <- d1[`Upload to SIPex?` == "Yes - upload to SIPex"]
#clean colnames:
colnames(d1)

#ID
#d1[, ID := seq(1,nrow(d1))]
setnames(d1, "doc_id", "ID")
setnames(d1, "Document", "Title")
setnames(d1, "Featured Topic Tag","Group")
setnames(d1, "License Type", "License")
setnames(d1, gsub("\n", "", names(d1), fixed = TRUE))  # Replace newlines with spaces
setnames(d1, gsub("/", "", names(d1), fixed = TRUE))
setnames(d1, gsub("\r", "", names(d1), fixed = TRUE))
setnames(d1, trimws(names(d1)))
names(d1)

d1[, Title := gsub("\r?\n", " ", Title)]
d1[, Title := gsub("\\s+", " ", trimws(Title))]
d1[, Title := gsub("/", " and ", trimws(Title))]

# Titles ---------------------------------
d1$Title
#Fix parts of the title that are fixable (remove colons, semicolons, periods):
#Title and Descriptions accept parenthesis, punctuations, apostrophes and hyphens
special_chars <- unique(unlist(strsplit(paste(d1$Title, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[:,/?\r–&*_;]"

d1[, Title := gsub(chars_to_remove, " ", Title)]
d1[, Title := gsub("\\s+", " ", trimws(Title))]
d1$Title

small_words <- c(
  "a", "an", "and", "as", "at", "but", "by", "for", "from",
  "if", "in", "into", "nor", "of", "on", "or", "over", "per",
  "the", "to", "up", "via", "with","when"
)

d1[, Title := sapply(Title, function(x) {
  words <- str_split(x, " ", simplify = TRUE)
  n <- length(words)
  
  words <- sapply(seq_along(words), function(i) {
    w <- words[i]
    # remove punctuation for logic checks
    w_clean <- str_remove_all(w, "^[^A-Za-z0-9]+|[^A-Za-z0-9]+$")
    
    if (str_detect(w_clean, "^[A-Z0-9\\-]+$") & nchar(w_clean) > 1) {
      # keep acronyms/codes as-is
      w
    } else if (tolower(w_clean) %in% small_words && i != 1 && i != n) {
      # keep small words lowercase unless first/last
      tolower(w)
    } else {
      # capitalize first letter only, preserve punctuation
      str_replace(w, "^[A-Za-z]", toupper)
    }
  })
  
  str_c(words, collapse = " ")
})]
d1$Title


# Organizations --------------------------
#Organization - looks like curation now has a single org
unique(d1$Organization)

#reorder
setcolorder(d1, c("ID","Title", "Organization", 
                  setdiff(names(d1), c("ID", "Title", "Organization"))))
#seperate to make the resources doc:
d4 <- d1[,.(ID, Title, `Document Name (title_location_year published)`)]
setnames(d4, c("ID","Title","Document Name (title_location_year published)"),
         c("Dataset_ID","Name","Path"))
#might need to get rid of parentheses in Organization - not sure yet
#now pull out groups and add to a new column:


# JOIN TAGS -----------------
#checking some tags
names(d1)
cols_to_combine <- names(d1)[!names(d1) %in% c("ID","Title", "Upload to SIPex?", "License",
                                               "Document Name (title_location_year published)",
                                               "Organization", "Year Published",
                                               "Author(s)", "Author contact",
                                               "Additional organizations",
                                               "Description", "Descriptive location",
                                               "Group", "Engagement Type",
                                               "DOI", "Name of Journal", #not sure
                                               "Who has copyright?", #not sure
                                               "Notes about Copyright")] #not sure
d1[, Tags := do.call(paste, c(.SD, sep = ",")), .SDcols = cols_to_combine]

d2 <- d1[,.(ID,Title,Organization, `Author(s)`, `Year Published`, Tags,`Descriptive location`,
            Group, License, Description)]
d2[, Tags := gsub(",+", ",", Tags)]  # Replace multiple commas with a single comma
d2[, Tags := gsub("^,|,$", "", Tags)]
#Tags don't accept special characters, but accepts dashes, spaces, 
#underscores and capitalization and periods. No slashes or ampersands

#first, get rid of the parentheses, but also anything inside the paraentheses:
d2[, Tags := gsub("\\(.*?\\)", "", Tags)]

#get rid of other special characters
special_chars <- unique(unlist(strsplit(paste(d2$Tags, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[/&?]"
d2[, Tags := gsub(chars_to_remove, " ", Tags)]
#apostrophe's with no spaces:
chars_to_remove <- "['’]"
d2[, Tags := gsub(chars_to_remove, "", Tags)]


#clean up the commas and spaces
d2[, Tags := gsub("NA", "", Tags)] # Remove "NA" 
d2[, Tags := gsub(",\\s*NA\\s*,", ",", Tags)] # Remove "NA" surrounded by commas
d2[, Tags := gsub("\\s*,\\s*", ", ", Tags)]   # Ensure a single space after each comma
d2[, Tags := gsub("\\s*,", ",", Tags)]        # Remove any spaces before a comma
d2[, Tags := gsub("\\s+$", "", Tags)]         # Remove trailing spaces
d2[, Tags := gsub(",+", ",", Tags)]  # Replace multiple commas with a single comma
d2[, Tags := gsub('["“”‘’]', '', Tags)]
d2[, Tags := gsub(",\\s*$", "", Tags)] 

#check tags ---------------------
sort(unique(trimws(unlist(strsplit(d2$Tags, ",")))))
d2[d2[, grepl("Caribou", Tags, ignore.case = TRUE)]]

#Description ----------------------------
d2[, Description := gsub("\\(.*?\\)", "", Description)]
special_chars <- unique(unlist(strsplit(paste(d2$Description, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[/&?\"]"
d2[, Description := gsub(chars_to_remove, "", Description)]
#clean up the commas and spaces
d2[, Description := gsub("\\s*,\\s*", ", ", Description)]   # Ensure a single space after each comma
d2[, Description := gsub("\\s*\\.\\s*", ". ", Description)]       # Remove any spaces before a period


#Descriptive location --------------------
sort(unique(trimws(unlist(strsplit(d2$`Descriptive location`, ",")))))
setnames(d2, c("Descriptive location"), 
         c("Descriptive Location"))



#cleaning authors names: ----------------------
sort(unique(trimws(unlist(strsplit(d2$`Author(s)`, ",")))))
d2[, `Author(s)` := gsub("([A-Z])\\.\\s+([A-Z])\\.", "\\1.\\2.", `Author(s)`)]   # collapse initials
d2[, `Author(s)` := gsub("([A-Z])\\.\\s+([A-Z])\\.", "\\1.\\2.", `Author(s)`)]   # collapse initials (third initial)
d2[, `Author(s)` := gsub("[\n\r]+", " ", `Author(s)`)]                           # remove newlines
d2[, `Author(s)` := gsub("\\.$", "", `Author(s)`)]                               # strip trailing periods
d2[, `Author(s)` := gsub("’", "'", `Author(s)`)]                                 # curly to straight apostrophe
d2[, `Author(s)` := gsub("O’", "O'", `Author(s)`)]                               # fix O’Neill
d2[, `Author(s)` := trimws(`Author(s)`)]
d2[, `Author(s)` := trimws(`Author(s)`)] 
sort(unique(trimws(unlist(strsplit(d2$`Author(s)`, ",")))))


#check descriptive location --------------------
sort(unique(trimws(unlist(strsplit(d2$`Descriptive Location`, ",")))))

#Optional: check descriptive location spelling against BC Gazetteer
if (isTRUE(check_loc_spelling)) {
  message("\033[31;1mChecking descriptive location spelling...\033[0m")
  library(openxlsx)
  gazetteer_file <- file.path(
    ref_df,
    "bc_gazetteer_2026_06_03.xlsx"
  )

  stopifnot(file.exists(gazetteer_file))

  bc_namesraw <- openxlsx::read.xlsx(
    xlsxFile = gazetteer_file,
    sheet = 1,
    check.names = FALSE
  )

  bc_namesraw$Official.Name.original <- bc_namesraw$Official.Name

  source(file.path(script_dir, "01_loc_spelling_inspec.R"))
  bc_namesraw$name_deco <- decode_html_numeric(bc_namesraw$Official.Name)
  bc_names <- bc_namesraw %>%
    select(name_deco)
}

if (isTRUE(check_loc_spelling)) {
  source(file.path(script_dir, "01_loc_spelling_inspec.R"))
  bc_vocabulary <- build_location_vocabulary(
  data = bc_names,
  column = "name_deco"
  )
}

if (isTRUE(check_loc_spelling)) {
  source(file.path(script_dir, "01_loc_spelling_inspec.R"))
  location_spelling_issues <- check_location_vocabulary(
  x = d2[["Descriptive Location"]],
  vocabulary = bc_vocabulary,
  vocab_col = "word"
  )

  write.csv(location_spelling_issues, file.path(ref_results, "current_loc_inspections.csv"), row.names = FALSE)
  message("\033[31;1mDescriptive location spelling results saved to ref_results.\033[0m")
} else {
  message("\033[31;1mSkipped descriptive location spelling check. Set check_loc_spelling = TRUE to run the audit.\033[0m")
}


#check groups --------------------
#sort(unique(trimws(unlist(strsplit(d2$Group, ",")))))
#d2[Group == "fire-prescribed-fire", .(ID, Title)]


#check organization --------------------
sort(unique(trimws(unlist(strsplit(d2$Organization, ",")))))
d2[, Organization := gsub("[\n\r]+", " ", Organization)]# remove newlines
d2[, Organization := gsub("\\.$", "", Organization)] # strip trailing periods
d2[, Organization := gsub("’", "'", Organization)] # curly to straight apostrophe
d2[, Organization := gsub('^"+|"+$', '', Organization)]
d2[, Organization := trimws(Organization)]
sort(unique(trimws(unlist(d2$Organization))))


d2$Title
d2$License

# Optional: Cross-reference tags with controlled taglist
# Using functions defined in 01_tag_audit.R in script_dir
if (isTRUE(run_tag_audit)) {
    message("\033[31;1mCross-referencing tags with controlled taglist...\033[0m")
  #cross reference check with control taglist:
# 1- build current tag inventory from the dataset
library(tidyr)
make_tag_key <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_nfc() %>%              # preserve Unicode consistently
    str_replace_all("[\u2010-\u2015]", "-") %>% # normalize weird dash types
    str_replace_all("\\s+", " ") %>%
    str_squish() %>%
    str_to_lower()
}

tags_long <- d2 %>%
  mutate(record_id = row_number()) %>%
  separate_longer_delim(Tags, delim = ",") %>%
  mutate(
    tag_raw = Tags,
    tag_clean = str_squish(tag_raw),
    tag_key = make_tag_key(tag_clean)
  ) %>%
  filter(!is.na(tag_clean), tag_clean != "")

tag_inventory <- tags_long %>%
  count(tag_clean, tag_key, name = "n_records") %>%
  arrange(tag_clean)

write.csv(tag_inventory, file.path(ref_results, "current_tag_inventory.csv"), row.names = FALSE)

#2- read in the controlled taglist and pivot to long format
library(readr)
controlled_wide <- readr::read_csv(
  (file.path(ref_df, "01_controlled taglist_jan_2026.csv")),
  skip = 1,
  show_col_types = FALSE
)

controlled_long <- controlled_wide %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "tag_category",
    values_to = "controlled_tag"
  ) %>%
  dplyr::mutate(
    controlled_tag = stringr::str_squish(as.character(controlled_tag))
  ) %>%
  dplyr::filter(
    !is.na(controlled_tag),
    controlled_tag != ""
  ) %>%
  dplyr::distinct(tag_category, controlled_tag) %>%
  dplyr::arrange(tag_category, controlled_tag)%>%
  dplyr::mutate(
    controlled_key = make_tag_key(controlled_tag)
  )

#2.1 (handles different meanings for same spellings but with upper and lower case (i.e. At vs AT))
controlled_exact_index <- controlled_long %>%
  dplyr::group_by(controlled_tag) %>%
  dplyr::summarise(
    exact_categories = paste(unique(tag_category), collapse = "; "),
    .groups = "drop"
  )

controlled_key_index <- controlled_long %>%
  dplyr::group_by(controlled_key) %>%
  dplyr::summarise(
    n_controlled_tags = dplyr::n_distinct(controlled_tag),
    possible_matches = paste(
      unique(paste0(tag_category, " = ", controlled_tag)),
      collapse = " | "
    ),
    .groups = "drop"
  )

#3- compare the two lists and identify any tags that are not in the controlled list
tag_audit <- tag_inventory %>%
  dplyr::left_join(
    controlled_exact_index,
    by = c("tag_clean" = "controlled_tag")
  ) %>%
  dplyr::left_join(
    controlled_key_index,
    by = c("tag_key" = "controlled_key")
  ) %>%
  dplyr::mutate(
    status = dplyr::case_when(
      !is.na(exact_categories) ~ "exact match",
      
      is.na(exact_categories) &
        !is.na(possible_matches) &
        n_controlled_tags == 1 ~ "normalized match only: review spelling/case/style",
      
      is.na(exact_categories) &
        !is.na(possible_matches) &
        n_controlled_tags > 1 ~ "ambiguous normalized match: review",
      
      TRUE ~ "not in controlled list"
    )
  ) %>%
  dplyr::arrange(status, tag_clean)

write.csv(tag_audit, file.path(ref_results, "current_tag_audits.csv"), row.names = FALSE)
message("\033[31;1mTag auditing results saved to ref_results\033[0m")

} else {
  message("\033[31;1mSkipped tag cross referencing. Set run_tag_audit = TRUE to run the audit.\033[0m")
}


#write out the dataset file:
fwrite(d2[1:10], file.path(out_dir,"datasets data","datasets_070526_batch1.csv"))

#write out the resources file:
fwrite(d4[1:10], file.path(out_dir,"resources data","resources_070526_batch1.csv"))

# write out the ones that failed to upload:
#write out the dataset file:
fwrite(d2[ID %in% c("C2-005", "C3-084", "C3-091", "C3-093", "C3-096", "C3-099", 
                    "C3-100", "C3-101", "C3-105", "C3-106", "C3-107", "C3-108", 
                    "C3-109", "C5-003", "T1-002", "T1-003", "T1-004", "T1-005", 
                    "T1-012", "T1-013", "T1-014", "T1-015", "T1-017", "T1-018", 
                    "COP1-002", "COP1-003", "COP1-004", "COP1-005", "COP1-029", 
                    "COP1-036", "COP1-050")], 
       file.path("../sipex_upload/datasets data","datasets_250925_f.csv"))

#write out the resources file:
fwrite(d4[Dataset_ID %in% c("C2-005", "C3-084", "C3-091", "C3-093", "C3-096", "C3-099", 
                    "C3-100", "C3-101", "C3-105", "C3-106", "C3-107", "C3-108", 
                    "C3-109", "C5-003", "T1-002", "T1-003", "T1-004", "T1-005", 
                    "T1-012", "T1-013", "T1-014", "T1-015", "T1-017", "T1-018", 
                    "COP1-002", "COP1-003", "COP1-004", "COP1-005", "COP1-029", 
                    "COP1-036", "COP1-050")], 
       file.path("../sipex_upload/resources data","resources_250925_f.csv"))


