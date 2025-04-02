library(data.table)

in_dir <- "downloads"
out_dir <- "processed"

d1 <- fread(file.path(in_dir,"Document Collection and Tagging-010425.csv"))

#clean colnames:
setnames(d1, "V1", "Title")
setnames(d1, gsub("\n", "", names(d1), fixed = TRUE))  # Replace newlines with spaces
setnames(d1, gsub("/", "", names(d1), fixed = TRUE))
setnames(d1, gsub("\r", "", names(d1), fixed = TRUE))
setnames(d1, trimws(names(d1)))
names(d1)

d1[, (names(d1)) := lapply(.SD, function(x) {
  x <- trimws(x)                  # Trim leading/trailing spaces and newlines
  x <- gsub("\n+", ", ", x)        # Replace one or more newlines with a single comma + space
  x <- gsub(",\\s*,", ",", x)      # Remove double commas caused by empty newlines
  x <- gsub("\r", "",x)
  return(x)
})]


#Fix parts of the title that are fixable (remove colons, semicolons, periods):
#Title and Descriptions accept parenthesis, punctuations, apostrophes and hyphens
special_chars <- unique(unlist(strsplit(paste(d1$Title, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[-:,\'/?\r–.&*’—_;]"

d1[, Title := gsub(chars_to_remove, " ", Title)]
d1[, Title := gsub("\\s+", " ", trimws(Title))]
d1[, ID := seq(1,nrow(d1))]

#reorder
setcolorder(d1, c("ID","Title", "Organization", 
                  setdiff(names(d1), c("ID", "Title", "Organization"))))

#seperate to make the resources doc:
d3 <- d1[,.(ID, Title, `Document Name (title_location_year published)`)]



# JOIN TAGS -----------------

#clean up the tags a bit:
cols_to_drop <- c("Notes", "Is the document uploaded to Google Drive?",
                  "Document Name (title_location_year published)")
d1 <- d1[, !cols_to_drop, with = FALSE]

#need to make "All" into a column specific "All"
d1[, (names(d1)) := lapply(.SD, 
                             function(x) gsub("All \\(since this can be applied to any forest in BC\\)",
                                              "All", x))]
# BECZone_Subzone_variant - remove underscores:
d1[, BECZone_Subzone_variant := gsub("_", "", BECZone_Subzone_variant)]

cols_to_combine <- names(d1)[!names(d1) %in% c("ID","Title", "Organization")]
d1[, Tags := do.call(paste, c(.SD, sep = ",")), .SDcols = cols_to_combine]

d2 <- d1[,.(ID,Title, Organization, Tags)]
d2[, Tags := gsub(",+", ",", Tags)]  # Replace multiple commas with a single comma
d2[, Tags := gsub("^,|,$", "", Tags)]

#Tags don't accept special characters but accepts dashes, spaces, 
#underscores and capitalization. No slashes or ampersands

#first, get rid of the parentheses, but also anything inside the paraentheses:
d2[, Tags := gsub("\\(.*?\\)", "", Tags)]

#get rid of other special characters
special_chars <- unique(unlist(strsplit(paste(d2$Tags, collapse = ""), "")))
special_chars <- special_chars[grepl("[^[:alnum:]\\s]", special_chars)]
chars_to_remove <- "[/&.?]"
d2[, Tags := gsub(chars_to_remove, " ", Tags)]

#clean up the commas and spaces
d2[, Tags := gsub("NA", "", Tags)] # Remove "NA" 
d2[, Tags := gsub(",\\s*NA\\s*,", ",", Tags)] # Remove "NA" surrounded by commas
d2[, Tags := gsub("\\s*,\\s*", ", ", Tags)]   # Ensure a single space after each comma
d2[, Tags := gsub("\\s*,", ",", Tags)]        # Remove any spaces before a comma
d2[, Tags := gsub("\\s+$", "", Tags)]         # Remove trailing spaces

#might need to get rid of parentheses in Organization - not sure yet


#now pull out groups and add to a new column:
groups <- c("Variable retention", "Reforestation", "Prescribed fire",
            "Indigenous fire stewardship", "Thinning", "Stand interventions",
            "Fuel management", "Forest products", "Monitoring")

d2[, Group := sapply(Tags, function(x) {
  # Check for each group and return the ones that match
  matched_groups <- groups[sapply(groups, grepl, x)]
  if (length(matched_groups) > 0) {
    return(paste(matched_groups, collapse = ", "))  # Join matched groups with commas
  } else {
    return(NA)  # Return NA if no match is found
  }
})]


#add a License column:
d2[, License:="Open Data Commons Attribution License"]

#reorder:
d2 <- d2[,.(ID,Title,Organization, Tags, License, Group)]

#print out 20 resources
fwrite(d2[20:40], file.path(out_dir,"datasets_010425.csv"))

#create the resource doc:
#need to watch the ids - as we are making them twice.
d3 <- d3[,.(Dataset_ID = ID, Name = Title, 
      Path = `Document Name (title_location_year published)`)]

#append the file type to the end of each path - for now it's pdf, would
# need to update this in the future depending on file types
d3[,Path := paste0(Path,".pdf")]

fwrite(d3[20:40], file.path(out_dir,"resources_010425.csv"), append = FALSE)




