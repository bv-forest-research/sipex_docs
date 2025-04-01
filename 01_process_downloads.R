

library(data.table)
in_dir <- "downloads"

d1 <- fread(file.path(in_dir,"Document Collection and Tagging-010425.csv"))

#clean colnames:
setnames(d1, "V1", "Doc name")
setnames(d1, gsub("\n", "", names(d1), fixed = TRUE))  # Replace newlines with spaces
setnames(d1, gsub("/", "", names(d1), fixed = TRUE))
setnames(d1, trimws(names(d1)))
names(d1)

d1[, (names(d1)) := lapply(.SD, function(x) {
  x <- trimws(x)                  # Trim leading/trailing spaces and newlines
  x <- gsub("\n+", ", ", x)        # Replace one or more newlines with a single comma + space
  x <- gsub(",\\s*,", ",", x)      # Remove double commas caused by empty newlines
  return(x)
})]


