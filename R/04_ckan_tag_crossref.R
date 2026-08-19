##### Supplementary R script for cross-referencing
##### control taglist against CKAN taglist on SIPex

rm(list = ls())
setwd("D:/BVRC/R/Github/sipex_docs")
ref_results <- "ref_results"
ref_df <- "ref_source"

library(readr)
library(dplyr)

# 1. read in controlled taglist from google drive
controlled_wide <- readr::read_csv(
  (file.path(ref_df, "01_controlled taglist_jan_2026.csv")),
  skip = 1,
  show_col_types = FALSE
)

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
  )%>%
  select(-tag_category)

# 2. read in CKAN taglist from SIPex
ckan_tags <- readr::read_csv((file.path(ref_df, "ckan_tags_20260817.csv")),
  show_col_types = FALSE
) %>%
  mutate(
    ckan_key = make_tag_key(name)
  )%>%
  rename(ckan_tag = "name") %>%
  select(ckan_tag, ckan_key) 


# 3. cross-reference controlled taglist against CKAN taglist
ckan_audit <- ckan_tags %>%
  mutate(
    status = case_when(
      ckan_tag %in% controlled_long$controlled_tag ~
        "Exact match",

      ckan_key %in% controlled_long$controlled_key ~
        "Normalized match",

      TRUE ~
        "Missing from controlled"
    )
  )

controlled_audit <- controlled_long %>%
  mutate(
    status = case_when(
      controlled_tag %in% ckan_tags$ckan_tag ~
        "Exact match",

      controlled_key %in% ckan_tags$ckan_key ~
        "Normalized match",

      TRUE ~
        "Missing from CKAN"
    )
  )

write.csv(ckan_audit, file.path(ref_results, "ckanxcontrol_audit.csv"), row.names = FALSE)
write.csv(controlled_audit, file.path(ref_results, "controlxckan_audit.csv"), row.names = FALSE)

View(ckan_audit)
View(controlled_audit)

ckan_audit %>%
  count(status)
controlled_audit %>%
  count(status)