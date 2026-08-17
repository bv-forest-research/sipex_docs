##### Supplementary R script for descriptive location spelling check; Defines all functions used in the main script (01_import_clean.R)
### function for converting first nation names to characters
decode_html_numeric <- function(x) {

  decode_one <- function(text) {

    if (is.na(text)) {
      return(NA_character_)
    }

    pattern <- "&#(x[0-9A-Fa-f]+|[0-9]+);"

    repeat {
      match <- regexpr(pattern, text, perl = TRUE)

      if (match[1] == -1) {
        break
      }

      entity <- regmatches(text, match)

      # Remove &# from the beginning and ; from the end
      number_text <- sub("^&#", "", entity)
      number_text <- sub(";$", "", number_text)

      if (grepl("^x", number_text, ignore.case = TRUE)) {
        code_point <- strtoi(
          sub("^x", "", number_text, ignore.case = TRUE),
          base = 16L
        )
      } else {
        code_point <- strtoi(number_text, base = 10L)
      }

      replacement <- intToUtf8(code_point)
      regmatches(text, match) <- replacement
    }

    text
  }

  vapply(x, decode_one, character(1), USE.NAMES = FALSE)
}



###### sub-function for normalizing location text
# Normalize location text while preserving accents,
# apostrophes, hyphens, letters, and numbers
normalize_location_text <- function(x, split_hyphens = TRUE) {

  x <- as.character(x)

  # Standardize Unicode representation of accented characters
  x <- stringi::stri_trans_nfc(x)

  # Make matching case-insensitive
  x <- stringr::str_to_lower(x)

  # Standardize apostrophe-like characters
  x <- stringr::str_replace_all(
    x,
    "[\u2018\u2019\u02BC\uFF07]",
    "'"
  )

  # Standardize different dash characters
  x <- stringr::str_replace_all(
    x,
    "[\u2010\u2011\u2012\u2013\u2014\u2212]",
    "-"
  )

  # Optionally treat hyphens as word separators
  if (isTRUE(split_hyphens)) {
    x <- stringr::str_replace_all(x, "-", " ")
  }

  # Replace other punctuation with spaces.
  # Unicode letters and numbers are retained.
  x <- stringr::str_replace_all(
    x,
    "[^\\p{L}\\p{N}'-]+",
    " "
  )

  # Remove duplicated and leading/trailing spaces
  stringr::str_squish(x)
}



#####sub-function for tokenizing location names (extracting individual words)
tokenize_location_names <- function(
    data,
    column,
    split_hyphens = TRUE
) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }

  if (
    length(column) != 1L ||
    !is.character(column) ||
    !column %in% names(data)
  ) {
    stop(
      "Column not found: ",
      paste(column, collapse = ", ")
    )
  }

  tibble::tibble(
    row_id = seq_len(nrow(data)),
    source_name = as.character(data[[column]])
  ) %>%
    dplyr::mutate(
      normalized_name = normalize_location_text(
        source_name,
        split_hyphens = split_hyphens
      ),
      word = normalized_name
    ) %>%
    tidyr::separate_rows(
      word,
      sep = "\\s+"
    ) %>%
    dplyr::filter(
      !is.na(word),
      word != "",
      stringr::str_detect(
        word,
        "[\\p{L}\\p{N}]"
      )
    )
}

### function for building a vocabulary of unique words from location names, along with their occurrence counts and example names
build_location_vocabulary <- function(
    data,
    column,
    split_hyphens = FALSE,
    example_count = 3L
) {

  words <- tokenize_location_names(
    data = data,
    column = column,
    split_hyphens = split_hyphens
  )

  words %>%
    dplyr::group_by(word) %>%
    dplyr::summarise(
      occurrence_count = dplyr::n(),
      location_count = dplyr::n_distinct(source_name),
      example_names = paste(
        utils::head(
          unique(source_name),
          example_count
        ),
        collapse = "; "
      ),
      .groups = "drop"
    ) %>%
    dplyr::arrange(word)
}



##### final function for cross-checking the vocabulary of unique words from location names 
##### against a reference list of valid words, and flagging any words that are not found in the reference list
check_location_vocabulary <- function(
    x,
    vocabulary,
    vocab_col = "word",
    split_hyphens = TRUE,
    suggest_matches = TRUE,
    max_edit_distance = 2L
) {

  # Check inputs
  if (!is.data.frame(vocabulary)) {
    stop("`vocabulary` must be a data frame.")
  }

  if (!vocab_col %in% names(vocabulary)) {
    stop(
      "Vocabulary column not found: ",
      vocab_col
    )
  }

  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Clean and standardize the official vocabulary
  official_words <- normalize_location_text(
    vocabulary[[vocab_col]],
    split_hyphens = split_hyphens
  )

  official_words <- unique(
    official_words[
      !is.na(official_words) &
        official_words != ""
    ]
  )

  # Break the locations being checked into individual words
  checked_words <- tibble::tibble(
    row_id = seq_along(x),
    descriptive_location = x,
    normalized_location = normalize_location_text(
      x,
      split_hyphens = split_hyphens
    )
  ) |>
    tidyr::separate_rows(
      normalized_location,
      sep = "\\s+"
    ) |>
    dplyr::rename(
      word = normalized_location
    ) |>
    dplyr::filter(
      !is.na(descriptive_location),
      word != "",
      stringr::str_detect(
        word,
        "[\\p{L}\\p{N}]"
      )
    ) |>
    dplyr::distinct(
      row_id,
      descriptive_location,
      word
    )

  # Retain words that do not occur in the official vocabulary
  spelling_issues <- checked_words |>
    dplyr::filter(
      !word %in% official_words
    )

  # Return an empty, consistently structured table when no issues exist
  if (nrow(spelling_issues) == 0L) {

    return(
      spelling_issues |>
        dplyr::mutate(
          suggested_word = character(),
          edit_distance = integer()
        )
    )
  }

  # Optionally identify the nearest official vocabulary word
  if (isTRUE(suggest_matches)) {

    unmatched_words <- unique(spelling_issues$word)

    suggestion_table <- lapply(
      unmatched_words,
      function(current_word) {

        distances <- as.integer(
          utils::adist(
            current_word,
            official_words
          )
        )

        closest_position <- which.min(distances)
        closest_distance <- distances[closest_position]

        suggested_word <- if (
          closest_distance <= max_edit_distance
        ) {
          official_words[closest_position]
        } else {
          NA_character_
        }

        tibble::tibble(
          word = current_word,
          suggested_word = suggested_word,
          edit_distance = closest_distance
        )
      }
    ) |>
      dplyr::bind_rows()

    spelling_issues <- spelling_issues |>
      dplyr::left_join(
        suggestion_table,
        by = "word"
      )
  } else {

    spelling_issues <- spelling_issues |>
      dplyr::mutate(
        suggested_word = NA_character_,
        edit_distance = NA_integer_
      )
  }

  spelling_issues |>
    dplyr::arrange(
      row_id,
      word
    )
}

