# Pedigree inbreeding pipeline (Wright's F via pedigreemm).
# Expected after column mapping: one row per animal with id, sire, dam.

ID_CANDIDATES <- c(
  "adu_dogid", "id", "animal", "animal_id", "animalid",
  "dogid", "dog_id", "dog"
)
SIRE_CANDIDATES <- c("adu_sire", "sire", "father", "dad")
DAM_CANDIDATES <- c("adu_dam", "dam", "mother", "mom")
NAME_CANDIDATES <- c(
  "dog_name", "name", "animal_name", "call_name", "registered_name"
)

is_missing_parent <- function(x) {
  is.na(x) | trimws(x) == "" | x == "0" | tolower(x) == "na"
}

normalize_id <- function(x) {
  x <- as.character(x)
  x[is_missing_parent(x)] <- NA_character_
  x
}

guess_column <- function(cols, candidates) {
  if (length(cols) == 0) {
    return("")
  }
  cols_l <- tolower(trimws(as.character(cols)))
  cands_l <- tolower(candidates)
  for (cand in cands_l) {
    hit <- which(cols_l == cand)
    if (length(hit) > 0) {
      return(cols[[hit[[1]]]])
    }
  }
  for (cand in cands_l) {
    hit <- which(grepl(cand, cols_l, fixed = TRUE))
    if (length(hit) > 0) {
      return(cols[[hit[[1]]]])
    }
  }
  cols[[1]]
}

guess_pedigree_columns <- function(cols) {
  list(
    id = guess_column(cols, ID_CANDIDATES),
    sire = guess_column(cols, SIRE_CANDIDATES),
    dam = guess_column(cols, DAM_CANDIDATES)
  )
}

default_keep_cols <- function(cols, guessed) {
  name_col <- character(0)
  cols_l <- tolower(trimws(as.character(cols)))
  for (cand in tolower(NAME_CANDIDATES)) {
    hit <- which(cols_l == cand)
    if (length(hit) > 0) {
      name_col <- cols[[hit[[1]]]]
      break
    }
  }
  unique(c(name_col, guessed$id, guessed$sire, guessed$dam))
}

read_pedigree_file <- function(path, name = path) {
  ext <- tolower(tools::file_ext(name))
  if (ext == "csv") {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    as.data.frame(readxl::read_excel(path))
  } else {
    stop("Please upload a CSV or Excel file (.csv, .xlsx, or .xls).")
  }
}

extract_pedigree <- function(dogs_df, id_col, sire_col, dam_col) {
  if (is.null(dogs_df) || nrow(dogs_df) == 0) {
    stop("The uploaded file has no rows.")
  }
  needed <- c(id_col, sire_col, dam_col)
  if (any(!nzchar(needed))) {
    stop("Choose columns for animal ID, sire, and dam.")
  }
  missing_cols <- setdiff(needed, names(dogs_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing column(s):", paste(missing_cols, collapse = ", ")))
  }
  if (length(unique(needed)) < 3) {
    stop("Animal ID, sire, and dam must be three different columns.")
  }

  data.frame(
    id = normalize_id(dogs_df[[id_col]]),
    sire = normalize_id(dogs_df[[sire_col]]),
    dam = normalize_id(dogs_df[[dam_col]]),
    stringsAsFactors = FALSE
  )
}

validate_pedigree <- function(ped) {
  errors <- character()
  warnings <- character()

  empty_id <- which(is.na(ped$id) | trimws(ped$id) == "")
  if (length(empty_id) > 0) {
    errors <- c(
      errors,
      paste0("Blank animal IDs at row(s): ", paste(empty_id, collapse = ", "))
    )
  }

  dup_ids <- unique(ped$id[duplicated(ped$id) & !is.na(ped$id)])
  if (length(dup_ids) > 0) {
    errors <- c(
      errors,
      paste0("Duplicate IDs found: ", paste(dup_ids, collapse = ", "))
    )
  }

  self_parent_rows <- which(
    (!is.na(ped$id) & !is.na(ped$sire) & ped$id == ped$sire) |
      (!is.na(ped$id) & !is.na(ped$dam) & ped$id == ped$dam)
  )
  if (length(self_parent_rows) > 0) {
    errors <- c(
      errors,
      paste0(
        "ID is own parent at row(s): ",
        paste(self_parent_rows, collapse = ", ")
      )
    )
  }

  parents <- unique(c(ped$sire, ped$dam))
  parents <- parents[!is.na(parents)]
  missing_from_id <- parents[!(parents %in% ped$id)]
  if (length(missing_from_id) > 0) {
    warnings <- c(
      warnings,
      paste0(
        length(missing_from_id),
        " parent(s) are missing their own ID rows and will be added as founders: ",
        paste(missing_from_id, collapse = ", ")
      )
    )
  }

  list(
    ok = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )
}

prepare_pedigree <- function(ped) {
  parents <- unique(c(ped$sire, ped$dam))
  parents <- parents[!is.na(parents)]
  missing_from_id <- parents[!(parents %in% ped$id)]
  if (length(missing_from_id) > 0) {
    founders <- data.frame(
      id = missing_from_id,
      sire = NA_character_,
      dam = NA_character_,
      stringsAsFactors = FALSE
    )
    ped <- rbind(ped, founders)
  }

  ord <- pedigree::orderPed(ped[, c("id", "sire", "dam")])
  ped_ordered <- ped[order(ord), , drop = FALSE]
  ped_ordered$recoded_id <- seq_len(nrow(ped_ordered))
  ped_ordered$sire_recoded <- ped_ordered$recoded_id[match(ped_ordered$sire, ped_ordered$id)]
  ped_ordered$dam_recoded <- ped_ordered$recoded_id[match(ped_ordered$dam, ped_ordered$id)]
  ped_ordered
}

compute_inbreeding <- function(ped_prepared) {
  loadNamespace("pedigreemm")
  p1 <- methods::new(
    "pedigree",
    sire = as.integer(ped_prepared$sire_recoded),
    dam = as.integer(ped_prepared$dam_recoded),
    label = as.character(ped_prepared$recoded_id)
  )
  pedigreemm::inbreeding(p1)
}

run_inbreeding <- function(dogs_df, id_col, sire_col, dam_col) {
  empty <- list(
    ok = FALSE,
    errors = character(),
    warnings = character(),
    results = NULL
  )

  ped <- tryCatch(
    extract_pedigree(dogs_df, id_col, sire_col, dam_col),
    error = function(e) e
  )
  if (inherits(ped, "error")) {
    empty$errors <- ped$message
    return(empty)
  }

  checks <- validate_pedigree(ped)
  if (!checks$ok) {
    return(list(
      ok = FALSE,
      errors = checks$errors,
      warnings = checks$warnings,
      results = NULL
    ))
  }

  ped_prepared <- tryCatch(
    prepare_pedigree(ped),
    error = function(e) e
  )
  if (inherits(ped_prepared, "error")) {
    return(list(
      ok = FALSE,
      errors = c(
        checks$errors,
        paste("Could not order the pedigree:", ped_prepared$message)
      ),
      warnings = checks$warnings,
      results = NULL
    ))
  }

  f_values <- tryCatch(
    compute_inbreeding(ped_prepared),
    error = function(e) e
  )
  if (inherits(f_values, "error")) {
    return(list(
      ok = FALSE,
      errors = c(
        checks$errors,
        paste("Could not calculate inbreeding:", f_values$message)
      ),
      warnings = checks$warnings,
      results = NULL
    ))
  }

  results <- data.frame(
    id = ped_prepared$id,
    sire = ped_prepared$sire,
    dam = ped_prepared$dam,
    inbreeding = as.numeric(f_values),
    inbreeding_pct = as.numeric(f_values) * 100,
    stringsAsFactors = FALSE
  )

  list(
    ok = TRUE,
    errors = character(),
    warnings = checks$warnings,
    results = results
  )
}
