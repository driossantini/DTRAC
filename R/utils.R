#' Parse column name from NSE or SE input
#' @param col_arg Quoted expression from enquo()
#' @return Character string of column name
parse_column_name <- function(col_arg) {
  rlang::as_name(col_arg)
}

#' Validate that a column exists and optionally check its type
#' @param data Data frame
#' @param col_name Character string of column name
#' @param col_type Optional type check:
#'   "numeric", "logical", "character", "factor"
#' @param arg_name Name of argument for error messages
#' @param allow_na Whether to allow NA values
validate_column <- function(data, col_name, col_type = NULL,
                            arg_name = col_name, allow_na = TRUE) {
  # Check existence
  if (!col_name %in% names(data)) {
    stop(sprintf(
      "Column '%s' not found in data (argument: %s)",
      col_name, arg_name
    ), call. = FALSE)
  }

  # Check type if specified
  if (!is.null(col_type)) {
    col_data <- data[[col_name]]

    type_valid <- switch(col_type,
      "numeric" = is.numeric(col_data),
      "logical" = is.logical(col_data),
      "character" = is.character(col_data),
      "factor" = is.factor(col_data),
      stop(sprintf("Unknown col_type: %s", col_type), call. = FALSE)
    )

    if (!type_valid) {
      stop(sprintf(
        "Column '%s' must be %s (argument: %s)",
        col_name, col_type, arg_name
      ), call. = FALSE)
    }
  }

  # Check for NAs if not allowed
  if (!allow_na && any(is.na(data[[col_name]]))) {
    n_missing <- sum(is.na(data[[col_name]]))
    warning(sprintf(
      "Column '%s' contains %d missing values",
      col_name, n_missing
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Batch validate multiple columns exist
validate_columns_exist <- function(data, col_names) {
  missing <- setdiff(col_names, names(data))

  if (length(missing) > 0) {
    stop(sprintf(
      "Columns not found in data: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate input is a data frame
validate_is_dataframe <- function(data) {
  ok <- inherits(data, "data.frame") || inherits(data, "lazy_dt")
  if (!ok) {
    stop("'data' must be a data.frame, tibble, data.table, or lazy_dt object.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Warn if about to overwrite an existing column
warn_if_overwriting <- function(data, col_name, add_message = NULL) {
  if (col_name %in% names(data)) {
    warning(sprintf("Overwriting existing column: '%s'", col_name),
      call. = FALSE
    )
    if (!is.null(add_message)) warning(paste(add_message), call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate and clean column name for R syntax
#' @param col_name Proposed column name
#' @return Syntactically valid column name
clean_colname <- function(col_name) {
  if (!identical(make.names(col_name), col_name)) {
    new_colname <- gsub("\\.+", ".", make.names(col_name))
    warning(sprintf(
      "Invalid column name '%s' changed to '%s'",
      col_name, new_colname
    ), call. = FALSE)
    return(new_colname)
  }
  col_name
}

#' Format a vector as copy-pasteable R code with line wrapping
#' @param vec Vector to format
#' @param width Maximum line width (default: 80)
#' @param indent Indentation for continuation lines (default: 2)
#' @return Character string with wrapped vector
#' @keywords internal
.format_vector_message <- function(vec, width = 80, indent = 2) {
  # Format as vector with quotes for character vectors
  if (is.character(vec)) {
    vec_string <- paste0("c(", paste(dQuote(vec, FALSE), collapse = ", "), ")")
  } else {
    vec_string <- paste0("c(", paste(vec, collapse = ", "), ")")
  }

  # Wrap at specified width
  wrapped <- strwrap(vec_string, width = width, exdent = indent)
  paste(wrapped, collapse = "\n")
}

#' Validate that baseline visit exists for all IDs
#' @param data Data frame
#' @param id_col Name of ID column
#' @param visit_col Name of visit column
#' @param baseline_tp Baseline visit value
validate_baseline <- function(data, id_col, visit_col, baseline_tp,
                              verbose = TRUE) {
  baseline_ids <- data |>
    dplyr::filter(.data[[visit_col]] == baseline_tp) |>
    dplyr::pull(.data[[id_col]]) |>
    unique()

  all_ids <- unique(data[[id_col]])

  missing_ids <- setdiff(all_ids, baseline_ids)

  if (length(missing_ids) > 0) {
    message("IDs missing baseline (visit = ", baseline_tp, "):")
    # Print missing ids as a vector
    message(.format_vector_message(missing_ids))
    stop(sprintf(
      "Baseline missing for %d IDs. See missing IDs above.",
      length(missing_ids)
    ), call. = FALSE)
  }
  invisible(TRUE)
}
