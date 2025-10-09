#' Calculate response based on a threshold comparison
#'
#' @param data Data frame in long format
#' @param metric_col Column containing metric values (NSE or quoted string)
#' @param threshold Threshold value for comparison
#' @param comparator Comparison operator: "<", ">", "<=", ">="
#' @param output_col Name for output column (auto-generated if NULL)
#' @param metric_name Name of metric for auto-naming (auto-detected if NULL)
#'
#' @return Data frame with additional logical column indicating response
#' @export
add_threshold <- function(data,
                          metric_col,
                          threshold,
                          comparator = "<",
                          output_col = NULL,
                          metric_name = NULL) {
  # Validate input
  validate_is_dataframe(data)

  # Parse column name (NSE/SE support)
  metric_col_name <- parse_column_name(rlang::enquo(metric_col))

  # Validate metric column
  validate_column(data, metric_col_name,
    col_type = "numeric",
    arg_name = "metric_col", allow_na = TRUE
  )

  # Auto-detect metric name if not provided
  if (is.null(metric_name)) {
    metric_name <- metric_col_name
  }

  # Validate comparator
  valid_comparators <- c("<", ">", "<=", ">=")
  if (!comparator %in% valid_comparators) {
    stop(
      sprintf(
        "comparator must be one of: %s",
        paste(valid_comparators, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Generate output column name
  if (is.null(output_col)) {
    comparator_text <- switch(comparator,
      "<" = "LT",
      ">" = "GT",
      "<=" = "LTE",
      ">=" = "GTE"
    )
    output_col <- paste(metric_name, comparator_text,
      threshold, "Resp",
      sep = "."
    )
  }
  output_col <- clean_colname(output_col)

  # Warn if overwriting
  warn_if_overwriting(
    data, output_col,
    add_message = "Specify new name in 'output_col'"
  )

  # Calculate response based on comparator
  comparison <- switch(comparator,
    "<" = data[[metric_col_name]] < threshold,
    ">" = data[[metric_col_name]] > threshold,
    "<=" = data[[metric_col_name]] <= threshold,
    ">=" = data[[metric_col_name]] >= threshold
  )

  # Add column using dplyr
  data <- data |>
    dplyr::mutate(!!output_col := comparison)

  data
}


#' Calculate DAS28 low disease activity response (DAS28 < 3.2)
#' @inheritParams add_threshold
#' @param threshold DAS28 threshold (default: 3.2 for LDA)
#' @export
add_das28_lda <- function(data, das28_col,
                          threshold = 3.2,
                          output_col = "DAS28.LDA.Resp") {
  add_threshold(
    data = data,
    metric_col = {{ das28_col }},
    threshold = threshold,
    comparator = "<",
    output_col = output_col,
    metric_name = "das28"
  )
}

#' Calculate DAS28 remission response (DAS28 < 2.6)
#' @inheritParams add_threshold
#' @param threshold DAS28 threshold (default: 2.6 for remission)
#' @export
add_das28_rem <- function(data, das28_col,
                          threshold = 2.6,
                          output_col = "DAS28.REM.Resp") {
  add_threshold(
    data = data,
    metric_col = {{ das28_col }},
    threshold = threshold,
    comparator = "<",
    output_col = output_col,
    metric_name = "das28"
  )
}