#' Add percentage improvement columns from baseline
#'
#' Calculates percentage improvement from a baseline visit for one or more
#' metric columns. For each metric, adds a column containing the percentage
#' change from baseline, calculated only for visits at or after the baseline
#' timepoint.
#'
#' @param data A data frame in long format with one row per ID per visit
#' @param metric_cols Column(s) containing metric values. Supports:
#'   \itemize{
#'     \item Single column (NSE): \code{cdai}
#'     \item Multiple columns (NSE): \code{c(cdai, das28, sdai)}
#'     \item Tidyselect helpers: \code{starts_with("outcome")}
#'     \item Quoted strings (SE): \code{"cdai"} or \code{c("cdai", "das28")}
#'   }
#' @param id_col Column containing ID values. Default: \code{"id"}
#' @param visit_col Column containing visit identifiers. Must be numeric for
#'   comparison with \code{baseline_tp}. Default: \code{"visit"}
#' @param baseline_tp Baseline visit value. All IDs must have data at this
#'   visit. Single value applied to all IDs. Default: \code{0}
#' @param output_suffix Suffix appended to metric column names to create output
#'   column names. Default: \code{".pct.imp"}
#' @param eps Small value to avoid division by zero. Baseline values with
#'   absolute value less than \code{eps} result in \code{NA}.
#'   Default: \code{1e-8}
#'
#' @return The input data frame with additional columns containing percentage
#'   improvement values. Output columns are named \code{<metric><output_suffix>}
#'   (e.g., \code{cdai.pct.imp}). Values are \code{NA} for visits before
#'   baseline or when baseline is zero/missing.
#'
#' @details
#' The percentage improvement is calculated as:
#' \deqn{\frac{\text{baseline} - \text{current}}{\text{baseline}} \times 100}
#'
#' Positive values indicate improvement (reduction in disease activity).
#'
#' The function performs the following validations:
#' \itemize{
#'   \item All metric columns must be numeric
#'   \item All IDs must have exactly one baseline observation
#'   \item Visit column must be numeric for temporal comparison
#' }
#'
#' @examples
#' # Single metric
#' trial_data |>
#'   add_improvement(cdai, baseline_tp = 0)
#'
#' # Multiple metrics with custom suffix
#' trial_data |>
#'   add_improvement(c(cdai, das28),
#'     baseline_tp = 0,
#'     output_suffix = "_change"
#'   )
#'
#' # Using tidyselect
#' trial_data |>
#'   add_improvement(starts_with("score"), baseline_tp = 1)
#'
#' # Standard evaluation
#' trial_data |>
#'   add_improvement("cdai", id_col = "patient_id", baseline_tp = 0)
#'
#' @export
add_improvement <- function(data,
                            metric_cols,
                            id_col = "id",
                            visit_col = "visit",
                            baseline_tp = 0,
                            output_suffix = "pct.imp",
                            eps = 1e-8) {
  # Validate input
  validate_is_dataframe(data)

  # Parse column names
  id_col_name <- parse_column_name(rlang::enquo(id_col))
  visit_col_name <- parse_column_name(rlang::enquo(visit_col))

  # Handle metric_cols - supports tidyselect
  metric_col_names <- names(dplyr::select(data, {{ metric_cols }}))

  if (length(metric_col_names) == 0) {
    stop("No metric columns selected", call. = FALSE)
  }

  # Validate columns
  validate_column(data, id_col_name, arg_name = "id_col", allow_na = FALSE)
  validate_column(data, visit_col_name,
    arg_name = "visit_col", allow_na = FALSE
  )

  invisible(lapply(metric_col_names, function(x) {
    validate_column(data, x,
      col_type = "numeric",
      arg_name = "metric_cols", allow_na = TRUE
    )
  }))

  # Validate baseline coverage
  validate_baseline(data, id_col_name, visit_col_name, baseline_tp)

  # Generate output column names directly from metric names
  output_cols <- vapply(
    paste0(metric_col_names, ".", output_suffix),
    clean_colname,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  # Warn if overwriting
  invisible(lapply(output_cols, function(x) {
    warn_if_overwriting(data, x)
  }))

  # Extract baseline values (one row per ID, multiple metric columns)
  baseline_data <- data |>
    dplyr::filter(.data[[visit_col_name]] == baseline_tp) |>
    dplyr::arrange(.data[[id_col_name]]) |>
    dplyr::distinct(.data[[id_col_name]], .keep_all = TRUE) |>
    dplyr::select(
      dplyr::all_of(id_col_name),
      dplyr::all_of(metric_col_names)
    ) |>
    dplyr::rename_with(
      ~ paste0("baseline___", .x),
      .cols = dplyr::all_of(metric_col_names)
    )

  # Join baseline to all rows
  data <- data |>
    dplyr::left_join(baseline_data, by = id_col_name)

  # Pre-build named expressions
  visit_sym <- rlang::sym(visit_col_name)


  mut_exprs <- Map(
    f = function(metric, out_col) {
      metric_sym <- rlang::sym(metric)
      base_sym <- rlang::sym(paste0("baseline___", metric))

      expr_val <- rlang::expr(
        dplyr::if_else(
          !!visit_sym >= !!baseline_tp & abs(!!base_sym) > !!eps,
          (!!base_sym - !!metric_sym) / !!base_sym * 100,
          NA_real_
        )
      )

      rlang::set_names(list(expr_val), out_col)
    },
    metric_col_names,
    output_cols
  )

  # Flatten list - unname() prevents double-naming from Map()
  mut_exprs <- unlist(unname(mut_exprs), recursive = FALSE)

  # Apply all mutations at once and remove baseline columns
  data |>
    dplyr::mutate(!!!mut_exprs) |>
    dplyr::select(-dplyr::starts_with("baseline___"))
}