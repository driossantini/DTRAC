#' Add CDAI50 response from percentage improvement column
#'
#' Creates a binary response column indicating 50% or greater improvement.
#' Requires a percentage improvement column created by \code{add_improvement()}.
#'
#' @param data A data frame
#' @param cdai_col Column containing CDAI percentage improvement values
#'   (NSE or quoted string). Default: \code{"CDAI.pct.imp"}
#' @param output_col Name for output column. Default: \code{"CDAI50.Resp"}
#'
#' @return The input data frame with an additional logical column
#'
#' @examples
#' # Efficient workflow: calculate improvement once, apply multiple thresholds
#' trial_data |>
#'   add_improvement(c(CDAI, DAS28), baseline_tp = 0) |>
#'   add_cdai50() |>
#'   add_cdai70() |>
#'   add_cdai85()
#'
#' # Custom column names
#' trial_data |>
#'   add_improvement(CDAI, baseline_tp = 0, output_suffix = "_change") |>
#'   add_cdai50(cdai_col = "CDAI_change", output_col = "cdai50_response")
#'
#' @export
add_cdai50 <- function(data,
                       cdai_col = "CDAI.pct.imp",
                       output_col = "CDAI50.Resp") {
  add_threshold(
    data = data,
    metric_col = {{ cdai_col }},
    threshold = 50,
    comparator = ">=",
    output_col = output_col
  )
}

#' Add CDAI70 response from percentage improvement column
#'
#' Creates a binary response column indicating 70% or greater improvement.
#' Requires a percentage improvement column created by \code{add_improvement()}.
#'
#' @inheritParams add_cdai50
#' @param output_col Name for output column. Default: \code{"CDAI70.Resp"}
#'
#' @export
add_cdai70 <- function(data,
                       cdai_col = "CDAI.pct.imp",
                       output_col = "CDAI70.Resp") {
  add_threshold(
    data = data,
    metric_col = {{ cdai_col }},
    threshold = 70,
    comparator = ">=",
    output_col = output_col
  )
}

#' Add CDAI85 response from percentage improvement column
#'
#' Creates a binary response column indicating 85% or greater improvement.
#' Requires a percentage improvement column created by \code{add_improvement()}.
#'
#' @inheritParams add_cdai50
#' @param output_col Name for output column. Default: \code{"CDAI85.Resp"}
#'
#' @export
add_cdai85 <- function(data,
                       cdai_col = "CDAI.pct.imp",
                       output_col = "CDAI85.Resp") {
  add_threshold(
    data = data,
    metric_col = {{ cdai_col }},
    threshold = 85,
    comparator = ">=",
    output_col = output_col
  )
}
