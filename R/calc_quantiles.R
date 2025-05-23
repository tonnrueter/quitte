#' Sample Quantiles
#'
#' This is a wrapper function for [quantile] for easy use with data frames.
#'
#' @param .data a data frame, possibly grouped
#' @param value column name for which sample quantiles should be calculated
#' @param probs named numeric vector of probabilities with values in
#'     \eqn{[0, 1]}.
#' @param na.rm logical; if `TRUE`, any [NA] and [NaN]s are removed from `data`
#'     before the quantiles are computed.
#' @param type an integer between 1 and 9 select one of the nine quantile
#'     algorithms detailed in [quantile] to be used.
#'
#' @return A data frame.
#'
#' @author Michaja Pehl
#'
#' @examples
#' require(dplyr)
#' require(tidyr)
#'
#' tibble(group = rep(c("A", "B"), 10),
#'                value = 1:20) %>%
#'     group_by(group) %>%
#'     calc_quantiles() %>%
#'     pivot_wider(names_from = 'quantile')
#'
#' @importFrom purrr map
#' @importFrom rlang !! sym
#' @importFrom stats quantile
#' @importFrom tidyr nest unnest
#'
#' @export
calc_quantiles <- function(.data,
                           value = NA,
                           probs = c("q0"   = 0,
                                     "q25"  = 0.25,
                                     "q50"  = 0.5,
                                     "q75"  = 0.75,
                                     "q100" = 1),
                           na.rm = TRUE,
                           type  = 7) {

    value <- deparse(substitute(value))
    if ("NA" == value)
        value <- "value"

    calc_quantiles_(.data, value, probs, na.rm, type)
}

#' @export
#' @rdname calc_quantiles
calc_quantiles_ <- function(.data,
                            value = "value",
                            probs = c("q0"   = 0,
                                      "q25"  = 0.25,
                                      "q50"  = 0.5,
                                      "q75"  = 0.75,
                                      "q100" = 1),
                            na.rm = TRUE,
                            type  = 7) {

    # guardians
    if (!is.data.frame(.data))
        stop("only works for data frames")

    if (!value %in% colnames(.data))
        stop("no column '", value, "'")

    . <- NULL

    .data %>%
        nest() %>%   # collapse groups into list of data frames
        mutate(value = map(data, function(data) {
            # process 'value' column of each data frame
            quantile(data[[value]], probs = probs, na.rm = na.rm,
                     names = FALSE, type = type) %>%
                # expand out to data frame again
                data.frame(quantile = names(probs), value = .)
            })
        ) %>%
        select(-'data') %>%
        unnest('value')
}
