#' Write quitte data frame to a `.gdx` file
#'
#' Writes one or more variables from a quitte data frame as GAMS parameters
#' to a `.gdx` file using [`gamstransfer`].
#'
#' @param df A quitte data frame.
#' @param gdxFn Path to the `.gdx` file to write.
#' @param varmap Named character vector mapping quitte `variable` values to
#'     valid GAMS parameter names, e.g.
#'     `c("Emissions|CO2" = "emico2", "GDP|PPP" = "gdpPPP")`.
#'     Only variables present as names in `varmap` are written; others are
#'     dropped with a warning.
#' @param dimCols Character vector of quitte columns to use as GAMS parameter
#'     dimensions.  Defaults to `c("region", "period")`.
#' @param verbose If `TRUE`, warn about variables in `df` not present in
#'     `varmap`.  Defaults to `FALSE`.
#'
#' @return Invisibly returns `gdxFn`.
#' @author Tonn Rueter
#'
#' @importFrom cli cli_abort cli_warn
#' @importFrom dplyr filter mutate select
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyselect all_of
#'
#' @export
write.gdx <- function(df, gdxFn, varmap, dimCols = c("region", "period"), verbose = FALSE) {

    # validate varmap ----
    if (!is.character(varmap) || is.null(names(varmap))) {
        cli_abort(c(
            '`varmap` must be a named character vector.',
            'i' = 'e.g. c("Emissions|CO2" = "emico2")'
        ))
    }

    # validate dimCols ----
    missingDimCols <- setdiff(dimCols, colnames(df))
    if (length(missingDimCols) > 0) {
        cli_abort('`dimCols` columns not found in `df`: {missingDimCols}')
    }

    # warn about unmapped/missing variables ----
    presentVars <- unique(as.character(df[['variable']]))
    mappedVars  <- intersect(presentVars, names(varmap))
    dropped     <- setdiff(presentVars, names(varmap))
    missing     <- setdiff(names(varmap), presentVars)

    if (verbose && length(dropped) > 0) {
        cli_warn(c(
            '{length(dropped)} variable{?s} in `df` not in `varmap` and will be dropped:',
            'i' = '{dropped}'
        ))
    }
    if (length(missing) > 0) {
        cli_warn(c(
            '{length(missing)} variable{?s} in `varmap` not found in `df`:',
            'i' = '{missing}'
        ))
    }
    if (length(mappedVars) == 0) {
        cli_abort('No variables in `df` match any name in `varmap`.')
    }

    # build gamstransfer container ----
    container <- gamstransfer::Container$new()

    for (quitteVar in mappedVars) {
        records <- df %>%
            filter(.data[['variable']] == quitteVar) %>%
            select(all_of(c(dimCols, 'value'))) %>%
            mutate(across(all_of(dimCols), as.character))

        if (nrow(records) == 0) next

        container$addParameter(
            name    = varmap[[quitteVar]],
            domain  = rep('*', length(dimCols)),
            records = as.data.frame(records)
        )
    }

    # write to file ----
    container$write(gdxFn)

    invisible(gdxFn)
}