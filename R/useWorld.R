
#' @title useWorld
#' @description Changes the identifier for the global region to "World".
#'
#' @param qu A quitte object with a global region.
#' @return The quitte object with the global region named "World".
#' @author Patrick Rein
#' @export
useWorld <- function(qu) {
  qu <- droplevels(qu)
  levels(qu$region)[levels(qu$region) == "GLO"] <- "World"
  qu$region <- factor(qu$region, levels = sort(levels(qu$region)))
  return(qu)
}
