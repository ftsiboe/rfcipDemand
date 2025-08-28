#' Calculate the Statistical Mode
#'
#' Returns the element that occurs most frequently in a vector.
#'
#' @param x A vector of any atomic type (numeric, character, factor,).
#' @param na.rm Logical; should missing values be ignored?  
#'   Defaults to `TRUE`. If `FALSE` and `x` contains any `NA`s, the
#'   function returns `NA`.
#'
#' @return A single value giving the modal element of `x`.  
#'   If two or more values are tied for the highest frequency,
#'   the first one encountered in `x` is returned.
#'
#' @details
#' Internally the function:
#' \enumerate{
#'   \item Optionally removes `NA`s (`na.rm = TRUE`).
#'   \item Builds a lookup table of unique values via `unique(x)`.
#'   \item Counts the frequency of each unique value with
#'     `tabulate(match(x, ux))`.
#'   \item Returns the value with the maximum count.
#' }
#'
#' Because it relies on base R functions, the implementation is
#' vectorised and generally fast for typical data-frame column sizes.
#' @export
calculate_mode <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

