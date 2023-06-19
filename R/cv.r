#' Coefficient of variation
#'
#' @description Coefficient of variation (standard deviation / mean).
#'
#' @param x Numeric vector or a `SpatRaster` "stack."
#' @param na.rm Logical: If `FALSE` (default), `NA` values are propagated.
#'
#' @returns Numeric or a `SpatRaster`.
#'
#' @example man/examples/ex_cv_sd.r
#'
#' @aliases cv
#' @rdname cv
#' @exportMethod cv
methods::setMethod(
	f = 'cv',
	signature = c(x = 'numeric'),
	definition = function(x, na.rm = FALSE) {
		stats::sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
	}
)

#' @aliases cv
#' @rdname cv
#' @exportMethod cv
methods::setMethod(
	f = 'cv',
	signature = c(x = 'SpatRaster'),
	definition = function(x) {
		stdev <- terra::app(x, sd)
		avg <- terra::mean(x)
		out <- stdev / avg
		names(out) <- 'cv'
		out
	} # EOF
)
