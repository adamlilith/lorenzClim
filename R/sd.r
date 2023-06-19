#' Standard deviation
#'
#' @description Standard deviation.
#'
#' @param x A `SpatRaster` "stack."
#'
#' @returns A `SpatRaster`.
#'
#' @example man/examples/ex_cv_sd.r
#'
#' @aliases sd
#' @rdname sd
#' @exportMethod sd
methods::setMethod(
	f = 'sd',
	signature = c(x = 'SpatRaster'),
	function(x) terra::app(x, sd)
)
