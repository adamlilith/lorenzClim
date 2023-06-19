#' @noRd
.etr <- function(
	key,
	rcp,
	gcm,
	start,
	end,
	summary,
	lorenz,
	yearByYear
) {

	# key		'ETR'

	# get rasters
	pet <- .getRasters(set=1, start=start, end=end, lorenz=lorenz, rcp=rcp, gcm=gcm, filename='ET')
	aet <- .getRasters(set=2, start=start, end=end, lorenz=lorenz, rcp=rcp, gcm=gcm, filename='ET')
	y <- aet / pet
	
	monthIndices <- 1L:(12L * (end - start + 1L)) + 12L * (start - 1950L)
	names(y) <- paste0('AET_', monthIndices)

	prettyKey <- toupper(key)

	# annual sum
	if ('summary' %in% summary) {
		thisOut <- .annual(y, 'mean', yearByYear=FALSE)
		names(thisOut) <- paste0('an_avg_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# annual variability
	if ('var' %in% summary) {
		thisOut <- .annual(y, 'cv', yearByYear=yearByYear)
		names(thisOut) <- paste0('an_cv_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# quarterly minimum
	if ('qlwr' %in% summary) {
		thisOut <- .quarter(y, min, qtFx='mean')
		names(thisOut) <- paste0('qt_lwr_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# quarterly maximum
	if ('qhgr' %in% summary) {
		thisOut <- .quarter(y, max, qtFx='mean')
		names(thisOut) <- paste0('qt_hgr_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# monthly minimum
	if ('mlwr' %in% summary) {
		thisOut <- .month(y, min)
		names(thisOut) <- paste0('mo_lwr_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# monthly maximum
	if ('mhgr' %in% summary) {
		thisOut <- .month(y, max)
		names(thisOut) <- paste0('mo_hgr_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	

	if ('raw' %in% summary) {

		if (exists('out', inherits=FALSE)) {
			out <- c(out, y)
		} else {
			out <- y
		}
		
	}
	
	out

}
