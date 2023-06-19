#' @noRd
### process tmax/tmin/tmean
.temperature <- function(
	key,
	rcp,
	gcm,
	start,
	end,
	summary,
	lorenz,
	yearByYear
) {

	# key		'tmax' or 'tmin' or 'tmean'

	# get rasters
	if (key == 'tmax') {
		y <- .getRasters(set=1, start=start, end=end, lorenz=lorenz, rcp=rcp, gcm=gcm, filename='temp')
	} else if (key == 'tmin') {
		y <- .getRasters(set=2, start=start, end=end, lorenz=lorenz, rcp=rcp, gcm=gcm, filename='temp')
	} else if (key == 'tmean') {
		y1 <- .getRasters(set=1, start=start, end=end, lorenz=lorenz, rcp=rcp, gcm=gcm, filename='temp')
		y2 <- .getRasters(set=2, start=start, end=end, lorenz=lorenz, rcp=rcp, gcm=gcm, filename='temp')
		y <- (y1 + y2) / 2
		
		monthIndices <- 1L:(12L * (end - start + 1L)) + 12L * (start - 1950L)
		names(y) <- paste0('tmean_', monthIndices)
		
	}

	prettyKey <- toupper(key)

	# annual mean
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
		thisOut <- .annual(y, 'sd', yearByYear=yearByYear)
		names(thisOut) <- paste0('an_sd_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# quarterly minimum
	if ('qlwr' %in% summary) {
		thisOut <- .quarter(y, fx=min, qtFx='mean')
		names(thisOut) <- paste0('qt_lwr_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# quarterly maximum
	if ('qhgr' %in% summary) {
		thisOut <- .quarter(y, fx=max, qtFx='mean')
		names(thisOut) <- paste0('qt_hgr_', prettyKey, yearByYear=yearByYear)
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
