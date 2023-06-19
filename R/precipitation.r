#' @noRd
.lorenzPrec <- function(
	summary,
	start,
	end,
	rcp,
	gcm,
	lorenz
) {
	.prec(
		key = 'prec',
		rcp = rcp,
		gcm = gcm,
		start = start,
		end = end,
		summary = summary,
		lorenz = lorenz
	)
}

### process precipitation
.prec <- function(
	key,
	rcp,
	gcm,
	start,
	end,
	summary,
	lorenz

) {

	# key		'prec'

	# get rasters
	y <- .getRasters(set=1, start=start, end=end, lorenz=lorenz, rcp=rcp, gcm=gcm, filename='prcp')

	prettyKey <- toupper(key)

	# annual sum
	if ('summary' %in% summary) {
		thisOut <- .annual(y, 'sum')
		names(thisOut) <- paste0('an_sum_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# annual variability
	if ('var' %in% summary) {
		thisOut <- .annual(y, 'cv')
		names(thisOut) <- paste0('an_cv_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# quarterly minimum
	if ('qlwr' %in% summary) {
		thisOut <- .quarter(y, min, 'sum')
		names(thisOut) <- paste0('qt_lwr_', prettyKey)
		if (exists('out', inherits=FALSE)) {
			out <- c(out, thisOut)
		} else {
			out <- thisOut
		}
	}
	
	# quarterly maximum
	if ('qhgr' %in% summary) {
		thisOut <- .quarter(y, max, 'sum')
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
