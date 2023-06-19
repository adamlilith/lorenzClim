#' Reconstruct future climate variables that match paleoclimate variables
#'
#' @description Lorenz et al. (2016) provide a set of two paleoclimate reconstructions for North America, from 22 or 21 Kybp the the present, plus a set of "historical" reconstructions from 1950 to 2006, plus a third set of CMIP5 "future" climate scenarios for RCPs 4.5 and 8.5. In theory, paleo, historical, and future sets have the same variables. In practice, the rasters for the historical period and future do not always represent the exact same variables as those for the past. For example, the past reconstructions provide the variables `an_avg_TMIN`, the annual average of minimum temperature, averaged across a 100 yr time span for each period. For the historical and future scenarios, minimum temperature is represented by one value per month from 1950 to 2100. These functions process the historical and future variables so they are commensurate with those from the paleoclimate reconstructions.
#'
#' Processing is "seemless" across the historical and future sets. For example, if you desire to calculate temperature across the period 1991 to 2020, the functions will combine the appropriate monthly values from the historical and future sets, so long as both sets are downloaded.
#'
#' **IMPORTANT**: To process historical and future rasters, they must be downloaded and saved as NetCDF files (the same format in which they are supplied). Data are available on [Dryad][https://datadryad.org//resource/doi:10.5061/dryad.1597g]
#' * Historical files: Historical rasters must be downloaded, unzipped, and saved in a folder named "`./historical`". Each subfolder therein should be the same of a GCM, and have the appropriate NetCDF files.
#' * Future files: The future files for RCP 4.5 are *must be* place a folder named `./rcp45`, and those for RCP 8.5 in `./rcp85`. Each of these folders should have one subfolder for each GCM, and the appropriate NetCDF files should be contained inside of them. 
#'
#' @param rcp Numeric or missing: If only historical (`start` and `end` are within 1950-2005) variables are desired, then this can be missing. Otherwise, it represents the Representative Concentration Pathway for rasters, and can be any of `45`, `4.5`, `85`, or `8.5`.
#'
#' @param gcm Character: Name(s) of the general circulation model(s) for which to obtain future variables. If more than one if specified, then the results will be returned as a named list, with one GCM per element in the list. Valid values are listed below. Partial matching is used, and case is ignored.
#' * `ACCESS1-3`, `CanESM2`, `CESM1-CAM5`, `CNRM-CM5`, `CSIRO-Mk3-6-0`, `GFDL-CM3`, `GISS-E2-R`, `HadGEM2-ES`, `inmcm4`, `IPSL-CM5A-MR`, `MIROC5`, `MRI-CGCM3`: GCMs named in [Table 3][https://www.nature.com/articles/sdata201648/tables/4] in Lorenz et al.
#' * `*`: All GCMs listed above.
#'
#' @param var Character vector: Name(s) of the variables to return. See [Table 1][https://www.nature.com/articles/sdata201648/tables/2] and [Table 3][https://www.nature.com/articles/sdata201648/tables/4] in Lorenz et al. (2016). These can be one or more of the following.  Partial matching is used, and case is ignored.
#' * `*`: All variables
#' * `tmax`: Maximum temperature (deg C)
#' * `tmin`: Maximum temperature (deg C)
#' * `tmean`: Mean temperature (deg C) (not part of original Lorenz variables)
#' * `prec`: Precipitation (mm)
#' * `GDD0` and `GDD5: Growing degree days using base 0 deg C or 5 deg C (days)
#' * `AET`: Actual evapotranspiration (mm)
#' * `PET`: Potential evapotranspiration (mm)
#' * `ETR`: Ratio of AET to PET (unit-less)
#' * `WDI`: Water deficit index (PET - precipitation; mm)
#' * `wind`: Wind speed (m / s) (Note: This variable is not available in the paleoclimate set.)
#' * `VP`: Vapor pressure of water (hPa) (Note: This variable is not available in the paleoclimate set.)
#' 
#' @param summary Character vector: Manner in which to summarize the variable(s). See See [Table 1][https://www.nature.com/articles/sdata201648/tables/2] in Lorenz et al. (2016). Partial matching is used, and case is ignored:
#' * `*`: All types (except "`raw`").
#' * `raw`: All monthly values from January of the `start` year to December of the `end` year.
#' * `annual`: All annual "summary" and "variability" variables (see next two items).
#' * `summary`: All annual "summary" variables (sum or mean, depending on the variable).
#' * `var`: All annual "variability" variables (standard deviation or coefficient of variation, depending on the variable)
#' * `quarter`: Both quarterly "lower" and "higher" variables (see next two items). Quarters are "calendar" quarters (Q1: January, February, March; Q2: April May, June; and so on). For temperature, ETR, wind, and vapor pressure, monthly values are combined into quarters using averages. For precipitation, GDD0, GDD5, AET, and PET, monthly values are combined into quarters using sums.
#' * `qlwr`: Values of the quarter with the minimum values of the variables. Returns the "`qt_lwr`" rasters ([Table 3][https://www.nature.com/articles/sdata201648/tables/4]).
#' * `qhgr`: Values of the quarter with the maximum values of the variables. Returns the "`qt_hgr`" rasters ([Table 3][https://www.nature.com/articles/sdata201648/tables/4]).
#' * `monthly`: Both monthly "lower" and "higher" variables (see next two items)
#' * `mlwr`: Minimum value across all 12 months of the year. Returns the "`mo_lwr`" rasters ([Table 3][https://www.nature.com/articles/sdata201648/tables/4]).
#' * `mhgr`: Maximum value across all 12 months of the year.  Returns the "`mo_hgr`" rasters ([Table 3][https://www.nature.com/articles/sdata201648/tables/4]).
#'
#' @param lorenz Character: Full path of the folder that contains `./rcp45` and/or `./rcp85`.
#'
#' @param start,end Numeric: Start and end years of the period over which the variables are to be calculated. If the period is longer than a year, then the averages across years will be returned. Values must be between 1950 and 2100, inclusive.
#'
#' @returns A `SpatRaster` with one or more layers. Each layer will be named as per the respetive paleoclimate variable in Lorenz et al. (2016), Table 2.
#'
#' @references Lorenz, D.J., Nieto-Lugilde, D., Blois, J.L., Fitzpatrick, M.C., and Williams, J.W.  2016.  Downscaled and debiased climate simulations for North America from 21,000 years ago to 2100AD.  *Scientific Data* 3:160048. \doi{10.1038/sdata.2016.48}
#'
#' Data for "Downscaled and debiased climate simulations for North America from 21,000 years ago to 2100AD", Dryad \doi{10.5061/dryad.1597g}
#' @example man/examples/ex_lorenzClim.r
#'
#' @export
lorenzClim <- function(
	var,
	summary,
	start,
	end,
	rcp,
	gcm,
	lorenz
) {

	### debugging
	#############
	
	if (FALSE) {
	
		var <- 'tmin'
		summary <- '*'
		start <- 2005
		end <- 2007
		rcp <- 85
		gcm <- 'ACCESS1-3'
		lorenz <- lorenz
	
	}

	### start/end years
	###################

		if (start < 1950 | start > 2100) stop('Argument <<start>> must be >= 1950 and <= 2100.')
		if (end < 1950 | end > 2100) stop('Argument <<end>> must be >= 1950 and <= 2100.')
		if (end < start) stop('Argument <<end>> must be >= argument <<start>>.')

	### RCP
	#######
		
		if (missing(rcp) && (start >= 2006 | end >= 2006)) {
			stop('You must specify the RCP for any periods that extend past 2005.')
		} else if (end >= 2006) {
			if (rcp %in% c(4.5, 8.5)) rcp <- rcp * 10
			if (!(rcp %in% c(45, 85))) stop('RCP must be 45 or 85.')
		} else if (end <= 2005) {
			rcp <- NULL
		}

	### GCM(s)
	##########
		
		options <- c('ACCESS1-3', 'CanESM2', 'CESM1-CAM5', 'CNRM-CM5', 'CSIRO-Mk3-6-0', 'GFDL-CM3', 'GISS-E2-R', 'HadGEM2-ES', 'inmcm4', 'IPSL-CM5A-MR', 'MIROC5', 'MRI-CGCM3')

		gcm <- unique(gcm)
		if ('*' %in% gcm) {
			gcm <- options
		} else {

			# match
			gcmMatch <- rep(NA_character_, length(gcm))
			for (i in seq_along(gcm)) {
			
				match <- pmatch(tolower(gcm[i]), tolower(options))
				if (is.na(match)) stop('Cannot match <<gcm>> ', gcm[i], '.')
				gcmMatch[i] <- options[match]

			}
			
			gcm <- gcmMatch
			
		}

	### variable(s)
	###############

		options <- c('tmax', 'tmin', 'tmean', 'prec', 'GDD0', 'GDD5', 'AET', 'PET', 'ETR', 'WDI', 'wind', 'VP')
		
		var <- unique(var)
		if ('*' %in% var) {
			var <- options
		} else {

			# match
			varMatch <- rep(NA_character_, length(var))
			for (i in seq_along(var)) {
			
				match <- pmatch(tolower(var[i]), tolower(options))
				if (is.na(match)) stop('Cannot match <<var>> ', var[i], '.')
				varMatch[i] <- options[match]

			}
			
			var <- varMatch
		}
		
	### summary of variable(s)
	##########################
		
		options <- c('annual', 'summary', 'var', 'quarter', 'qlwr', 'qhgr', 'monthly', 'mlwr', 'mhgr', 'raw')

		summary <- unique(summary)
		if ('*' %in% summary) {
			summary <- options
		} else {

			# match
			summaryMatches <- rep(NA_character_, length(summary))
			for (i in seq_along(summary)) {
			
				match <- pmatch(tolower(summary[i]), tolower(options))
				if (is.na(match)) stop('Cannot match <<summary>> ', summary[i], '.')
				summaryMatches[i] <- options[match]

			}
			
			summary <- summaryMatches
			
		}
			
		# get basic types
		if ('annual' %in% summary) {
			summary <- c(summary, 'summary', 'var')
			summary <- summary[summary != 'annual']
		}
		if ('quarter' %in% summary) {
			summary <- c(summary, 'qlwr', 'qhgr')
			summary <- summary[summary != 'quarter']
		}
		if ('monthly' %in% summary) {
			summary <- c(summary, 'mlwr', 'mhgr')
			summary <- summary[summary != 'monthly']
		}
		summary <- unique(summary)

	### check files
	###############

		if (any(c('tmin', 'tmax', 'tmean') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='temp')
		if (any(c('prec', 'WDI') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='prcp')
		if (any(c('GDD0', 'GDD5') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='gdd')
		if (any(c('AET', 'PET', 'ETR', 'WDI') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='ET')
		if (any(c('wind') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='wind')
		if (any(c('VP') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='vap')
	
	### process individual variables
	################################
	
	out <- list()

	for (countGcm in seq_along(gcm)) {
	
		thisGcm <- gcm[countGcm]
		
		if (any('tmax' %in% var)) {
			thisOut <- .lorenzTmax(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('tmin' %in% var)) {
			thisOut <- .lorenzTmin(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('tmean' %in% var)) {
			thisOut <- .lorenzTmean(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('prec' %in% var)) {
			thisOut <- .lorenzPrec(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('GDD0' %in% var)) {
			thisOut <- .lorenzGDD0(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('GDD5' %in% var)) {
			thisOut <- .lorenzGDD5(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('AET' %in% var)) {
			thisOut <- .lorenzAET(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('PET' %in% var)) {
			thisOut <- .lorenzPET(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('ETR' %in% var)) {
			thisOut <- .lorenzETR(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

		if (any('WDI' %in% var)) {
			thisOut <- .lorenzWDI(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}
		
		if (any('wind' %in% var)) {
			thisOut <- .lorenzWind(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}
		
		if (any('VP' %in% var)) {
			thisOut <- .lorenzVP(rcp=rcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz)
			if (length(out) < countGcm) {
				out[[countGcm]] <- thisOut
			} else {
				out[[countGcm]] <- c(out[[countGcm]], thisOut)
			}
		}

	} # next GCM

	names(out) <- gcm
	if (length(gcm) == 1L) out <- out[[1L]]
	out

}

### check file names
.checkFile <- function(lorenz, rcp, gcm, start, end, filename) {

	for (thisGcm in gcm) {

		if (start <= 2005) {
			file <- paste0(lorenz, '/historical/', thisGcm, '/', filename, '.nc')
			if (!file.exists(file)) {
				stop('File not found at:\n  ', file)
			}
		}
		if (end >= 2006) {
			file <- paste0(lorenz, '/rcp', rcp, '/', thisGcm, '/', filename, '.nc')
			if (!file.exists(file)) {
				stop('File not found at:\n  ', file)
			}
		}
		
	}
	invisible(TRUE)
}

### get rasters for given time period
.getRasters <- function(
	set,
	start,
	end, 
	lorenz,
	rcp,
	gcm,
	filename
) {

	### historic climate
	if (start <= 2005) {

		hist <- rast(paste0(lorenz, '/historical/', gcm, '/', filename, '.nc'))
		if (set == 1) {
			hist <- hist[[1L:672L]]
		} else {
			hist <- hist[[673L:1344L]]
		}
		
		thisEnd <- min(2005L, end)
		duration_yr <- thisEnd - start + 1L
		baseIndex <- 1L:(duration_yr * 12L)
		yearsFrom1950 <- start - 1950L
		monthsFrom1950 <- 12L * yearsFrom1950
		monthsIndices <- monthsFrom1950 + baseIndex
		
		hist <- hist[[monthsIndices]]
		
	}
	
	### future climate
	if (end >= 2006) {
	
		fut <- rast(paste0(lorenz, '/rcp', rcp, '/', gcm, '/', filename, '.nc'))
		if (set == 1) {
			fut <- fut[[1L:1440L]]
		} else {
			fut <- fut[[1441:2280L]]
		}
	
		thisStart <- max(2006L, start)
		duration_yr <- end - max(2006L, thisStart) + 1L
		baseIndex <- 1L:(duration_yr * 12L)
		yearsFrom2006 <- thisStart - 2006L
		monthsFrom2006 <- 12L * yearsFrom2006
		monthsIndices <- monthsFrom2006 + baseIndex
		
		fut <- fut[[monthsIndices]]
	
	}
	
	if (exists('hist', inherits = FALSE) & exists('fut', inherits=FALSE)) {
		out <- c(hist, fut)
	} else if (exists('hist', inherits = FALSE)) {
		out <- hist
	} else if (exists('fut', inherits = FALSE)) {
		out <- fut
	}
	
	out

}
	
# annual summary function
.annual <- function(y, fx) {

	# y			raster stack
	# fx 		summary function as a character: 'mean', 'sum', 'sd', or 'cv'

	mos <- terra::nlyr(y)
	yrs <- mos / 12L
	
	for (yr in seq_len(yrs)) {

		indices <- 12L * (yr - 1) + 1L:12L
		yy <- y[[indices]]

		if (fx == 'mean') {
			yyy <- terra::mean(yy)
		} else if (fx == 'sd') {
			yyy <- terra::app(yy, function(x) sd(x))
		} else if (fx == 'sum') {
			yyy <- sum(yy)
		} else if (fx == 'cv') {
			yyy <- cv(yy)
		}

		if (yr == 1L) {
			out <- yyy
		} else {
			out <- c(out, yyy)
		}
	
	}
	
	terra::mean(out)
	
}

# values of quarter with lowest/highest values
.quarter <- function(y, fx, qtFx) {

	# y		raster stack
	# fx	summary function *across* quarters (min or max)
	# qtFx	summary function *within* quarters (mean or sum)

	mos <- terra::nlyr(y)
	yrs <- mos / 12L
	
	for (yr in seq_len(yrs)) {
	
		q1 <- 12L * (yr - 1) + 1L:3L
		q2 <- 12L * (yr - 1) + 4L:6L
		q3 <- 12L * (yr - 1) + 7L:9L
		q4 <- 12L * (yr - 1) + 10L:12L
		
		q1 <- y[[q1]]
		q2 <- y[[q2]]
		q3 <- y[[q3]]
		q4 <- y[[q4]]
		
		if (qtFx == 'mean') {
			
			q1 <- terra::mean(q1)
			q2 <- terra::mean(q2)
			q3 <- terra::mean(q3)
			q4 <- terra::mean(q4)
			
		} else if (qtFx == 'sum') {
		
			q1 <- sum(q1)
			q2 <- sum(q2)
			q3 <- sum(q3)
			q4 <- sum(q4)
		
		}
			
		q <- c(q1, q2, q3, q4)
		thisOut <- terra::app(q, fx)
		
		if (yr == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}
		
	}
	
	terra::mean(out)
	
}

# values of month with lowest/highest values
.month <- function(y, fx) {

	# y		raster stack
	# fx	summary function (min or max)

	mos <- terra::nlyr(y)
	yrs <- mos / 12L
	
	for (yr in seq_len(yrs)) {
	
		indices <- 12L * (yr - 1) + 1L:12L
		yy <- y[[indices]]
		thisOut <- terra::app(yy, fx)
		
		if (yr == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}
		
	}
	
	terra::mean(out)
	
}
