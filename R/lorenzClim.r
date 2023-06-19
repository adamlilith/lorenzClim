#' Reconstruct future climate variables that match paleoclimate variables
#'
#' @description Lorenz et al. (2016) provide a set of two paleoclimate reconstructions for North America, from 22 or 21 Kybp the the present, plus a set of "historical" reconstructions from 1950 to 2006, plus a third set of CMIP5 "future" climate scenarios for RCPs 4.5 and 8.5. In theory, paleo, historical, and future sets have the same variables. In practice, the rasters for the historical period and future do not always represent the exact same variables as those for the past. For example, the past reconstructions provide the variables `an_avg_TMIN`, the annual average of minimum temperature, averaged across a 100 yr time span for each period. For the historical and future scenarios, minimum temperature is represented by one value per month from 1950 to 2100. These functions process the historical and future variables so they are commensurate with those from the paleoclimate reconstructions.
#'
#' Processing is "seemless" across the historical and future sets. For example, if you desire to calculate temperature across the period 1991 to 2020, the functions will combine the appropriate monthly values from the historical and future sets, so long as both sets are downloaded.
#'
#' **IMPORTANT**: To process historical and future rasters, they must be downloaded and saved as NetCDF files (the same format in which they are supplied). Data are available on [Dryad][https://datadryad.org//resource/doi:10.5061/dryad.1597g]
#' * Historical files: Historical rasters must be downloaded, unzipped, and *must be* saved in a folder named "`./historical`". Each subfolder therein should be the same of a GCM, and have the appropriate NetCDF files.
#' * Future files: The future files for RCP 4.5 *must be* place a folder named `./rcp45`, and those for RCP 8.5 in `./rcp85`. Each of these folders should have one subfolder for each GCM, and the appropriate NetCDF files should be contained inside of them. 
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
#' * `prcp`: Precipitation (mm)
#' * `GDD0` and `GDD5: Growing degree days using base 0 deg C or 5 deg C (days)
#' * `AET`: Actual evapotranspiration (mm)
#' * `PET`: Potential evapotranspiration (mm)
#' * `ETR`: Ratio of AET to PET (unit-less)
#' * `WDI`: Water deficit index (PET - precipitation; mm)
#' * `wind`: Wind speed (m / s) (Note: This variable is not available in the paleoclimate set.)
#' * `VP`: Vapor pressure of water (hPa) (Note: This variable is not available in the paleoclimate set.)
#' * `LWN`: Net long-wave radiation at the surface (W / m2) (Note: This variable is not available in the paleoclimate set.)
#' 
#' @param summary Character vector: Manner in which to summarize the variable(s). See See [Table 1][https://www.nature.com/articles/sdata201648/tables/2] in Lorenz et al. (2016). Partial matching is used, and case is ignored.
#' * `*`: All types (except "`raw`").
#' * `raw`: All monthly values from January of the `start` year to December of the `end` year.
#' * `annual`: All annual "summary" and "variability" variables (see next two items).
#' * `summary`: All annual "summary" variables (sum or mean, depending on the variable--`na.rm` is set to `TRUE`).
#' * `var`: All annual "variability" variables (standard deviation or coefficient of variation, depending on the variable--`na.rm` is set to `TRUE`)
#' * `quarter`: Both quarterly "lower" and "higher" variables (see next two items). Quarters are "calendar" quarters (Q1: January, February, March; Q2: April May, June; and so on). For temperature, ETR, wind, vapor pressure, and net longwave radiation, monthly values are combined into quarters using averages. For precipitation, GDD0, GDD5, AET, and PET, monthly values are combined into quarters using sums.
#' * `qlwr`: Values of the quarter with the minimum values of the variables. Returns the "`qt_lwr`" rasters ([Table 3][https://www.nature.com/articles/sdata201648/tables/4]--`na.rm` is set to `TRUE`).
#' * `qhgr`: Values of the quarter with the maximum values of the variables. Returns the "`qt_hgr`" rasters ([Table 3][https://www.nature.com/articles/sdata201648/tables/4]--`na.rm` is set to `TRUE`).
#' * `monthly`: Both monthly "lower" and "higher" variables (see next two items)
#' * `mlwr`: Minimum value across all 12 months of the year. Returns the "`mo_lwr`" rasters ([Table 3][https://www.nature.com/articles/sdata201648/tables/4]).
#' * `mhgr`: Maximum value across all 12 months of the year.  Returns the "`mo_hgr`" rasters ([Table 3][https://www.nature.com/articles/sdata201648/tables/4]).
#'
#' @param lorenz Character: Full path of the folder that contains `./rcp45` and/or `./rcp85`.
#'
#' @param yearByYear Logical: If `FALSE` (default), then for the annual "variability" summary statistic, first take the mean for each month (annual summaries) or quarter (quarter summaries) across years, then calculate the summary statistic across the 12 or 4 rasters. If `TRUE`, calculate the summary statistic across each year first, then take the average across years.
#'
#' @param start,end Numeric: Start and end years of the period over which the variables are to be calculated. If the period is longer than a year, then the averages across years will be returned. Values must be between 1950 and 2100, inclusive.
#'
#' @param correctWDI Logical: Water deficit index (WDI) is defined as PET - precipitation. However, the WDI rasters in the Lorenz et al. (2016) paleoclimate data (up to version Version 2017-06-16) appear to be precipitation - PET. To ensure comparability, you can calculate WDI using either version. If `correctWDI` is `TRUE` (default), then calculate historical/future WDI as PET - precipitation. If `FALSE`, use precipitation - PET to ensure comparability with the paleoclimate rasters (or just multiple the paleoclimate rasters by -1).
#'
#' @param verbose Logical: If `TRUE`, display progress.
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
	lorenz,
	correctWDI = TRUE,
	yearByYear = FALSE,
	verbose = FALSE
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
		yearByYear <- FALSE
	
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
		} else if ('*' %in% rcp & end >= 2006) {
			rcp <- c(45, 85)
		} else if ('*' %in% rcp & end <= 2005) {
			rcp <- NA
		} else if (end >= 2006) {
			for (i in seq_along(rcp)) if (rcp[i] %in% c(4.5, 8.5)) rcp[i] <- rcp[i] * 10
			if (!(any(rcp %in% c(45, 85)))) stop('RCP must be 45 and/or 85.')
		} else if (end <= 2005) {
			rcp <- NA
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

		options <- c('tmax', 'tmin', 'tmean', 'prcp', 'GDD0', 'GDD5', 'AET', 'PET', 'ETR', 'WDI', 'wind', 'VP', 'LWN')
		
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
			summary <- options[options != 'raw']
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
		if (any(c('prcp', 'WDI') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='prcp')
		if (any(c('GDD0', 'GDD5') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='gdd')
		if (any(c('AET', 'PET', 'ETR', 'WDI') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='ET')
		if (any(c('wind') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='wind')
		if (any(c('VP') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='vap')
		if (any(c('LWN') %in% var)) .checkFile(lorenz=lorenz, rcp=rcp, gcm=gcm, start=start, end=end, filename='lwn')
	
	### process individual variables
	################################
	
	out <- list()
	count <- 0L
	for (countGcm in seq_along(gcm)) {
	
		thisGcm <- gcm[countGcm]
		
		for (thisRcp in rcp) {
			
			count <- count + 1L
			
			if (verbose) {
				if (is.na(rcp)) {
					cat(thisGcm, 'GCM\n')
				} else {
					cat(thisGcm, 'GCM | RCP', rcp, '\n')
				}
				utils::flush.console()
			}
			
			if (any('tmax' %in% var)) {
				thisOut <- .temperature(key='tmax', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('tmin' %in% var)) {
				thisOut <- .temperature(key='tmin', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('tmean' %in% var)) {
				thisOut <- .temperature(key='tmean', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('prcp' %in% var)) {
				thisOut <- .prec(key='prcp', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('GDD0' %in% var)) {
				thisOut <- .gdd(key='GDD0', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('GDD5' %in% var)) {
				thisOut <- .gdd(key='GDD5', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('AET' %in% var)) {
				thisOut <- .et(key='AET', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('PET' %in% var)) {
				thisOut <- .et(key='PET', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('ETR' %in% var)) {
				thisOut <- .etr(key='ETR', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('WDI' %in% var)) {
				thisOut <- .wdi(key='WDI', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear, correctWDI=correctWDI)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}
			
			if (any('wind' %in% var)) {
				thisOut <- .wind(key='wind', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}
			
			if (any('VP' %in% var)) {
				thisOut <- .vp(key='VP', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			if (any('LWN' %in% var)) {
				thisOut <- .lwn(key='LWN', rcp=thisRcp, gcm=thisGcm, start=start, end=end, summary=summary, lorenz=lorenz, yearByYear=yearByYear)
				if (length(out) < countGcm) {
					out[[count]] <- thisOut
				} else {
					out[[count]] <- c(out[[countGcm]], thisOut)
				}
			}

			names(out)[count] <- if (!is.na(thisRcp[1L])) {
				paste0(gcm, '_rcp', thisRcp)
			} else {
				gcm
			}

		} # next RCP

	} # next GCM

	if (length(gcm) == 1L) out <- out[[1L]]
	out

}

### check file names
.checkFile <- function(lorenz, rcp, gcm, start, end, filename) {

	if (is.null(rcp)) rcp <- NA

	for (thisGcm in gcm) {

		for (thisRcp in rcp) {

			if (start <= 2005) {
				file <- paste0(lorenz, '/historical/', thisGcm, '/', filename, '.nc')
				if (!file.exists(file)) {
					stop('File not found at:\n  ', file)
				}
			}
			if (end >= 2006) {
				file <- paste0(lorenz, '/rcp', thisRcp, '/', thisGcm, '/', filename, '.nc')
				if (!file.exists(file)) {
					stop('File not found at:\n  ', file)
				}
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

		hist <- terra::rast(paste0(lorenz, '/historical/', gcm, '/', filename, '.nc'))
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
	
		fut <- terra::rast(paste0(lorenz, '/rcp', rcp, '/', gcm, '/', filename, '.nc'))
		if (set == 1) {
			fut <- fut[[1L:1140L]]
		} else {
			fut <- fut[[1141:2280L]]
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
.annual <- function(y, fx, yearByYear) {

	# y			raster stack
	# fx 		summary function as a character: 'mean', 'sum', 'sd', or 'cv'
	# yearByYear logical

	mos <- terra::nlyr(y)
	yrs <- mos / 12L
	
	# collate by month first
	if (!yearByYear) {
	
		for (yr in 1L:12L) {

			indices <- 1L:yrs * 12L - (12L - yr + 1L) + 1L
			yy <- y[[indices]]
			yyy <- terra::mean(yy)
			if (yr == 1L) {
				out <- yyy
			} else {
				out <- c(out, yyy)
			}
		
		}
	
		if (fx == 'mean') {
			out <- terra::mean(out, na.rm=TRUE)
		} else if (fx == 'sd') {
			out <- terra::app(out, function(x) sd(x, na.rm=TRUE))
		} else if (fx == 'sum') {
			out <- sum(out, na.rm=TRUE)
		} else if (fx == 'cv') {
			out <- cv(out, na.rm=TRUE)
		}

	# collate year-by-year
	} else {
		
		for (yr in seq_len(yrs)) {

			indices <- 12L * (yr - 1) + 1L:12L
			yy <- y[[indices]]

			if (fx == 'mean') {
				yyy <- terra::mean(yy, na.rm=TRUE)
			} else if (fx == 'sd') {
				yyy <- terra::app(yy, function(x) sd(x))
			} else if (fx == 'sum') {
				yyy <- sum(yy, na.rm=TRUE)
			} else if (fx == 'cv') {
				yyy <- cv(yy, na.rm=TRUE)
			}

			if (yr == 1L) {
				out <- yyy
			} else {
				out <- c(out, yyy)
			}
		
		}
		
		out <- terra::mean(out)
	}
	out
		
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
			
			q1 <- terra::mean(q1, na.rm=TRUE)
			q2 <- terra::mean(q2, na.rm=TRUE)
			q3 <- terra::mean(q3, na.rm=TRUE)
			q4 <- terra::mean(q4, na.rm=TRUE)
			
		} else if (qtFx == 'sum') {
		
			q1 <- sum(q1, na.rm=TRUE)
			q2 <- sum(q2, na.rm=TRUE)
			q3 <- sum(q3, na.rm=TRUE)
			q4 <- sum(q4, na.rm=TRUE)
		
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
		thisOut <- terra::app(yy, fx, na.rm=TRUE)
		
		if (yr == 1L) {
			out <- thisOut
		} else {
			out <- c(out, thisOut)
		}
		
	}
	
	terra::mean(out)
	
}
