# lorenzClim
<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![cran version](https://www.r-pkg.org/badges/version/lorenzClim)](https://cran.r-project.org/package=lorenzClim)

<!-- badges: end -->

<strong>Match future, historical, and paleoclimate variables from Lorenz et al. (2016 Scientific Data)</strong>

<a href="https://dx.doi.org/10.1038/sdata.2016.48">Lorenz et al. (2016 Scientific Data)</a> provide a set of two paleoclimate reconstructions for North America, from 22 or 21 Kybp the the present, plus a set of "historical" reconstructions from 1950 to 2006, plus a third set of CMIP5 "future" climate scenarios for RCPs 4.5 and 8.5. In theory, paleo, historical, and future sets have the same <a href="https://www.nature.com/articles/sdata201648/tables/4">variables</a>. In practice, the rasters for the historical period and future do not always represent the exact same variables as those for the past. For example, the past reconstructions provide the variables `an_avg_TMIN`, the annual average of minimum temperature, averaged across a 100 yr time span for each period. For the historical and future scenarios, minimum temperature is represented by one value per month from 1950 to 2100. These functions process the historical and future variables so they are commensurate with those from the paleoclimate reconstructions.

Processing is "seemless" across the historical and future sets. For example, if you desire to calculate temperature across the period 1991 to 2020, the functions will combine the appropriate monthly values from the historical and future sets, so long as both sets are downloaded.

Each <a href="https://www.nature.com/articles/sdata201648/tables/3">general circulation model</a> served by Lorenz et al. can be processed one at time, or all at once.

**IMPORTANT**: To process historical and future rasters, they must be downloaded and saved as NetCDF files (the same format in which they are supplied). Data are available on <a href="https://datadryad.org//resource/doi:10.5061/dryad.1597g">Dryad</a>.
* Historical files: Historical rasters must be downloaded, unzipped, and saved in a folder named "`./historical`". Each subfolder therein should be the same of a GCM, and have the appropriate NetCDF files.
* Future files: The future files for RCP 4.5 are *must be* place a folder named `./rcp45`, and those for RCP 8.5 in `./rcp85`. Each of these folders should have one subfolder for each GCM, and the appropriate NetCDF files should be contained inside of them.

# Installation #

You can install the development version of this package using:

`remotes::install_github('adamlilith/lorenzClim', dependencies = TRUE)`  

You may need to install the `remotes` package first.

# How-to #

The key function in the package is `lorenzClim()`. To use it, you need to:

1. Download the future and/or historical climate rasters from <a href="https://datadryad.org//resource/doi:10.5061/dryad.1597g">Dryad</a>.

2. Unzip the files.

     a. For the "historical" set, put them in a folder named "`historical`". Inside this folder should be sub-folders, one per GCM. Inside them should be NetCDF files.
     
	 b. For the "future" sets, put them in folders named "`rcp45`" and "`rcp85`". Inside these folders should be sub-folders, one per GCM. Inside them should be NetCDF files.
	 
3. Let's do it!

```
library(lorenzClim)
library(terra) # needed to support rasters

# This is the path where the "/historical", "/rcp45" and "/rcp85" folders
# reside. Each of these needs to have subfolders with each GCM's output. Change
# this according to where you have downloaded and unzipped the files. This
# example is for a Windows system:
lorenz <- 'C:/Ecology/Data/Lorenz et al 2016'

# one variable and one GCM, collated first across years by month
outMxM <- lorenzClim(
    var = 'tmean',
    summary = '*',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz,
    yearByYear = FALSE
)

outMxM

# one variable and one GCM, collated first across years by month
outYxY <- lorenzClim(
    var = 'tmean',
    summary = '*',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz,
    yearByYear = TRUE
)

outYxY

plot(outMxM[['an_sd_TMEAN']] - outYxY[['an_sd_TMEAN']])

# two variables and one GCM
twos <- lorenzClim(
    var = c('tmin', 'tmax'),
    summary = 'annual',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz,
    yearByYear = FALSE
)

plot(twos)

# one variable and two RCPs
rcps <- lorenzClim(
    var = 'GDD0',
    summary = 'monthly',
    start = 2005,
    end = 2007,
    rcp = '*',
    gcm = 'ACCESS1-3',
    lorenz = lorenz,
    yearByYear = FALSE
)

plot(rcps)

# two variables and two GCMs
et <- lorenzClim(
    var = c('PET', 'AET'),
    summary = 'quarter',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = c('ACCESS1-3', 'CanESM2'),
    lorenz = lorenz,
    yearByYear = FALSE,
	verbose = TRUE
)

et

# one variable, all GCMs, one summary statistic
wind <- lorenzClim(
    var = 'wind',
    summary = 'summary',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = '*',
    lorenz = lorenz,
    yearByYear = FALSE,
	verbose = TRUE
)

wind

# "raw" monthly rasters
raw <- lorenzClim(
    var = 'etr',
    summary = 'raw',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz,
    yearByYear = FALSE
)

raw

# difference between corrected/uncorrected WDI rasters
correct <- lorenzClim(
    var = 'WDI',
    summary = 'summary',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz,
	correctWDI = TRUE,
    yearByYear = FALSE
)

incorrect <- lorenzClim(
    var = 'WDI',
    summary = 'summary',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz,
	correctWDI = FALSE,
    yearByYear = FALSE
)

plot(c(correct, incorrect, -1 * incorrect))

# EVERYTHING!!! Can take a few minutes, even for a 3-yr time span...
all <- lorenzClim(
    var = '*',
    summary = '*',
    start = 2005,
    end = 2007,
    rcp = '*',
    gcm = '*',
    lorenz = lorenz,
    yearByYear = FALSE,
	verbose = TRUE
)

all
```

# Citations #

Please cite:

Lorenz, D.J., Nieto-Lugilde, D., Blois, J.L., Fitzpatrick, M.C., and Williams, J.W.  2016.  Downscaled and debiased climate simulations for North America from 21,000 years ago to 2100AD.  *Scientific Data* 3:160048. DOI: <a href="https://dx.doi.org/10.1038/sdata.2016.48">10.1038/sdata.2016.48</a>

Smith, A. 2023. `lorenzClim`: Match future, historical, and paleoclimate variables from Lorenz et al. R package version 1.0.0 <https://github.com/adamlilith/lorenzClim>.
 