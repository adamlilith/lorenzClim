\dontrun{
# This example is behind `dontrun{}` because it requires the Lorenz
# rasters to be downloaded and saved to the user's hard drive in the
# appropriately-named folders.

# This is the path where the "/historical", "/rcp45" and "/rcp85" folders
# reside. Each of these needs to have subfolders with each GCM's output. Change
# this according to where you have downloaded and unzipped the files. This
# example is for a Windows system:
lorenz <- 'C:/Ecology/Data/Lorenz et al 2016'

library(terra)

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
    var = 'gdd0',
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

}
