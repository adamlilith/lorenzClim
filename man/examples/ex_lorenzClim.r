\dontrun{
# This example is behind `dontrun{}` because it requires the Lorenz
# rasters to be downloaded and saved to the user's hard drive in the
# appropriately-named folders.

# This is the path where the "/historical", "/rcp45" and "/rcp85" folders
# reside. Each of these needs to have subfolders with each GCM's output. Change
# this according to where you have downloaded and unzipped the files. This
# example is for a Windows system:
lorenz <- 'C:/Ecology/Data/Lorenz et al 2016'

# one variable and one GCM
out <- lorenzClim(
    var = 'tmean',
    summary = '*',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz
)

plot(out)

# two variables and one GCM
out <- lorenzClim(
    var = c('tmean', 'prec'),
    summary = 'annual',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz
)

plot(out)

# two variables and two GCMs
out <- lorenzClim(
    var = c('tmean', 'prec'),
    summary = 'annual',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = c('ACCESS1-3', 'CanESM2'),
    lorenz = lorenz
)

out

# one variable, all GCMs, one summary
out <- lorenzClim(
    var = 'PET',
    summary = 'summary',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = '*',
    lorenz = lorenz
)

out

# "raw" monthly rasters
out <- lorenzClim(
    var = 'wind',
    summary = 'raw',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = 'ACCESS1-3',
    lorenz = lorenz
)

out

# EVERYTHING!!!
out <- lorenzClim(
    var = '*',
    summary = '*',
    start = 2005,
    end = 2007,
    rcp = 85,
    gcm = '*',
    lorenz = lorenz
)

out

}
