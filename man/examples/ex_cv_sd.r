
### numeric
n <- rnorm(100)
cv(n)

# same as
sd(n) / mean(n)

### raster
library(terra)
r1 <- r2 <- r3 <- rast()
nc <- ncell(r1)
r1[] <- runif(nc)
r2[] <- runif(nc)
r3[] <- runif(nc)

r <- c(r1, r2, r3)
sd(r) # standard deviation
cv(r) # coefficient of variation

# same as:
app(r, sd) # standard deviation
app(r, sd) / mean(r) # coefficient of variation
