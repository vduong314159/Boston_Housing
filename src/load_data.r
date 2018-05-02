library(mlbench)

data(BostonHousing2)

# indexing dataframe using Census tract code
rownames(BostonHousing2) <- BostonHousing2$tract

# removing attributes in updated dataset BostonHousing2 and not in original BostonHousing.
# I removed tract because I made it my datasets row names above and do not want it as a feature.
BostonHousing2 <- subset(BostonHousing2, select = -c(cmedv, town, lon, lat, tract))