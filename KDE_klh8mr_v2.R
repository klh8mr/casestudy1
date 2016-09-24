# Kernel Density Estimation 
# Leigh Harton klh8mr
library(maptools)
library(readr)
library(rgdal)
library(ks)
library(RColorBrewer)

# read NYC boundary

# read.shapefile function
read.shapefile = function(path, sourceProj, targetProj)
{
  shapefile = readShapePoly(path)
  
  #reproject shapefile
  proj4string(shapefile) = sourceProj
  shapefile = spTransform(shapefile, CRS(targetProj))
  
  return (shapefile)
}

# read in NYC boundary using read.shapefile
city.boundary = read.shapefile("~/Documents/MSDS/SYS 6018/nybb_16c/nybb.shp", 
                               "+init=epsg:2263", "+init=epsg:32118")

# read Uber data
uber_raw_data <- read_csv("~/Documents/MSDS/SYS 6018/uber-raw-data-sep14.csv")

# reproject lon/lat points to meters
uber.locations.lonlat = cbind(uber_raw_data$Lon, uber_raw_data$Lat)
uber.locations.meters = project(uber.locations.lonlat, proj = "+init=epsg:32118")

# cut data down to x,y coordinates
uber_data = as.data.frame(cbind(uber.locations.meters[,1],uber.locations.meters[,2]))
names(uber_data) = c("x", "y")

# get estimation points for KDE -- a grid of evenly spaced points
resolution.meters = 200

# get.grid.points function
get.grid.points = function(shapefile, resolution.meters)
{
  shapefile.range = bbox(shapefile)
  x.values = seq(shapefile.range[1], shapefile.range[3], resolution.meters)
  y.values = seq(shapefile.range[2], shapefile.range[4], resolution.meters)
  grid.points = expand.grid(x.values, y.values)
  names(grid.points) = c("x", "y")
  return(grid.points)
}

# create kde estimation points
kde.est.points = get.grid.points(city.boundary, resolution.meters)

# run and plot KDE, using 500 points from the sample

# run.spatial.kde function
run.spatial.kde = function(sample.points, est.points, sample.size)
{
  sample.points = sample.points[sample(nrow(sample.points), size=sample.size),]
  # compute optimal KDE bandwidth
  h = Hpi(sample.points, pilot = "dscalar")
  # run KDE
  est = kde(sample.points, H=h , eval.points = est.points)$estimate
  return(est)
}

# caculate kde estimate using run.spatial.kde
kde.est = run.spatial.kde(uber_data, kde.est.points, 500)

# plot.spatial.kde function
plot.spatial.kde = function(kde.est, kde.est.points)
{
  image.x.values = sort(unique(kde.est.points[,1]))
  image.y.values = sort(unique(kde.est.points[,2]))
  image (x = image.x.values,
         y = image.y.values,
         z = matrix(kde.est, ncol=length(image.y.values),byrow = FALSE),
         col = colorRampPalette(rev(brewer.pal(11, 'Spectral'))) (32),
         xlab = "West-to-East (M)", ylab = "South-to-North (M)",
         asp = 1)
}

# plot the kde
plot.spatial.kde(kde.est, kde.est.points)

# add city boundary to KDE plot for interpretability
plot(city.boundary, add = TRUE)