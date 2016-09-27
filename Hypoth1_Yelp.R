source("YelpSource.R")

###################
### Hypothesis 1 ##
###################

# Urbana City Bounds
# "northeast" : {
#   "lat" : 40.1574203,
#   "lng" : -88.1530727
# },
# "southwest" : {
#   "lat" : 40.0732478,
#   "lng" : -88.23302029999999
# }

# Create list of coordinates that span Urbana
long <- c(-88.23302, -88.15307)
lat <- c(40.07324, 40.15742)
bounds = (cbind(long, lat))
bounds.meters = project(bounds, proj ="+init=epsg:26971")
long <- seq(bounds.meters[1,1], bounds.meters[2,1], length.out = 10777 ) # choose 10777 so there is an equal number of 
lat <- seq(bounds.meters[1,2], bounds.meters[2,2], length.out = 10777 ) # crime and non crime points
urbana_full <- cbind(0, long, lat)
urbana_full <- as.data.frame(urbana_full)
names(urbana_full) <- c("response", "longmeters", "latmeters")

## TRAIN MODEL ON RESPONSES FROM 2014, USING PREDICTORS FROM 2013 ##

# Training Set for 2014 (Non-Crime coordinates and Yelp Data)
# remove crime coordinates from non-crime coordinates
urbana_full14 <- urbana_full[!(urbana_full$longmeters %in% crime_data14$longmeters),]
# bind the non-crime coordinates with the crime coordinates
train_crime_data14 = crime_data14[37:39] # there are 10777 crime points
train14 <- rbind(urbana_full14, train_crime_data14)

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
# calculate crime density based on 2013
crime_density = run.spatial.kde(crime_data13[,c("longmeters", "latmeters")], train14[,c("longmeters", "latmeters")], 1000)

# remove lines with "NA" in attributes parking garage
yelp_parking_garage14 <- yelp_data14[!(is.na(yelp_data14$attributes.Parking.garage)),]
# find businesses with a parking garage
yelp_parking_garage14 <- yelp_parking_garage14[yelp_parking_garage14$attributes.Parking.garage == TRUE,]

# remove lines with "NA" in attributes parking street
yelp_parking_street14 <- yelp_data14[!(is.na(yelp_data14$attributes.Parking.street)),]
# find businesses with a street parking
yelp_parking_street14 <- yelp_parking_street[yelp_parking_street14$attributes.Parking.street == TRUE,]

# remove lines with "NA" in attributes parking validated
yelp_parking_validated14 <- yelp_data14[!(is.na(yelp_data14$attributes.Parking.validated)),]
# find businesses with validated parking
yelp_parking_validated14 <- yelp_parking_validated[yelp_parking_validated14$attributes.Parking.validated == TRUE,]

# remove lines with "NA" in attributes parking lot
yelp_parking_lot14 <- yelp_data14[!(is.na(yelp_data14$attributes.Parking.lot)),]
# find businesses with parking lot
yelp_parking_lot14 <- yelp_parking_lot[yelp_parking_lot14$attributes.Parking.lot == TRUE,]

# remove lines with "NA" in attributes parking valet
yelp_parking_valet14 <- yelp_data14[!(is.na(yelp_data14$attributes.Parking.valet)),]
yelp_parking_valet14 <- yelp_parking_valet[yelp_parking_valet14$attributes.Parking.valet == TRUE,]

# there are no businesses with valet parking or who validate parking- remove those from hypothesis

# functions for min distance
get.euc.distance = function(point.1, point.2)
{
  return (sqrt(sum((point.1 - point.2)^2)))
}

get.min.distances = function(points.1, points.2)
{
  return (apply(points.1, 
                1,
                function(point.1)
                {
                  distances = apply(points.2,
                                    1,
                                    function(point.2, point.1)
                                    {
                                      return (get.euc.distance(point.1, point.2))
                                    },
                                    point.1)
                  
                  return (min(distances))
                }))
}

# calculate distance to nearest business that has a parking garage from each training point
garage.min.dist = get.min.distances(train14[,c("longmeters","latmeters")], yelp_parking_garage14[,c("longmeters","latmeters")])
# calculate distance to nearest business that has street parking from each training point
street.min.dist = get.min.distances(train14[,c("longmeters","latmeters")], yelp_parking_street14[,c("longmeters","latmeters")])
# calculate distance to nearest business that has a parking lot from each training point
lot.min.dist = get.min.distances(train14[,c("longmeters","latmeters")], yelp_parking_lot14[,c("longmeters","latmeters")])

# add predictor columns (density and distance to businesses with different parking attributes) to training data
train14 = cbind(train14, crime_density, garage.min.dist, street.min.dist, lot.min.dist)

# fit GLM - all variables
glm.fit.all = glm(response ~ . -longmeters -latmeters, data = train14, family = binomial)
## Summary ##
# Coefficients:
#                    Estimate   Std. Error z value Pr(>|z|)    
#   (Intercept)     -1.891e+00  7.989e-02 -23.668  < 2e-16 ***
#   crime_density    4.868e+07  8.689e+05  56.025  < 2e-16 ***
#   garage.min.dist  1.030e-03  7.776e-05  13.245  < 2e-16 ***
#   street.min.dist -6.924e-04  8.675e-05  -7.982 1.45e-15 ***
#   lot.min.dist    -1.482e-03  5.693e-05 -26.038  < 2e-16 ***

# all Z-values are high, so let's look at multicollinearity 
vif(glm.fit.all)

# remove the variable with the highest VIF (street.min.dist) and repeat
# fit GLM - all variables - street.min.distance
glm.fit.garagelotdensity = glm(response ~ . -longmeters -latmeters -street.min.dist, data = train14, family=binomial)
## Summary ##
# Coefficients:
#                    Estimate   Std. Error z value Pr(>|z|)    
#   (Intercept)     -1.551e+00  6.653e-02  -23.32   <2e-16 ***
#   crime_density    4.728e+07  8.327e+05   56.78   <2e-16 ***
#   garage.min.dist  4.453e-04  2.364e-05   18.84   <2e-16 ***
#   lot.min.dist    -1.676e-03  5.262e-05  -31.85   <2e-16 ***

# all Z-values are high, so let's look at multicollinearity 
vif(glm.fit.garagelotdensity)

# there's no mulicollinearity, but there are only 4 businesses with a garage in 2014
# so let's take out garage.min.dist and compare
# fit GLM - density and lot
glm.fit.lotdensity = glm(response ~ crime_density + lot.min.dist, data = train14, family = binomial)
# Summary ##
# Coefficients:
#                  Estimate   Std. Error z value Pr(>|z|)    
#   (Intercept)   -9.576e-01  5.728e-02  -16.72   <2e-16 ***
#   crime_density  4.104e+07  6.971e+05   58.87   <2e-16 ***
#   lot.min.dist  -1.118e-03  4.259e-05  -26.24   <2e-16 ***

## PREDICT RESPONSES FOR 2015, USING MODEL FROM 2014 ##

# build dataframe to predict, based on 2014 data
# Create list of coordinates that span Urbana
long <- c(-88.23302, -88.15307)
lat <- c(40.07324, 40.15742)
bounds = (cbind(long, lat))
bounds.meters = project(bounds, proj ="+init=epsg:26971")
long <- seq(bounds.meters[1,1], bounds.meters[2,1], length.out = 9501 ) # 9501 so there are equal number of crime
lat <- seq(bounds.meters[1,2], bounds.meters[2,2], length.out = 9501 ) # and non crime points
prediction.points <- cbind(long, lat)
prediction.points <- as.data.frame(prediction.points)
names(prediction.points) <- c("longmeters", "latmeters")
add.prediction.points <- crime_data15[38:39] # add point where we know crime happened - 9501 pts
prediction.points <- rbind(prediction.points, add.prediction.points)

# calculate crime density based on 2014
crime_density = run.spatial.kde(crime_data14[,c("longmeters", "latmeters")], prediction.points, 1000)

# remove lines with "NA" in attributes parking garage
yelp_parking_garage15 <- yelp_data15[!(is.na(yelp_data15$attributes.Parking.garage)),]
# find businesses with a parking garage
yelp_parking_garage15 <- yelp_parking_garage15[yelp_parking_garage15$attributes.Parking.garage == TRUE,]

# remove lines with "NA" in attributes parking street
yelp_parking_street15 <- yelp_data15[!(is.na(yelp_data15$attributes.Parking.street)),]
# find businesses with a street parking
yelp_parking_street15 <- yelp_parking_street15[yelp_parking_street15$attributes.Parking.street == TRUE,]

# remove lines with "NA" in attributes parking lot
yelp_parking_lot15 <- yelp_data15[!(is.na(yelp_data15$attributes.Parking.lot)),]
# find businesses with parking lot
yelp_parking_lot15 <- yelp_parking_lot15[yelp_parking_lot15$attributes.Parking.lot == TRUE,]

# calculate distance to nearest business that has a parking garage from each prediction point
garage.min.dist = get.min.distances(prediction.points, yelp_parking_garage15[,c("longmeters","latmeters")])
# calculate distance to nearest business that has street parking from each prediction point
street.min.dist = get.min.distances(prediction.points, yelp_parking_street15[,c("longmeters","latmeters")])
# calculate distance to nearest business that has a parking lot from each prediction point
lot.min.dist = get.min.distances(prediction.points, yelp_parking_lot15[,c("longmeters","latmeters")])

# add predictor columns (density and distance to businesses with different parking attributes) to prediction data
predict15 = as.data.frame(cbind(prediction.points, crime_density, garage.min.dist, street.min.dist, lot.min.dist))

# run prediction - on garagelotdensity
threats.garagelotdensity = predict(glm.fit.garagelotdensity, predict15, type = "response")
# run prediction - on lotdensity
threats.lotdensity = predict(glm.fit.lotdensity, predict15, type = "response")

# build prediction dataframe for evaluation- garagelotdensity
crime_predict15_garagelotdensity = cbind(prediction.points, threats.garagelotdensity, 1)
names(crime_predict15_garagelotdensity) = c("longmeters", "latmeters", "threat", "true")
crime_predict15_garagelotdensity[1:9501,4] <- 0
# build prediction dataframe for evaluation- lotdensity
crime_predict15_lotdensity = cbind(prediction.points, threats.lotdensity, 1)
names(crime_predict15_lotdensity) = c("longmeters", "latmeters", "threat", "true")
crime_predict15_lotdensity[1:9501,4] <- 0

# ROC Curve - garagelotdensity
pred.gld <- prediction(crime_predict15_garagelotdensity$threat, crime_predict15_garagelotdensity$true)
pref.gld <- performance(pred.gld, "tpr", "fpr")
plot(pref.gld, col = 'indianred4')
abline(a=0, b=1)
# ROC Curve - lotdensity
pred.ld <- prediction(crime_predict15_lotdensity$threat, crime_predict15_lotdensity$true)
pref.ld <- performance(pred.ld, "tpr", "fpr")
plot(pref.ld, col = 'lightsteelblue3', add = TRUE)

# the ROC curves look preform pretty well - let's compare the AUC
# AUC - garagelotdensity
auc.gld <- performance(pred.gld, measure = "auc")
auc.gld <- auc.gld@y.values[[1]]
auc.gld
# AUC - lotdensity
auc.ld <- performance(pred.ld, measure = "auc")
auc.ld <- auc.ld@y.values[[1]]
auc.ld
