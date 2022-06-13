####################################################
#' CMP 6455 - Advanced GIS Applications
#' Project Name - A GIS based Accessibility Analysis on Transit Equity in Salt Lake County
#' Project Team - Faria Afrin Zinia, Justice Prosper Tuffour, Pukar Bhandari
#' 
#' Submitted to - Andy Hong
####################################################

# ==================================================
# 1. Setting up the environment
# ==================================================

# -------------------------------------
# Load packages
# -------------------------------------
install.packages("tidyverse")
install.packages("tidycensus")
install.packages("sf")
install.packages("tmap")
install.packages("mapview")
install.packages("matrixStats")
install.packages("leaflet")
install.packages("spdep")
install.packages("tigris")
install.packages("rmapshaper")
install.packages("broom")
install.packages("car")
install.packages("stargazer")
install.packages("spatialreg")
install.packages("knitr")
install.packages("ggpubr")

# install.packages("remotes")
# remotes::install_github("GIScience/openrouteservice-r")
# remotes::install_github("tlorusso/traveltime")

require(tidyverse)
require(tidycensus)
require(sf)
require(tmap)
require(mapview)
require(matrixStats)
require(openrouteservice)
require(traveltime)
require(leaflet)
require(spdep)
library(tigris)
library(rmapshaper)
library(broom)
library(car)
library(spatialreg)
library(knitr)
library(stargazer)
library(ggpubr)

## Setting the API Key

# Register Census API Key
# census_api_key("[CENSUS API]", install = TRUE)

census_api_key("[CENSUS API]", install=TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")

# -------------------------------------
# 2. Read data
# -------------------------------------

# Set your main data folder

# For MacOS
# path = "/Users/(Username)/MCMP/Semester II/CMP 6455 Advanced GIS Application/Assignments/Term Project/Data"

# For Windows
path = "D:/MCMP/Semester II/CMP 6455 Advanced GIS Application/Assignments/Term Project/Data"

# Search for variables in Census 
v19 = load_variables(2019, "acs5", cache = TRUE)
v19 %>% View

# Get Salt Lake County Census Block Group data
slco_blockgrp = get_acs(
  year = 2019, 
  survey = "acs5", 
  geography = "block group", 
  state = "UT", 
  county = "Salt Lake",
  variables = c(
    total_pop = "B02001_001",    # Total population
    total_hh = "B25003_001",     # Total households
    
    total_age ="B01001_001",   # Total age
    age1 = "B01001_003",   # Under 5 years (Male)
    age2 = "B01001_004",   # 5-9 years (Male)
    age3 = "B01001_005",   # 10-14 years (Male)
    age4 = "B01001_006",   # 15-17 years (Male)
    age5 = "B01001_020",   # 65-66 years (Male)
    age6 = "B01001_021",   # 67-69 years (Male)
    age7 = "B01001_022",   # 70-74 years (Male)
    age8 = "B01001_023",   # 75-79 years (Male)
    age9 = "B01001_024",   # 80-84 years (Male)
    age10 = "B01001_025",   # Over 85 years (Male)
    age11 = "B01001_027",   # Under 5 years (Female)
    age12 = "B01001_028",   # 5-9 years (Female)
    age13 = "B01001_029",   # 10-14 years (Female)
    age14 = "B01001_030",   # 15-17 years (Female)
    age15 = "B01001_044",   # 65-66 years (Female)
    age16 = "B01001_045",   # 67-69 years (Female)
    age17 = "B01001_046",   # 70-74 years (Female)
    age18 = "B01001_047",   # 75-79 years (Female)
    age19 = "B01001_048",   # 80-84 years (Female)
    age20 = "B01001_049",   # Over 85 years (Female)
    
    total_income = "B19001_001",  # Total income
    Inc1 = "B19001_002",        # Less than $10,000
    Inc2 = "B19001_003",        # $10,000 to $14,999
    Inc3 = "B19001_004",        # $15,000 to $19,999
    Inc4 = "B19001_005",        # $20,000 to $24,999
    Inc5 = "B19001_006",        # $25,000 to $29,999
    Inc6 = "B19001_007",        # $30,000 to $34,999
    Inc7 = "B19001_008",        # $35,000 to $39,999
    Inc8 = "B19001_009",        # $40,000 to $44,999
    Inc9 = "B19001_010",        # $45,000 to $49,999
    Inc10 = "B19001_011",       # $50,000 to $59,999
    
    pop_white = "B02001_002",    # White alone
    pop_black = "B02001_003",    # Black or African American alone
    pop_ameind = "B02001_004",   # American Indian and Alaska Native alone
    pop_asian = "B02001_005",    # Asian alone
    pop_native = "B02001_006",   # Native Hawaiian and Other Pacific Islander alone
    pop_others = "B02001_007",   # Some other race alone
    pop_2races = "B02001_008",   # Two or more races
    
    pop_nonhisp = "B03003_002",  # Not Hispanic or Latino
    pop_hispanic = "B03003_003", # Hispanic or Latino
    
    own_novehicle = "B25044_003", #Owners with No Vehicle
    rent_novehicle = "B25044_010", #Renters with No Vehicle
    
    pop_educ1 = "B15003_002", #No Diploma (No School)
    pop_educ2 = "B15003_003", #No Diploma (Nursery)
    pop_educ3 = "B15003_004", #No Diploma (Kindergarten)
    pop_educ4 = "B15003_005", #No Diploma (1st Grade)
    pop_educ5 = "B15003_006", #No Diploma (2nd Grade)
    pop_educ6 = "B15003_007", #No Diploma (3rd Grade)
    pop_educ7 = "B15003_008", #No Diploma (4th Grade)
    pop_educ8 = "B15003_009", #No Diploma (5th Grade)
    pop_educ9 = "B15003_010", #No Diploma (6th Grade)
    pop_educ10 = "B15003_011", #No Diploma (7th Grade)
    pop_educ11 = "B15003_012", #No Diploma (8th Grade)
    pop_educ12 = "B15003_013", #No Diploma (9th Grade)
    pop_educ13 = "B15003_014", #No Diploma (10th Grade)
    pop_educ14 = "B15003_015", #No Diploma (11th Grade)
    pop_educ15 = "B15003_016", #No Diploma (12th Grade)   
   
    pop_employ = "B23025_004",   # Employed civilian labor force
    pop_unemp = "B23025_005",    # Unemployed civilian labor force
    
    hh_owner = "B25003_002",     # Owner occupied households
    hh_renter = "B25003_003"     # Renter occupied households
  ),
  output = "wide", 
  geometry = TRUE
) %>% st_transform(3566) # Re-project map to EPSG 3566

# Check
slco_blockgrp %>% glimpse
slco_blockgrp %>% st_geometry %>% plot
#slco_blockgrp %>% plot(max.plot = 130)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Equity Indicators
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Indicator 1 = Age (<18 and >65)
slco_blockgrp = slco_blockgrp %>% 
  mutate(
    p_Age = ((age1E+age2E+age3E+age4E+age5E+age6E+age7E+age8E+age9E+age10E+age11E+age12E+age13E+age14E+age15E+age16E+age17E+age18E+age19E+age20E)/total_ageE)*100   # Percentage 18>Age>65
  )

# Indicator 2 = HH Income (less than 80% of the Area media HH income)
# the Area Median household Income in Salt Lake County 2019 was 74,865 U.S. dollars
slco_blockgrp = slco_blockgrp %>% 
  mutate(
    p_Income = ((Inc1E+Inc2E+Inc3E+Inc4E+Inc5E+Inc6E+Inc7E+Inc8E+Inc9E+Inc10E)/total_incomeE)*100   # Percentage Income Below AMI
  )

# Indicator 3 = Race (Non-white population)
slco_blockgrp = slco_blockgrp %>% 
  mutate(
    p_Race = ((pop_blackE+pop_ameindE+pop_asianE+pop_nativeE+pop_othersE+pop_2racesE)/total_popE)*100   # Percentage Non-White Population
  )

# Indicator 4 = Ethnicity (Hispanic/Latino population)
slco_blockgrp = slco_blockgrp %>% 
  mutate(
    p_Ethnicity = ((pop_hispanicE)/total_popE)*100   # Percentage Hispanic Population
  )

# Indicator 5 = Employment (Unemployed)
slco_blockgrp = slco_blockgrp %>% 
  mutate(
    p_Employment = ((pop_unempE)/pop_employE+pop_unempE)*100   # Percentage Unemployed
  )

# Indicator 6 = Education (over 25 years of age without a Diploma)
slco_blockgrp = slco_blockgrp %>% 
  mutate(
    p_Education = ((pop_educ1E+pop_educ2E+pop_educ3E+pop_educ4E+pop_educ5E+pop_educ6E+pop_educ7E+pop_educ8E+pop_educ9E+pop_educ10E+pop_educ11E+pop_educ12E+pop_educ13E+pop_educ14E+pop_educ15E)/total_popE)*100   # Percentage Without Diploma
  )

# Indicator 7 = Vehicle ownership (no vehicles)
slco_blockgrp = slco_blockgrp %>% 
  mutate(
    p_VOwnership = ((own_novehicleE + rent_novehicleE)/total_hhE)*100   # Percentage No Vehicles
  )

# Indicator 8 = House Ownership (renters)
slco_blockgrp = slco_blockgrp %>% 
  mutate(
    p_HMOwnership = ((hh_renterE)/total_hhE)*100   # Percentage Renters
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load UTA TRAX Station shapefile
trax_stn = st_read(file.path(path,"LightRailStations_UTA_3566.shp"))
trax_line = st_read(file.path(path,"LightRail_UTA_3566.shp"))

# Register the API key
ors_api_key("[ORS API]")

# ------------------------------
# 3. Calculate the supply ratio
# ------------------------------

# TRAX Station network catchment using a 20-min foot-walking time

# Get TRAX Station coordinates based on NAD83 (epsg 3566)
trax_coords = trax_stn %>% 
  st_transform(3566) %>%
  st_coordinates()

# Create an empty data frame for the loop
# output = data_frame()
output = tibble()

# Loop through trax_stn
# 20-min walking travel time
# 20-minute range split into 5 minute intervals
# The unit is in seconds, so we multiply by 60.
for(i in 1:57){
  cat(paste("Calculating isochrone for TRAX Stations:", i,"\n"))
  res = ors_isochrones(
    trax_coords[i, ], 
    range = 20*60,
    # interval = 5*60,
    profile = "foot-walking",
    output = "sf")
  Sys.sleep(2) # Pause for 2 seconds to avoid IP blocking
  output = rbind(output, res)   # Add the output incrementally
}

# Let's see the result
output

output %>% st_geometry %>% plot

st_write(output %>% select(-center), file.path(path,"trax_iso.shp"))

trax_iso = st_read(file.path(path,"trax_iso.shp"))


# Add the original TRAX Station information back to the data
# And transform the CRS to 3566
trax_catchment = trax_iso %>%
  mutate(
    OBJECTID = trax_stn$OBJECTID,
    STATIONNAM = trax_stn$STATIONNAM,
    Boarding = trax_stn$Boarding,
    Alighting = trax_stn$Alighting,
    AvgRider = trax_stn$AvgRider,
    ServCapaCT = trax_stn$ServCapaCT
  ) %>% st_transform(3566)

# Create census blockgroup centroid
blockgrp_centroid = slco_blockgrp %>% st_centroid()

#Check CRS
trax_catchment %>% st_crs
blockgrp_centroid %>% st_crs

# Spatial join TRAX Station catchment and block group centroid
# Calculate a supply ratio (TRAX-to-population)
supply_ratio = 
  st_join(trax_catchment, blockgrp_centroid, left=F) %>%
  group_by(OBJECTID) %>%
  summarise(
    sum_pop = sum(total_popE, na.rm=T),
    supply_ratio1 = head(ServCapaCT, 1) / sum_pop,
    supply_ratio2 = head(AvgRider, 1) / sum_pop
  ) %>% st_drop_geometry()

# Check data
trax_catchment %>% st_geometry %>% plot
blockgrp_centroid %>% st_geometry %>% plot
supply_ratio

# Create TRAX-to-population TRAX Station point data
trax_to_pop = 
  left_join(trax_stn, supply_ratio, by="OBJECTID") %>%
  select(supply_ratio1, supply_ratio2)

# -----------------------------------
# 4. Calculate the demand ratio
# -----------------------------------

# Population network catchment a 15-min walking time
# It takes too long to calculate the isochrones for all
# 617 tract centroids. So I already computed the task.
# I commented out the procedures below for those interested
# in using this code for other projects.

# # Get blockgroup centroid coordinates based on NAD83 (epsg 3566)
blockgrp_coords = blockgrp_centroid %>%
  st_transform(3566) %>%
  st_coordinates()

st_write(blockgrp_centroid, file.path(path, "blockgrp_centroid.shp"))

###### ADDTIONAL MODULES TO CALCULATE ISOCHRONES ###################

# Loop through tracts
output1 = tibble()

for(i in 1:350){
  cat(paste("Calculating isochrone for blockgroups:", i,"\n"))
  res = ors_isochrones(
    blockgrp_coords[i, ],
    range = 20*60,
    # interval = 5*60,
    profile = "foot-walking",
    output = "sf")
  Sys.sleep(2) # Pause for 2 seconds to avoid IP blocking
  output1 = rbind(output1, res) # Add the output incrementally
}


# Loop through tracts
output2 = tibble()

for(i in 351:612){
  cat(paste("Calculating isochrone for blockgroups:", i,"\n"))
  res = ors_isochrones(
    blockgrp_coords[i, ],
    range = 20*60,
    # interval = 5*60,
    profile = "foot-walking",
    output = "sf")
  Sys.sleep(2) # Pause for 2 seconds to avoid IP blocking
  output2 = rbind(output2, res) # Add the output incrementally
}

output1
output2

output = rbind(output1, output2)

output %>% View

st_write(output %>% select(-center), file.path(path, "blockgrp_iso.shp"))

blockgrp_iso = st_read(file.path(path,"blockgrp_iso.shp"))

blockgrp_iso %>% st_geometry %>% plot

################################################################

# Add the original blockgroup information back to the data
# And transform the CRS to 3566
pop_catchment = blockgrp_iso %>%
  mutate(
    GEOID = blockgrp_centroid$GEOID,
    total_pop = blockgrp_centroid$total_pop
  ) %>% st_transform(3566)

# Map the isochrones
pop_catchment %>% st_geometry %>% plot

# Spatial join population catchment and trax_to_pop
supply_demand_ratio = st_join(pop_catchment, trax_to_pop) %>%
  group_by(GEOID) %>%
  summarise(
    access1 = sum(supply_ratio1, na.rm=T),
    access2 = sum(supply_ratio2, na.rm=T)
  ) %>% st_drop_geometry()

supply_demand_ratio

# Create accessibility blockgroup data
TSFCA_network = left_join(slco_blockgrp, supply_demand_ratio, by="GEOID")

# --------------------------------------
# 5. Map the 2SFCA using network buffers
# --------------------------------------

tm_shape(TSFCA_network, unit="mile") +
  tm_polygons(
    col = "access1", style = "jenks", palette = "Reds",
    border.alpha = 0.1, title = "2SFCA (network)"
  ) +
  tm_shape(trax_stn) + tm_dots() +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top")) +
  tm_layout(
    main.title = "UTA TRAX accessibility in Salt Lake County",
    main.title.size = 1, frame = FALSE,
    legend.position = c("right","bottom"))

st_write(TSFCA_network, file.path(path,"TSFCA_Network.shp"))



# --------------------------------------
# 6. Correlation Analysis
# --------------------------------------

# Correlation plot for accessibility and percentage of Non-White Population
race_plot = TSFCA_network %>% 
  ggplot(aes(x = p_Race, y = access1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Non white Population", 
    y = "Transit Accessibility"
  )

race_plot %>% plot

# Correlation plot for accessibility and percentage of Hispanic Population
ethnicity_plot = TSFCA_network %>% 
  ggplot(aes(x = p_Ethnicity, y = access1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Hispanic Population", 
    y = "Transit Accessibility"
  )

ethnicity_plot %>% plot

# Correlation plot for accessibility and percentage of Population Age group .....
age_plot = TSFCA_network %>% 
  ggplot(aes(x = p_Age, y = access1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Population within age group ", 
    y = "Transit Accessibility"
  )

age_plot %>% plot

# Correlation plot for accessibility and percentage of Unemployed Population
employment_plot = TSFCA_network %>% 
  ggplot(aes(x = p_Employment, y = access1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Unemployed Population", 
    y = "Transit Accessibility"
  )

employment_plot %>% plot

# Correlation plot for accessibility and percentage of Population aged above 25 and not having Diploma
education_plot = TSFCA_network %>% 
  ggplot(aes(x = p_Education, y = access1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Population aged above 25 and not having Diploma", 
    y = "Transit Accessibility"
  )

education_plot %>% plot

# Correlation plot for accessibility and percentage of renters
tenure_plot = TSFCA_network %>% 
  ggplot(aes(x = p_HMOwnership, y = access1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Renters", 
    y = "Transit Accessibility"
  )

tenure_plot %>% plot

# Correlation plot for accessibility and percentage of Household with income below 80% of AMHI
HHInc_plot = TSFCA_network %>% 
  ggplot(aes(x = p_Income, y = access1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Household with income below 80% of AMHI", 
    y = "Transit Accessibility"
  )

HHInc_plot %>% plot

# Correlation plot for accessibility and percentage of Households having no car
caronwership_plot = TSFCA_network %>% 
  ggplot(aes(x = p_VOwnership, y = access1)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001,
    label.y = 25,
    size = 5
  ) +
  labs(
    x = "% Households having no Car", 
    y = "Transit Accessibility"
  )

caronwership_plot %>% plot

# Plot the correlational map
# mfrow = c(rows, columns)
# mar = c(bottom, left, top, right) 
# par(mfrow=c(2, 4), mar=c(3, 2, 3, 2))

# race_plot %>% plot(main="Clustering of Pedestrian Crashes, 2018")
# ethnicity_plot %>% plot(main="Degree of Clustering (K-function)")
# age_plot %>% plot(main="Clustering of Bicycle Crashes, 2018")
# employment_plot %>% plot(main="Degree of Clustering (K-function)")
# education_plot %>% plot(main="Degree of Clustering (K-function)")
# tenure_plot %>% plot(main="Degree of Clustering (K-function)")
# HHInc_plot %>% plot(main="Degree of Clustering (K-function)")
# caronwership_plot %>% plot(main="Degree of Clustering (K-function)")

# --------------------------------------
# 7. Regression Analysis
# --------------------------------------

# Running simple linear regression model among Transit ACcessibity anf selected variables
Model1.simpleLR <- lm(access1 ~ p_Race + p_Ethnicity + p_Employment + p_Age
                      + p_Education + p_HMOwnership + p_Income + p_VOwnership, data = TSFCA_network)

summary(Model1.simpleLR)

# Diagnostic of LR Model 1

ggplot() + 
  geom_histogram(mapping = aes(x=resid(Model1.simpleLR))) +
  xlab("OLS residuals")

#QQ plot to see if the error terms are normally distributed
qqPlot(Model1.simpleLR)


# check assumption errors are not heteroskedastic - that is the variance of residuals are constant
plot(resid(Model1.simpleLR))

# --------------------------------------
# 7. Spatial Auto Correlation
# --------------------------------------

# # create the neighbor nb object for Queen contiguity
# slco_blockgrpb<-poly2nb(TSFCA_Network, queen=T)
# 
# # the listw weights object
# slco_blockgrpw<-nb2listw(slco_blockgrpb, style="W", zero.policy = TRUE)
# 
# # Examine the Moran scatterplot
# moran.plot(TSFCA_Network$access1, listw=slco_blockgrpw, 
#            xlab="Standardized TRAX Accessibility", ylab="Neighbors Standardized TRAX Accessibility",
#            main=c("Moran Scatterplot for TRAX Accessibility", "in Salt Lake County") )
# 
# # use monte carlo simulation to get the p-value
# moran.mc(TSFCA_Network$access1, slco_blockgrpw, nsim=999)
# 
# # Repeat for the OLS residuals using the lm.morantest() function.
# lm.morantest(Model1.simpleLR, slco_blockgrpw)

# -------------------------------------
# Contiguity-based association
# -------------------------------------

# Compute contiguity-based neighbors
queen = poly2nb(TSFCA_network, queen=T) # Queen Contiguity

blockgrp_centr = st_centroid(st_geometry(slco_blockgrp), of_largest_polygon = T)

TSFCA_network %>% st_geometry %>% plot(border="lightgrey")
plot(queen, blockgrp_centr, 
     pch = 19, cex = 0.6, col= "red", add=T)
title("Queen Contiguity")

  #==================================================
  # Measures of autocorrelation
  # ==================================================

# -------------------------------------
# Compute global Moran's I
# -------------------------------------

# To compute Moran's I, we need to obtain weights from the neighbors
# We can use any neighbors, but we will use the queen neighbor

# Compute queen contiguity
queen = poly2nb(TSFCA_network, queen=T) 

# Row-standardized weights matrix
weights = nb2listw(neighbours = queen, style = "W")  

# See the first row of the weights matrix
weights$weights

# Moran's I test
moran.test(TSFCA_network$access1, listw = weights)

# Moran's I test based on Monte-Carlo simulation
moran = moran.mc(TSFCA_network$access1, listw = weights, nsim = 999)

# See the result
moran

# Visualizing Moran's I 
hist(moran$res, freq = TRUE, 
     breaks = 20, xlab = "Simulated Moran's I")
abline(v=0, col="red", lwd=3)

# -------------------------------------
# Calculate Moranâs I Scatterplot
# -------------------------------------

# We first need to create a z-score (standardized score)
TSFCA_network$zscore = as.numeric(scale(TSFCA_network$access1))

# Moran's scatterplot
moran.plot(
  TSFCA_network$zscore, listw = weights, 
  pch = 19, cex = 0.6, col= "blue",
  main = "Moran's I Scatterplot",
  xlab = "Trax Accessibility (scaled)", 
  ylab = "Spatially lagged Trax Accessibility")

# ==================================================
# Local autocorrelation statistics
# ==================================================

# -------------------------------------
# Local Moran's I (LISA)
# -------------------------------------

# Let's recompute the weights matrix
queen = poly2nb(TSFCA_network, queen=T) 
weights = nb2listw(neighbours = queen, style = "W")  

# Compute Local Moran's I a.k.a. LISA
# LISA stands for local indicator of spatial association
# We will also compute both z-scores and lagged z-scores
# and make a new variable indicating whether the polygons 
# are clustered or not.

LISA = TSFCA_network %>%
  mutate(
    moran.p = localmoran(access1, listw = weights)[, 5],
    zscore = scale(access1),
    lagged = lag.listw(zscore, x = weights),
    LISA = ifelse(
      zscore >= 0 & lagged >= 0 & moran.p <= 0.05,
      "Cluster", "No cluster")
  )

# Map the clustering pattern of TRAX Accessibility
tm_shape(LISA) +
  tm_polygons("LISA", palette=c("red", "gray95"), title="TRAX Accessibility Cluster") +
  tm_scale_bar(position = c("right", "bottom")) +
  tm_compass(position = c("left", "top")) +
  tm_layout(
    main.title = "Clustering pattern of TRAX Accessibility in Salt Lake County",
    main.title.size = 1, frame = FALSE, legend.outside = F)

# mapview(LISA, zcol="LISA")

st_write(LISA, file.path(path, "LISA.shp"))


# --------------------------------------
# 8. Spatial lag model
# --------------------------------------


Model2.lag<-lagsarlm(access1 ~ p_Race + p_Ethnicity + p_Employment + p_Age
                     + p_Education + p_HMOwnership + p_Income + p_VOwnership, data = TSFCA_network,
                     listw = weights)

summary(Model2.lag)



# --------------------------------------
# 9. Spatial Error  model
# --------------------------------------
Model3.err<-errorsarlm(access1 ~ p_Race + p_Ethnicity + p_Employment + p_Age
                       + p_Education + p_HMOwnership + p_Income + p_VOwnership, data = TSFCA_network, 
                       listw = weights)

summary(Model3.err)


# --------------------------------------
# 10. Presenting Results
# --------------------------------------

summary(Model1.simpleLR)
Model1.simpleLR %>% 
  tidy() %>%
  kable(digits = 3)


summary(Model2.lag)
Model2.lag %>% 
  tidy() %>%
  kable(digits = 3)

summary(Model3.err)
Model3.err %>% 
  tidy() %>%
  kable(digits = 3)


# Combined table for three Model
stargazer(Model1.simpleLR, Model2.lag, Model3.err, type = "html",
          title="Title: Regression Results")

# --------------------------------------
# 11. Picking a model 
# --------------------------------------

AIC(Model1.simpleLR)
AIC(Model2.lag)
AIC(Model3.err)

#Save AIC values
AICs<-c(AIC(Model1.simpleLR),AIC(Model2.lag), AIC(Model3.err))
labels<-c("OLS", "SLM","SEM" )

kable(data.frame(Models=labels, AIC=round(AICs, 2)))

spatialreg::impacts(obj = SpatialLag.Model, listw = weights,zero.policy = TRUE)





#Save Workspace
save.image("D:/MCMP/Semester II/CMP 6455 Advanced GIS Application/Assignments/Term Project/6455_Transit Equity/6455_TransitEquity.RData")
#Save History
savehistory("D:/MCMP/Semester II/CMP 6455 Advanced GIS Application/Assignments/Term Project/6455_Transit Equity/6455_TransitEquity.Rhistory")








