#title: promise zone annual crime count
#name: nissim lebovits
#last edit: 6/1/2022

#explanation: This script sums annual crime counts in the promise zone by category. 
#These categories are based on UCR reporting, explained briefly here: https://www.cityofroseville.com/DocumentCenter/View/26568/Description-of-Uniform-Crime-Offenses#:~:text=Part%20I%20Offenses%20are%20generally%20referred%20to%20as,I%20Offenses%20include%20murder%2C%20rape%2C%20aggravated%20assault%2C%20robbery%2C
#Our own data reporting does not count crims in a way that matches these definitions exactly,
#so I have opted to simply report the UCR categories of crimes in this spreadsheet,
#with the exception of summary columns for pt 1, pt 2, and assault crimes. 
#The easiest way to summarize these is to simply open the .csv file in Excel and do it manually.

#####steps
#step 1: import data
#step 2: filter for in pz
#step 3: group + count by type
#step 4: group into summary dataframe to combine all years
#step 5: add columns for pt 1, pt 2, all assaults
#step 6: write csv

#################SETUP############################

library(tidyverse)
library(data.table) #for dcast
library(sf)
library(mapview)

options(tigris_use_cache = TRUE)

setwd("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/Public Safety")
##################IMPORT DATA##############

#promise zone shapefile
pz = st_read("C:/Users/Nissim.Lebovits/OneDrive - City of Philadelphia/Desktop/Data/R Scripts and Datasets/General Boundaries/Shapefiles/PZ_Shapefile",
             "PZ_Boundaries") |>
  st_transform(crs = st_crs("EPSG:4326"))


#all crime data downloaded as csv files from opendata philly: https://www.opendataphilly.org/dataset?q=crime+incidents&sort=score+desc%2C+metadata_modified+desc
#might be nice to figure out how to use the sql api to link to R automatically--that's for a future vista, though.


#################IMPORT + CLEAN CRIME DATA###########
#2021-----

#2021 crime; convert to sf; make sure all CRS matches
crime_2021 = read.csv("./Crime/PhilaCrime2021.csv") |>
                filter(!is.na(lat),
                       !is.na(lng)) |>
                st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

#filter for in pz
pz_crime_2021 = crime_2021[pz, ]

#tally
pz_crime_tallies_2021 = pz_crime_2021 |>
                          as.data.frame() |> #convert to nonspatial df
                          dplyr::select(-geometry) #remove geom column
                          
#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2021$text_general_code =  str_trim(pz_crime_tallies_2021$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2021 = pz_crime_tallies_2021 |>
                          group_by(text_general_code) |>
                          tally()

pz_crime_tallies_2021$text_general_code = pz_crime_tallies_2021$text_general_code |>
                                            tolower() |>
                                            str_remove_all("-") |>
                                            str_remove_all("\\(") |>
                                            str_remove_all("\\)") |>
                                            str_remove_all("\\/") |>
                                            str_replace_all(" ", "_") |>            
                                            str_replace_all("__", "_")

pz_crime_tallies = as.data.frame(t(pz_crime_tallies_2021)) |>
                      dplyr::slice(-1)

colnames(pz_crime_tallies) = pz_crime_tallies_2021$text_general_code
rownames(pz_crime_tallies) = "2021"


#2020-----

#same for 2020  
crime_2020 = read.csv("./Crime/PhilaCrime2020.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2020 = crime_2020[pz, ]

#tally
pz_crime_tallies_2020 = pz_crime_2020 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2020$text_general_code =  str_trim(pz_crime_tallies_2020$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2020 = pz_crime_tallies_2020 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2020$text_general_code = pz_crime_tallies_2020$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2020_forbind = as.data.frame(t(pz_crime_tallies_2020)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2020_forbind) = pz_crime_tallies_2020$text_general_code
rownames(pz_crime_tallies_2020_forbind) = "2020"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2020_forbind)  


#2019-----

#2019
crime_2019 = read.csv("./Crime/PhilaCrime2019.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2019 = crime_2019[pz, ]

#tally
pz_crime_tallies_2019 = pz_crime_2019 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2019$text_general_code =  str_trim(pz_crime_tallies_2019$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2019 = pz_crime_tallies_2019 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2019$text_general_code = pz_crime_tallies_2019$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2019_forbind = as.data.frame(t(pz_crime_tallies_2019)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2019_forbind) = pz_crime_tallies_2019$text_general_code
rownames(pz_crime_tallies_2019_forbind) = "2019"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2019_forbind)  


#2018----

#2018  
crime_2018 = read.csv("./Crime/PhilaCrime2018.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2018 = crime_2018[pz, ]

#tally
pz_crime_tallies_2018 = pz_crime_2018 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2018$text_general_code =  str_trim(pz_crime_tallies_2018$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2018 = pz_crime_tallies_2018 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2018$text_general_code = pz_crime_tallies_2018$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2018_forbind = as.data.frame(t(pz_crime_tallies_2018)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2018_forbind) = pz_crime_tallies_2018$text_general_code
rownames(pz_crime_tallies_2018_forbind) = "2018"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2018_forbind)



#2017-----

#2017  
crime_2017 = read.csv("./Crime/PhilaCrime2017.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2017 = crime_2017[pz, ]

#tally
pz_crime_tallies_2017 = pz_crime_2017 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2017$text_general_code =  str_trim(pz_crime_tallies_2017$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2017 = pz_crime_tallies_2017 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2017$text_general_code = pz_crime_tallies_2017$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2017_forbind = as.data.frame(t(pz_crime_tallies_2017)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2017_forbind) = pz_crime_tallies_2017$text_general_code
rownames(pz_crime_tallies_2017_forbind) = "2017"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2017_forbind)


#2016-----

#2016  
crime_2016 = read.csv("./Crime/PhilaCrime2016.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2016 = crime_2016[pz, ]

#tally
pz_crime_tallies_2016 = pz_crime_2016 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2016$text_general_code =  str_trim(pz_crime_tallies_2016$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2016 = pz_crime_tallies_2016 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2016$text_general_code = pz_crime_tallies_2016$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2016_forbind = as.data.frame(t(pz_crime_tallies_2016)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2016_forbind) = pz_crime_tallies_2016$text_general_code
rownames(pz_crime_tallies_2016_forbind) = "2016"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2016_forbind)


#2015-----

#2015  
crime_2015 = read.csv("./Crime/PhilaCrime2015.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2015 = crime_2015[pz, ]

#tally
pz_crime_tallies_2015 = pz_crime_2015 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2015$text_general_code =  str_trim(pz_crime_tallies_2015$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2015 = pz_crime_tallies_2015 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2015$text_general_code = pz_crime_tallies_2015$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2015_forbind = as.data.frame(t(pz_crime_tallies_2015)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2015_forbind) = pz_crime_tallies_2015$text_general_code
rownames(pz_crime_tallies_2015_forbind) = "2015"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2015_forbind)


#2014-----

#2014  
crime_2014 = read.csv("./Crime/PhilaCrime2014.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2014 = crime_2014[pz, ]

#tally
pz_crime_tallies_2014 = pz_crime_2014 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2014$text_general_code =  str_trim(pz_crime_tallies_2014$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2014 = pz_crime_tallies_2014 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2014$text_general_code = pz_crime_tallies_2014$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2014_forbind = as.data.frame(t(pz_crime_tallies_2014)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2014_forbind) = pz_crime_tallies_2014$text_general_code
rownames(pz_crime_tallies_2014_forbind) = "2014"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2014_forbind)


#2013-----

#2013   
crime_2013 = read.csv("./Crime/PhilaCrime2013.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2013 = crime_2013[pz, ]

#tally
pz_crime_tallies_2013 = pz_crime_2013 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2013$text_general_code =  str_trim(pz_crime_tallies_2013$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2013 = pz_crime_tallies_2013 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2013$text_general_code = pz_crime_tallies_2013$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2013_forbind = as.data.frame(t(pz_crime_tallies_2013)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2013_forbind) = pz_crime_tallies_2013$text_general_code
rownames(pz_crime_tallies_2013_forbind) = "2013"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2013_forbind)


#2012-----

#2012  
crime_2012 = read.csv("./Crime/PhilaCrime2012.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2012 = crime_2012[pz, ]

#tally
pz_crime_tallies_2012 = pz_crime_2012 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2012$text_general_code =  str_trim(pz_crime_tallies_2012$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2012 = pz_crime_tallies_2012 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2012$text_general_code = pz_crime_tallies_2012$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2012_forbind = as.data.frame(t(pz_crime_tallies_2012)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2012_forbind) = pz_crime_tallies_2012$text_general_code
rownames(pz_crime_tallies_2012_forbind) = "2012"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2012_forbind)
  

#2011-----

#2011 
crime_2011 = read.csv("./Crime/PhilaCrime2011.csv") |>
  filter(!is.na(lat),
         !is.na(lng)) |>
  st_as_sf(coords = c("lng", "lat"), crs = "EPSG:4326")

pz_crime_2011 = crime_2011[pz, ]

#tally
pz_crime_tallies_2011 = pz_crime_2011 |>
  as.data.frame() |> #convert to nonspatial df
  dplyr::select(-geometry) #remove geom column

#need to remove whitespace here bc of a typo in the data
pz_crime_tallies_2011$text_general_code =  str_trim(pz_crime_tallies_2011$text_general_code, side = "both")

#now that it's clean, group + tally
pz_crime_tallies_2011 = pz_crime_tallies_2011 |>
  group_by(text_general_code) |>
  tally()

pz_crime_tallies_2011$text_general_code = pz_crime_tallies_2011$text_general_code |>
  tolower() |>
  str_remove_all("-") |>
  str_remove_all("\\(") |>
  str_remove_all("\\)") |>
  str_remove_all("\\/") |>
  str_replace_all(" ", "_") |>            
  str_replace_all("__", "_")

pz_crime_tallies_2011_forbind = as.data.frame(t(pz_crime_tallies_2011)) |>
  dplyr::slice(-1)

colnames(pz_crime_tallies_2011_forbind) = pz_crime_tallies_2011$text_general_code
rownames(pz_crime_tallies_2011_forbind) = "2011"

#bind_rows to full tally df
#this function can handle unequal numbers of columns
pz_crime_tallies = bind_rows(pz_crime_tallies, pz_crime_tallies_2011_forbind)

#definitions of pt 1 and pt 2 crime here: https://www.cityofroseville.com/DocumentCenter/View/26568/Description-of-Uniform-Crime-Offenses#:~:text=Part%20I%20Offenses%20are%20generally%20referred%20to%20as,I%20Offenses%20include%20murder%2C%20rape%2C%20aggravated%20assault%2C%20robbery%2C

pz_crime_tallies[is.na(pz_crime_tallies)] = 0

pz_crime_tallies[] <- lapply(pz_crime_tallies, as.numeric)

pz_crime_tallies = pz_crime_tallies |>
                      mutate(pt_1_total = homicide_criminal +
                                          rape +
                                          robbery_firearm +
                                          robbery_no_firearm +
                                          aggravated_assault_firearm + 
                                          aggravated_assault_no_firearm + 
                                          burglary_residential +
                                          burglary_nonresidential +
                                          thefts +
                                          motor_vehicle_theft +
                                          arson,
                             pt_2_total = other_assaults + 
                                          forgery_and_counterfeiting + 
                                          fraud +
                                          embezzlement +
                                          receiving_stolen_property +
                                          vandalismcriminal_mischief +
                                          weapon_violations +
                                          prostitution_and_commercialized_vice +
                                          narcotic_drug_law_violations +
                                          offenses_against_family_and_children +
                                          driving_under_the_influence +
                                          liquor_law_violations +
                                          disorderly_conduct +
                                          vagrancyloitering,
                             assaults_total = aggravated_assault_firearm + 
                                               aggravated_assault_no_firearm + 
                                               other_assaults)


#######################WRITE CSV###############################
write.csv(pz_crime_tallies,
          file = "pz_annual_crime_tallies.csv")