library(leaflet)
library(rgdal)
library(tidyverse)
schoolDistricts <- rgdal::readOGR("School Districts.geojson")

school_dist = schoolDistricts@data[["school_dist"]]

leaflet() %>% addTiles() %>% 
  addPolygons(data=schoolDistricts,stroke = T, label = school_dist)
                
                

pal <- colorNumeric("viridis", NULL)

leaflet(schoolDistricts) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(pop)))
              #label = ~paste0(county, ": ", formatC(pop, big.mark = ","))) %>%
  #addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
   #         labFormat = labelFormat(transform = function(x) round(10^x)))

  
district_testscore <- read.csv("district_testscores.csv")
district_testscore$District = as.factor(district_testscore$District)

school_testscores <- read.csv("school_testscores.csv")
school_testscores$School.Name = as.factor(school_testscores$School.Name)

ggplot(data=school_testscores)+geom_histogram(aes(x=Avg_Test_Score))
