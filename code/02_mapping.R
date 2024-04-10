source('code/00_load_dependencies.R')


#####
### Map Labels and chloropeth colors
#####
# Poverty Layer
map_poverty_popup <- paste("<h2>","School District:",school_dist_shp$school_dis,"</h2>",
                    "<hr>",
                    "<b>","Total Enrolled Students:</b>",prettyNum(school_dist_shp$total_enrollment, big.mark = ','),"<br>",
                    "<b>","Number of Students in Poverty:</b>",prettyNum(school_dist_shp$number_poverty, big.mark = ','),"<br>",
                    "<b>","Percent of Students in Poverty:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_poverty),"<br>")

poverty_cuts <- classIntervals(var = school_dist_shp$percent_poverty,n=3)
poverty_pal = councildown::colorBin(
  palette = "warm",
  bins = poverty_cuts$brks,
  domain = school_dist_shp$percent_poverty,
  na.color = "#FFFFFF"
)

# Anaphalytic Layer
map_allergy_popup <- paste("<h2>","School District:",school_dist_shp$school_dis,"</h2>",
                           "<hr>",
                           "<b>","Total Enrolled Students:</b>",prettyNum(school_dist_shp$total_enrollment, big.mark = ','),"<br>",
                           "<b>","Number of Students with Anaphylaxis:</b>",prettyNum(school_dist_shp$anaphylaxis, big.mark = ','),"<br>",
                           "<b>","Percent of Students with Anaphylaxis:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_anaphylaxis),"<br>")

allergy_cuts <- classIntervals(var = school_dist_shp$percent_anaphylaxis,n=3)
allergy_pal = councildown::colorBin(
  palette = "warm",
  bins = allergy_cuts$brks,
  domain = school_dist_shp$percent_anaphylaxis,
  na.color = "#FFFFFF"
)

# Asthma Layer
map_asthma_popup <- paste("<h2>","School District:",school_dist_shp$school_dis,"</h2>",
                           "<hr>",
                           "<b>","Total Enrolled Students:</b>",prettyNum(school_dist_shp$total_enrollment, big.mark = ','),"<br>",
                           "<b>","Number of Students with Asthma:</b>",prettyNum(school_dist_shp$asthma, big.mark = ','),"<br>",
                           "<b>","Percent of Students with Asthma:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_asthma),"<br>")

asthma_cuts <- classIntervals(var = school_dist_shp$percent_asthma,n=3)
asthma_pal = councildown::colorBin(
  palette = "warm",
  bins = asthma_cuts$brks,
  domain = school_dist_shp$percent_asthma,
  na.color = "#FFFFFF"
)

# Diabetes1 Layer
map_diabetes1_popup <- paste("<h2>","School District:",school_dist_shp$school_dis,"</h2>",
                          "<hr>",
                          "<b>","Total Enrolled Students:</b>",prettyNum(school_dist_shp$total_enrollment, big.mark = ','),"<br>",
                          "<b>","Number of Students with Diabetes 1:</b>",prettyNum(school_dist_shp$diabetes1, big.mark = ','),"<br>",
                          "<b>","Percent of Students with Diabetes 1:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_diabetes1),"<br>")

diabetes1_cuts <- classIntervals(var = school_dist_shp$percent_diabetes1,n=3)
diabetes1_pal = councildown::colorBin(
  palette = "warm",
  bins = diabetes1_cuts$brks,
  domain = school_dist_shp$percent_diabetes1,
  na.color = "#FFFFFF"
)

# Diabetes2 Layer
map_diabetes2_popup <- paste("<h2>","School District:",school_dist_shp$school_dis,"</h2>",
                             "<hr>",
                             "<b>","Total Enrolled Students:</b>",prettyNum(school_dist_shp$total_enrollment, big.mark = ','),"<br>",
                             "<b>","Number of Students with Diabetes 2:</b>",prettyNum(school_dist_shp$diabetes2, big.mark = ','),"<br>",
                             "<b>","Percent of Students with Diabetes 2:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_diabetes2),"<br>")

diabetes2_cuts <- classIntervals(var = school_dist_shp$percent_diabetes2,n=3)
diabetes2_pal = councildown::colorBin(
  palette = "warm",
  bins = diabetes2_cuts$brks,
  domain = school_dist_shp$percent_diabetes2,
  na.color = "#FFFFFF"
)

#####
### Marker Labels
#####

# Separate the dataset
# Schools w/o SBHC or SMHC
s_wo_hc_df <- map_df %>%
  filter()

# SBHC Popups
SBHC_labels <- paste("<h2>",map_sf$campus_name,"</h2>",
                    "<b>","Schools:","</b><br>",
                    gsub(",","<br>",map_sf$open_schools),
                    "<hr>",
                    "<b>","Schools Served:","</b><br>",
                    gsub(",","<br>",map_sf$schools_served),"<br>",
                    '<b style="font-size:14px;">',"Services Provided:","</b><br>",
                    gsub(",","<br>",map_sf$services_provided))



# Leaflet Map
leaflet() %>%
  #addTiles() %>%
  # Overlay Groups
  # Default Base Layer - poverty
  addPolygons(data = school_dist_shp,
              fillColor = ~poverty_pal(percent_poverty),
              fillOpacity = 1,
              color = "black",
              opacity = 1,
              weight = 3,
              label = ~lapply(map_poverty_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "poverty") %>%
  # anaphylaxis
  addPolygons(data = school_dist_shp,
              fillColor = ~allergy_pal(percent_anaphylaxis),
              fillOpacity = 1,
              color = "black",
              opacity = 1,
              weight = 3,
              label = ~lapply(map_allergy_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "anaphylaxis") %>%
  # asthma
  addPolygons(data = school_dist_shp,
              fillColor = ~asthma_pal(percent_asthma),
              fillOpacity = 1,
              color = "black",
              opacity = 1,
              weight = 3,
              label = ~lapply(map_asthma_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "asthma") %>%
  # diabetes 1
  addPolygons(data = school_dist_shp,
              fillColor = ~diabetes1_pal(percent_diabetes1),
              fillOpacity = 1,
              color = "black",
              opacity = 1,
              weight = 3,
              label = ~lapply(map_diabetes1_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "diabetes1") %>%
  # diabetes 2
  addPolygons(data = school_dist_shp,
              fillColor = ~diabetes2_pal(percent_diabetes2),
              fillOpacity = 1,
              color = "black",
              weight = 3,
              label = ~lapply(map_diabetes2_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "diabetes2") %>%
  # SBHC marker layers
  addCircleMarkers(data = map_sf %>%
                     filter(is.na(campus_name)),
             lng = ~longitude,
             lat = ~latitude,
             radius = 0.2,
             label = ~building_code,
             group = "Schools") %>%
  addCircleMarkers(data = map_sf %>%
                     filter(!is.na(campus_name)),
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = 5,
                   popup = ~lapply(SBHC_labels[which(!is.na(map_sf$campus_name))], HTML),
                   popupOptions = popupOptions(closeOnClick = TRUE),
                   label = ~campus_name,
                   color = "green",
                   group = "Schools with SBHCs") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("poverty","anaphylaxis","asthma","diabetes1","diabetes2"),
    overlayGroups = c("Schools","Schools with SBHCs"),
    options = layersControlOptions(collapsed = TRUE),
    position = "topright"
  )
