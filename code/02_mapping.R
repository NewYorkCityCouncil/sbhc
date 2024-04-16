source('code/00_load_dependencies.R')


#####
### Map Labels and chloropeth colors
#####
# Base layer - Total Enrollment
map_total_enrollment_popup <- paste('<b style="font-size: large">',"School District:",school_dist_shp$school_dis,"</b><br>",
                                    "<b>","Total Open Schools:</b>",prettyNum(school_dist_shp$num_open_schools, big.mark = ','),"<br>",
                                    "<b>","Total Enrolled Students:</b>",prettyNum(school_dist_shp$total_enrollment, big.mark = ','),"<br>",
                                    "<b>","Number of Open SBHC:</b>",prettyNum(school_dist_shp$num_sbhc, big.mark = ','),"<br>",
                                    "<b>","Number Schools SBHC Served:</b>",prettyNum(school_dist_shp$num_sbhc_schools_served, big.mark = ','),"<br>",
                                    "<b>","Number of Schools SMHC Served:</b>",prettyNum(school_dist_shp$num_schools_smhc_served, big.mark = ','),"<br>",
                                    "<hr>",
                                    "<b>","Percent of Students in Poverty:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_poverty),"<br>",
                                    "<b>","Percent of Students with Anaphylaxis:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_anaphylaxis),"<br>",
                                    "<b>","Percent of Students with Diabetes 1:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_diabetes1),"<br>",
                                    "<b>","Percent of Students with Diabetes 2:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_diabetes2),"<br>")

enrollment_cuts <- classIntervals(var = school_dist_shp$total_enrollment,n=3)
enrollment_pal = councildown::colorBin(
  palette = "bw",
  bins = enrollment_cuts$brks,
  domain = school_dist_shp$total_enrollment,
  na.color = "#FFFFFF"
)
                                    

# Poverty Layer
map_poverty_popup <- paste("<h2>","School District:",school_dist_shp$school_dis,"</h2>",
                    "<hr>",
                    "<b>","Total Enrolled Students:</b>",prettyNum(school_dist_shp$total_enrollment, big.mark = ','),"<br>",
                    "<b>","Number of Students in Poverty:</b>",prettyNum(school_dist_shp$number_poverty, big.mark = ','),"<br>",
                    "<b>","Percent of Students in Poverty:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_poverty),"<br>",
                    "<b>","Percent of Students with Asthma:</b>",label_percent(accuracy = 0.01)(school_dist_shp$percent_asthma),"<br>"
                    )

poverty_cuts <- classIntervals(var = school_dist_shp$percent_poverty,n=3)
poverty_pal = councildown::colorBin(
  palette = "bw",
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
  palette = "bw",
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
  palette = "bw",
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
  palette = "bw",
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
  palette = "bw",
  bins = diabetes2_cuts$brks,
  domain = school_dist_shp$percent_diabetes2,
  na.color = "#FFFFFF"
)

## Size based on total enrollment
#total_enrollment_cuts <- classIntervals(var = map_sf %>% filter(!is.na(total_enrolled)) %>% pull(total_enrolled),n=3)
#
#map_sf <- map_sf %>%
#  mutate(size_enrolled = cut(total_enrolled,
#                             labels = c("3","7","10"),
#                             breaks = total_enrollment_cuts$brks,
#                             include.lowest = T,
#                             right = F))
#map_sf$size_enrolled <- as.numeric(map_sf$size_enrolled)
#map_sf[which(is.na(map_sf$size_enrolled)),"size_enrolled"] <- 2

#####
### Marker Labels
#####

# Separate the dataset
# Schools w/o SBHC or SMHC
map_sf <- map_sf %>%
  mutate(marker_type = case_when(
    building_code %in% closed_sbhc | grepl("Temporarily Closed",campus_name) ~ "X",
    !is.na(services_provided) & !is.na(campus_name) ~"filledcircle",
    !is.na(services_provided) ~ "circle",
    !is.na(campus_name) ~ "fill",
    TRUE ~ "dot"
  ))

# Marker Popups
map_sf <- map_sf %>%
  mutate(marker_popups = case_when(
    is.na(campus_name) & is.na(services_provided) ~ paste0('<b style="font-size: large">',top_school_enrolled,"</b><br>",
                                                          '<a style="font-size: small">',proper(primary_address), ", ",
                                                          proper(city),'</a>',
                                                          "<br>", "Total Enrollment: ",prettyNum(total_enrolled, big.mark = ','),"<br>",
                                                          "<hr>",
                                                          "<b>","Schools:","</b><br>",
                                                          gsub("\\|","<br>",open_schools)),
    is.na(campus_name) & !is.na(services_provided) ~ paste0('<b style="font-size: large">',top_school_enrolled,"</b><br>",
                                                            '<a style="font-size: small">',proper(primary_address), ", ",
                                                            proper(city),'</a>',
                                                            "<br>", "Total Enrollment: ",prettyNum(total_enrolled, big.mark = ','),"<br>",
                                                            "<hr>",
                                                            "<b>","Schools:","</b><br>",
                                                            gsub("\\|","<br>",open_schools),"<br>",
                                                            '<b>',"Services Provided:","</b><br>",
                                                            gsub(",","<br>",services_provided)),
    TRUE ~ paste0('<b style="font-size: large">',campus_name,"</b><br>",
                                       '<a style="font-size: small">',proper(primary_address), ", ",
                                       proper(city),'</a>',
                                       "<br>","Total Enrollment: ",prettyNum(total_enrolled, big.mark = ','),"<br>",
                                       "<hr>",
                                       "<b>","Schools Served:","</b><br>",
                                       gsub("\\|","<br>",schools_served),"<br>",
                                       '<b>',"Services Provided:","</b><br>",
                                       gsub(",","<br>",services_provided))))

# set size legend features
colors <- c("black","transparent","cyan","pink")
labels <- c("Schools","MH Services Provided","SBHC","Closed SBHC")
sizes <- c(2.5,4,4,4)
shapes <- rep("circle",4)
borders <- c("black","blue","transparent","red")

# Find centers for school district labels
school_dist_shp <- school_dist_shp  %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

# Leaflet Map
m <- leaflet() %>%
  setView(-73.984865, 40.710542, zoom = 11) %>%
  # add school district labels
  leaflet::addLabelOnlyMarkers(data = school_dist_shp, 
                               lat = ~lat, lng = ~lon, label = ~school_dis, labelOptions = leaflet::labelOptions(permanent = TRUE, 
                                                                                                                    noHide = TRUE, textOnly = TRUE, textsize = 12, 
                                                                                                                    direction = "center", style = list(color = "#23417D", 
                                                                                                                                                       `font-family` = "'Open Sans', sans-serif", 
                                                                                                                                                       `font-weight` = "bold"))) %>%
  #addTiles() %>%
  # Overlay Groups
  # Default Base Layer - total enrollment
  addPolygons(data = school_dist_shp,
              fillColor = ~enrollment_pal(total_enrollment),
              fillOpacity = 0.5,
              color = "black",
              opacity = 1,
              weight = 3,
              popup = ~lapply(map_total_enrollment_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              popupOptions = popupOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "enrollment") %>%
  # poverty
  addPolygons(data = school_dist_shp,
              fillColor = ~poverty_pal(percent_poverty),
              fillOpacity = 0.5,
              color = "black",
              opacity = 1,
              weight = 3,
              popup = ~lapply(map_poverty_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              popupOptions = popupOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "poverty") %>%
  # anaphylaxis
  addPolygons(data = school_dist_shp,
              fillColor = ~allergy_pal(percent_anaphylaxis),
              fillOpacity = 0.5,
              color = "black",
              opacity = 1,
              weight = 3,
              popup = ~lapply(map_allergy_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              popupOptions = popupOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "anaphylaxis") %>%
  # asthma
  addPolygons(data = school_dist_shp,
              fillColor = ~asthma_pal(percent_asthma),
              fillOpacity = 0.5,
              color = "black",
              opacity = 1,
              weight = 3,
              popup = ~lapply(map_asthma_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              popupOptions = popupOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "asthma") %>%
  # diabetes 1
  addPolygons(data = school_dist_shp,
              fillColor = ~diabetes1_pal(percent_diabetes1),
              fillOpacity = 0.5,
              color = "black",
              opacity = 1,
              weight = 3,
              popup = ~lapply(map_diabetes1_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              popupOptions = popupOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "diabetes1") %>%
  # diabetes 2
  addPolygons(data = school_dist_shp,
              fillColor = ~diabetes2_pal(percent_diabetes2),
              fillOpacity = 0.8,
              color = "black",
              opacity = 1,
              weight = 3,
              popup = ~lapply(map_diabetes2_popup,HTML),
              highlight = highlightOptions(color = "green", weight = 4),
              popupOptions = popupOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              group = "diabetes2") %>%
  # Marker layers
  # Schools w/o SBHC or Mental Health Services
  addCircleMarkers(data = map_sf %>%
                     filter(marker_type == "dot"),
             lng = ~longitude,
             lat = ~latitude,
             radius = 2.5,
             label = ~top_school_enrolled,
             labelOptions = labelOptions(textsize = "12px"),
             color = 'black',
             fillOpacity = 1,
             stroke = FALSE,
             popup = ~lapply(marker_popups, HTML),
             group = "Schools w/o Services") %>%
  # Schools w/ Mental Health Services
  addCircleMarkers(data = map_sf %>%
                     filter(marker_type %in% c("circle","filledcircle")),
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = 4,
                   popup = ~lapply(marker_popups, HTML),
                   popupOptions = popupOptions(closeOnClick = TRUE),
                   label = ~top_school_enrolled,
                   labelOptions = labelOptions(textsize = "12px"),
                   color = "blue",
                   stroke = TRUE,
                   weight = 2,
                   opacity = 1,
                   fillColor = "cyan",
                   fill = ~ifelse(marker_type == "filledcircle", TRUE,FALSE),
                   fillOpacity = 1,
                   group = "Schools w/ Mental Health Services") %>%
  # Schools w/ SBHCs
  addCircleMarkers(data = map_sf %>%
                     filter(marker_type %in% c("fill","filledcircle")),
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = 4,
                   popup = ~lapply(marker_popups, HTML),
                   popupOptions = popupOptions(closeOnClick = TRUE),
                   label = ~campus_name,
                   labelOptions = labelOptions(textsize = "12px"),
                   color = "blue",
                   stroke = ~ifelse(marker_type == "filledcircle", TRUE,FALSE),
                   opacity = 1,
                   weight = 2,
                   fillColor = "cyan",
                   fillOpacity = 1,
                   group = "Schools w/ SBHCs") %>%
  # Closed SBHCs
  addCircleMarkers(data = map_sf %>%
              filter(marker_type %in% c("X")),
            lng = ~longitude,
            lat = ~latitude,
            #icon = makeIcon(iconUrl = "visuals/icons/x-mark.png", iconWidth = 15, iconHeight = 15),
            radius = 4,
            label = ~campus_name,
            labelOptions = labelOptions(textsize = "12px"),
            popup = ~lapply(marker_popups, HTML),
            popupOptions = popupOptions(closeOnClick = TRUE),
            color = "red",
            stroke = TRUE,
            opacity = 1,
            weight = 2,
            fillColor = "pink",
            fillOpacity = 1,
            group = "Schools w/ SBHCs") %>%
  
  # Layers control
  addLayersControl(
    baseGroups = c("enrollment","poverty","anaphylaxis","asthma","diabetes1","diabetes2"),
    overlayGroups = c("Schools w/o Services","Schools w/ Mental Health Services","Schools w/ SBHCs"),
    options = layersControlOptions(collapsed = TRUE),
    position = "bottomleft"
  ) %>%
  # Hide Markers Default
  hideGroup(c("Schools w/o Services","Schools w/ Mental Health Services")) %>%
  # Add legends
  addLegend(pal = enrollment_pal,
            values = school_dist_shp$total_enrollment,
            labFormat = labelFormat(digits = 0, big.mark = ","),
            title = "Total Enrollment in SD",
            layerId = "enrollment") %>%
  addLegend(pal = poverty_pal,
            values = school_dist_shp$percent_poverty,
            labFormat = labelFormat(suffix = "%",digits = 1,transform = function(x) 100 * x),
            title = "Percent SD in Poverty",
            layerId = "poverty") %>%
  addLegend(pal = allergy_pal,
            values = school_dist_shp$percent_anaphylaxis,
            labFormat = labelFormat(suffix = "%",digits = 1,transform = function(x) 100 * x),
            title = "Percent SD w/ Allergies",
            layerId = "anaphylaxis") %>%
  addLegend(pal = asthma_pal,
            values = school_dist_shp$percent_asthma,
            labFormat = labelFormat(suffix = "%",digits = 1,transform = function(x) 100 * x),
            title = "Percent SD w/ Asthma",
            layerId = "asthma") %>%
  addLegend(pal = diabetes1_pal,
            values = school_dist_shp$percent_diabetes1,
            labFormat = labelFormat(suffix = "%",digits = 1,transform = function(x) 100 * x),
            title = "Percent SD w/ Diabetes 1",
            layerId = "diabetes1") %>%
  addLegend(pal = diabetes2_pal,
            values = school_dist_shp$percent_diabetes2,
            labFormat = labelFormat(suffix = "%",digits = 1,transform = function(x) 100 * x),
            title = "Percent SD w/ Diabetes 2",
            layerId = "diabetes2") %>%
  addLegendCustom(colors, labels, sizes, shapes, borders, title = "Legend",position = "bottomright") %>%
  htmlwidgets::onRender("
    function(el, x) {
      var initialLegend = 'enrollment' // Set the initial legend to be displayed by layerId
      var myMap = this;
      for (var legend in myMap.controls._controlsById) {
        var el = myMap.controls.get(legend.toString())._container;
        if(legend.toString() === initialLegend) {
          el.style.display = 'block';
        } else {
          el.style.display = 'none';
        };
      };
    myMap.on('baselayerchange',
      function (layer) {
        for (var legend in myMap.controls._controlsById) {
          var el = myMap.controls.get(legend.toString())._container;
          if(legend.toString() === layer.name) {
            el.style.display = 'block';
          } else {
            el.style.display = 'none';
          };
        };
      });
    }")

mapshot(m,file="visuals/poverty_sbhc_map.png")

  #addLegendCustom(colors, labels, sizes, shapes, borders, title = "school size",position = "bottomright")
  htmlwidgets::onRender("
    function(el, x) {
      this.on('baselayerchange', function(e) {
        e.layer.bringToBack();
      })
    }
  ")
  
  
