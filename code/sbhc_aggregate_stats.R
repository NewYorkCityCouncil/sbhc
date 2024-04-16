library(gtExtras)
library(plotly)
library(htmlwidgets)
library(janitor)
library(readxl)
library(kableExtra)
library(councildown)

map_sf <- read.csv("../sbhc/data/output/map_sf.csv")

# Read in SBHC data
sbhc <- read.csv("../sbhc/data/input/nyc_sbhc_23-24.csv") %>%
  clean_names() %>%
  mutate(bldg = trimws(bldg,which = "both"))

# Read in SBHC 2022 data
sbhc_22 <- read.csv("../sbhc/data/input/nyc_sbhc_22-23.csv") %>%
  clean_names() %>%
  mutate(clean_bldg = trimws(building_code_2023,which = "both"))

# Which ones are no longer in the 2023-2024 SBHC?
closed_sbhc <- sbhc_22$clean_bldg[which(is.element(sbhc_22$clean_bldg,unique(sbhc$bldg)) == FALSE)]

closed_sbhc_merge <- data.frame(building_code = closed_sbhc,
                                campus_name = c("[Closed] Fredrick Douglass Academy",
                                                "[Closed] Grand Street Campus",
                                                "[Closed] IS 145",
                                                "[Closed] IS 49 Campus",
                                                "[Closed] Norman Thomas Campus",
                                                "[Closed] PS 161 Pedro Albizu Campos",
                                                "[Closed] PS 197 Russwurm Campus",
                                                "[Closed] Springfield Gardens Campus",
                                                "[Closed] PS 192 Campus"),
                                sbhc_sponsor = c(rep("H+H Gotham",8), "Heritage Health Care Center"))

sbhc_data <- map_sf %>% 
  filter(!is.na(campus_name)) %>%
  filter(!building_code %in% closed_sbhc)

sbhc_data[sbhc_data$building_code=="X161","total_enrolled"] <- 323
sbhc_data[sbhc_data$building_code=="X098","total_enrolled"] <- 113
sbhc_data[sbhc_data$building_code=="M506","total_enrolled"] <- 444

# There are 18 sponsors - Montefiore medical center has the most sbhc and top 3 sponsors have 50.8% of all the sbhc
sbhc_data %>% 
  group_by(sbhc_sponsor) %>%
  summarize(count=n(), enrollment=sum(total_enrolled)) %>%
  arrange(desc(count)) %>%
  mutate(percent=round((count/sum(count))*100,0), percent=paste0(percent,"%")) %>%
  kbl(align="lrr", booktabs = TRUE, col.names = c("Sponsor", "# SBHC", "Percent")) %>%
  kable_material(c("striped", "hover")) %>% 
  save_kable("../sbhc/visuals/melissa/sponsor_table.html")

# Read in
sbhc_providers <- read_excel("../sbhc/data/input/Local\ Law\ 12\ School\ Year\ 2021-22.xlsx", sheet=2)

sbhc_providertype_2023 <- sbhc_providers %>% select(BLDG, Sponsor, SiteName, `Provider Type`) %>% right_join(sbhc_data, by=c("BLDG"="building_code"))
sbhc_providertype_2023$`Provider Type` <- ifelse(sbhc_providertype_2023$BLDG=="Q452","Federally Qualified Health Center",sbhc_providertype_2023$`Provider Type`)

# There are 3 provider types, majority of sbhc are federally qualified health centers
sbhc_providertype_2023 %>%
  group_by(`Provider Type`) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(percent=(count/sum(count))*100)

# 2023-24
bar_chart <- sbhc_providertype_2023 %>%
  group_by(`Provider Type`) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(percent=(count/sum(count))*100) %>%
  ggplot(aes(x=reorder(`Provider Type`,-count), y=percent, label=percent, fill = c("#2F56A6","grey","black"))) + 
  geom_bar(stat="identity") + 
  councildown::scale_color_nycc() + 
  scale_x_discrete(labels= scales::label_wrap(18)) +
  #coord_flip() +
  labs( 
    x = "", 
    y = "Percent", 
    title = "Percent of School Based Health Centers by Provider Type") +
  #geom_text(size = 4, aes(label = paste0(round(percent),"%"), vjust = -1, hjust=0.5))+ 
  scale_y_continuous(
    breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80),
    label = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%")
  ) +
  theme(legend.position="none", 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        #panel.grid.major.y = element_line(colour = "#E6E6E6"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "#666666"),
        axis.title.x = element_text(margin = 
                                      margin(t = 10, r = 0, b = 0, l = 0)),
        #        text = element_text(family = "Open Sans"),
        axis.text.y = element_text(size = 12, 
                                   margin = margin(t = 0, r = 10, b = 0, l = 0)),
        
        axis.text.x = element_text(size = 12, 
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.subtitle=element_text(size=12),
        plot.title = element_text(family = "Georgia",size = 14)) + 
  aes(text=map(paste(paste0(count, " (", round(percent),"%",")")), HTML))

ggplotly(bar_chart, tooltip = c("text")) %>% 
  saveWidget(file = "../sbhc/visuals/melissa/bar_chart.html") #htmltools::save_html("/Users/mel/Desktop/bar_chart.html")


over_time <- read_excel("../sbhc/data/input/sbhc_aggregate_data.xlsx")

# over time
over_time %>%
  ggplot(aes(x=year, y = num_sbhc)) + 
  geom_point() +
  geom_line() +
  scale_color_nycc()

library(sf)

council_dist_shp <- st_read(unzip_sf("https://data.cityofnewyork.us/api/geospatial/ve3w-z72j?method=export&format=Shapefile")) %>%
  st_transform(st_crs(4326))

sbhc_data %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = st_crs(4326)) %>%
  st_join(council_dist_shp) %>%
  group_by(coun_dist) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>%
  mutate(percent=round((count/sum(count))*100),coun_dist=as.character(coun_dist), percent=paste0(percent,"%")) %>% 
  select(coun_dist, count, percent) %>% 
  st_drop_geometry() %>%
  kbl(align="lrr", booktabs = TRUE, col.names = c("Council District", "# SBHC", "Percent")) %>%
  kable_material(c("striped", "hover")) %>%
  save_kable("/Users/mel/Desktop/cd_table.html")
#htmltools::save_html("/Users/mel/Desktop/cd_table.html")





