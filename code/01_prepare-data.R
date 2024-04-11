source("code/00_load_dependencies.R")

# Read in SBHC data
sbhc <- read.csv("data/input/nyc_sbhc_23-24.csv") %>%
  clean_names() %>%
  mutate(bldg = trimws(bldg,which = "both"))

# Read in SBHC 2022 data
sbhc_22 <- read.csv("data/input/nyc_sbhc_22-23.csv") %>%
  clean_names() %>%
  mutate(clean_bldg = trimws(building_code_2023,which = "both"))

# Which ones are no longer in the 2023-2024 SBHC?
closed_sbhc <- sbhc_22$clean_bldg[which(is.element(sbhc_22$clean_bldg,unique(sbhc$bldg)) == FALSE)]

# Read in Mental Health HC data
#smhc <- read.socrata("https://data.cityofnewyork.us/resource/qxbt-vysj.json?$limit=9999999") %>%
#  clean_names()

smhc <- read_excel("data/input/mental-health-services-landscape-english.xlsx",sheet = "SMH Service Coverage") %>%
  clean_names()

# Merge by building numbers
merge_df <- smhc %>%
  group_by(building_code) %>%
  # Collapse all the unique services provided in the building code
  mutate(services_provided = paste(sort(unique(service)),collapse = ", ")) %>%
  filter(row_number() == 1) %>%
  select(building_code,services_provided)

#sbhc_merge <- sbhc %>%
#  left_join(merge_df, by = join_by(bldg == building_code))

# Prepare this data for school merge by building number
# Unique District 79 Programs
d79_codes <- sbhc %>% filter(grepl("#",dbn)) %>% pull(dbn) %>% lapply(.,function(x) gsub("#.*", "", x)) %>% unlist %>% unique()

sbhc_merge <- sbhc %>%
  # Add '[D79]' in front of all their alternative programs
  mutate(school_name = ifelse(grepl(paste(d79_codes,collapse="|"),dbn),paste("[D79]",school_name),school_name)) %>%
  group_by(bldg) %>%
  # collapse all schools in building code to one line
  mutate(schools_served = paste(sort(school_name),collapse = "|"),
         num_school_served = length(school_name)) %>%
  filter(row_number() == 1) %>%
  select(bldg,campus_name,room,sbhc_sponsor,schools_served,num_school_served)

# Read in school data
school_sf <- read.csv("data/input/Schooldata_24.csv") %>%
  clean_names()

# add school-level enrollment data
enrollment_school <- read_excel("data/input/demographic-snapshot-2018-19-to-2022-23-(public).xlsx", sheet = "School") %>%
  filter(Year == "2022-23") %>%
  clean_names()

school_sf <- school_sf %>%
  left_join(enrollment_school %>%
              select(dbn,total_enrollment), 
            by = join_by(ats_system_code == dbn))

# 2019 schools lat/lons
school_long_lat_df <- read.socrata("https://data.cityofnewyork.us/resource/wg9x-4ke6.csv?$limit=9999999") %>% 
  filter(latitude > 1 & latitude != "NULL") %>%
  select(primary_building_code,longitude,latitude) %>%
  distinct(primary_building_code, .keep_all = TRUE)

# Add 2019 long + lats to 2024 school data
school_sf <- school_sf %>%
  left_join(school_long_lat_df, by = join_by(building_code == primary_building_code))

#school_sf %>%
#  filter(is.na(longitude)) %>%
#  View()

# Only 39 missing, nb

# Collapse school data by building code
school_building_sf <- school_sf %>%
  filter(status_description == "Open") %>%
  group_by(building_code) %>%
  # retrieve the top enrolled school in building
  arrange(desc(total_enrollment)) %>%
  # collapse all open schools in building code to one line
  mutate(open_schools = paste(sort(location_name),collapse = "|"),
         num_open_schools = length(location_name),
         top_school_enrolled = first(location_name),
         total_enrolled = sum(total_enrollment)) %>%
  filter(row_number() == 1) %>%
  select(building_code,open_schools,num_open_schools,top_school_enrolled,total_enrolled,primary_address,city,longitude,latitude)

# Merge sbhc data with school buildings
map_sf <- school_building_sf %>%
  left_join(sbhc_merge, by = join_by(building_code == bldg)) %>%
  left_join(merge_df) 
## Check for same differences in open schools and schools served by SBHC
#%>%
#  rowwise() %>%
#  mutate(diff_a = list(Reduce(setdiff, strsplit(c(open_schools, schools_served), split = ", "))),
#         diff_b = list(Reduce(setdiff, strsplit(c(schools_served, open_schools), split = ", "))))

# Fill in blank long+lats
map_sf <- map_sf %>% 
  mutate(longitude = ifelse(is.na(longitude),strsplit(Gcode(primary_address,city),',')[[1]][1],longitude),
         latitude = ifelse(is.na(latitude),strsplit(Gcode(primary_address,city),',')[[1]][2],latitude)) %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

# 8 addresses left to fill manually
#map_sf %>%
#  filter(is.na(longitude)) %>%
#  View()

map_sf[which(map_sf$building_code == "OBIG"),"longitude"] <- -73.836970
map_sf[which(map_sf$building_code == "OBIG"),"latitude"] <- 40.915440

map_sf[which(map_sf$building_code == "M464"),"longitude"] <- -74.010910
map_sf[which(map_sf$building_code == "M464"),"latitude"] <- 40.709270

map_sf[which(map_sf$building_code == "M391"),"longitude"] <- -74.015671
map_sf[which(map_sf$building_code == "M391"),"latitude"] <- 40.705650

map_sf[which(map_sf$building_code == "K749"),"longitude"] <- -74.015671
map_sf[which(map_sf$building_code == "K749"),"latitude"] <- 40.705650

map_sf[which(map_sf$building_code == "K671"),"longitude"] <- -74.002140
map_sf[which(map_sf$building_code == "K671"),"latitude"] <- 40.653320

map_sf[which(map_sf$building_code == "L001"),"longitude"] <- -73.487240
map_sf[which(map_sf$building_code == "L001"),"latitude"] <- 40.821810

map_sf[which(map_sf$building_code == "R086"),"longitude"] <- -74.103660
map_sf[which(map_sf$building_code == "R086"),"latitude"] <- 40.608430

map_sf[which(map_sf$building_code == "MCFC"),"longitude"] <- -73.912150
map_sf[which(map_sf$building_code == "MCFC"),"latitude"] <- 40.870970

# Add additional information on the 9 SBHC that has closed since 2023
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

map_sf[match(closed_sbhc_merge$building_code,map_sf$building_code),"campus_name"] <- closed_sbhc_merge$campus_name
map_sf[match(closed_sbhc_merge$building_code,map_sf$building_code),"sbhc_sponsor"] <- closed_sbhc_merge$sbhc_sponsor

# School Districts shapefile prep
school_dist_shp <- st_read(unzip_sf("https://data.cityofnewyork.us/api/geospatial/r8nu-ymqj?method=export&format=Shapefile")) %>%
  st_transform(st_crs(4326))

# Combine district 10
dist_10 <- list(10,NA,NA,st_combine(school_dist_shp %>% filter(school_dis == 10)))

school_dist_shp <- school_dist_shp %>%
  filter(school_dis != 10)

school_dist_shp[nrow(school_dist_shp)+1,] <- dist_10

# Add stats to school dist
ll12_21 <- read_excel("data/input/Local Law 12 School Year 2021-22.xlsx", sheet = "Reports_Data", skip = 1)

ll12_21_merge <- data.frame(school_dis = ll12_21$`Geographic Community School District`,
                            num_medication_orders = ll12_21$...12,
                            anaphylaxis = ll12_21$`Anaphylaxis (allergies)`,
                            asthma = ll12_21$Asthma,
                            diabetes1 = ll12_21$`Diabetes Type 1`,
                            diabetes2 = ll12_21$`Diabetes Type 2`,
                            sbhc_students_served = ll12_21$...17
                            ) %>%
  slice(-nrow(.)) %>%
  mutate(school_dis = as.numeric(school_dis))

school_dist_shp <- school_dist_shp %>%
  left_join(ll12_21_merge)

# Use enrollment data
enrollment_district <- read_excel("data/input/demographic-snapshot-2018-19-to-2022-23-(public).xlsx", sheet = "District") %>%
  mutate(school_dis = as.numeric(`Administrative District`)) %>%
  filter(Year == "2021-22",
         school_dis <= 32) %>%
  clean_names()

school_dist_shp <- school_dist_shp %>%
  left_join(enrollment_district %>%
              select(school_dis,total_enrollment,number_poverty,percent_poverty,number_students_with_disabilities,percent_students_with_disabilities,economic_need_index))

# Normalize/Percentage of students with the 4 diseases
school_dist_shp <- school_dist_shp %>%
  mutate(percent_anaphylaxis = anaphylaxis/total_enrollment,
         percent_asthma = asthma/total_enrollment,
         percent_diabetes1 = diabetes1/total_enrollment,
         percent_diabetes2 = diabetes2/total_enrollment)

# Make all empty to NAs
map_sf$services_provided[map_sf$services_provided==""] <- NA

# Remove unused assets
rm(merge_df,sbhc_merge,d79_codes,school_building_sf,school_long_lat_df,school_sf, dist_10,ll12_21,ll12_21_merge,enrollment_school,enrollment_district,closed_sbhc_merge)

# Save map and school district data as csvs
#write_csv(map_sf,"data/output/map_sf.csv")
#write_csv(school_dist_shp,"data/output/school_districts_shp.csv")

