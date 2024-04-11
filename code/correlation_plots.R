# static and interractive correlation plots btwn health condition x sbhcs

library(vroom)
library(dplyr)
library(tidyverse)
library(zoo)
library(scales)
library(councilverse)
library(ggplot2)
library(ggiraph)
library(leaflet)
library(sf)
library(janitor)

# ---- load data ----
health_data <- read.csv("../data/input/health_master_2122.csv") %>% clean_names() # copy pasted 2021-2022 LL12, DOE, data by school dist
# append vaccination info from OpenData
vax <- vroom::vroom("https://data.cityofnewyork.us/resource/ne9b-qgmm.csv?$limit=999999999")
# ---- clean data ----
# grab first two characters from DBN to get school dist
vax$geographic_community_school_district <- as.numeric(substr(vax$school_dbn, start = 1, stop = 2))
full_vax_by_dist <- vax %>% 
  group_by(geographic_community_school_district) %>% 
  summarise(n_fully_vaccinated = sum(of_students_fully_vaccinated)) %>%
  filter(!is.na(geographic_community_school_district))
# merge on district
health_data <-
  health_data %>% left_join(full_vax_by_dist, by = "geographic_community_school_district")

health_data$n_not_fully_vaccinated = health_data$total_enrollment - health_data$n_fully_vaccinated

# ---- plot prevalence rates x total sbhcs ----

# Diabetes 1 (1, LL12)

p1 <- ggplot(data=health_data, aes(x=diabetes_type_1/total_enrollment, 
                                   y = total_sbh_cs)) +
  geom_point(size = health_data$total_enrollment/20000) +
  geom_label(data = health_data[health_data$geographic_community_school_district %in% c(9,10),],
             aes(x = diabetes_type_1/total_enrollment,total_sbh_cs,
                 label=geographic_community_school_district),
             nudge_y = .5, nudge_x = -.00015,
             size = 1.8) +
  labs(x = "Diabetes 1 \n(% Total Enrollment)", y = NULL) +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc()

# Diabetes 2 (2, LL12)
  
p2 <- ggplot(data=health_data, aes(x=diabetes_type_2/total_enrollment, 
                                   y = total_sbh_cs)) + # static vers
  geom_point(size = health_data$total_enrollment/20000) +
  geom_label(data = health_data[health_data$geographic_community_school_district %in% c(9,10),],
             aes(x = diabetes_type_2/total_enrollment,total_sbh_cs,
                 label=geographic_community_school_district),
             nudge_y = .5, nudge_x = .000075,
             size = 1.8) +
  labs(x = "Diabetes 2 \n(% Total Enrollment)", y = NULL) +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc()

# Asthma (3, LL12)

p3 <- ggplot(data=health_data, aes(x=asthma/total_enrollment, 
                                   y = total_sbh_cs)) + # static vers
  geom_point(size = health_data$total_enrollment/20000) +
  geom_label(data = health_data[health_data$geographic_community_school_district %in% c(9,10),],
             aes(x = asthma/total_enrollment,total_sbh_cs,
                 label=geographic_community_school_district),
             nudge_y = 1.5, nudge_x = .01,
             size = 1.8) +
  labs(x = "Asthma \n(% Total Enrollment)", y = NULL) +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc()

# Allergies (4, LL12)

p4 <- ggplot(data=health_data, aes(x=anaphylaxis_allergies/total_enrollment, 
                                   y = total_sbh_cs)) + # static vers
  geom_point(size = health_data$total_enrollment/20000) +
  geom_label(data = health_data[health_data$geographic_community_school_district %in% c(9,10),],
             aes(x = anaphylaxis_allergies/total_enrollment,total_sbh_cs,
                 label=geographic_community_school_district),
             nudge_y = 1.5, nudge_x = -.002,
             size = 1.8) +
  labs(x = "Anaphylaxis \n(% Total Enrollment)", y = NULL) +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc()

# Poverty (5, DOE)

p5 <- ggplot(data=health_data, aes(x=x_poverty/total_enrollment, 
                                   y = total_sbh_cs)) + # static vers
  geom_point(size = health_data$total_enrollment/20000) +
  geom_label(data = health_data[health_data$geographic_community_school_district %in% c(9,10),],
             aes(x = x_poverty/total_enrollment,total_sbh_cs,
                 label=geographic_community_school_district),
             nudge_y = 1.55, nudge_x = -.002,
             size = 1.8) +
  labs(x = "Poverty \n(% Total Enrollment)", y = NULL) +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc()


# Vaccinations (6, OpenData)

# p6 <- ggplot(data=health_data, aes(x=n_not_fully_vaccinated/total_enrollment, 
#                                    y = total_sbh_cs)) + # static vers
#   geom_point(size = health_data$total_enrollment/20000) +
#   geom_label(data = health_data[health_data$geographic_community_school_district %in% c(9,10),],
#              aes(x = x_poverty/total_enrollment,total_sbh_cs,
#                  label=geographic_community_school_district),
#              nudge_y = 1.55, nudge_x = -.002,
#              size = 1.8) +
#   labs(x = "Not Fully Vaccinated \n(% Total Enrollment)", y = NULL) +
#   scale_x_continuous(labels = scales::percent) +
#   theme_nycc()


# # Medication orders (7, LL12)
# 
# p7 <- ggplot(data=health_data, aes(x=total_number_of_medication_orders_reviewed_by_osh, 
#                                    y = total_sbh_cs)) + # static vers
#   geom_point(size = health_data$total_enrollment/20000) +
#   labs(x = "Medication Orders", y = NULL) + 
#   geom_label(data = health_data[health_data$total_number_of_medication_orders_reviewed_by_osh > 6000,],
#              size=(health_data[health_data$total_number_of_medication_orders_reviewed_by_osh > 6000,]$total_enrollment/20000),
#              aes(x = total_number_of_medication_orders_reviewed_by_osh,total_sbh_cs,
#                  label=geographic_community_school_district),
#              nudge_y = 3, nudge_x = -200) +
#   scale_x_continuous(labels = scales::comma) +
#   theme_nycc() 
# 
# # Student Encounters (8, LL12)
# 
# p8 <- ggplot(data=health_data, aes(x=total_number_of_student_encounters_of_all_med_room_visits_in_ashr, 
#                                    y = total_sbh_cs)) + # static vers
#   geom_point(size = health_data$total_enrollment/20000) +
#   geom_label(data = health_data[health_data$total_number_of_student_encounters_of_all_med_room_visits_in_ashr > 150000,],
#                    size=(health_data[health_data$total_number_of_student_encounters_of_all_med_room_visits_in_ashr > 150000,]$total_enrollment/20000),
#                    aes(x = total_number_of_student_encounters_of_all_med_room_visits_in_ashr,total_sbh_cs,
#                        label=geographic_community_school_district),
#              nudge_y = 3, nudge_x = -5000) +
#   labs(x = "Student Encounters", y = NULL) + 
#   scale_x_continuous(labels = scales::comma) +
#   theme_nycc()

# grid all
grid.arrange(main, p5,p1,p2, p4, p3, # p6, # p7, p8, - commenting out, data includes nurses offices, hiding vaccinations too
             ncol = 2,
             left = "Number of School Based Health Clinics in District")

# enrollment/sbhc outliers
model <- lm(total_sbh_cs ~ total_enrollment, data = health_data)
stud_resids <- studres(model)
plot(health_data$geographic_community_school_district, stud_resids,  
     ylab='Studentized Residuals', xlab='School District') # school dists 9, 10

main <- ggplot(data=health_data, aes(x=total_enrollment, 
                             y = total_sbh_cs)) +
  geom_point(size = health_data$total_enrollment/20000) +
  # geom_smooth(method='lm', se = FALSE) +
  geom_label(data = health_data[health_data$geographic_community_school_district %in% c(9,10),],
             aes(x = total_enrollment,total_sbh_cs,
                 label=geographic_community_school_district),
             nudge_y = -2, nudge_x = -1500,
             size = 1.4)  +
  labs(x = "Total Enrollment", y = NULL) +
  theme_nycc()

