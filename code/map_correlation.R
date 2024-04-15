library(ggpubr)
# run 01_prepare-data.R first - need school_dist_shp, map_sf
source("code/01_prepare-data.R")

# graph # sbhcs per dis
ll12_21 <- read_excel("data/input/Local Law 12 School Year 2021-22.xlsx", sheet = "Reports_Data", skip = 1)
ll12_21_merge <- data.frame(school_dis = as.numeric(ll12_21$`Geographic Community School District`),
                            num_sbhcs = ll12_21$`Total SBHCs`)
# merge to school dist shp
school_dist_shp <-
  school_dist_shp %>% left_join(ll12_21_merge, by = "school_dis")
rm(ll12_21)
rm(ll12_21_merge)

# filter map 
map_sf <- map_sf %>%
  filter(is.na(campus_name))

#st_crs(school_dist_shp) = 4326

# ---- Poverty x Total SBHCs ----

p_map <- ggplot(NULL) + 
  geom_sf_interactive(data = school_dist_shp, size = 0.1, 
                      aes(fill = percent_poverty,
                          data_id = school_dis)) +
  geom_sf_label(data=school_dist_shp, aes(label = school_dis), 
                label.size  = .05, 
                alpha = .2, 
                size =2,
                color = "navy") +
  # geom_point(data = map_sf, # add schools
  #            aes(x = longitude, y = latitude,
  #                col="grey", alpha = 0.2)) + 
  scale_fill_distiller(direction = 1,
                       breaks = 0.1*0:10,
                       labels = percent(0.1*0:10)) +
  theme_nycc() + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  labs(fill = "Percent Poverty",
       x = NULL,
       y = NULL) 


p_plot <- ggplot(school_dist_shp, 
       aes(x = percent_poverty, y = num_sbhcs, color = percent_poverty,
           data_id = school_dis)) + 
  stat_cor(method="pearson") +
  geom_point_interactive(size = (school_dist_shp$total_enrollment)/10000,
                         aes(tooltip = paste0("<strong>School District: </strong>", school_dis, "<br>",
                                              "SBHCs: ", num_sbhcs, "<br>", 
                                              "Poverty: ", round(percent_poverty*100,0), "%")),
                         show.legend=FALSE) + 
  scale_color_distiller(direction = 1) +
  # scale_y_log10() + 
  scale_x_continuous(labels = scales::percent) +
  # theme_minimal(base_size = 12) + 
  theme_nycc() +
  labs(color = "Percent Poverty",
       x = "Percent Poverty",
       y = "Total School Based Health Centers")

map_plot <- girafe(ggobj = p_plot + p_map, width_svg = 10, height_svg = 5.5)  %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:moccasin;",
                   only_shiny = FALSE),
                 opts_tooltip(
                   opacity = 0.8,
                   css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                 #opts_hover_inv(css = "stroke-width: 1;opacity:0.6;"),
                 #opts_hover(css = "stroke-width: 4; opacity: 1; color:orange"),
                 opts_toolbar(pngname = "poverty_correlation")
  )

htmltools::save_html(map_plot, "visuals/poverty-correlation-map_interactive.html")

# ---- Asthma x SBHCs ----

a_map <- ggplot(NULL) + 
  geom_sf_interactive(data = school_dist_shp, size = 0.1, 
                      aes(fill = percent_asthma,
                          data_id = school_dis)) +
  geom_sf_label(data=school_dist_shp, aes(label = school_dis), 
                label.size  = NA, alpha = 0) +
  # geom_point(data = map_sf, # add schools
  #            aes(x = longitude, y = latitude,
  #                col="grey", alpha = 0.2)) + 
  scale_fill_distiller(direction = 1,
                       breaks = 0.1*0:10,
                       labels = percent(0.1*0:10)) +
  theme_nycc() + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  labs(fill = "Percent Asthma",
       x = NULL,
       y = NULL) 

a_plot <- ggplot(school_dist_shp, 
                 aes(x = percent_asthma, y = num_sbhcs, color = percent_asthma,
                     data_id = school_dis)) + 
  geom_point_interactive(size = (school_dist_shp$total_enrollment)/10000,
                         show.legend = FALSE) + 
  scale_color_distiller(direction = 1) +
  # scale_y_log10() + 
  scale_x_continuous(labels = scales::percent) +
  # theme_minimal(base_size = 12) + 
  theme_nycc() +
  labs(color = "Percent Asthma",
       x = "Percent Asthma",
       y = "Total School Based Health Centers")

map_plot <- girafe(ggobj = a_plot + a_map, width_svg = 10, height_svg = 5.5)  %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:moccasin;",
                   only_shiny = FALSE)
  )

htmltools::save_html(map_plot, "visuals/asthma-correlation-map_interactive.html")
