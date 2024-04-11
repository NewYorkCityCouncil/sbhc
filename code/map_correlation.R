# run 01_prepare-data.R first - need school_dist_shp

# grap # sbhcs per dis
ll12_21 <- read_excel("data/input/Local Law 12 School Year 2021-22.xlsx", sheet = "Reports_Data", skip = 1)
ll12_21_merge <- data.frame(school_dis = as.numeric(ll12_21$`Geographic Community School District`),
                            num_sbhcs = ll12_21$`Total SBHCs`)
# merge to school dist shp
school_dist_shp <-
  school_dist_shp %>% left_join(ll12_21_merge, by = "school_dis")
rm(ll12_21)
rm(ll12_21_merge)

p_map <- ggplot(school_dist_shp, aes(fill = percent_poverty, data_id = school_dis)) + 
  geom_sf_interactive(size = 0.1) +
  scale_fill_distiller(direction = 1) +
  theme_nycc() + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  labs(fill = "Percent Poverty") 

p_plot <- ggplot(school_dist_shp, 
       aes(x = percent_poverty, y = num_sbhcs, color = percent_poverty,
           data_id = school_dis)) + 
  geom_point_interactive(size = (school_dist_shp$total_enrollment)/10000,
                         show.legend = FALSE) + 
  scale_color_distiller(direction = 1) +
  # scale_y_log10() + 
  scale_x_continuous(labels = scales::percent) +
  # theme_minimal(base_size = 12) + 
  theme_nycc() +
  labs(color = "Percent Poverty",
       x = "Percent Poverty",
       y = "Total SBHCs")

girafe(ggobj = p_plot + p_map, width_svg = 10, height_svg = 5.5)  %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:cyan;",
                   only_shiny = FALSE)
  )

