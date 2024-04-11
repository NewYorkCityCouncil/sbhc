# run 01_prepare-data.R first

p_map <- ggplot(school_dist_shp, aes(fill = percent_poverty, data_id = school_dis)) + 
  geom_sf_interactive(size = 0.1) +
  scale_fill_distiller() +
  theme_nycc() + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) +
  labs(fill = "Percent Poverty") 

p_plot <- ggplot(school_dist_shp, 
       aes(x = percent_poverty, y = total_enrollment, color = percent_poverty,
           data_id = school_dis)) + 
  geom_point_interactive(size = (school_dist_shp$total_enrollment)/10000,
                         show.legend = FALSE) + 
  scale_color_distiller() +
  # scale_y_log10() + 
  scale_x_continuous(labels = scales::percent) +
  # theme_minimal(base_size = 12) + 
  theme_nycc() +
  labs(color = "Percent Poverty",
       x = "Percent Poverty",
       y = "Total Enrollment (will be changed to # SBHCs)")

girafe(ggobj = p_plot + p_map, width_svg = 10, height_svg = 5.5)  %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:cyan;",
                   only_shiny = FALSE)
  )

