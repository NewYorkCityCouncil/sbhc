library(readxl)
library(ggplot2)
library(dplyr)
library(councildown)

agg_sbhc <- read_excel("/Users/nycc/Desktop/sbhc_aggregate_data.xlsx", sheet=1)
agg_sbhc_2 <- read_excel("/Users/nycc/Desktop/sbhc_aggregate_data.xlsx", sheet=2)

# over time
agg_sbhc %>%
  ggplot(aes(x=year, y = num_sbhc)) + 
  geom_point() +
  geom_smooth(method="loess") +
  scale_color_nycc()
    
agg_sbhc_2 %>% 
  tidyr::drop_na() %>%
  filter(provider_type!="total") %>%
  mutate(relevel_provider=factor(provider_type, levels = c("diagnostic_and_treatment_centers","hospital","fed_qualifed_centers"))) %>%
  ggplot(aes(x=year, y=count, fill=relevel_provider, label=count)) + 
  geom_col() + 
  scale_color_nycc() + 
  theme(legend.title = element_blank(), legend.position="top") +
  geom_text(size=3,
    aes(label = after_stat(y), group = year), 
    stat = 'summary', fun = sum, vjust = 0.5, hjust=-0.2
  ) + 
  guides(colour = guide_legend(nrow = 2)) +
  coord_flip() +
  xlab("") +
  ylab("Number of SBHC by Provider Type")

# 2021-2022
agg_sbhc_2 %>% 
  tidyr::drop_na() %>%
  filter(provider_type!="total", year=="2021-2022") %>%
  mutate(relevel_provider=factor(provider_type, levels = c("diagnostic_and_treatment_centers","hospital","fed_qualifed_centers"))) %>%
  #mutate(relevel_provider=factor(provider_type, levels = c("fed_qualifed_centers", "hospital", "diagnostic_and_treatment_centers"))) %>%
  ggplot(aes(x=relevel_provider, y=count, label=count, fill=relevel_provider)) + 
  geom_bar(stat="identity") + 
  scale_color_nycc() + 
  theme(legend.title = element_blank(), legend.position="top") +
  geom_text(size=3,
            aes(label = after_stat(y), group = year), 
            stat = 'summary', fun = sum, vjust = 0.5, hjust=-0.2
  ) + 
  guides(colour = guide_legend(nrow = 2)) +
  coord_flip() +
  xlab("") +
  ylab("Number of SBHC by Provider Type") 

by_boro <- read_excel("/Users/nycc/Downloads/Local_Law_12_School_Year_2021-22.xlsx", sheet=2)

by_boro$borough <- substring(by_boro$BLDG, 1, 1)

by_boro %>% 
  filter(borough!="T") %>%
  group_by(borough) %>% 
  summarize(num_sbhc=n(), total_site_enrollment=sum(`Campus Population`)) %>%
  arrange(desc(num_sbhc)) %>%
  mutate(percent=(num_sbhc/sum(num_sbhc))*100)


by_boro %>%
  group_by(GeographicalDistrict) %>% 
  summarize(num_sbhc=n(), total_site_enrollment=sum(`Campus Population`)) %>%
  arrange(desc(num_sbhc)) %>%
  mutate(percent=(num_sbhc/sum(num_sbhc))*100)



