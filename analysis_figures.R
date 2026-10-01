

################################################# Figures ##
install.packages('hrbrthemes')
library(hrbrthemes)

N_mean<-NUE_R_GLU_Y %>%
  group_by(region, year) %>%
  summarise(mean_N_surplus = mean(N_surplus_Mt, na.rm = TRUE), 
            mean_NUE = mean(NUE, na.rm = TRUE))%>%
  ungroup() 


# Reshape to long format
try_1_long <- N_mean %>%
  pivot_longer(
    cols = c(mean_NUE),
    names_to = "variable",
    values_to = "value"
  )

# Create grouping variable for facet rows
try_1_long <- try_1_long %>%
  mutate(group_ab = ifelse(value > 50, "a) Above 50%", "b) 50% or Below"))

# Ensure order of facet rows
try_1_long$group_ab <- factor(try_1_long$group_ab,
                              levels = c("a) Above 50%", "b) 50% or Below"))

# Plot: facet_wrap with 2 rows, country names as strip labels
ggplot(try_1_long, aes(x = year, y = value, fill = variable)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "red", size = 1) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "blue", size = 1) +
  scale_fill_manual(values = c(mean_NUE = "#69b3a2"),
                    labels = c(mean_NUE = "NUE%")) +
  scale_y_continuous(name = "Nitrogen Use Efficiency (%)") +
  facet_wrap(~ region, nrow = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  # Force facet order so row 1 = above 50%, row 2 = below 50%
  facet_wrap(~ region, nrow = 2, labeller = labeller(region = label_value))





BNF_R_Y<- ggplot(BNF_Mt_R_GLU_Y, aes(x = year, y = BNF_Mt, color = basin, group = basin)) +
  geom_point(size = 1.5) +    
  geom_line(size = 1) +        
  labs(x = "",y = "Biological Nitrogen Fixation (Mt)",color = "Basin") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ region,
             labeller = labeller(
               region = function(x) stringr::str_wrap(x, width = 15)))


### BNF Map 
install.packages('ggspatial')
library(rmap)
library(ggspatial)
library(sf)

basin_spatial_data <- mapGCAMBasins%>%
  rename(subRegionMap = subRegion)

basin_ID <- mapping_gcambasins %>%
  rename(basin = subRegion)

BNF_Mt_R_2021<- BNF_Mt_R_GLU_Y %>%
  group_by(region, basin, year) %>%
  filter(year==2021)%>%
  rename(BNF_Mt_2021=BNF_Mt)%>%
  left_join(basin_ID, by = "basin")

BNF_Mt_R_2050<- BNF_Mt_R_GLU_Y %>%
  group_by(region, basin, year) %>%
  filter(year==2050)%>%
  rename(BNF_Mt_2050=BNF_Mt)%>%
  left_join(basin_ID, by = "basin")

BNF_Mt_R_compare<- BNF_Mt_R_2021 %>%
  left_join(BNF_Mt_R_2050, by = c("region","basin","subRegionMap"))%>%
  select(-scenario.x, -year.x, -scenario.y, -year.y)%>%
  mutate(Percent_change= ((BNF_Mt_2050 - BNF_Mt_2021)/ (BNF_Mt_2021))*100,
         Percent_change = replace(Percent_change, is.infinite(Percent_change), NA),
         Percent_change = pmin(Percent_change, 100, na.rm = TRUE) # cap at 100
  )


# Join index to map
map_index_2021 <- basin_spatial_data  %>%
  left_join(BNF_Mt_R_2021, by = c("subRegionMap"))

map_index_2050 <- basin_spatial_data  %>%
  left_join(BNF_Mt_R_2050, by = c("subRegionMap"))

map_index_compare <- basin_spatial_data  %>%
  left_join(BNF_Mt_R_compare, by = c("subRegionMap"))

# Create quartile bins
map_index_2021 <- map_index_2021 %>%
  mutate(
    BNF_quartile = cut(
      BNF_Mt,
      breaks = quantile(BNF_Mt, probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("0–25%", "25–50%", "50–75%", "75–100%")
    )
  )

# Create quartile bins
map_index_2050 <- map_index_2050 %>%
  mutate(
    BNF_quartile = cut(
      BNF_Mt,
      breaks = quantile(BNF_Mt, probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("0–25%", "25–50%", "50–75%", "75–100%")
    )
  )

#map_percentile_rank <- basin_spatial_data  %>%
#  left_join(BNF_Mt_R_2021, by = c("subRegionMap"))%>%
#  mutate(percentile = percent_rank(BNF_Mt)) #computes the percentile rank for each value


## raw values
map_rawvalues <- ggplot(data = map_index_compare) +
  geom_sf(aes(fill = Percent_change), color = "grey60", size = 0.2) +
  scale_fill_gradientn(
    colours = c("red", "lightpink", "purple", "black"),  # 4 colors
    values = scales::rescale(c(-5, 0, 5, 12)),          # map data range to colors
    name = "% Change",
    na.value = "grey"  # color for missing data
  ) +
  labs(title = "Biological Nitrogen Fixation % Change 2021-2050")+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

map_rawvalues+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "BNF_change.png"), height = 8, width = 11, units = "in")}


# raw values binned into quartiles
map_rawvalues_quartiles_2021 <- ggplot(data = map_index_2021) +
  geom_sf(aes(fill = BNF_quartile), color = "grey60", size = 0.2) +
  scale_fill_brewer(
    palette = "Greens",   # or any ColorBrewer palette
    name = "Quartile",                  # legend ticks at quantiles
    na.value = "grey"  
  ) +
  # Add title
  labs(title = "Biological Nitrogen Fixation 2021") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

map_rawvalues_quartiles_2021+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "BNF_quartile.png"), height = 8, width = 11, units = "in")}


# raw values binned into quartiles
map_rawvalues_quartiles_2050 <- ggplot(data = map_index_2050) +
  geom_sf(aes(fill = BNF_quartile), color = "grey60", size = 0.2) +
  scale_fill_brewer(
    palette = "Greens",   # or any ColorBrewer palette
    name = "Quartile",                  # legend ticks at quantiles
    na.value = "grey"  
  ) +
  # Add title
  labs(title = "Biological Nitrogen Fixation 2050") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

map_rawvalues_quartiles_2050+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "BNF_quartile.png"), height = 8, width = 11, units = "in")}


# percent rank
map_percentrank <- ggplot(data = map_percentile_rank) +
  geom_sf(aes(fill = percentile), color = "grey60", size = 0.2) +
  scale_fill_gradient(
    low = "lightgreen",
    high = "darkgreen",
    labels = scales::percent_format(accuracy = 1), # Show as %
    name = "BNF percentile rank",
    na.value = "white"  # color for missing data
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

############# N Dep Map

N_deposition_Mt_R_GLU_Y

N_deposition_Mt_R_GLU_2021<- N_deposition_Mt_R_GLU_Y%>%
  group_by(region, basin, year) %>%
  filter(year==2021)%>%
  rename(N_deposition_Mt_2021=N_deposition_Mt)%>%
  left_join(basin_ID, by = "basin")

############# N input Map

N_inputs_Mt_R_GLU_Y 

N_inputs_Mt_R_GLU_2021<- N_inputs_Mt_R_GLU_Y%>%
  group_by(region, basin, year) %>%
  filter(year==2021)%>%
  rename(input_N_Mt_2021=input_N_Mt)%>%
  left_join(basin_ID, by = "basin")

N_inputs_Mt_R_GLU_2050<- N_inputs_Mt_R_GLU_Y %>%
  group_by(region, basin, year) %>%
  filter(year==2050)%>%
  rename(input_N_Mt_2050=input_N_Mt)%>%
  left_join(basin_ID, by = "basin")

N_inputs_Mt_R_GLU_compare<- N_inputs_Mt_R_GLU_2021%>%
  left_join(N_inputs_Mt_R_GLU_2050, by = c("region","basin","subRegionMap"))%>%
  select(-scenario.x, -year.x, -scenario.y, -year.y)%>%
  mutate(
    Percent_change = (input_N_Mt_2050 - input_N_Mt_2021) / 
      input_N_Mt_2021 * 100,
    Percent_change = replace(Percent_change, is.infinite(Percent_change), NA),
    Percent_change = pmin(Percent_change, 100, na.rm = TRUE) # cap at 100
  )


# Join index to map
Ninput_map_index <- basin_spatial_data  %>%
  left_join(N_inputs_Mt_R_GLU_2021, by = c("subRegionMap"))

Ninput_map_index_compare <- basin_spatial_data  %>%
  left_join(N_inputs_Mt_R_GLU_compare, by = c("subRegionMap"))

# Create quartile bins
Ninput_map_index <- Ninput_map_index %>%
  mutate(
    N_input_quartile = cut(
      input_N_Mt_2021,
      breaks = quantile(input_N_Mt_2021, probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("0–25%", "25–50%", "50–75%", "75–100%")
    )
  )


# raw values binned into quartiles
Ninput_map_rawvalues_quartiles <- ggplot(data = Ninput_map_index) +
  geom_sf(aes(fill = N_input_quartile), color = "grey60", size = 0.2) +
  scale_fill_brewer(
    palette = "Oranges",   # or any ColorBrewer palette
    name = "Quartile",                  # legend ticks at quantiles
    na.value = "grey"  
  )  +
  # Add title
  labs(title = "Nitrogen Input 2021") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

Ninput_map_rawvalues_quartiles+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Ninput_quartile.png"), height = 8, width = 11, units = "in")}



## raw values
Ninput_map_rawvalues <- ggplot(data = Ninput_map_index_compare) +
  geom_sf(aes(fill = Percent_change), color = "grey60", size = 0.2) +
  scale_fill_gradientn(
    colours = c("red", "lightpink", "purple", "black"),  # 4 colors
    values = scales::rescale(c(-5, 0, 5, 12)),          # map data range to colors
    name = "% Change",
    na.value = "grey"  # color for missing data
  ) +
  labs(title = "Nitrogen Input % Change 2021-2050")+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

Ninput_map_rawvalues+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Ninput_change.png"), height = 8, width = 11, units = "in")}

##########################################

N_harvested_Mt_R_GLU_Y


N_harvested_Mt_R_GLU_2021<-N_harvested_Mt_R_GLU_Y%>%
  group_by(region, basin, year) %>%
  filter(year==2021)%>%
  rename(harvested_N_Mt_2021=harvested_N_Mt)%>%
  left_join(basin_ID, by = "basin")

N_harvested_Mt_R_GLU_2050<-N_harvested_Mt_R_GLU_Y%>%
  group_by(region, basin, year) %>%
  filter(year==2050)%>%
  rename(harvested_N_Mt_2050=harvested_N_Mt)%>%
  left_join(basin_ID, by = "basin")

N_harvested_Mt_R_GLU_compare<- N_harvested_Mt_R_GLU_2021%>%
  left_join(N_harvested_Mt_R_GLU_2050, by = c("region","basin","subRegionMap"))%>%
  select(-scenario.x, -year.x, -scenario.y, -year.y)%>%
  mutate(
    Percent_change = (harvested_N_Mt_2050 - harvested_N_Mt_2021) / 
      harvested_N_Mt_2021 * 100,
    Percent_change = replace(Percent_change, is.infinite(Percent_change), NA),
    Percent_change = pmin(Percent_change, 100, na.rm = TRUE) # cap at 100
  )


# Join index to map
Nharvest_map_index <- basin_spatial_data  %>%
  left_join(N_harvested_Mt_R_GLU_2021, by = c("subRegionMap"))

Nharvest_map_index_compare <- basin_spatial_data  %>%
  left_join(N_harvested_Mt_R_GLU_compare, by = c("subRegionMap"))

# Create quartile bins
Nharvest_map_index <- Nharvest_map_index %>%
  mutate(
    N_harvest_quartile = cut(
      harvested_N_Mt_2021,
      breaks = quantile(harvested_N_Mt_2021, probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("0–25%", "25–50%", "50–75%", "75–100%")
    )
  )


# raw values binned into quartiles
Nharvest_map_rawvalues_quartiles <- ggplot(data = Nharvest_map_index) +
  geom_sf(aes(fill = N_harvest_quartile), color = "grey60", size = 0.2) +
  scale_fill_brewer(
    palette = "Reds",   # or any ColorBrewer palette
    name = "Quartile",                  # legend ticks at quantiles
    na.value = "grey"  
  )  +
  # Add title
  labs(title = "Harvested Nitrogen 2021") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

Nharvest_map_rawvalues_quartiles+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Nharvest_quartile.png"), height = 8, width = 11, units = "in")}



## raw values
Nharvest_map_rawvalues <- ggplot(data = Nharvest_map_index_compare) +
  geom_sf(aes(fill = Percent_change), color = "grey60", size = 0.2) +
  scale_fill_gradientn(
    colours = c("red", "lightpink", "purple", "black"),  # 4 colors
    values = scales::rescale(c(-5, 0, 5, 12)),          # map data range to colors
    name = "% Change",
    na.value = "grey"  # color for missing data
  ) +
  labs(title = "Harvested Nitrogen % Change 2021-2050")+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

Nharvest_map_rawvalues+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Nharvest_change.png"), height = 8, width = 11, units = "in")}

########################################

N_surplus_Mt_R_GLU_Y


N_surplus_Mt_R_GLU_2021<-N_surplus_Mt_R_GLU_Y%>%
  group_by(region, basin, year) %>%
  filter(year==2021)%>%
  rename(N_surplus_Mt_2021=N_surplus_Mt)%>%
  left_join(basin_ID, by = "basin")

N_surplus_Mt_R_GLU_2050<-N_surplus_Mt_R_GLU_Y%>%
  group_by(region, basin, year) %>%
  filter(year==2050)%>%
  rename(N_surplus_Mt_2050=N_surplus_Mt)%>%
  left_join(basin_ID, by = "basin")

N_surplus_Mt_R_GLU_compare<- N_surplus_Mt_R_GLU_2021%>%
  left_join(N_surplus_Mt_R_GLU_2050, by = c("region","basin"))%>%
  select(-scenario.x, -year.x, -scenario.y, -year.y)%>%
  mutate(
    Percent_change = (N_surplus_Mt_2050 - N_surplus_Mt_2021) / 
      N_surplus_Mt_2021 * 100,
    Percent_change = replace(Percent_change, is.infinite(Percent_change), NA),
    Percent_change = pmax(pmin(Percent_change, 100, na.rm = TRUE), -100, na.rm = TRUE)
  )%>%
  rename(subRegionMap=subRegionMap.x)


# Join index to map
Nsurplus_map_index <- basin_spatial_data  %>%
  left_join(N_surplus_Mt_R_GLU_2021, by = c("subRegionMap"))

Nsurplus_map_index_compare <- basin_spatial_data  %>%
  left_join(N_surplus_Mt_R_GLU_compare, by = c("subRegionMap"))

# Create quartile bins
Nsurplus_map_index <- Nsurplus_map_index %>%
  mutate(
    N_surplus_quartile = cut(
      N_surplus_Mt_2021,
      breaks = quantile(N_surplus_Mt_2021, probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("0–25%", "25–50%", "50–75%", "75–100%")
    )
  )


# raw values binned into quartiles
Nsurplus_map_rawvalues_quartiles <- ggplot(data = Nsurplus_map_index) +
  geom_sf(aes(fill = N_surplus_quartile), color = "grey60", size = 0.2) +
  scale_fill_brewer(
    palette = "Blues",   # or any ColorBrewer palette
    name = "Quartile",                  # legend ticks at quantiles
    na.value = "grey"  
  )  +
  # Add title
  labs(title = "Nitrogen Surplus 2021") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

Nsurplus_map_rawvalues_quartiles+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Nsurplus_quartile.png"), height = 8, width = 11, units = "in")}



## raw values
Nsurplus_map_rawvalues <- ggplot(data = Nsurplus_map_index_compare) +
  geom_sf(aes(fill = Percent_change), color = "grey60", size = 0.2) +
  scale_fill_gradientn(
    colours = c("red", "lightpink", "purple", "black"),  # 4 colors
    values = scales::rescale(c(-5, 0, 5, 12)),          # map data range to colors
    name = "% Change",
    na.value = "grey"  # color for missing data
  ) +
  labs(title = "Nitrogen Surplus % Change 2021-2050")+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

Nsurplus_map_rawvalues+ theme(
  panel.background = element_rect(fill = "lightblue"),  # Background inside plotting area
  panel.grid = element_blank()  # Remove grid lines for cleaner look
)+
  annotation_north_arrow(
    location = "bl",              # top-right corner
    which_north = "true",         # true north
    style = north_arrow_fancy_orienteering,
    height = grid::unit(1.5, "cm"),
    width = grid::unit(1.5, "cm"),
    pad_x = grid::unit(0.5, "cm"),
    pad_y = grid::unit(0.5, "cm")
  ) +
  annotation_scale(
    location = "bl",              # bottom-left corner
    width_hint = 0.2,              # relative width of scale bar
    bar_cols = c("black", "white"),
    text_cex = 0.8
  )


# if (FIGS_SAVE) {ggsave(paste0(FIGS_DIR, "Nsurplus_change.png"), height = 8, width = 11, units = "in")}
