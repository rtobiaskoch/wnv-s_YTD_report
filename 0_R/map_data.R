list2env(readRDS("1_input/config_params.RDS"),           
         envir = .GlobalEnv)


suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(tidyverse,rquery, #manipulation
                 ggpubr, wesanderson, paletteer, leaflet, patchwork# plotting
  )
})

data_input0 = read.csv(fn_database_input) #pool level data (database)

#complete lat and long if missing from known traps
trap = data_input0 %>%
  filter(!is.na(lat)) %>%
  distinct(zone, trap_id, .keep_all = T) %>% #didn't include lat and long because some traps in berthoud have multiple lat long
  select(zone, trap_id, lat, long)
  

data_input = data_input0 %>%
  rquery::natural_join(trap, by = c("trap_id", "zone")) %>%
  filter(year == year_filter) %>%
  filter(zone %in% fc_zones) %>%
  group_by(year, zone, trap_id, lat, long) %>% #group to summarize pools
  reframe(total = sum(total),
          log_total = log2(sum(total)),
          positive_pools = sum(test_code, na.rm = T),
          n_pools = n()) 

# Create a color palette based on the 'zone' column
pal <- colorNumeric(
  palette = c("darkblue", "red"), # Define cold-to-hot color gradient
  domain = data_input$positive_pools  # Apply it to the range of positive pools
)


# Create the Leaflet map
leaflet(data_input) %>%
 # addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>% 
  addTiles() %>%
  addCircleMarkers(
    ~long, ~lat,
    color = ~pal(positive_pools),
    radius = ~log_total,
    popup = ~paste0("Trap ID: ", trap_id, "<br>Positive Pools: ", positive_pools)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~positive_pools, title = "Positive pools")


library(ggspatial)


data_input = data_input0 %>%
  rquery::natural_join(trap, by = c("trap_id", "zone")) %>%
  filter(zone %in% fc_zones) %>%
  group_by(year, zone, trap_id, lat, long) %>% #group to summarize pools
  reframe(total = sum(total),
          log_total = log2(sum(total)),
          positive_pools = sum(test_code, na.rm = T),
          n_pools = n())

ggplot() +
  annotation_map_tile(type = "stamen") + # OpenStreetMap tiles
  geom_jitter(
    data = data_input,
    aes(x = long, y = lat, color = positive_pools, size = total),
  ) +
  facet_wrap(~year, nrow = 1) + # Facet by zone
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text = element_blank())
 