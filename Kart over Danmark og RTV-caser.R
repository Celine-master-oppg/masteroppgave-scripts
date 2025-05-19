#Kart over Danmark og RTV-caser

library(readxl)

# Laster inn datasettet
rtv <- read_excel("celine_data/rtv_datasett_masteroppgave.xlsx")

# Filtrerer for Danmark
rtv_danmark <- rtv %>%
  filter(iso3 == "DNK")

# Sjekker
table(rtv_danmark$iso3)

library(sf)

rtv_danmark_sf <- st_as_sf(
  rtv_danmark,
  coords = c("location_longitude", "location_latitude"),
  crs = 4326  # WGS 84
)

# Sjekker at det ser greit ut
print(rtv_danmark_sf)

danmark_landsdeler <- st_read("data/landsdele.geojson")

colnames(danmark_landsdeler)
unique(danmark_landsdeler$navn)

rtv_danmark_fylke <- st_join(rtv_danmark_sf, danmark_landsdeler, join = st_within)

# Sjekk om noen ikke ble koblet til en landsdel
sum(is.na(rtv_danmark_fylke$navn))

#Lager tekst:
caser_per_fylke <- rtv_danmark_fylke %>%
  group_by(navn) %>%
  summarise(antall = n()) %>%
  arrange(desc(antall)) %>%
  mutate(
    x_pos = max(st_bbox(danmark_landsdeler)["xmax"]) - 1.5,
    y_pos = min(st_bbox(danmark_landsdeler)["ymin"]) + 1 + seq(0.2, length.out = n(), by = 0.2)
  )

View(caser_per_fylke)

#Lager plot:

ggplot() +
  geom_sf(data = danmark_landsdeler, fill = "gray85", color = "gray40") +
  geom_sf(data = rtv_danmark_fylke, color = "red", size = 4, alpha = 0.4) +
  geom_text(data = caser_per_fylke, aes(x = x_pos, y = y_pos, label = paste(navn, antall, sep = ": ")),
            size = 4, color = "black", hjust = 0) +
  coord_sf(xlim = c(min(st_bbox(danmark_landsdeler)["xmin"]),
                    max(st_bbox(danmark_landsdeler)["xmax"]) + 1.5),
           ylim = c(min(st_bbox(danmark_landsdeler)["ymin"]),
                    max(st_bbox(danmark_landsdeler)["ymax"]))) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "RTV i Danmark etter fylker", x = NULL, y = NULL)

ggsave("visuals/kart_caser_danmark.png", width = 10, height = 7, dpi = 300)


