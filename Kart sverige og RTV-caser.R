## Lage kart med casene fra RTV etter fylker-----
install.packages("lwgeom")

install.packages(c("tidyverse", "sf", "ggplot2", "lwgeom", "readxl"))

library(tidyverse)  # inkluderer bl.a. dplyr og ggplot2
library(sf)         # jobber med kartdata
library(lwgeom)     # utvidelse for sf
library(readxl)     # for å lese Excel-filer


#laster ned datasettet

rtv <- read_excel("celine_data/rtv_datasett_masteroppgave.xlsx")
View(rtv)
colnames(rtv)

# Filtrerer ut bare Sverige
rtv_sverige <- rtv %>%
  filter(iso3 == "SWE")  # Du kan også bruke country_name == "Sweden" hvis det passer bedre

# Sjekker at filtreringen funket
table(rtv_sverige$iso3)

# Lager sf-objekt av svenske caser
rtv_sverige_sf <- st_as_sf(
  rtv_sverige,
  coords = c("location_longitude", "location_latitude"),
  crs = 4326  # WGS 84 - standard geografisk koordinatsystem
)

# Sjekker at det ser greit ut
print(rtv_sverige_sf)

## Laster ned sverige fylker----

sverige_fylker <- st_read("data/alla_lan/alla_lan.shp", options = "ENCODING=ISO-8859-1")
plot(st_geometry(sverige_fylker))
colnames(sverige_fylker)

st_crs(rtv_sverige_sf)
st_crs(sverige_fylker)

sverige_fylker <- st_transform(sverige_fylker, crs = 4326)


# Koble sammen hendelser med fylker
rtv_sverige_fylke <- st_join(rtv_sverige_sf, sverige_fylker, join = st_within)

View(rtv_sverige_fylke)

## Plott RTV-hendelser på Sverige-kart
ggplot() +
  geom_sf(data = sverige_fylker, fill = "lightgray", color = "black") +  # Fylkene
  geom_sf(data = rtv_sverige_fylke, color = "red", size = 3, alpha = 0.4) +  # Punktene
  theme_minimal() +
  labs(title = "RTV-hendelser i Sverige", x = NULL, y = NULL)

#Fått testa at det funker, nå vil jeg ha litt annen layoute:

colnames(sverige_fylker)


# Siden hendelsen i Gotland antakelig har feil koordinater og blir plassert feil
# så må det manuelt legges på (dette ser dobbelsjekket i datasettet):

rtv_sverige_fylke$LAN_NAMN[is.na(rtv_sverige_fylke$LAN_NAMN)] <- "Gotlands län"

# Generer fylkesoppsummering på nytt
caser_per_fylke <- rtv_sverige_fylke %>%
  group_by(LAN_NAMN) %>%
  summarise(antall = n()) %>%
  arrange(desc(antall)) %>%
  mutate(
    x_pos = max(st_bbox(sverige_fylker)["xmax"]) - 3,
    y_pos = min(st_bbox(sverige_fylker)["ymin"]) + seq(0.5, length.out = n(), by = 0.5)
  )

ggplot() +
  geom_sf(data = sverige_fylker, fill = "gray85", color = "gray40") +
  geom_sf(data = rtv_sverige_fylke, color = "red", size = 2, alpha = 0.4) +
  geom_text(data = caser_per_fylke, aes(x = x_pos, y = y_pos, label = paste(LAN_NAMN, antall, sep = ": ")),
            size = 3, color = "black", hjust = 0) +
  coord_sf(xlim = c(min(st_bbox(sverige_fylker)["xmin"]),
                    max(st_bbox(sverige_fylker)["xmax"]) + 3),
           ylim = c(min(st_bbox(sverige_fylker)["ymin"]),
                    max(st_bbox(sverige_fylker)["ymax"]))) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(title = "RTV i Sverige etter fylker", x = NULL, y = NULL)

rtv_sverige_fylke$LAN_NAMN[is.na(rtv_sverige_fylke$LAN_NAMN)] <- "Gotlands l\u00e4n"

ggsave("visuals/kart_caser_sverige.png", width = 14, height = 7, dpi = 300)
