#kart for Sverige og fylker
library(sf)
install.packages("sf")


sverige_fylker <- st_read("data/alla_lan/alla_lan.shp", options = "ENCODING=ISO-8859-1")
colnames(sverige_fylker)
unique(sverige_fylker$LAN_NAMN)  # Bytt til riktig kolonnenavn

#Fjerner rare navn:

install.packages("stringi")  # Hvis du ikke har pakken
library(stringi)

sverige_fylker$LAN_NAMN <- stri_trans_general(sverige_fylker$LAN_NAMN, "latin-ascii")

str(sverige_fylker$LAN_NAMN)
str(sverige_fylker)


#Filterer ut de tre fylkene jeg trenger:------

skane <- sverige_fylker[sverige_fylker$LAN_NAMN == "Skane lan", ]
stockholm <- sverige_fylker[sverige_fylker$LAN_NAMN == "Stockholms lan", ]
vastra_gotaland <- sverige_fylker[sverige_fylker$LAN_NAMN == "Vastra Gotalands lan", ]

print(skane)
print(stockholm)
print(vastra_gotaland)

## Filtrerer ut fylkene fra rtv som er relevante-------

rtv_skane <- rtv[rtv$region == "Skane", ]
rtv_stockholm <- rtv[rtv$region == "Stockholm", ]
rtv_vastra_gotaland <- rtv[rtv$region == "Vastra Gotaland", ]


## Lage plots------

library(sf)  
library(ggplot2)  
library(dplyr)  
library(stringr)

library(tidyr)  # Pakke for drop_na()

# Fjern NA for alle tre fylker
rtv_skane <- rtv_skane %>% drop_na(location_longitude, location_latitude)
rtv_stockholm <- rtv_stockholm %>% drop_na(location_longitude, location_latitude)
rtv_vastra_gotaland <- rtv_vastra_gotaland %>% drop_na(location_longitude, location_latitude)

# Gjør om alle caser til sf-objekter
rtv_skane <- st_as_sf(rtv_skane, coords = c("location_longitude", "location_latitude"), crs = 4326)
rtv_stockholm <- st_as_sf(rtv_stockholm, coords = c("location_longitude", "location_latitude"), crs = 4326)
rtv_vastra_gotaland <- st_as_sf(rtv_vastra_gotaland, coords = c("location_longitude", "location_latitude"), crs = 4326)

colnames(rtv_skane)
View(rtv_skane)

#Skåne:
ggplot() +
  geom_sf(data = skane, fill = "gray90", color = "gray40", size = 0.4) +
  geom_sf(data = rtv_skane, color = "red", size = 3, alpha = 0.4) +
  theme_minimal() +
  labs(title = "Antall caser i Skane", x = NULL, y = NULL)

rtv_skane_coords <- st_transform(rtv_skane_coords, crs = st_crs(skane))

st_crs(rtv_skane_coords)  # Skal være EPSG:3006
st_crs(skane)             # Skal være EPSG:3006

head(st_coordinates(rtv_skane_coords))
head(st_coordinates(skane))

rtv_skane_coords <- cbind(rtv_skane_coords, st_coordinates(rtv_skane_coords))
colnames(rtv_skane_coords)

ggplot() +
  geom_sf(data = skane, fill = "lightgrey", color = "black") +  # Kart over Skåne
  geom_point(data = rtv_skane_coords, aes(x = X, y = Y),  # Bruker X og Y fra st_coordinates()
             color = "red", size = 2, alpha = 0.6) +  # Plot casene
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  labs(title = "Hendelsene fordelt i Skane")

ggsave("visuals/kart_caser_skane.png", width = 10, height = 7, dpi = 300)


# For Stockholm og Vestre Gotaland

rtv_stockholm_coords <- st_transform(rtv_stockholm, crs = 3006)
rtv_vastra_gotaland_coords <- st_transform(rtv_vastra_gotaland, crs = 3006)

rtv_stockholm_coords <- cbind(rtv_stockholm_coords, st_coordinates(rtv_stockholm_coords))
rtv_vastra_gotaland_coords <- cbind(rtv_vastra_gotaland_coords, st_coordinates(rtv_vastra_gotaland_coords))

ggplot() +
  geom_sf(data = stockholm, fill = "lightgrey", color = "black") +
  geom_point(data = rtv_stockholm_coords, aes(x = X, y = Y), 
             color = "red", size = 2, alpha = 0.6) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  labs(title = "Hendelsene fordelt i Stockholm")

ggsave("visuals/kart_caser_stockholm.png", width = 10, height = 7, dpi = 300)


ggplot() +
  geom_sf(data = vastra_gotaland, fill = "lightgrey", color = "black") +
  geom_point(data = rtv_vastra_gotaland_coords, aes(x = X, y = Y), 
             color = "red", size = 2, alpha = 0.6) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  labs(title = "Hendelsene fordelt i Vastra Gotaland")

ggsave("visuals/kart_caser_VG.png", width = 10, height = 7, dpi = 300)




