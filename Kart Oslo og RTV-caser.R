# Kart Oslo og RTV-caser---------

#Datasett med kun rtv fra Norge: rtv_norge

View(rtv_norge)

# Filterer ut Oslo
rtv_oslo <- rtv_norge[rtv_norge$region == "Oslo", ]
View(rtv_oslo)

#Datasett for kun Oslo:
library(jsonlite)

oslo_kart <- fromJSON("data/Bydeler_Oslo_u_marka-S.json")
oslo_kart <- st_read("data/Bydeler_Oslo_u_marka-S.json")

st_crs(oslo_kart)
st_crs(oslo_kart) <- 4326



#Plotter casene på karter-----
library(ggplot2)
library(sf)
install.packages("ggthemes")
library(ggthemes)  # Valgfritt, gir et rent kartutseende


ggplot() +
  geom_sf(data = oslo_kart, fill = "lightgrey", color = "black") +  # Oslo-kartet
  geom_point(data = rtv_oslo, aes(x = location_longitude, y = location_latitude), 
             color = "red", size = 2, alpha = 0.6) +  # Plotter sakene
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  # Fjerner gridlines
    axis.text = element_blank(),   # Fjerner tallene på aksene
    axis.ticks = element_blank(),  # Fjerner tick-marks
    axis.title = element_blank()   # Fjerner x- og y-aksenes titler
  ) +
  labs(title = "Hendelser fordelt i Oslo") + 
  coord_sf(lims_method = "geometry_bbox")

ggsave("visuals/kart_caser_oslo.png", width = 10, height = 7, dpi = 300)


st_bbox(oslo_kart)  # Viser bounding box for kartet
summary(rtv_oslo$location_longitude)
summary(rtv_oslo$location_latitude)

# Må gjøre om koordinatene til numeriske variabler:
rtv_oslo$location_longitude <- as.numeric(rtv_oslo$location_longitude)
rtv_oslo$location_latitude <- as.numeric(rtv_oslo$location_latitude)

