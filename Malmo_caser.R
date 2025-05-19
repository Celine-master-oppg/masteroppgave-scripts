# Kart Malmø-----


library(sf)

# Installer og last inn nødvendige pakker
install.packages("sf")
library(sf)

malmo <- st_read("data/malmo.geojson")

# Eksempel: Hvis rtv_malmo bruker EPSG:4326 (WGS 84)
malmo <- st_transform(malmo, crs = st_crs(rtv_malmo))



