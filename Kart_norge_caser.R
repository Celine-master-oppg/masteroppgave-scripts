## Lage kart med casene fra RTV etter fylker-----
install.packages("lwgeom")
library(lwgeom)
library(sf)
library(ggplot2)
library(dplyr)

#laster ned datasettet

rtv <- read_excel("celine_data/rtv_datasett_masteroppgave.xlsx")
View(rtv)
colnames(rtv)

rtv_norge <- rtv %>%
  filter(iso3 == "NOR")  # Eller bruk country_name == "Norway" hvis nødvendig
table(rtv_norge$iso3)  # Skal kun vise "NOR"
rtv_norge_sf <- st_as_sf(rtv_norge, coords = c("location_longitude", "location_latitude"), crs = 4326)
rtv_fylke <- st_join(rtv_norge_sf, fylker_norge, join = st_within)
sum(is.na(rtv_fylke$navn))

View(rtv_norge)

#laster ned fylker

fylker_norge <- st_read("data/Fylker.geojson")

#Lager plot-----

library(ggplot2)

#Lager plot med heatmap----
library(ggplot2)
library(dplyr)

# Tell antall caser per unike lokasjon
rtv_fylke_counts <- rtv_fylke %>%
  group_by(geometry) %>%
  summarise(antall = n()) 



#denne funker, men prøver litt flere:

ggplot() +
  geom_sf(data = fylker_norge, fill = "lightgray", color = "black") +  # Tegn fylkene
  geom_sf(data = rtv_fylke, color = "red", size = 3, alpha = 0.3) +  # Større og mer gjennomsiktige punkter
  theme_minimal() +
  labs(title = "Fordeling av caser i norske fylker")


# Prøver å bruke større størrelse på punktene

rtv_fylke_counts <- rtv_fylke %>%
  group_by(geometry) %>%
  summarise(antall = n()) 

ggplot() +
  geom_sf(data = fylker_norge, fill = "lightgray", color = "black") +  # Tegn fylkene
  geom_sf(data = rtv_fylke_counts, aes(size = antall), 
          color = "red", alpha = 0.4) +  # Punktene blir større jo flere caser på samme sted
  scale_size(range = c(2, 2)) +  # Juster størrelsesskalaen
  theme_minimal() +
  labs(title = "Fordeling av caser i norske fylker", size = "Antall caser")


#Tester noe annet----
library(ggplot2)

# Koble fylkesdata med antall caser
caser_per_fylke <- caser_per_fylke %>%
  filter(antall >= 2)  # Kun steder med 5+ hendelser

ggplot() +
  geom_sf(data = fylker_norge, fill = "lightgray", color = "black") +
  geom_sf(data = rtv_fylke, color = "red", size = 3, alpha = 0.4) +
  geom_text(data = caser_per_fylke, aes(x = 10, y = 60, label = paste(navn, antall, sep=": ")), 
            size = 5, color = "black") +
  theme_minimal() +
  labs(title = "Antall caser per fylke i Norge")

caser_per_fylke <- caser_per_fylke %>%
  mutate(label = ifelse(navn == "Oslo", paste("Oslo", antall), paste(navn, antall, sep=": ")),
         x_pos = ifelse(navn == "Oslo", 17.5, st_coordinates(st_centroid(fylker_norge[match(navn, fylker_norge$navn),]))[,1]),
         y_pos = ifelse(navn == "Oslo", 59.9, st_coordinates(st_centroid(fylker_norge[match(navn, fylker_norge$navn),]))[,2]))

ggplot() +
  geom_sf(data = fylker_norge, fill = "lightgray", color = "black") +  # Tegn fylkene
  geom_sf(data = rtv_fylke, color = "red", size = 3, alpha = 0.4) +  # Plot punktene
  geom_text(data = caser_per_fylke, aes(x = x_pos, y = y_pos, label = label), 
            size = 5, color = "black") +  # Legg på fylkesnavn og antall caser
  theme_minimal() +
  theme(axis.text = element_blank(),  # Fjerner tekst på aksene
        axis.ticks = element_blank(),  # Fjerner ticks på aksene
        panel.grid = element_blank()) +  # Fjerner bakgrunnsgrid
  labs(title = "RTV fordelt i Norge", x = NULL, y = NULL)

ggsave("visuals/kart_caser_norge.png", width = 10, height = 7, dpi = 300)


## Forsøker å lage samme plot, bare med alle fylkene og antall på siden----

caser_per_fylke <- rtv_fylke %>%
  group_by(navn) %>%
  summarise(antall = n()) %>%
  arrange(desc(antall))  # Sorterer fra høyest til lavest

caser_per_fylke <- caser_per_fylke %>%
  mutate(x_pos = max(st_bbox(fylker_norge)["xmax"]) - 13,  # Plasser listen litt til høyre
         y_pos = min(st_bbox(fylker_norge)["ymin"]) + seq(0.5, length.out = n(), by = 0.5))  # Plasser nederst og gå oppover


ggplot() +
  geom_sf(data = fylker_norge, fill = "gray85", color = "gray40") +  # Tegn fylkene
  geom_sf(data = rtv_fylke, color = "red", size = 3, alpha = 0.4) +  # Plot punktene
  geom_text(data = caser_per_fylke, aes(x = x_pos, y = y_pos, label = paste(navn, antall, sep=": ")), 
            size = 3.5, color = "black", hjust = 0) +  # Justerer teksten til venstre
  theme_minimal() +
  theme(axis.text = element_blank(),  # Fjerner tekst på aksene
        axis.ticks = element_blank(),  # Fjerner ticks på aksene
        panel.grid = element_blank()) +  # Fjerner bakgrunnsgrid
  labs(title = "RTV i Norge etter fylker", x = NULL, y = NULL)

print(last_plot())  # Prøver å vise siste plott på nytt

ggsave("visuals/kart_caser_norge2.png", width = 10, height = 7, dpi = 300)


