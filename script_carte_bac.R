# Charger toutes les librairies nécessaires
library(ggplot2)
library(sf)
library(dplyr)
library(geodata)
library(ggtext) 

# 1. Données du BAC Bénin
bac_data <- data.frame(
  DEPARTEMENT = c("Alibori", "Atacora", "Atlantique", "Borgou", "Collines", 
                  "Couffo", "Donga", "Littoral", "Mono", "Oueme", 
                  "Plateau", "Zou"),
  TAUX = c(60.55, 65.54, 78.13, 70.94, 70.30, 77.95, 65.57, 
           73.47, 76.00, 70.76, 70.92, 75.38)
)

# 2. Préparation de la carte 
benin_map_sf <- st_as_sf(gadm(country = "BEN", level = 1, path = tempdir()))
benin_map_sf <- st_make_valid(benin_map_sf)

benin_map_sf <- benin_map_sf %>%
  mutate(NAME_1 = case_when(
    NAME_1 == "Atakora" ~ "Atacora",
    NAME_1 == "Kouffo"  ~ "Couffo",
    TRUE                ~ NAME_1
  ))

benin_map_sf$NAME_1 <- iconv(benin_map_sf$NAME_1, from = "UTF-8", to = "ASCII//TRANSLIT")
map_data <- left_join(benin_map_sf, bac_data, by = c("NAME_1" = "DEPARTEMENT"))

# 3. Création des coordonnées MANUELLES
label_coords <- map_data %>%
  filter(!is.na(TAUX)) %>% 
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon_centroid = st_coordinates(centroid)[, 1],
    lat_centroid = st_coordinates(centroid)[, 2]
  ) %>%
  mutate(
        lon_text = case_when(
      NAME_1 %in% c("Atacora", "Donga", "Zou", "Couffo", "Mono") ~ 0.6,
      NAME_1 %in% c("Alibori", "Borgou") ~ 3.8,
      TRUE ~ 3.5 
    ),
    lon_elbow = case_when(
      NAME_1 %in% c("Alibori", "Borgou") ~ 3.3,
      lon_text > 2 ~ 3.0,
      TRUE ~ lon_text
    ),
    lat_text = case_when(
      NAME_1 == "Alibori"    ~ 11.9,
      NAME_1 == "Borgou"     ~ 9.9,  
      NAME_1 == "Collines"   ~ 8.5,
      NAME_1 == "Plateau"    ~ 7.6,
      NAME_1 == "Atlantique" ~ 7.1,
      NAME_1 == "Oueme"      ~ 6.7,
      NAME_1 == "Littoral"   ~ 6.3,
      NAME_1 == "Atacora"    ~ 10.8,
      NAME_1 == "Donga"      ~ 9.6,
      NAME_1 == "Zou"        ~ 8.2,
      NAME_1 == "Couffo"     ~ 7.1,
      NAME_1 == "Mono"       ~ 6.6
    )
  ) %>%
  mutate(
    label_text = paste0(
      "**", NAME_1, "**<br>",
      "<span style='color:\"#D8000C\";'>",
      format(TAUX, nsmall = 2, decimal.mark = ","), "%",
      "</span>"
    ),
    hjust_value = if_else(lon_text > 2.0, 0, 1)
  )

# 4. Création du graphique final
carte_finale_style <- ggplot(data = map_data) +
  
  geom_sf(fill = "#E8E8E8", color = "grey85", linewidth = 0.6) + 
  
  geom_segment(data = filter(label_coords, NAME_1 %in% c("Atacora", "Couffo")),
               aes(x = lon_centroid, y = lat_centroid, xend = lon_text, yend = lat_text), color = "black", linewidth = 0.4) +
  geom_segment(data = filter(label_coords, !NAME_1 %in% c("Atacora", "Couffo")),
               aes(x = lon_centroid, y = lat_centroid, xend = lon_elbow, yend = lat_centroid), color = "black", linewidth = 0.4) +
  geom_segment(data = filter(label_coords, !NAME_1 %in% c("Atacora", "Couffo")),
               aes(x = lon_elbow, y = lat_centroid, xend = lon_text, yend = lat_text), color = "black", linewidth = 0.4) +
  
  geom_sf(data = label_coords$centroid, aes(size = label_coords$TAUX), color = "darkorange", alpha = 1) +
  geom_richtext(data = label_coords, aes(x = lon_text, y = lat_text, label = label_text, hjust = hjust_value),
                size = 3.5, lineheight = 1.2, fill = NA, label.color = NA) +
  scale_size_continuous(range = c(2, 8)) +
  labs(title = "BAC 2025 BENIN", caption = "Source: Office du BAC Bénin | Réalisation: Ozias OROU YAWA") +
  
  # --- MODIFICATION: Ajustement du cadrage pour remplir la page A5 ---
  coord_sf(
    xlim = c(0.4, 4.2), 
    ylim = c(6.0, 12.6),
    expand = FALSE,     
    clip = "off"
  ) +
  
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 2, r = 1, b = 1.5, l = 1.5, "cm"), 
    plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(size = 8, face = "italic", color = "grey30", hjust = 0.5, margin = margin(t = 15))
  )

# Afficher la carte
print(carte_finale_style)

# Sauvegarder la carte finale en JPG format A5
ggsave(
  filename = "carte_BAC 2025.jpg",
  plot = carte_finale_style,
  width = 148,
  height = 210,
  units = "mm",
  dpi = 1000,
  bg = "white"
)
