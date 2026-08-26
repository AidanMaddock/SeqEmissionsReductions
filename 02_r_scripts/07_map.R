`library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lwgeom)

theme_set(theme_bw(base_family = "Times New Roman"))

# Read the country grouping file
groupings <- read_csv("01_tidy_data/CountryGroupings.csv", show_col_types = FALSE) %>%
  filter(!is.na(ISO), ISO != "EU27_2020") %>% # Remove EU aggregate
  distinct(ISO, .keep_all = TRUE) %>%
  mutate(
    in_grouping = TRUE,
    is_hic = Classification == "UMICs" | Classification == "LMICs" # Grouping by income 
  )

# World map data
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  left_join(groupings, by = c("iso_a3" = "ISO")) %>%
  mutate(
    in_grouping = if_else(is.na(in_grouping), FALSE, in_grouping),
    is_hic = if_else(is.na(is_hic), FALSE, is_hic),
    in_grouping = if_else(admin == "France", TRUE, in_grouping),
    fill_status = if_else(in_grouping, "Included", "Not Present")
  )

# Wrap around dateline so that outlines aren't funny
world <- sf::st_wrap_dateline(
  world,
  options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")
)

world <- world |>
  filter(admin != "Antarctica")

# Plot
ggplot(world) +
  geom_sf(
    aes(fill = fill_status),
    color = "grey80",
    linewidth = 0.15
  ) +
  geom_sf(
    data = filter(world, is_hic),
    aes(color = "Developing Country"),
    fill = NA,
    linewidth = 0.35,
    show.legend = TRUE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Included" = "#005800",
      "Not Present" = "white"
    )
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Developing Country" = "#9B1E0F")
  ) +
  guides(
    fill = guide_legend(
      order = 1,
      override.aes = list(color = "grey70", linewidth = 0.15)
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(fill = NA, linewidth = 0.8)
    )
  ) +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_bw(base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom"
  ) `