## -----------------------------------------------------------------------------------------------------
library(cancensus)
library(tidyverse)
library(mapview)
library(ggiraph)
library(patchwork)
options(cancensus.api_key = Sys.getenv("CANCENSUS_API_KEY"))

vancouver_extract <- get_census(
  dataset = "CA16",
  regions = list(CSD = "5915022"),
  vectors = c("v_CA16_1364", "v_CA16_2397",
              "v_CA16_244"),
  level = "CT",
  geo_format = "sf",
  labels = "short"
)

plot(vancouver_extract$geometry)


## -----------------------------------------------------------------------------------------------------

vancouver_data <- vancouver_extract %>%
  transmute(
    tract_id = GeoUID,
    pct_english = 100 * (v_CA16_1364 / Population),
    median_income = v_CA16_2397,
    pct_65_up = 100 * (v_CA16_244 / Population)
  )



## -----------------------------------------------------------------------------------------------------
vancouver_data


## -----------------------------------------------------------------------------------------------------
library(spdep)
set.seed(123456)

nb <- poly2nb(vancouver_data,
              queen = FALSE)


## ----------------------------------------------------------------------------------------
van_coords <- vancouver_data %>%
  st_centroid() %>%
  st_coordinates()

plot(vancouver_data$geometry)
plot(nb,
     coords = van_coords,
     add = TRUE,
     col = "blue",
     points = FALSE)


## ---- echo = FALSE------------------------------------------------------------------------------------

input_vars <- vancouver_data %>%
  select(pct_english:pct_65_up) %>%
  st_drop_geometry() %>%
  scale() %>%
  as.data.frame()

costs <- nbcosts(nb, input_vars)
weights <- nb2listw(nb, costs, style = "B")

mst <- mstree(weights)

regions <- skater(mst[,1:2], input_vars, ncuts = 5,
                  crit = 3)

vancouver_data$group <- as.character(regions$group)

m1 <- mapview(vancouver_data, zcol = "group", col.regions = RColorBrewer::brewer.pal(6, "Set1"))

# htmlwidgets::saveWidget(m1@map, "img/mapview.html")

## ----------------------------------------------------------------------------------------
# Example adapted from Claus Wilke, https://wilkelab.org/SDS375/slides/interactive-plots.html#1

scatter <- vancouver_data %>%
  ggplot(aes(x = pct_english, y = median_income, color = group)) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::dollar) +
  geom_point_interactive(
    aes(
      data_id = tract_id
    ),
    na.rm = TRUE, size = 3
  ) +
  theme_minimal() +
  guides(color = FALSE) +
  labs(x = "% speaking English at home",
       y = "Median household income (2015)")

tract_map <- vancouver_data %>%
  ggplot(aes(fill = group)) +
  scale_fill_brewer(palette = "Set1") +
  geom_sf_interactive(
    aes(
      data_id = tract_id
    ),
    size = 0.2, color = "black"
  ) +
  theme_void() +
  theme(legend.position = "left") +
  labs(fill = "Cluster")

g1 <- girafe(
  ggobj = scatter + tract_map,
  width_svg = 10,
  height_svg = 4
) %>%
  girafe_options(opts_hover(css = "fill:cyan;"))

htmlwidgets::saveWidget(g1, "img/ggiraph.html")

