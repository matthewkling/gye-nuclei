
# load packages
library(terra)
library(tidyverse)
library(sf)
library(patchwork)
library(geojsonsf)


# classify climate zones
r <- rast("data/climate.tif")
d <- r %>% as.data.frame(xy = T) %>% as_tibble()
dd <- d %>%
      mutate(park = is.finite(park),
             nucleus = between(t1, quantile(t2[park], .25), quantile(t2[park], .75)),
             vacuum = between(t2, quantile(t2[park], .25), quantile(t2[park], .75)),
             crucible = between(t1, quantile(t1[park], .25), quantile(t1[park], .75)),
             refugium = between(t2, quantile(t1[park], .25), quantile(t1[park], .75)))


# reformat data
pd1 <- ddd <- dd %>%
      gather(stat, value, nucleus:refugium) %>%
      mutate(val = case_when(value & stat %in% c("nucleus", "vacuum") ~ "warm",
                             value & stat %in% c("crucible", "refugium") ~ "cool",
                             TRUE ~ "neutral")) %>%
      mutate(value = case_when(stat == "nucleus" & value ~ t1,
                               stat == "crucible" & value ~ t1,
                               stat == "vacuum" & value ~ t2,
                               stat == "refugium" & value ~ t2,
                               TRUE ~ NA))
pd2 <- d %>%
      gather(stat, value, t1, t2)
pd <- bind_rows(pd1, pd2) %>%
      mutate(stat = factor(stat,
                           levels = c("t1", "nucleus", "crucible",
                                      "t2", "vacuum", "refugium"),
                           labels = c("temperature,\n1981-2010",
                                      "potential nuclei: 1981-2010\nlocations of future park temperature",
                                      "current dominant park temperature:\nYNP median 50%, 1981-2010",
                                      "temperature,\n2041-2070, SSP585",
                                      "future dominant park temperature:\nYNP median 50%, 2041-2070",
                                      "potential refugia: 2041-2070\nlocations of current park temp.")))


# hillshade
hs <- rast("data/hillshade.tif") %>%
      as.data.frame(xy = T)


# GYE & YNP boundaries
gye <- read_csv("data/gye.csv") %>% # from https://www.arcgis.com/home/item.html?id=b55466380b614c1dbf686d2baf149b5b%2F1000
      select(x, y) %>%
      mutate(group = 1) %>%
      st_as_sf(coords = c("x", "y")) %>%
      group_by(group) %>%
      summarise(do_union = FALSE) %>%
      st_cast("POLYGON") %>%
      st_set_crs(3857) %>%
      st_transform(crs(r))
yell <- st_read( "data/YELL_tracts/YELL_boundary.shp") %>%
      st_transform(crs(r)) %>%
      st_as_sf()


# panel lettering
lettering <- pd %>%
      select(stat, x, y) %>%
      group_by(stat) %>%
      summarize(x = min(x), y = max(y), .groups = "drop") %>%
      arrange(stat) %>%
      mutate(label = paste0("(", letters[1:6], ")"))


# construct plot
p <- ggplot() +
      facet_wrap(~stat, nrow = 2) +
      geom_raster(data = hs, aes(x, y), fill = "gray80") +
      geom_raster(data = pd, aes(x, y, fill = value)) +
      geom_raster(data = hs, aes(x, y, alpha = -hillshade), fill = "black") +
      geom_sf(data = gye, color = "black", fill = NA, size = 1) +
      geom_sf(data = yell, color = "black", fill = NA, linewidth = .5) +
      geom_text(data = lettering, aes(x, y, label = label),
                family = "HelveticaNeue-CondensedBold",
                size = 5, hjust = 0, vjust = 1) +
      scale_fill_gradientn(colors = c("cyan", "dodgerblue", "purple", "red", "orange"), na.value = "white") +
      scale_alpha(range = 0:1, guide = "none") +
      guides(fill = guide_colorbar(barwidth = 12, barheight = .5)) +
      xlim(min(pd$x) - .1, max(pd$x) + .1) +
      scale_y_continuous(limits = c(min(pd$y) - .1, max(pd$y) + .1), expand = c(0, 0)) +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank(),
            legend.position = "bottom") +
      labs(fill = "mean annual temperature (°C) ")
ggsave("figures/gye.png", p, width = 8, height = 7, units = "in", dpi = 800, device = png)
