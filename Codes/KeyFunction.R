if (!require(sf)) install.packages('sf')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(tidyr)) install.packages('tidyr')
if (!require(dplyr)) install.packages('dplyr')

library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)

xhrt <- function(t) 16 * sin(t)^3
yhrt <- function(t) 13 * cos(t) - 5 * cos(2 * t) - 2 * cos(3 * t) - cos(4 * t)

heart_sf <- tibble(t = seq(0, 2 * pi, by = .1)) %>%
  mutate(y = yhrt(t),
         x = xhrt(t)) %>%
  bind_rows(., head(., 1)) %>%
  dplyr::select(x, y) %>%
  as.matrix() %>%
  list() %>%
  st_polygon() %>%
  st_sfc(crs = 2154)

g1 <-
  ggplot(heart_sf) +
  geom_sf(fill = "#cb181d", color = '#cb181d') +
  annotate('text', x = 0, y = 0, label = 'H. Geng', size = 10, fontface = "italic") +
  coord_sf(crs = 2154, datum = 2154) +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

dev.new()
plot(g1)

dev.new()
plot(g1)
