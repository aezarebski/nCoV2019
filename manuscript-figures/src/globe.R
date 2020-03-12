library("sf")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")


#' Use a projection of the world as it appears from space centered near India.
#'
#'   https://spatialreference.org/ref/sr-org/6980/
#'
proj_string <- "+proj=ortho +lat_0=22 +lon_0=80 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs "

world <- ne_countries(scale = "medium", returnclass = "sf")

cases <- read.csv("../covid19/data/clean-outside-hubei.csv", stringsAsFactors = FALSE)
cases <- subset(cases, select = c(longitude, latitude))
cases <- subset(cases, subset = !is.na(latitude))
cases <- subset(cases, subset = !is.na(longitude))
#' Use k-means clustering so we only need to draw a relatively small number of points.
tmp <- kmeans(cases, centers = 100)
cases <- as.data.frame(tmp$centers)
cases$number <- tmp$size
rm(tmp)
#' We need to specify that these points are given as latitudes and longitudes.
cases <- st_as_sf(cases, coords = c("longitude", "latitude"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")


ggplot(data = world) +
    geom_sf() +
    geom_sf(data = cases, aes(size = number), colour = "red") +
    coord_sf(crs = proj_string) +
    scale_size(range = c(1, 10), trans = "identity") +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_line(color = gray(0.5), linetype = "solid", size = 1))

