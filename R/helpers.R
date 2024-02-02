# Copyright (C) 2022-2022 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

.getCity <- function(name) {
  if (is.null(name)) {
    city <- .randomCity(NULL)
  } else {
    if (inherits(name, "data.frame")) {
      stopifnot("input data frame is missing 'name' column" = "name" %in% colnames(name))
      stopifnot("input data frame is missing 'country' column" = "country" %in% colnames(name))
      stopifnot("input data frame is missing 'lat' column" = "lat" %in% colnames(name))
      stopifnot("input data frame is missing 'long' column" = "long" %in% colnames(name))
      city <- name
    } else {
      dataset <- rcityviews::cities
      indexes <- which(dataset[["name"]] == name)
      index <- .resolveConflicts(name, indexes, dataset)
      if (is.null(index)) {
        return(NULL)
      }
      city <- dataset[index, ]
    }
  }
  return(city)
}

.randomCity <- function(seed) {
  set.seed(seed)
  dataset <- rcityviews::cities
  dataset <- subset(dataset, dataset[["population"]] > 200000)
  index <- sample.int(nrow(dataset), size = 1)
  selected <- dataset[index, ]
  return(selected)
}

.resolveConflicts <- function(name, indexes, dataset) {
  index <- indexes
  if (length(indexes) == 0) {
    stop(paste0("There is no city called '", name, "' in the available data.\nUse 'new_city()' or create an issue including lat/long coordinates at https://github.com/koenderks/rcityviews/issues."))
  } else if (length(indexes) > 1) {
    selection <- utils::menu(
      choices = paste0(dataset[indexes, 1], ", ", dataset[indexes, 2], " | Lat: ", round(dataset[indexes, 3], 3), " | Long: ", round(dataset[indexes, 4], 3)),
      title = "More than one city matched to this name, which one to pick?"
    )
    if (selection == 0) {
      return(NULL)
    }
    index <- indexes[selection]
  }
  return(index)
}

# .tick <- function(verbose, progBar, ticks, shiny) {
#   if (shiny) {
#     shiny::incProgress(amount = 1 / ticks)
#   } else {
#     if (verbose) {
#       progBar$tick()
#     }
#   }
# }

.themeOptions <- function(theme) {
  colors <- switch(theme,
  
# Kolory z oryginalnego pakietu usunięte
    
# Kolory dodane przeze mnie
    "prism" = list(
      "background" = NA,
      "water" = "#1D6996",
      "landuse" = "#73AF48",
      "contours" = "#666666",
      "streets" = "#EDAD08",
      "rails" = c("#94346E", "#6F4070"),
      "buildings" = "#E17C05",
      "text" = NA,
      "waterlines" = "#1D6996"
    ),
    "prism2" = list(
      "background" = NA,
      "water" = "#2589c2",
      "landuse" = "76C16A",
      "contours" = "#1d1d1d",
      "streets" = "#FF8700",
      "rails" = c("#ED3F29", "#b02d1e"),
      "buildings" = "#DC7E76",
      "text" = NA,
      "waterlines" = "#2589c2"
    ),
    "darkmint" = list(
      "background" = "#D2FBD4",
      "water" = "#123F5A",
      "landuse" = "#A5DBC2",
      "contours" = "#235D72",
      "streets" = "#559C9E",
      "rails" = c("#A5DBC2", "#7BBCB0"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#123F5A"
    ),
    "grey" = list(
      "background" = "#f2f2f2",
      "water" = "#cccccc",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#595959",
      "rails" = c("#7f7f7f", "#f2f2f2"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#cccccc"
    ),
    "grey_blue" = list(
      "background" = "#f2f2f2",
      "water" = "#199ee5",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#595959",
      "rails" = c("#7f7f7f", "#f2f2f2"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#199ee5"
    ),
    "grey_blue2" = list(
      "background" = "#f2f2f2",
      "water" = "#bed7e5",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#595959",
      "rails" = c("#7f7f7f", "#f2f2f2"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#bed7e5"
    ),
    "grey_blue3" = list(
      "background" = "#ffffff",
      "water" = "#f0f0f0",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#323232",
      "rails" = c("#595959", "#f2f2f2"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#f0f0f0"
    ),
    "bw" = list(
      "background" = "#232323",
      "water" = "#ffffff",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#ffffff",
      "rails" = c("#f0f0f0", "#000000"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = NA
    ),
    "bw_black" = list(
      "background" = "#000000",
      "water" = "#ffffff",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#ffffff",
      "rails" = c("#f0f0f0", "#000000"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = NA
    ),
    "wb" = list(
      "background" = "#ffffff",
      "water" = "#232323",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#232323",
      "rails" = c("#f0f0f0", "#000000"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = NA
    ),
    "wb_blue" = list(
      "background" = "#ffffff",
      "water" = "#199ee5",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#323232",
      "rails" = c("#f0f0f0", "#323232"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#199ee5"
    ),
    "wb_blue2" = list(
      "background" = "#ffffff",
      "water" = "#bed7e5",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#323232",
      "rails" = c("#f0f0f0", "#323232"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#bed7e5"
    ),
    "wb_blue3" = list(
      "background" = "#ffffff",
      "water" = "#bed7e5",
      "landuse" = "#deffdf",
      "contours" = NA,
      "streets" = "#323232",
      "rails" = c("#f0f0f0", "#323232"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#bed7e5"
    ),
    "wb_red" = list(
      "background" = NA,
      "water" = "#ff0000",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#ff00ff",
      "rails" = c("#f0f0f0", "#323232"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#ff0000"
    ),
    "wb_alpha" = list(
      "background" = NA,
      "water" = "#ffffff",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#232323",
      "rails" = c("#f0f0f0", "#000000"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#ffffff"
    ),
    "wb_black" = list(
      "background" = "#ffffff",
      "water" = "#000000",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#000000",
      "rails" = c("#f0f0f0", "#000000"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = NA
    ),
    "sepia1" = list(
      "background" = "#f8edeb",
      "water" = "#FEC89A",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#F9DCC4",
      "rails" = c("#F8EDEB", "#FCD5CE"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#FEC89A"
    ),
    "blond1" = list(
      "background" = "#babbbd",
      "water" = "#4b9475",
      "landuse" = "#babbbd",
      "contours" = "#5b5152",
      "streets" = "#e282af",
      "rails" = "#5b5152",
      "buildings" = "#5b5152",
      "text" = "#5b5152",
      "waterlines" = "#4b9475",
      "textshadow" = "#9a7958"
    ),
    "blond2" = list(
      "background" = "#ffffff",
      "background" = "#fff7d8",
      "water" = "#169873",
      "landuse" = "#babbbd",
      "landuse" = "#FFD3BA",
      "contours" = "#5b5152",
      "streets" = "#5b5152",
      "rails" = c("#9EBD6E", "#ffffff"),
      "buildings" = c("#FFD3BA", "#FAB9BB","#F49FBC", "#805D93"),
      "rails" = c("#9EBD6E", "#fff7d8"),
      "buildings" = c("#babbbd", "#FAB9BB","#F49FBC", "#805D93"),
      "text" = "#32130f",
      "waterlines" = "#169873"
    ),
    "blond3" = list(
      "background" = "#ffffff",
      "water" = "#98a89d",
      "landuse" = "#ffffff",
      "water" = "#169873",
      "landuse" = "#babbbd",
      "contours" = "#5b5152",
      "streets" = "#5b5152",
      "rails" = c("#5b5152", "#ffffff"),
      "buildings" = c("#b79798", "#9a7958", "#babbbd"),
      "text" = "#5b5152",
      "rails" = c("#9EBD6E", "#ffffff"),
      "buildings" = c("#FFD3BA", "#FAB9BB","#F49FBC", "#805D93"),
      "text" = "#32130f",
      "waterlines" = "#169873"
    ),
    "minimal_green" = list(
      "background" = "#FFFFFF",
      "water" = "#169873",
      "landuse" = "#FFFFFF",
      "contours" = "#169873",
      "streets" = "#169873",
      "rails" = c("#169873", "#FFFFFF"),
      "buildings" = c("#169873"),
      "text" = "#169873",
      "waterlines" = "#169873"
    ),
    "minimal_green_invert" = list(
      "background" = "#000000",
      "water" = "#169873",
      "landuse" = "#000000",
      "contours" = "#169873",
      "streets" = "#169873",
      "rails" = c("#169873", "#000000"),
      "buildings" = c("#169873"),
      "text" = "#169873",
      "waterlines" = "#169873"
    )
  )
  font <- switch(theme,
    "grey" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
    "grey_blue" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
    "grey_blue2" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
    "grey_blue3" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
    "bw" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
    "wb" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
    "bw_black" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
    "wb_black" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
    "blond1" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "blond2" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "blond3" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "minimal_green" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "minimal_green_invert" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "darkmint" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "prism" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "prism2" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "wb_blue" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "wb_blue2" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "wb_blue3" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "wb_red" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "wb_alpha" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "sepia1" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    )
  )
  size <- list()
  size[["borders"]] <- list(
    "contours" = 0.15,
    "water" = 0.4,
    "canal" = 0.5,
    "river" = 0.6
  )
#   size[["streets"]] <- list(
#     "path" = 0.2,
#     "residential" = 0.3,
#     "structure" = 0.35,
#     "tertiary" = 0.4,
#     "secondary" = 0.5,
#     "primary" = 0.6,
#     "motorway" = 0.8,
#     "rails" = 0.75,
#     "runway" = 3
#   )
  size[["streets"]] <- list(
    "path" = 0.75,
    "residential" = 1,
    "structure" = 2.5,
    "tertiary" = 2,
    "secondary" = 3,
    "primary" = 4,
    "motorway" = 5,
    "rails" = 2.5,
    "runway" = 3
  )
  themeOptions <- list(
    "colors" = colors,
    "font" = font,
    "size" = size
  )
  return(themeOptions)
}
