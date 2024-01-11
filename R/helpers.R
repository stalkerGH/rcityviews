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
  
# Kolory z oryginalnego pakietu  
    "vintage" = list(
      "background" = "#fff7d8",
      "water" = "#9ebfaa",
      "landuse" = "#fff7d8",
      "contours" = "#32130f",
      "streets" = "#32130f",
      "rails" = c("#32130f", "#fff7d8"),
      "buildings" = c("#facc87", "#f39848", "#f8c98c", "#f58762"),
      "text" = NA,
      "waterlines" = "#9ebfaa"
    ),
    "modern" = list(
      "background" = "#e6ddd6",
      "water" = "#656c7c",
      "landuse" = "#7c9c6b",
      "contours" = "#e6ddd6",
      "streets" = "#fafafa",
      "rails" = c("#fafafa", "#e6ddd6"),
      "buildings" = "#eb3e20",
      "text" = NA,
      "waterlines" = "#656c7c"
    ),
    "bright" = list(
      "background" = "#eeefc9",
      "water" = "#9ddffb",
      "landuse" = c("#f2f4cb", "#d0f1bf", "#64b96a"),
      "contours" = "#eeefc9",
      "streets" = "#2f3737",
      "rails" = c("#2f3737", "#eeefc9"),
      "buildings" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
      "text" = NA,
      "waterlines" = "#9ddffb"
    ),
    "delftware" = list(
      "background" = "#fafafa",
      "water" = "#fafafa",
      "landuse" = c("#7ebaee", "#8da8d7", "#3259a6", "#0c133f", "#080e1c"),
      "contours" = "#fafafa",
      "streets" = "#1F305E",
      "rails" = c("#1F305E", "#fafafa"),
      "buildings" = c("#7ebaee", "#8da8d7", "#3259a6", "#0c133f", "#080e1c"),
      "text" = NA,
      "waterlines" = "#fafafa"
    ),
    "comic" = list(
      "background" = "#ffffff",
      "water" = "#607ba4",
      "landuse" = "#4b9475",
      "contours" = "#222222",
      "streets" = "#222222",
      "rails" = c("#222222", "#ffffff"),
      "buildings" = c("#f4d749", "#daa520", "#a63c44"),
      "text" = NA,
      "waterlines" = "#607ba4"
    ),
    "rouge" = list(
      "background" = "#a25543",
      "water" = "#f2deb8",
      "landuse" = "#a25543",
      "contours" = "#f2deb8",
      "streets" = "#f2deb8",
      "rails" = c("#f2deb8", "#a25543"),
      "buildings" = "#f2deb8",
      "text" = NA,
      "waterlines" = "#f2deb8"
    ),
    "original" = list(
      "background" = "#fdf9f5",
      "water" = "#fdf9f5",
      "landuse" = "#fdf9f5",
      "contours" = "#32130f",
      "streets" = "#32130f",
      "rails" = "#32130f",
      "buildings" = "#fdf9f5",
      "text" = NA,
      "waterlines" = "#32130f"
    ),
    "midearth" = list(
      "background" = "#b8a580",
      "water" = "#c3c9b6",
      "landuse" = "#b8a580",
      "contours" = "#53402a",
      "streets" = "#221c18",
      "rails" = "#221c18",
      "buildings" = "#53402a",
      "text" = NA,
      "waterlines" = "#c3c9b6",
      "textshadow" = NA
    ),
    "batik" = list(
      "background" = "#161417",
      "water" = "#214040",
      "landuse" = c("#ece3d9", "#9e5426", "#5d473c", "#C0b28a"),
      "contours" = "#1d1d23",
      "streets" = "#d7c5b8",
      "rails" = "#d7c5b8",
      "buildings" = c("#ece3d9", "#9e5426", "#5d473c", "#c0b28a"),
      "text" = NA,
      "waterlines" = "#214040"
    ),
    "vice" = list(
      "background" = "#ffffff",
      "water" = "#a3bff4",
      "landuse" = "#6ece92",
      "contours" = "#000000",
      "streets" = "#e282af",
      "rails" = "#e282af",
      "buildings" = "#fff01f",
      "text" = NA,
      "waterlines" = "#a3bff4",
      "textshadow" = "#e282af"
    ),
    "auburn" = list(
      "background" = "#FFFFFF",
      "water" = "#a8e1e6",
      "landuse" = "#8BB174",
      "contours" = "#32130f",
      "streets" = "#2F3737",
      "rails" = c("#F2F4CB", "#FFFFFF"),
      "buildings" = c("#433633", "#FF5E5B", "#FF5E5B"),
      "text" = "#343b47",
      "waterlines" = "#a8e1e6"
    ),
    
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
      "background" = "#eef6f8",
      "water" = "#02306a",
      "landuse" = NA,
      "contours" = NA,
      "streets" = "#5b8e98",
      "rails" = c("#eef6f8", "#01022b"),
      "buildings" = NA,
      "text" = NA,
      "waterlines" = "#02306a"
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
    "peach" = list(
      "background" = "#F9EFDC",
      "water" = "#a1e3ff",
      "landuse" = "#64B96A",
      "contours" = "#32130f",
      "streets" = "#2F3737",
      "rails" = c("#F2F4CB", "#F9EFDC"),
      "buildings" = c("#FFC857", "#E9724C","#C5283D"),
      "text" = "#343b47",
      "waterlines" = "#85c9e6"
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
    "vintage" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "modern" = list(
      "family" = "Imbue",
      "face" = "plain",
      "scale" = 0
    ),
    "bright" = list(
      "family" = "Damion",
      "face" = "plain",
      "scale" = 0
    ),
    "delftware" = list(
      "family" = "Dancing Script",
      "face" = "bold",
      "scale" = 0
    ),
    "comic" = list(
      "family" = "Rampart One",
      "face" = "plain",
      "scale" = 0
    ),
    "rouge" = list(
      "family" = "Oswald",
      "face" = "bold",
      "scale" = 0
    ),
    "blues" = list(
      "family" = "Oswald",
      "face" = "bold",
      "scale" = 0
    ),
    "original" = list(
      "family" = "Caveat",
      "face" = "bold",
      "scale" = 0
    ),
    "midearth" = list(
      "family" = "American Uncial Regular",
      "face" = "plain",
      "scale" = 0
    ),
    "batik" = list(
      "family" = "Walter Turncoat",
      "face" = "plain",
      "scale" = 0
    ),
    "vice" = list(
      "family" = "Rage",
      "face" = "bold",
      "scale" = 0
    ),
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
    "peach" = list(
      "family" = "Fredericka the Great",
      "face" = "plain",
      "scale" = 0
    ),
    "auburn" = list(
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
