# télécharger la BAN depuis https://adresse.data.gouv.fr/ et dézipper les fichier csv

setwd("./BAN_licence_gratuite_repartage")

# import de l'intégralité de la BAN
library(data.table)

files <- list.files(path = "./BAN_licence_gratuite_repartage", pattern = ".csv")
temp <- lapply(files, fread, sep=";",dec=".", header=T,stringsAsFactors = FALSE)
BAN <- rbindlist( temp )

# ou seul département
BAN <- fread("BAN_licence_gratuite_repartage_35.csv", sep=";",dec=".", ,stringsAsFactors = FALSE)

library(dplyr)

# sélection des numéros d'une commune en particulier (ici Rennes)
nums <-
  BAN %>%
  as.data.frame() %>%
  filter(rep %in% '' & !numero %in% '' & !code_insee %in% '' & !nom_afnor %in% '') %>%
  select(code_insee, nom_afnor,numero, x, y) %>%
  mutate(id_rue = paste0(code_insee,"_",nom_afnor)) %>%
  arrange(id_rue) %>%
  filter(code_insee %in% c('35238') )

# identifiant unique de chaque rue
nums_rues <-
  nums %>%
  distinct(id_rue) %>%
  mutate(id_rue_nn = row_number())
  
# liste des numéros avec identifiant unique des rues
nums <-
  nums %>%
  left_join(nums_rues,
            by = c("id_rue" = "id_rue")
  ) 


###################################
## fonction de création des lignes rues à partir des points adresses
## source : https://rpubs.com/walkerke/points_to_line

library(sp)
library(maptools)
library(rgdal)
library(rgeos)

points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
  
  # Convert to SpatialPointsDataFrame
  coordinates(data) <- c(long, lat)
  
  # If there is a sort field...
  if (!is.null(sort_field)) {
    if (!is.null(id_field)) {
      data <- data[order(data[[id_field]], data[[sort_field]]), ]
    } else {
      data <- data[order(data[[sort_field]]), ]
    }
  }
  
  # If there is only one path...
  if (is.null(id_field)) {
    
    lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
    
    return(lines)
    
    # Now, if we have multiple lines...
  } else if (!is.null(id_field)) {  
    
    # Split into a list by ID field
    paths <- sp::split(data, data[[id_field]])
    
    sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
    
    # I like for loops, what can I say...
    for (p in 2:length(paths)) {
      id <- paste0("line", as.character(p))
      l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
      sp_lines <- spRbind(sp_lines, l)
    }
    
    return(sp_lines)
  }
}

# création des lignes
v_lines <- points_to_line(data = nums, 
                          long = "x", 
                          lat = "y", 
                          id_field = "id_rue_nn", 
                          sort_field = "numero")

# récupération des infos par rue
v_lines$id <- row.names(v_lines)
v_lines@data <-
  v_lines@data %>%
  mutate(id_rue_nn = row_number()) %>%
left_join (nums_rues,
           by = c("id_rue_nn" = "id_rue_nn")
) 


# affichage des lignes
plot(v_lines)

# zoom sur une ville du centre
place_rep <- subset(v_lines, id_rue %in% "35238_PLACE DE LA REPUBLIQUE")

# et sortie de l'image en png
setwd("..")
png("croquis_ban.png", width=20, height=16, units="cm", res=350)
par(mar = rep(0, 4))

plot(gBuffer(place_rep, width=2000),border="white")
plot(v_lines, col="#545E6E",add=T)

dev.off()
