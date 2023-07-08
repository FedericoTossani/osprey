

# =============================== #
#       1.SetWD & packages       #
# =============================== #

# Here you will load all the packages needed and also the working directory
          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_WD_packages.r")

# =========================================== #
#          2.Data import & selection          #
# =========================================== #

# Here you will load the raw dataset and the filtered one 
          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_data_import_selection.r")

# ===================================== #
#          3.Basemaps & extent          #
# ===================================== #

# Here you will load all the maps needed and also the cropped geographic extent for all individual ospreys
          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_basemap_extent.r")




# Experiments to categorize fixes with HMM


# visualize the data set
ggplot(data, aes(lon, lat, col = ID)) +
geom_point(size = 0.5) +
geom_path() +
coord_map("mercator")


data_reg <- crawlWrap(data_sf, timeStep = "1 hour")

ggplot(data_pred, aes(lon, lat, col = ID)) +
geom_point(size = 0.5) +
geom_path()

data_pred <- data_reg$crwPredict





# split at gap function 

          split_at_gap <- function(data, max_gap = 60, shortest_track = 0) {
              # Number of tracks
              n_tracks <- length(unique(data$ID))

              # Save old ID and reinitialise ID column
              data$ID_old <- data$ID
              data$ID <- character(nrow(data))

              # Loop over tracks (i.e., over IDs)
              for(i_track in 1:n_tracks) {
                  # Indices for this track
                  ind_this_track <- which(data$ID_old == unique(data$ID_old)[i_track])
                  track_length <- length(ind_this_track)

                  # Time intervals in min
                  dtimes <- difftime(data$time[ind_this_track[-1]], 
                                     data$time[ind_this_track[-track_length]],
                                     units = "mins")

                  # Indices of gaps longer than max_gap
                  ind_gap <- c(0, which(dtimes > max_gap), track_length)

                  # Create new ID based on split track
                  subtrack_ID <- rep(1:(length(ind_gap) - 1), diff(ind_gap))
                  data$ID[ind_this_track] <- paste0(data$ID_old[ind_this_track], "-", subtrack_ID)
              }

              # Only keep sub-tracks longer than some duration
              track_lengths <- sapply(unique(data$ID), function(id) {
                  ind <- which(data$ID == id)
                  difftime(data$time[ind[length(ind)]], data$time[ind[1]], units = "min")
              })
              ID_keep <- names(track_lengths)[which(track_lengths >= shortest_track)]
              data <- subset(data, ID %in% ID_keep)

              return(data)
          }

# Regularise tracks with foieGras

fit <- fit_ssm(data, vmax= 4, model = "crw", time.step = 24, control = ssm_control(verbose = 0, se = FALSE))

fmp <- fit_mpm(fit, what = "predicted", model = "jmpm", control = mpm_control(verbose = 0))

plot(fmp, pages = 1, ncol = 3, pal = "Zissou1", rev = TRUE)



fmap(fit, fmp, what = "predicted", pal = "Cividis")



# Regularise tracks with crawl packages


data_sf <- sf::st_as_sf(data, coords = c("lon","lat")) %>% 
  sf::st_set_crs(4326)

data_sf_lines <- data_sf %>% 
  dplyr::arrange(ID, time) %>% 
  sf::st_geometry() %>% 
  sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(data_sf$ID))) %>% 
  sf::st_cast("MULTILINESTRING") %>% 
  sf::st_sf(ID = as.factor(unique(data_sf$ID)))


esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

ggplot() + 
  annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
  layer_spatial(data_sf, size = 0.75,aes(color = ID)) +
  scale_x_continuous(expand = expand_scale(mult = c(.6, .6))) +
  #scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  facet_wrap(~ID)+
  ggtitle("Observed Location Paths", 
          subtitle = "Ospreys (n=14), Mediterranean basin, UE")


# 

data_sf <- sf::st_transform(data_sf, 3857)

sf::st_geometry(data_sf)

data_sf <- data_sf %>%
  dplyr::mutate(
    error_semi_major_axis = ifelse(type == 'FastGPS', 50,
                                   error_semi_major_axis),
    error_semi_minor_axis = ifelse(type == 'FastGPS', 50,
                                   error_semi_minor_axis),
    error_ellipse_orientation = ifelse(type == 'FastGPS', 0,
                                       error_ellipse_orientation)
    )

crawl::crwMLE(data_sf)





