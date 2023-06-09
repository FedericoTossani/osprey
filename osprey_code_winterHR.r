



####################
# Winter Homerange #
####################


# KernelUD al 95 a al 50 (è la core area) 

# Here I will calculate the homerange of Ospreys during the winter before the beginning of natal dispersal movements

# Let's import countries boundaries shapefile
         countries <- vect('C:/Tesi/data/countries_boundaries_4326.shp')
         europe <- countries%>%
                           filter(EU_STAT == "T")

# H7 WinterHR

         # First let's crop 
                  h7_ext <- ext(c(-7.0000, -5.30000, 35.7000, 37.00000))
                  h7_eu <- crop(countries, h7_ext)

         # First define the winter period
                  h7_winter <- osprey%>%
                                    dplyr::filter(ID == "H7" & date >= "2014-12-01" & date < "2015-04-02")%>%
                                    dplyr::select(lon, lat, date)

         # Then I will remove date column in order to create a spatial point object
                  h7_winter_2 <- h7_winter%>%
                                    select(-date)

                  h7_winter_sp <- SpatialPoints(h7_winter_2)

                  # Fix it
                  # h7_winter_sp <- crs(h7_winter_sp, value = "+proj=longlat +datum=WGS84")

         # Calculate distances between pairs of points using the euclidean method

                  # Project to UTM
                  llcoord <- st_as_sf(h7_winter_sp[, c("lon", "lat")], coords = c("lon", "lat"),
                  crs = CRS("+proj=longlat +datum=WGS84"))
                  utmcoord <- st_transform(llcoord, crs = CRS("+proj=utm +zone=32 +datum=WGS84"))

                  distances <- distance(h7_winter_sp, unit = "m")

         # Here I calculate the winter homerange with Minumum Convex Polygon
                  h7_winter_mcp <- mcp(h7_winter_sp, percent = 95)

                #  writePolyShape(h7_winter_mcp,"h7_winter_mcp")

         # Here I calculate the winter homerange with clusterhr() function    !!! IMP CONTROLLARE LA FUNZIONE !!!
         #        h7_winter_hr <- clusthr(h7_winter_sp)

         # Here I calculate the winter homerange with a Kernel Density Estimation
                  h7_winter_kde <- kernelUD(h7_winter_sp, h = "href")
                  h7_winter_khr <- getverticeshr(h7_winter_kde)

                  #  writePolyShape(h7_winter_khr, "h7_winter_khr")

         # fortify() function is needed to plot the winter homerange with ggplot
                  h7_w_hr <- fortify(h7_winter_khr)

         # Plot the winter homerange
                  h7_plot <- 
                  ggplot(h7_eu) +
                  geom_spatvector()+
                  #  geom_path(data = h7_winter, aes(x = lon, y = lat), 
                  #            linewidth = 0.5, lineend = "round", col = 'red') +
                  geom_polygon(h7_w_hr, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                    labs(x = " ", y = " ", title = "H7 2014/2015 winter homerange") +
                    theme_minimal() #+
                    theme(legend.position = "none")

                  h7_plot

# A7 WinterHR

         # First let's crop
 
                  a7_ext <- ext(c(14.13162, 16.28647, 39.53903, 41.62583 ))
                  a7_eu <- crop(countries, a7_ext)
                                                 

         # First define the winter period
                  a7_winter <- osprey%>%
                                    dplyr::filter(ID == "A7" & date >= "2016-12-01" & date < "2017-02-20")%>%
                                    dplyr::select(lon, lat, date)

         # Then I will remove date column in order to create a spatial point object
                  a7_winter_no_date <- a7_winter%>%
                                    select(-date)

                  a7_winter_sp <- SpatialPoints(a7_winter_no_date)

         # Here I calculate the winter homerange with Minumum Convex Polygon
                  a7_winter_mcp <- mcp(a7_winter_sp, percent = 95) 

                 # writePolyShape(a7_winter_mcp,"a7_winter_mcp")

         # Here I calculate the winter homerange with clusterhr() function    !!! IMP CONTROLLARE LA FUNZIONE !!!
         #         a7_winter_hr <- clusthr(a7_winter_sp)

         # Here I calculate the winter homerange with a Kernel Density Estimation
                  a7_winter_kde <- kernelUD(a7_winter_sp, h = "href")
                  a7_winter_khr <- getverticeshr(a7_winter_kde)

                  #  writePolyShape(a7_winter_khr, "a7_winter_khr")

         # fortify() function is needed to plot the winter homerange with ggplot
                  a7_w_hr <- fortify(a7_winter_khr)

         # Plot the winter homerange
                  a7_plot <- 
                  ggplot(a7_eu) +
                  geom_spatvector()+
                  #  geom_path(data = h7_winter, aes(x = lon, y = lat), 
                  #            linewidth = 0.5, lineend = "round", col = 'red') +
                  geom_polygon(a7_winter_mcp, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                    labs(x = " ", y = " ", title = "A7 2016/2017 winter homerange") +
                    theme_minimal() #+
                    theme(legend.position = "none")

                  a7_plot

# E7 WinterHR

         # First let's crop
 
                  e7_ext <- ext(c(11.48978, 13.6005, 36.9409, 39.00523))
                  e7_eu <- crop(countries, e7_ext)
                                                 
         # First define the winter period
                  e7_winter <- osprey%>%
                                    dplyr::filter(ID == "E7" & date >= "2015-12-01" & date < "2016-03-10")%>%
                                    dplyr::select(lon, lat, date)

         # Then I will remove date column in order to create a spatial point object
                  e7_winter_no_date <- e7_winter%>%
                                    select(-date)

                  e7_winter_sp <- SpatialPoints(e7_winter_no_date)

         # Here I calculate the winter homerange with Minumum Convex Polygon
                  e7_winter_mcp <- mcp(e7_winter_sp, percent = 95)

                  #  writePolyShape(e7_winter_mcp,"e7_winter_mcp")

         # Here I calculate the winter homerange with clusterhr() function    !!! IMP CONTROLLARE LA FUNZIONE !!!
         #         e7_winter_hr <- clusthr(e7_winter_sp)

         # Here I calculate the winter homerange with a Kernel Density Estimation
                  e7_winter_kde <- kernelUD(e7_winter_sp, h = "href")
                  e7_winter_khr <- getverticeshr(e7_winter_kde)

                  # writePolyShape(e7_winter_khr, "e7_winter_khr")

         # fortify() function is needed to plot the winter homerange with ggplot
                  e7_w_hr <- fortify(e7_winter_khr)

         # Plot the winter homerange
                  e7_plot <- 
                  ggplot(e7_eu) +
                  geom_spatvector()+
                  #  geom_path(data = h7_winter, aes(x = lon, y = lat), 
                  #            linewidth = 0.5, lineend = "round", col = 'red') +
                  geom_polygon(e7_winter_mcp, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                    labs(x = " ", y = " ", title = "E7 2016/2017 winter homerange") +
                    theme_minimal() #+
                    theme(legend.position = "none")

                  e7_plot

# IAD WinterHR

         # First let's crop
 
                  iad_ext <- ext(c(7.316383, 10.019932, 38.89987, 41.91255))
                  iad_eu <- crop(countries,iad_ext)
                                                 

         # First define the winter period
                  iad_winter <- osprey%>%
                                    dplyr::filter(ID == "IAD" & date >= "2017-12-01" & date < "2018-03-28")%>%
                                    dplyr::select(lon, lat, date)

         # Then I will remove date column in order to create a spatial point object
                  iad_winter_no_date <- iad_winter%>%
                                    select(-date)

                  iad_winter_sp <- SpatialPoints(iad_winter_no_date)

                  proj4string(iad_winter_sp) <- CRS("+proj=longlat +datum=WGS84")

         # Here I calculate the winter homerange with Minumum Convex Polygon
                  iad_winter_mcp <- mcp(iad_winter_sp, percent = 95)

                  # writePolyShape(iad_winter_mcp, "iad_winter_mcp")

         # Here I calculate the winter homerange with clusterhr() function    !!! IMP CONTROLLARE LA FUNZIONE !!!
         #         iad_winter_hr <- clusthr(iad_winter_sp)

         # Here I calculate the winter homerange with a Kernel Density Estimation
                  iad_winter_kde <- kernelUD(iad_winter_sp, h = "href")
                  iad_winter_khr <- getverticeshr(iad_winter_kde)

                  # writePolyShape(iad_winter_khr, "iad_winter_khr")

         # fortify() function is needed to plot the winter homerange with ggplot
                  iad_w_hr <- fortify(iad_winter_khr)

         # Plot the winter homerange    
                  iad_plot <- 
                  ggplot(iad_eu) +
                  geom_spatvector()+
                  geom_polygon(iad_w_hr, mapping = aes(x=long, y=lat, fill = "NA"), color = "black") +
                  geom_path(data = iad_winter, aes(x = lon, y = lat), linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IAD 2017/2018 winter homerange") +
                    theme_minimal() #+
                    theme(legend.position = "none")

                  iad_plot

# CIV Winter HR in 2014/2015 poi stat solo primo viaggio (analizza poi anche il secondo)


# Ospreys WinterHR

         # First let's crop
 
                  osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
                  osprey_eu <- crop(countries, osprey_ext)
                                                 

         # First define the winter period
                  osprey_last_winter <- osprey%>%
                                    dplyr::filter(ID == "H7" & date >= "2014-12-01" & date < "2015-04-02" |
                                                  ID == "A7" & date >= "2016-12-01" & date < "2017-02-20" |
                                                  ID == "E7" & date >= "2015-12-01" & date < "2016-03-10" |
                                                  ID == "IAD" & date >= "2017-12-01" & date < "2018-03-28"
                                                 )%>%
                                    dplyr::select(ID, lon, lat, date)

         # Then I will remove date column in order to create a spatial point object
                  osprey_last_winter_no_date <- osprey_last_winter%>%
                                    select(-date)
                  
                  osprey_last_winter_no_date$ID <- as.factor(osprey_last_winter_no_date$ID)
         
                  coords <- c(osprey_last_winter_no_date$lon, osprey_last_winter_no_date$lat)
                  coords <- coordinates(coords)
                  osprey_last_winter_sp <- SpatialPointsDataFrame(coords, osprey_last_winter_no_date)

                  proj4string(osprey_last_winter_sp) <- CRS("+proj=longlat +datum=WGS84")

         # Here I calculate the winter homerange with Minumum Convex Polygon
                  osprey_last_winter_mcp <- mcp(iad_winter_sp, percent = 95) 

         # Here I calculate the winter homerange with clusterhr() function    !!! IMP CONTROLLARE LA FUNZIONE !!!
         #         iad_winter_hr <- clusthr(iad_winter_sp)

         # Here I calculate the winter homerange with a Kernel Density Estimation
                  osprey_last_winter_kde <- kernelUD(iad_winter_sp, h = "href")
                  osprey_last_winter_khr <- getverticeshr(iad_winter_kde)

         # fortify() function is needed to plot the winter homerange with ggplot
                 ospreys_w_hr <- fortify(osprey_last_winter_khr)

         # Plot the winter homerange    
                  iad_plot <- 
                  ggplot(iad_eu) +
                  geom_spatvector()+
                  geom_polygon(ospreys_w_hr, mapping = aes(x=long, y=lat, fill = "NA"), color = "black") +
                  geom_path(data = osprey_last_winter, aes(x = lon, y = lat), linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IAD 2017/2018 winter homerange") +
                    theme_minimal() #+
                    theme(legend.position = "none")

                  iad_plot

