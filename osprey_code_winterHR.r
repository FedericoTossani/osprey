
# ================================ #
#       1.SetWD and packages       #
# ================================ #
          setwd("C:/Tesi/R/osprey/data/")

         # this doesn't work, fix it!
         # source(file = "https://github.com/FedericoTossani/osprey/blob/main/osprey_code_packages.r")


         list.of.packages <- c("tidyverse",
                               "lubridate",
                               "sf", 
                               "mapview", 
                               "adehabitatLT",
                               "adehabitatHR",
                               "stargazer",
                               "kableExtra",
                               "ggmap",
                               "terra",
                               "tidyterra",
                               "gridExtra",
                               "ggpubr",
                               "DescTools")

         # with this line of code I check if alll the packages are installed and then I load it

         {
           new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

           if(length(new.packages)) install.packages(new.packages)

           lapply(list.of.packages, require, character.only = TRUE)
         }

# ================================= #
#          2.Data import            #
# ================================= #

         list_csv <- list.files(pattern = "20")
         csv_allfile <- lapply(list_csv, read.csv)

         col_selected <- c("timestamp", "location.long", "location.lat", "external.temperature", 
                           "individual.local.identifier")

         csv_file_sel_col <- lapply(csv_allfile, "[", , col_selected)
         osprey_raw <- dplyr::bind_rows(csv_file_sel_col)

         osprey_death <- read.csv("osprey_death.csv")


# ================================= #
#        3. Filtering data          #
# ================================= #

# dead ospreys

         osprey_dead <- c("Balearics2013_FOSP11-Juv_ringH7", "Corsica2013_FOSP17_Juv_ringCBK",
                          "Italy2017_FOSP43_juv_ringIBI_Agrippa", "Italy2018_FIOS45_juv_ringIAB_Anassagora")

# dataset to use for further analysis

         osprey <- osprey_raw %>%
                     dplyr::select("timestamp", "location.long", "location.lat", "external.temperature", 
                                   "individual.local.identifier")%>%
                     rename("lon"="location.long",
                            "lat"="location.lat",
                            "ext_temp"="external.temperature",
                            "id"="individual.local.identifier")%>%
                     mutate(id = as.factor(id),
                            time = as.POSIXct(timestamp, tz = "UTC"),
                            signal_interruption_cause = ifelse (id %in% osprey_dead, "Death", "GPS lifecycle"),
                            date = as_date(parse_date_time(timestamp, orders = c("%Y-%m-%d %H:%M:%S"))),
                            day = day(time),
                            month = month(time),
                            year = year(time))%>%
                     tidyr::unite(m_day, c(month, day), sep="/", remove = F)%>%
                     dplyr::left_join(osprey_death, by = c("id" = "id"))%>%
                     mutate( season = dplyr::case_when(
                                month %in% 10:12 ~ "Fall",
                                month %in%  1:3  ~ "Winter",
                                month %in%  4:6  ~ "Spring",
                                TRUE ~ "Summer"),
                             ID = dplyr::case_when(
                             id == "Balearics2013_FOSP11-Juv_ringH7" ~ "H7",
                             id == "Corsica2013_FOSP17_Juv_ringCBK" ~ "CBK",
                             id == "Corsica2014_FOSP21-Juv_ringCIV" ~ "CIV",
                             id == "Italy2014_FOSP27-Juv_ringE7_Edy" ~ "E7",
                             id == "Italy2015_FOSP30-juv-ringA7-Cook" ~ "A7",
                             id == "Italy2015_FOSP33-juv-Antares" ~ "Antares",
                             id == "Italy2016_FOSP28_juv_ringIAD_Ciccia" ~ "IAD",
                             id == "Italy2016_FOSP37-adult-ringCAM Mora" ~ "CAM",
                             id == "Italy2017_FOSP43_juv_ringIBI_Agrippa" ~ "IBI",
                             id == "Italy2018_FIOS45_juv_ringIAB_Anassagora" ~ "IAB",
                             id == "Italy2019_OrnitelaWhite_juv_ringICZ_Odaba" ~ "ICZ",
                             id == "Italy2020_FIOS21_juv_ringIBS_Mauna Loa" ~ "IBS",
                             id == "Italy2020_Ornitela_juv_ringIBH_Infiernillo" ~ "IBH",
                             id == "Italy2020_Ornitela_juv_ringIBK_Imbabura" ~ "IBK",
                             id == "Italy2022_OSPI08_juv_ringIFP_Ildebrando" ~ "IFP"))

         osprey <- osprey%>%
         select(-c("timestamp", "id"))%>%
         relocate("ID", "time", "date", "day", "month", "year", "m_day",
                              "death_date", "season", "ext_temp", "lon", "lat", "signal_interruption_cause", "death_comment")%>%
         unique()

# Convert lon and lat columns to a spatial object with WGS84 coordinate system
osp_hr_v <- vect(osprey, geom = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

# Define the desired projected coordinate system in meters. You can choose a suitable CRS for your specific location.
proj_crs <- "+proj=utm +zone=32 +datum=WGS84 +ellps:WGS84 +units=m"

# Convert the lon-lat points to the projected coordinate system
osp_hr_utm <- terra::project(osp_hr_v, proj_crs)

utm_coord <- geom(osp_hr_utm, df = T)

# Extract the X and Y coordinates in meters from the projected points
osprey <- osprey%>%
                  mutate(x = utm_coord$x,
                         y = utm_coord$y)


####################
# Homerange's List #
####################

# KernelUD al 95 e al 50 (è la core area) 

# Here I will calculate the homerange of Ospreys during the winter before the beginning of natal dispersal movements

# Let's import countries boundaries shapefile
                  countries <- vect('C:/Tesi/data/countries_boundaries_4326.shp')
                  osprey_ext <- ext(c(-7.436733, 21.24755, 35.40968, 55.77745))
                  osprey_eu <- crop(countries, osprey_ext)
                  osprey_eu_utm <- terra::project(osprey_eu, proj_crs)

# WinterHR

                   # First define the winter season
                            osp_winter <- osprey%>%
                                              dplyr::filter(season == "Winter")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            osp_winter$ID <- factor(osp_winter$ID)
          
                   # Let's create a spatialPoint object
                            osp_winter_sp <- SpatialPointsDataFrame(osp_winter[,c("x", "y")], osp_winter)   
          
                            ext_kud <- c(-874519.5, 1311590, 4064962, 5429805)
          
                   # Here I calculate the winter homerange with a Kernel Density Estimation
                            osp_winter_kde <- kernelUD(osp_winter_sp[,1], h = "href") # h = "LSCV"
          
                            winter_HR <- unlist(osp_winter_kde)

          
# "A7"
                  # First let's crop
          
                           A7_ext <- ext(c(7.00000, 17.50000, 40.00000, 46.50000 ))
                           A7_eu <- crop(countries, A7_ext)
                           A7_eu_utm <- terra::project(A7_eu, proj_crs)

                  # get A7 winter HR
                           A7_winter_HR <- getverticeshr(winter_HR$A7, percent = 50) # 50% is the value to obtain the core area of the HR
                           A7_winter_HR <- A7_winter_HR%>%
                                        dplyr::filter(group == "homerange.1") # homerange.1 is the real homerange, n° 2 is already during natal dispersal (but still in winter)

                  # fortify() function is needed to plot the winter homerange with ggplot
                           A7_winter_HR <- fortify(A7_winter_HR)

                  A7<- osprey%>%
                           filter(ID == 'A7')

                  A7_nd<- osprey_nd%>%
                           filter(ID == 'A7')

                  # Plot the winter homerange
                           A7_HR_plot <- 
                           ggplot(A7_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(A7_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = A7, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = A7_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "A7 winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           A7_HR_plot
         
# "Antares" 
          # Messy track, difficult to find separete travel
                  # First let's crop
                           Antares_ext <- ext(c(8.50000, 14.50000, 40.50000, 45.50000 ))
                           Antares_eu <- crop(countries, Antares_ext)
                           Antares_eu_utm <- terra::project(Antares_eu, proj_crs)

                 # get Antares winter HR
                            Antares_winter_HR <- getverticeshr(winter_HR$Antares, percent = 50) # 50% is the value to obtain the core area of the HR
                            Antares_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           Antares_winter_HR <- fortify(Antares_winter_HR)

                            Antares <- osprey%>%
                                     filter(ID == 'Antares')

                            Antares_nd <- osprey_nd%>%
                                     filter(ID == 'Antares')

                  # Plot the winter homerange
                           Antares_HR_plot <- 
                           ggplot(Antares_eu_utm) +
                           geom_spatvector()+
                           geom_path(data = Antares, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_polygon(Antares_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = Antares_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "Antares winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           Antares_HR_plot

# "CAM"    
                  # First let's crop
                           CAM_ext <- ext(c(5.00000, 12.00000, 39.50000, 44.50000 ))
                           CAM_eu <- crop(countries, CAM_ext)
                           CAM_eu_utm <- terra::project(CAM_eu, proj_crs)

                 # get CAM winter HR
                            CAM_winter_HR <- getverticeshr(winter_HR$CAM, percent = 50) # 50% is the value to obtain the core area of the HR
                            CAM_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           CAM_winter_HR <- fortify(CAM_winter_HR)

                            CAM <- osprey%>%
                                     filter(ID == 'CAM')

                            CAM_nd1 <- osprey_nd%>%
                                     filter(ID == "CAM" & time >= "2016-04-14 06:00:00" & time <= "2016-04-19 20:00:00")

                            CAM_nd2 <- osprey_nd%>%
                                     filter(ID == "CAM" & time >= "2016-05-03 08:00:00" & time <= "2016-05-06 18:00:00")

                            CAM_nd3 <- osprey_nd%>%
                                     filter(ID == "CAM" & time >= "2016-05-20 05:00:00" & time <= "2016-05-24 18:00:00")

                            CAM_nd4 <- osprey_nd%>%
                                     filter(ID == "CAM" & time >= "2016-07-02 23:00:00" & time <= "2016-07-06 20:00:00")

                  # Plot the winter homerange
                           CAM_HR_plot <- 
                           ggplot(CAM_eu_utm) +
                           geom_spatvector()+
                           geom_path(data = CAM, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CAM_nd1, aes(x = x, y = y, colour = "Natal dispersal first travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CAM_nd2, aes(x = x, y = y, colour = "Natal dispersal second travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CAM_nd3, aes(x = x, y = y, colour = "Natal dispersal third travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CAM_nd4, aes(x = x, y = y, colour = "Natal dispersal fourth travel"), linewidth = 0.5, lineend = "round") +
                           geom_polygon(CAM_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           labs(x = " ", y = " ", title = "CAM winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal first travel" = "red", "Natal dispersal second travel" = "orange", "Natal dispersal third travel" = "yellow", "Natal dispersal fourth travel" = "magenta"))
         
                           CAM_HR_plot

# "CBK"     
          # Doesn't work: Errore in re[[i]] : subscript fuori limite
                  # First let's crop
                           CBK_ext <- ext(c(7.0000, 12.5000, 37.5000, 44.00000))
                           CBK_eu <- crop(countries, CBK_ext)
                           CBK_eu_utm <- terra::project(CBK_eu, proj_crs)

                 # get CBK winter HR
                            CBK_winter_HR <- getverticeshr(winter_HR$CBK) # 50% is the value to obtain the core area of the HR
                            CBK_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           CBK_winter_HR <- fortify(CBK_winter_HR)

                            CBK <- osprey%>%
                                     filter(ID == 'CBK')

                            CBK_nd <- osprey_nd%>%
                                     filter(ID == 'CBK')

                  # Plot the winter homerange
                           CBK_HR_plot <- 
                           ggplot(CBK_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(CBK_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = CBK, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CBK_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "CBK winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           CBK_HR_plot

# "CIV"     
          # Doesn't work: Errore in re[[i]] : subscript fuori limite
                  # First let's crop
                           CIV_ext <- ext(c(5.0000, 12.0000, 36.0000, 44.00000))
                           CIV_eu <- crop(countries, CIV_ext)
                           CIV_eu_utm <- terra::project(CIV_eu, proj_crs)

                 # get CIV winter HR
                            CIV_winter_HR <- getverticeshr(winter_HR$CIV, percent = 50) # 50% is the value to obtain the core area of the HR
                            CIV_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           CIV_winter_HR <- fortify(CIV_winter_HR)

                            CIV <- osprey%>%
                                     filter(ID == 'CIV')

                            CIV_nd15 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time > '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00')

                            CIV_nd16 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time > '2016-03-29 00:01:00' & time < '2016-10-29 18:00:00')

                  # Plot the winter homerange
                           CIV_HR_plot <- 
                           ggplot(CIV_eu_utm) +
                           geom_spatvector()+
                           # geom_polygon(CIV_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = CIV, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd15, aes(x = x, y = y, colour = "Natal dispersal 2015"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd16, aes(x = x, y = y, colour = "Natal dispersal 2016"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "CIV winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal 2015" = "red", "Natal dispersal 2016" = "orange"))
         
                           CIV_HR_plot

# "E7"     
                  # First let's crop
                           E7_ext <- ext(c(-1.00000, 17.5000, 36.9409, 49.00000))
                           E7_eu <- crop(countries, E7_ext)
                           E7_eu_utm <- terra::project(E7_eu, proj_crs)

                 # get E7 winter HR
                            E7_winter_HR <- getverticeshr(winter_HR$E7, percent = 50) # 50% is the value to obtain the core area of the HR
                            E7_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           E7_winter_HR <- fortify(E7_winter_HR)

                            E7 <- osprey%>%
                                     filter(ID == 'E7')

                            E7_nd <- osprey_nd%>%
                                     filter(ID == 'E7')

                  # Plot the winter homerange
                           E7_HR_plot <- 
                           ggplot(E7_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(E7_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = E7, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = E7_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "E7 winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           E7_HR_plot

# "H7"      
                  # First let's crop
                           H7_ext <- ext(c(-7.0000, 8.50000, 35.5000, 45.00000))
                           H7_eu <- crop(countries, H7_ext)
                           H7_eu_utm <- terra::project(H7_eu, proj_crs)

                 # get H7 winter HR
                            H7_winter_HR <- getverticeshr(winter_HR$H7) # 50% is the value to obtain the core area of the HR
                            H7_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           H7_winter_HR <- fortify(H7_winter_HR)

                            H7 <- osprey%>%
                                     filter(ID == 'H7')

                            H7_nd <- osprey_nd%>%
                                     filter(ID == 'H7')

                  # Plot the winter homerange
                           H7_HR_plot <- 
                           ggplot(H7_eu_utm) +
                           geom_spatvector()+
                           geom_path(data = H7, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = H7_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           geom_polygon(H7_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           labs(x = " ", y = " ", title = "H7 winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           H7_HR_plot

# "IAB"     
                  # First let's crop
                           IAB_ext <- ext(c(7.00000, 17.50000, 37.00000, 45.00000 ))
                           IAB_eu <- crop(countries, IAB_ext)
                           IAB_eu_utm <- terra::project(IAB_eu, proj_crs)

                 # get IAB winter HR
                            IAB_winter_HR <- getverticeshr(winter_HR$IAB, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAB_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IAB_winter_HR <- fortify(IAB_winter_HR)

                            IAB <- osprey%>%
                                     filter(ID == 'IAB')

                            IAB_nd19 <- osprey_nd%>%
                                     filter(ID == 'IAB' & time >= '2019-05-05 06:00:00' & time <= '2019-06-10 14:00:00')

                            IAB_nd20 <- osprey_nd%>%
                                     filter(ID == 'IAB' & time >= '2020-03-16 00:00:00')

                  # Plot the winter homerange
                           IAB_HR_plot <- 
                           ggplot(IAB_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IAB_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = IAB, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IAB_nd19, aes(x = x, y = y, colour = "Natal dispersal 2019"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IAB_nd20, aes(x = x, y = y, colour = "Natal dispersal 2020"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IAB winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal 2019" = "red", "Natal dispersal 2020" = "orange"))
         
                           IAB_HR_plot

# "IAD"    
                  # First let's crop
                           IAD_ext <- ext(c(1.50000, 19.50000, 38.00000, 54.00000 ))
                           IAD_eu <- crop(countries, IAD_ext)
                           IAD_eu_utm <- terra::project(IAD_eu, proj_crs)

                 # get IAD winter HR
                            IAD_winter_HR <- getverticeshr(winter_HR$IAD, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAD_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IAD_winter_HR <- fortify(IAD_winter_HR)

                            IAD <- osprey%>%
                                     filter(ID == 'IAD')

                            IAD_nd18 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00')

                            IAD_nd19 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-05-07 15:00:00')

                  # Plot the winter homerange
                           IAD_HR_plot <- 
                           ggplot(IAD_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IAD_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = IAD, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IAD_nd18, aes(x = x, y = y, colour = "Natal dispersal 2018"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IAD_nd19, aes(x = x, y = y, colour = "Natal dispersal 2019"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IAD winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal 2018" = "red", "Natal dispersal 2019" = "orange"))
         
                           IAD_HR_plot

# "IBH"     
                  # First let's crop
                           IBH_ext <- ext(c(9.00000, 16.50000, 36.50000, 45.50000))
                           IBH_eu <- crop(countries, IBH_ext)
                           IBH_eu_utm <- terra::project(IBH_eu, proj_crs)

                 # get IBH winter HR
                            IBH_winter_HR <- getverticeshr(winter_HR$IBH) # 50% is the value to obtain the core area of the HR
                            IBH_winter_HR

                              # Errore in getverticeshr.estUD(winter_HR$IBH) : 
                              # The grid is too small to allow the estimation of home-range.
                              # You should rerun kernelUD with a larger extent parameter

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IBH_winter_HR <- fortify(IBH_winter_HR)

                            IBH <- osprey%>%
                                     filter(ID == 'IBH')

                            IBH_nd <- osprey_nd%>%
                                     filter(ID == 'IBH')

                  # Plot the winter homerange
                           IBH_HR_plot <- 
                           ggplot(IBH_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IBH_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = IBH, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBH_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBH winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           IBH_HR_plot

# "IBI"    
                  # First let's crop
                           IBI_ext <- ext(c(6.50000, 13.00000, 41.50000, 45.00000))
                           IBI_eu <- crop(countries, IBI_ext)
                           IBI_eu_utm <- terra::project(IBI_eu, proj_crs)

                 # get IBI winter HR
                            IBI_winter_HR <- getverticeshr(winter_HR$IBI, percent = 50) # 50% is the value to obtain the core area of the HR
                            IBI_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IBI_winter_HR <- fortify(IBI_winter_HR)

                            IBI <- osprey%>%
                                     filter(ID == 'IBI')

                            IBI_nd <- osprey_nd%>%
                                     filter(ID == 'IBI')

                  # Plot the winter homerange
                           IBI_HR_plot <- 
                           ggplot(IBI_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IBI_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = IBI, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBI_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBI winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           IBI_HR_plot

# "IBK"    
                  # First let's crop
                           IBK_ext <- ext(c(9.00000, 12.00000, 42.00000, 43.50000))
                           IBK_eu <- crop(countries, IBK_ext)
                           IBK_eu_utm <- terra::project(IBK_eu, proj_crs)

                 # get IBK winter HR
                            IBK_winter_HR <- getverticeshr(winter_HR$IBK, percent = 50) # 50% is the value to obtain the core area of the HR
                            IBK_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IBK_winter_HR <- fortify(IBK_winter_HR)

                            IBK <- osprey%>%
                                     filter(ID == 'IBK')

                            IBK_nd <- osprey_nd%>%
                                     filter(ID == 'IBK')

                  # Plot the winter homerange
                           IBK_HR_plot <- 
                           ggplot(IBK_eu_utm) +
                           geom_spatvector()+
                           geom_path(data = IBK, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBK_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           geom_polygon(IBK_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           labs(x = " ", y = " ", title = "IBK winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           IBK_HR_plot

# "IBS"     
                  # First let's crop
                           IBS_ext <- ext(c(2.00000, 21.00000, 39.00000, 48.00000 ))
                           IBS_eu <- crop(countries, IBS_ext)
                           IBS_eu_utm <- terra::project(IBS_eu, proj_crs)

                 # get IBS winter HR
                            IBS_winter_HR <- getverticeshr(winter_HR$IBS, percent = 50) # 50% is the value to obtain the core area of the HR
                            IBS_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IBS_winter_HR <- fortify(IBS_winter_HR)

                            IBS <- osprey%>%
                                     filter(ID == 'IBS')

                            IBS_nd <- osprey_nd%>%
                                     filter(ID == 'IBS')

                  # Plot the winter homerange
                           IBS_HR_plot <- 
                           ggplot(IBS_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IBS_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = IBS, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBS winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           IBS_HR_plot

# "ICZ"     
                  # First let's crop
                           ICZ_ext <- ext(c(10.00000, 16.00000, 37.00000, 44.00000 ))
                           ICZ_eu <- crop(countries, ICZ_ext)
                           ICZ_eu_utm <- terra::project(ICZ_eu, proj_crs)

                 # get ICZ winter HR
                            ICZ_winter_HR <- getverticeshr(winter_HR$ICZ, percent = 50) # 50% is the value to obtain the core area of the HR
                            ICZ_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           ICZ_winter_HR <- fortify(ICZ_winter_HR)

                            ICZ <- osprey%>%
                                     filter(ID == 'ICZ')

                            ICZ_nd <- osprey_nd%>%
                                     filter(ID == 'ICZ')

                  # Plot the winter homerange
                           ICZ_HR_plot <- 
                           ggplot(ICZ_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(ICZ_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = ICZ, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = ICZ_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "ICZ winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           ICZ_HR_plot

# "IFP"
                  # First let's crop
                           IFP_ext <- ext(c(4.00000, 17.00000, 41.00000, 45.00000))
                           IFP_eu <- crop(countries, IFP_ext)
                           IFP_eu_utm <- terra::project(IFP_eu, proj_crs)

                 # get IFP winter HR
                            IFP_winter_HR <- getverticeshr(winter_HR$IFP, percent = 50) # 50% is the value to obtain the core area of the HR
                            IFP_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IFP_winter_HR <- fortify(IFP_winter_HR)

                            IFP <- osprey%>%
                                     filter(ID == 'IFP')

                            IFP_nd1 <- osprey_nd%>%
                                     filter(ID == 'IFP' & time >= '2023-04-24 00:00:00' & time <= '2023-04-30 04:00:00')

                            IFP_nd2 <- osprey_nd%>%
                                     filter(ID == "IFP" & time > '2023-05-16 00:00:00' & time < '2023-06-08 20:00:00')

                  # Plot the winter homerange
                           IFP_HR_plot <- 
                           ggplot(IFP_eu_utm) +
                           geom_spatvector()+
                           geom_path(data = IFP, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IFP_nd1, aes(x = x, y = y, colour = "Natal dispersal first travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IFP_nd2, aes(x = x, y = y, colour = "Natal dispersal second travel"), linewidth = 0.5, lineend = "round") +
                           geom_polygon(IFP_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           labs(x = " ", y = " ", title = "IFP winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal first travel" = "red", "Natal dispersal second travel" = "orange"))
         
                           IFP_HR_plot
