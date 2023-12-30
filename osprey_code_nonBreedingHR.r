
# ================================ #
#       1.SetWD and packages       #
# ================================ #

          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_WD_packages.r")

# =========================================== #
#          2.Data import & selection          #
# =========================================== #

          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_data_import_selection.r")

# ===================================== #
#          3.Basemaps & extent          #
# ===================================== #

          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_basemap_extent.r")

###########################
#     Non-breeding HR     #
###########################

# "A7" OK

                    # First define the non-breeding period
                            A7_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == "A7")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            A7_nonb$ID <- factor(A7_nonb$ID)
          
                   # Let's create a spatialPoint object
                            A7_nonb_sp <- SpatialPointsDataFrame(A7_nonb[,c("x", "y")], A7_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            A7_nonb_kde <- kernelUD(A7_nonb_sp[,1], h = "href", grid = 750) # h = "LSCV"


                  # get A7 non-breeding HR
                           A7_nonb_HR <- getverticeshr(A7_nonb_kde, percent = 95)
                           A7_nonb_HR

                           A7_nonb_HRcore <- getverticeshr(A7_nonb_kde, percent = 50)
                           A7_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           A7_nonb_HR <- fortify(A7_nonb_HR)
                           A7_nonb_HRcore <- fortify(A7_nonb_HRcore)

                  A7_nonb1 <- osprey_nonb%>%
                           filter(ID == 'A7' & time >= '2015-08-18 12:00:00' & time <= '2017-02-20 00:00:00')

                  A7_nonb2 <- osprey_nonb%>%
                           filter(ID == 'A7' & time >= '2017-03-01 20:00:00' & time <= '2017-03-17 00:00:00')

                  A7_nonb3 <- osprey_nonb%>%
                           filter(ID == 'A7' & time >= '2017-06-12 20:00:00' & time <= '2017-07-12 18:00:00')

                  A7_nonb4 <- osprey_nonb%>%
                           filter(ID == 'A7' & time >= '2017-07-28 06:00:00')

                  A7_nd1 <- osprey_nd%>%
                           filter(ID == 'A7' & time >= '2015-08-14 09:00:00' & time <= '2015-08-18 12:00:00')

                  A7_nd2 <- osprey_nd%>%
                           filter(ID == 'A7' & time >= '2017-02-20 00:00:00' & time <= '2017-03-01 20:00:00')

                  A7_nd3 <- osprey_nd%>%
                           filter(ID == 'A7' & time >= '2017-03-17 00:00:00' & time <= '2017-06-12 20:00:00')

                  A7_nd4 <- osprey_nd%>%
                           filter(ID == 'A7' & time >= '2017-07-12 18:00:00' & time <= '2017-07-28 06:00:00')

                  # Plot the NonBreeding HR
                           A7_HR_plot <- 
                           ggplot() + #A7_eu_utm
                           geom_spatvector()+
                           geom_polygon(A7_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                           geom_polygon(A7_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                           geom_path(data = A7_nonb1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = A7_nonb2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = A7_nonb3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = A7_nonb4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = A7_nd1, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = A7_nd2, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = A7_nd3, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = A7_nd4, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "A7 non-breeding HR and ND movements tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                          "Natal dispersal 1st travel" = "blue",
                                                                          "Natal dispersal 2nd travel" = "orange",
                                                                          "Natal dispersal 3rd travel" = "brown",
                                                                          "Natal dispersal 4th travel" = "magenta")) +
                            scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                              "Non-Breeding HR core area" = "red"))
         
                           A7_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/A7_HR_ND_plot.jpg", plot = A7_HR_plot)
         

# "E7" OK

                    # First define the non-breeding period
                            E7_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'E7')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            E7_nonb$ID <- factor(E7_nonb$ID)
          
                   # Let's create a spatialPoint object
                            E7_nonb_sp <- SpatialPointsDataFrame(E7_nonb[,c("x", "y")], E7_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            E7_nonb_kde <- kernelUD(E7_nonb_sp[,1], h = "href", grid=750) # h = "LSCV"

                 # get E7 non-breeding HR
                            E7_nonb_HR <- getverticeshr( E7_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            E7_nonb_HR

                            E7_nonb_HRcore <- getverticeshr( E7_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            E7_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           E7_nonb_HR <- fortify(E7_nonb_HR)
                           E7_nonb_HRcore <- fortify(E7_nonb_HRcore)

                            E7_nd1 <- osprey_nd%>%
                                     filter(ID == 'E7' & time >= '2014-08-16 08:30:00' & time <= '2014-08-27 13:00:00')

                            E7_nd2 <- osprey_nd%>%
                                     filter(ID == 'E7' & time >= '2016-03-10 05:00:00')

                  # Plot the non-breeding homerange
                                     E7_HR_plot <- 
                                     ggplot() +  #E7_eu_utm
                                     geom_spatvector()+
                                     geom_polygon(E7_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                                     geom_polygon(E7_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                                     geom_path(data = E7_nonb, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                                     #geom_path(data = E7_nd1, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                                     #geom_path(data = E7_nd2, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                                     labs(x = " ", y = " ", title = "E7 non-breeding HR and ND movements tracks") +
                                     theme_minimal()+
                                     scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                                    "Natal dispersal 1st travel" = "blue",
                                                                                    "Natal dispersal 2nd travel" = "orange")) +
                                      scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                                        "Non-Breeding HR core area" = "red"))
                   
                                     E7_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/E7_HR_ND_plot.jpg", plot = E7_HR_plot)

          
          # Non-breeding ltraj object

                       # Let's create a ltraj object with UTM coordinates
                              
                            E7_nonb_lt <- osprey_nonb%>%
                                              dplyr::filter(ID == 'E7')
         
                            E7_nonb_lt <- as.ltraj(E7_nonb_lt[, c("x", "y")],
                                                  date = E7_nonb_lt$time, 
                                                  id = E7_nonb_lt$ID,
                                                  typeII=TRUE)

# "H7" OK

                    # First define the non-breeding period
                            H7_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'H7')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            H7_nonb$ID <- factor(H7_nonb$ID)
          
                   # Let's create a spatialPoint object
                            H7_nonb_sp <- SpatialPointsDataFrame(H7_nonb[,c("x", "y")], H7_nonb)

          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            H7_nonb_kde <- kernelUD(H7_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get H7 non-breeding HR
                            H7_nonb_HR <- getverticeshr(H7_nonb_kde, percent = 95)
                            H7_nonb_HR

                            H7_nonb_HRcore <- getverticeshr(H7_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            H7_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           H7_nonb_HR <- fortify(H7_nonb_HR)
                           H7_nonb_HRcore <- fortify(H7_nonb_HRcore)

                    H7_nd1 <- osprey_nd%>%
                                     filter(ID == 'H7' & time >= '2013-08-04 09:30:00' & time <= '2013-08-09 15:30:00')

                    H7_nd2 <- osprey_nd%>%
                                     filter(ID == 'H7' & time >= '2015-04-02 05:00:00')

                  # Plot the non-breeding homerange  -> H7_eu_utm
                           H7_HR_plot <- 
                                     ggplot(H7_eu_utm) +
                                     geom_spatvector()+
                                     geom_polygon(H7_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                                     geom_polygon(H7_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                                     geom_path(data = H7_nonb, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = H7_nd1, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = H7_nd2, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                                     labs(x = " ", y = " ", title = "H7 non-breeding HR and ND movements tracks") +
                                     theme_minimal()+
                                     scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                                    "Natal dispersal 1st travel" = "blue",
                                                                                    "Natal dispersal 2nd travel" = "orange")) +
                                      scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                                        "Non-Breeding HR core area" = "red"))         

                           H7_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/H7_HR_ND_plot.jpg", plot = H7_HR_plot)

          
          # Non-breeding ltraj object

                       # Let's create a ltraj object with UTM coordinates
                              
                            H7_nonb_lt <- osprey%>%
                                              dplyr::filter(ID == 'H7' & time >= '2013-08-10 00:00:00' & time <= '2015-04-02 05:00:00')
         
                            H7_nonb_lt <- as.ltraj(H7_nonb_lt[, c("x", "y")],
                                                  date = H7_nonb_lt$time, 
                                                  id = H7_nonb_lt$ID,
                                                  typeII=TRUE)

# "IFP" OK
# too small dataset to calculate HR

                    # First define the non-breeding period
                            IFP_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IFP')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IFP_nonb$ID <- factor(IFP_nonb$ID)
          
                   # Let's create a spatialPoint object
                            IFP_nonb_sp <- SpatialPointsDataFrame(IFP_nonb[,c("x", "y")], IFP_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            IFP_nonb_kde <- kernelUD(IFP_nonb_sp[,1], h = "href", grid = 70) # h = "LSCV"

                 # get IFP non-breeding HR
                            IFP_nonb_HR <- getverticeshr(IFP_nonb_kde, percent = 95) 
                            IFP_nonb_HR

                            IFP_nonb_HRcore <- getverticeshr(IFP_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IFP_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           IFP_nonb_HR <- fortify(IFP_nonb_HR)
                           IFP_nonb_HRcore <- fortify(IFP_nonb_HRcore)

                            IFP_nd22 <- osprey_nd%>%
                                     filter(ID == 'IFP' & time >= '2022-07-23 06:00:00' & time <= '2022-08-15 00:00:00')

                            IFP_nd23a <- osprey_nd%>%
                                     filter(ID == 'IFP' & time >= '2023-04-24 00:00:00' & time <= '2023-04-30 04:00:00')

                            IFP_nd23b <- osprey_nd%>%
                                     filter(ID == "IFP" & time >= '2023-05-16 00:00:00' & time <= '2023-06-08 20:00:00')

                  # Plot the non-breeding homerange
                           IFP_HR_plot <- 
                                     ggplot(IFP_eu_utm) +
                                     geom_spatvector()+
                                     #geom_polygon(IFP_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                                     #geom_polygon(IFP_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                                     geom_path(data = IFP_nonb, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = IFP_nd22, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = IFP_nd23a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = IFP_nd23b, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                                     labs(x = " ", y = " ", title = "IFP non-breeding HR and ND movements tracks") +
                                     theme_minimal()+
                                     scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                                    "Natal dispersal 1st travel" = "blue",
                                                                                    "Natal dispersal 2nd travel" = "orange",
                                                                                    "Natal dispersal 3rd travel" = "brown")) +
                                      scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                                        "Non-Breeding HR core area" = "red"))         
                           IFP_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/IFP_HR_ND_plot.jpg", plot = IFP_HR_plot)

          
          # Non-breeding ltraj object

                       # Let's create a ltraj object with UTM coordinates
                              
                            IFP_nonb_lt <- osprey%>%
                                              dplyr::filter(ID == 'IFP' & time >= '2022-08-15 12:00:00' & time <= '2023-04-24 00:00:00')
         
                            IFP_nonb_lt <- as.ltraj(IFP_nonb_lt[, c("x", "y")],
                                                  date = IFP_nonb_lt$time, 
                                                  id = IFP_nonb_lt$ID,
                                                  typeII=TRUE)

# "Antares"
# Traccaito difficile da analizzare, da rivedere

                    # First define the non-breeding period
                            Antares_nonb15 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'Antares', year == "2015")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            Antares_nonb15$ID <- factor(Antares_nonb15$ID)


                            Antares_nonb16 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'Antares', year == "2016")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            Antares_nonb16$ID <- factor(Antares_nonb16$ID)

                            Antares_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'Antares')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            Antares_nonb$ID <- factor(Antares_non$ID)
          
                   # Let's create a spatialPoint object
                            Antares_nonb15_sp <- SpatialPointsDataFrame(Antares_nonb15[,c("x", "y")], Antares_nonb15)  
                            Antares_nonb16_sp <- SpatialPointsDataFrame(Antares_nonb16[,c("x", "y")], Antares_nonb16)   
                            Antares_nonb_sp <- SpatialPointsDataFrame(Antares_nonb[,c("x", "y")], Antares_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            Antares_nonb15_kde <- kernelUD(Antares_nonb15_sp[,1], h = "href", grid = 90) # h = "LSCV"
                            Antares_nonb16_kde <- kernelUD(Antares_nonb16_sp[,1], h = "href") # h = "LSCV"
                            Antares_nonb_kde <- kernelUD(Antares_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get Antares non-breeding HR
                            Antares_nonb15_HR <- getverticeshr(Antares_nonb15_kde, percent = 95)
                            Antares_nonb15_HR

                            Antares_nonb15_HRcore <- getverticeshr(Antares_nonb15_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            Antares_nonb15_HRcore

                            Antares_nonb16_HR <- getverticeshr(Antares_nonb16_kde, percent = 95) 
                            Antares_nonb16_HR

                            Antares_nonb16_HRcore <- getverticeshr(Antares_nonb16_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            Antares_nonb16_HRcore

                            Antares_nonb_HR <- getverticeshr(Antares_nonb_kde, percent = 95) 
                            Antares_nonb_HR

                            Antares_nonb_HRcore <- getverticeshr(Antares_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            Antares_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           Antares_nonb15_HR <- fortify(Antares_nonb15_HR)
                           Antares_nonb15_HRcore <- fortify(Antares_nonb15_HRcore)

                           Antares_nonb16_HR <- fortify(Antares_nonb16_HR)
                           Antares_nonb16_HRcore <- fortify(Antares_nonb16_HRcore)

                           Antares_nonb_HR <- fortify(Antares_nonb_HR)
                           Antares_nonb_HRcore <- fortify(Antares_nonb_HRcore)

                            Antares <- osprey%>%
                                     filter(ID == 'Antares')

                            Antares_nd15_1 <- osprey_nd%>%
                                     filter(ID == "Antares" & time >= "2015-08-15 11:00:00" & time <= "2015-08-18 19:00:00")

                            Antares_nd15_2 <- osprey_nd%>%
                                     filter(ID == "Antares" & time >= "2015-09-08 10:30:00" & time <= "2015-09-13 20:00:00")

                            Antares_nd16 <- osprey_nd%>%
                                     filter(ID == "Antares" & time >= "2016-03-19 13:00:00" & time <= "2016-06-28 18:00:00" )

                  # Plot the non-breeding homerange
                           Antares_HR_plot <- 
                           ggplot(Antares_eu_utm) +
                           geom_spatvector()+
                           #geom_polygon(Antares_nonb16_HR, mapping = aes(x=long, y=lat), fill = "light blue") +
                           #geom_polygon(Antares_nonb16_HRcore, mapping = aes(x=long, y=lat), fill = "dark blue") +
                           #geom_polygon(Antares_nonb15_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                           #geom_polygon(Antares_nonb15_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                           geom_polygon(Antares_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                           geom_polygon(Antares_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                           geom_path(data = Antares, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = Antares_nd15_1, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = Antares_nd15_2, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = Antares_nd16, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "Antares non-breeding HR and ND movements tracks") +
                           theme_minimal()+
                                     scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                                    "Natal dispersal 1st travel" = "blue",
                                                                                    "Natal dispersal 2nd travel" = "orange",
                                                                                    "Natal dispersal 3rd travel" = "brown")) +
                                      scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                                        "Non-Breeding HR core area" = "red"))  

                           Antares_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/Antares_HR_ND_plot.jpg", plot = Antares_HR_plot)

          
          # Non-breeding ltraj object

                       # Let's create a ltraj object with UTM coordinates
                              
                            Antares_nonb_lt <- osprey_nonb%>%
                                              dplyr::filter(ID == 'Antares')
         
                            Antares_nonb_lt <- as.ltraj(Antares_nonb_lt[, c("x", "y")],
                                                  date = Antares_nonb_lt$time, 
                                                  id = Antares_nonb_lt$ID,
                                                  typeII=TRUE)

# CAM OK

                    # First define the non-breeding period
                            CAM_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'CAM')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            CAM_nonb$ID <- factor(CAM_nonb$ID)
          
                   # Let's create a spatialPoint object
                            CAM_nonb_sp <- SpatialPointsDataFrame(CAM_nonb[,c("x", "y")], CAM_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            CAM_nonb_kde <- kernelUD(CAM_nonb_sp[,1], h = "href", grid = 750) # h = "LSCV"

                  # get CAM non-breeding HR
                            CAM_nonb_HR <- getverticeshr(CAM_nonb_kde, percent = 95)
                            CAM_nonb_HR

                            CAM_nonb_HRcore <- getverticeshr(CAM_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            CAM_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           CAM_nonb_HR <- fortify(CAM_nonb_HR)
                           CAM_nonb_HRcore <- fortify(CAM_nonb_HRcore)

                            CAM_nd1 <- osprey_nd%>%
                                     filter(ID == 'CAM' & time >= "2016-04-14 06:00:00" & time <= "2016-04-19 20:00:00")

                            CAM_nd2 <- osprey_nd%>%
                                     filter(ID == "CAM" & time >= "2016-05-03 08:00:00" & time <= "2016-05-06 18:00:00")

                            CAM_nd3 <- osprey_nd%>%
                                     filter(ID == "CAM" & time >= "2016-05-20 05:00:00" & time <= "2016-05-24 18:00:00")

                            CAM_nd4 <- osprey_nd%>%
                                     filter(ID == "CAM" & time >= "2016-07-02 23:00:00" & time <= "2016-07-06 20:00:00")

                  # Plot the non-breeding homerange
                           CAM_HR_plot <- 
                                     ggplot(CAM_eu_utm) +
                                     geom_spatvector()+
                                     geom_polygon(CAM_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                                     geom_polygon(CAM_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                                     geom_path(data = CAM_nonb, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = CAM_nd1, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = CAM_nd2, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = CAM_nd3, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = CAM_nd4, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
                                     labs(x = " ", y = " ", title = "CAM non-breeding HR and ND movements tracks") +
                                     theme_minimal()+
                                     scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                                    "Natal dispersal 1st travel" = "blue",
                                                                                    "Natal dispersal 2nd travel" = "orange",
                                                                                    "Natal dispersal 3rd travel" = "brown",
                                                                                    "Natal dispersal 4th travel" = "magenta")) +
                                      scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                                        "Non-Breeding HR core area" = "red")) 
         
                           CAM_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/CAM_HR_ND_plot.jpg", plot = CAM_HR_plot)

# "CBK" OK

                    # First define the non-breeding period
                            CBK_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'CBK')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            CBK_nonb$ID <- factor(CBK_nonb$ID)
          
                   # Let's create a spatialPoint object
                            CBK_nonb_sp <- SpatialPointsDataFrame(CBK_nonb[,c("x", "y")], CBK_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            CBK_nonb_kde <- kernelUD(CBK_nonb_sp[,1], h = "href", grid = 200) # h = "LSCV"

                 # get CBK non-breeding HR
                            CBK_nonb_HR <- getverticeshr(CBK_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            CBK_nonb_HR

                            CBK_nonb_HRcore <- getverticeshr(CBK_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            CBK_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           CBK_nonb_HR <- fortify(CBK_nonb_HR)
                           CBK_nonb_HRcore <- fortify(CBK_nonb_HRcore)

                            CBK_nd1 <- osprey_nd%>%
                                     filter(ID == 'CBK' & time >= '2013-08-15 00:00:00' & time <= '2013-08-21 12:00:00')

                            CBK_nd2 <- osprey_nd%>%
                                     filter(ID == 'CBK' & time >= '2014-03-20 04:30:00' & time <= '2014-03-22 10:30:00')

                            CBK_nd3 <- osprey_nd%>%
                                     filter(ID == 'CBK' & time >= '2014-04-08 06:30:00' & time <= '2014-04-12 11:00:00')

                  # Plot the non-breeding homerange
                           CBK_HR_plot <- 
                                     ggplot(CBK_eu_utm) +
                                     geom_spatvector()+
                                     geom_polygon(CBK_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                                     geom_polygon(CBK_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                                     geom_path(data = CBK_nonb, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = CBK_nd1, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = CBK_nd2, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                                     geom_path(data = CBK_nd3, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                                     labs(x = " ", y = " ", title = "CBK non-breeding HR and ND movements tracks") +
                                     theme_minimal()+
                                     scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                                    "Natal dispersal 1st travel" = "blue",
                                                                                    "Natal dispersal 2nd travel" = "orange",
                                                                                    "Natal dispersal 3rd travel" = "brown",
                                                                                    "Natal dispersal 4th travel" = "magenta")) +
                                      scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                                        "Non-Breeding HR core area" = "red")) 
         
         
                           CBK_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/CBK_HR_ND_plot.jpg", plot = CBK_HR_plot)

# "CIV"

                    # First define the non-breeding period
                            CIV_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'CIV')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            CIV_nonb$ID <- factor(CIV_nonb$ID)

                            CIV_nonb1516 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'CIV' & time >= "2014-10-22 12:00:00" & time <= '2015-06-04 03:00:00' |
                                                            ID == 'CIV' & time >= '2015-11-26 24:00:00' & time <= '2016-03-29 00:01:00')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            CIV_nonb1516$ID <- factor(CIV_nonb1516$ID)

                            CIV_nonb16 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'CIV', year == "2016")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            CIV_nonb16$ID <- factor(CIV_nonb16$ID)
          
                   # Let's create a spatialPoint object
                            CIV_nonb_sp <- SpatialPointsDataFrame(CIV_nonb[,c("x", "y")], CIV_nonb)
                            CIV_nonb1516_sp <- SpatialPointsDataFrame(CIV_nonb1516[,c("x", "y")], CIV_nonb1516)
                            CIV_nonb16_sp <- SpatialPointsDataFrame(CIV_nonb16[,c("x", "y")], CIV_nonb16) 
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            CIV_nonb_kde <- kernelUD(CIV_nonb_sp[,1], h = "href") # h = "LSCV"
                            CIV_nonb1516_kde <- kernelUD(CIV_nonb1516_sp[,1], h = "href", grid = 500) # h = "LSCV"
                            CIV_nonb16_kde <- kernelUD(CIV_nonb16_sp[,1], h = "href", grid = 100) # h = "LSCV"

                 # get CIV non-breeding HR
                            CIV_nonb_HR <- getverticeshr(CIV_nonb_kde, percent = 95) 
                            CIV_nonb_HR

                            CIV_nonb_HRcore <- getverticeshr(CIV_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            CIV_nonb_HRcore

                            CIV_nonb1516_HR <- getverticeshr(CIV_nonb1516_kde, percent = 95) 
                            CIV_nonb1516_HR

                            CIV_nonb1516_HRcore <- getverticeshr(CIV_nonb1516_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            CIV_nonb1516_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           CIV_nonb_HR <- fortify(CIV_nonb_HR)
                           CIV_nonb_HRcore <- fortify(CIV_nonb_HRcore)

                           CIV_nonb1516_HR <- fortify(CIV_nonb1516_HR)
                           CIV_nonb1516_HRcore <- fortify(CIV_nonb1516_HRcore)

                            CIV_nonb <- osprey_nonb%>%
                                     filter( ID == 'CIV' & time >= "2014-10-22 12:00:00" & time <= '2015-06-04 03:00:00' |
                         ID == 'CIV' & time >= '2015-11-26 24:00:00' & time <= '2016-03-29 00:01:00')

                            CIV_nd14 <- osprey_nd%>%
                                     filter( ID == 'CIV' & time >= "2014-08-16 10:00:00" & time <= "2014-08-22 06:00:00" |
                         ID == 'CIV' & time >= "2014-09-11 08:00:00" & time <= "2014-09-13 17:00:00" |
                         ID == 'CIV' & time >= "2014-10-21 08:00:00" & time <= "2014-10-21 21:00:00")

                            CIV_nd15 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= "2015-03-21 02:00:00" & time <= "2015-03-21 24:00:00" |
                         ID == 'CIV' & time >= "2015-06-04 04:00:00" & time <= "2015-06-22 18:00:00" |
                         ID == 'CIV' & time >= "2015-08-13 06:00:00" & time <= "2015-08-14 00:00:00" |
                         ID == 'CIV' & time >= "2015-11-21 16:00:00" & time <= "2015-11-27 00:00:00")

                            CIV_nd16 <- osprey_nd%>%
                                     filter( ID == 'CIV' & time >= '2016-03-28 18:00:00' & time <= '2016-04-01 06:00:00' |
                         ID == 'CIV' & time >= '2016-04-15 06:00:00' & time <= '2016-04-18 22:00:00' |
                         ID == 'CIV' & time >= '2016-10-28 06:00:00' & time <= '2016-10-30 06:00:00')

                              CIV_nd1 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= "2014-08-16 10:00:00" & time <= "2014-08-22 06:00:00")

                              CIV_nd2 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= "2014-09-11 08:00:00" & time <= "2014-09-13 17:00:00")

                              CIV_nd3 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= "2014-10-21 08:00:00" & time <= "2014-10-21 21:00:00")

                              CIV_nd4 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= "2015-03-21 02:00:00" & time <= "2015-03-21 24:00:00")

                              CIV_nd5 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= "2015-06-04 04:00:00" & time <= "2015-06-22 18:00:00")

                              CIV_nd6 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= "2015-08-13 06:00:00" & time <= "2015-08-14 00:00:00")

                              CIV_nd7 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= "2015-11-21 16:00:00" & time <= "2015-11-27 00:00:00")

                              CIV_nd8 <- osprey_nd%>%
                                     filter( ID == 'CIV' & time >= '2016-03-28 18:00:00' & time <= '2016-04-01 06:00:00')

                              CIV_nd9 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= '2016-04-15 06:00:00' & time <= '2016-04-18 22:00:00')

                              CIV_nd10 <- osprey_nd%>%
                                     filter(ID == 'CIV' & time >= '2016-10-28 06:00:00' & time <= '2016-10-30 06:00:00')

                  # Plot the non-breeding homerange    
                           CIV_HR_plot <- 
                           ggplot(CIV_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(CIV_nonb1516_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                           geom_polygon(CIV_nonb1516_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                           geom_path(data = CIV_nonb, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = CIV_nd14, aes(x = x, y = y, colour = "Natal dispersal 2014"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = CIV_nd15, aes(x = x, y = y, colour = "Natal dispersal 2015"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = CIV_nd16, aes(x = x, y = y, colour = "Natal dispersal 2016"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd1, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd2, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd3, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd4, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd5, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd6, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd7, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd8, aes(x = x, y = y, colour = "Natal dispersal 8th travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd9, aes(x = x, y = y, colour = "Natal dispersal 9th travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd10, aes(x = x, y = y, colour = "Natal dispersal 10th travel"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "CIV non-breeding HR and ND movements tracks") +
                           theme_minimal()+
                                     scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                                    "Natal dispersal 1st travel" = "blue",
                                                                                    "Natal dispersal 2nd travel" = "orange",
                                                                                    "Natal dispersal 3rd travel" = "brown",
                                                                                    "Natal dispersal 4th travel" = "magenta",
                                                                                    "Natal dispersal 5th travel" = "pink",
                                                                                    "Natal dispersal 6th travel" = "light blue",
                                                                                    "Natal dispersal 7th travel" = "black",
                                                                                    "Natal dispersal 8th travel" = "grey",
                                                                                    "Natal dispersal 9th travel" = "purple",
                                                                                    "Natal dispersal 10th travel" = "red")) +
                                      scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                                        "Non-Breeding HR core area" = "red")) 
         
                           CIV_HR_plot
          
                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/CIV_HR_ND_plot.jpg", plot = CIV_HR_plot)

# "IAB" -> da rivedere HR troppo grande + aggiungere tracciati

                    # First define the non-breeding period
                            IAB_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IAB')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IAB_nonb$ID <- factor(IAB_nonb$ID)

                            IAB_nonb19 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IAB', year == "2019")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IAB_nonb19$ID <- factor(IAB_nonb19$ID)

                            IAB_nonb20 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IAB', year == "2020")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IAB_nonb20$ID <- factor(IAB_nonb20$ID)
          
                   # Let's create a spatialPoint object
                            IAB_nonb_sp <- SpatialPointsDataFrame(IAB_nonb[,c("x", "y")], IAB_nonb)

                            IAB_nonb19_sp <- SpatialPointsDataFrame(IAB_nonb19[,c("x", "y")], IAB_nonb19) 

                            IAB_nonb20_sp <- SpatialPointsDataFrame(IAB_nonb20[,c("x", "y")], IAB_nonb20) 
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            IAB_nonb_kde <- kernelUD(IAB_nonb_sp[,1], h = "href") # h = "LSCV"

                            IAB_nonb19_kde <- kernelUD(IAB_nonb19_sp[,1], h = "href") # h = "LSCV"

                            IAB_nonb20_kde <- kernelUD(IAB_nonb20_sp[,1], h = "href") # h = "LSCV"

                 # get IAB non-breeding HR
                            IAB_nonb_HR <- getverticeshr(IAB_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            IAB_nonb_HR

                            IAB_nonb_HRcore <- getverticeshr(IAB_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAB_nonb_HRcore

                            IAB_nonb19_HR <- getverticeshr(IAB_nonb19_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            IAB_nonb19_HR

                            IAB_nonb19_HRcore <- getverticeshr(IAB_nonb19_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAB_nonb19_HRcore

                            IAB_nonb20_HR <- getverticeshr(IAB_nonb20_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            IAB_nonb20_HR

                            IAB_nonb20_HRcore <- getverticeshr(IAB_nonb20_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAB_nonb20_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           IAB_nonb_HR <- fortify(IAB_nonb_HR)
                           IAB_nonb_HRcore <- fortify(IAB_nonb_HRcore)

                           IAB_nonb19_HR <- fortify(IAB_non19b_HR)
                           IAB_nonb19_HRcore <- fortify(IAB_nonb19_HRcore)

                           IAB_nonb20_HR <- fortify(IAB_nonb20_HR)
                           IAB_nonb20_HRcore <- fortify(IAB_nonb20_HRcore)

                            IAB <- osprey%>%
                                     filter(ID == 'IAB')

                            IAB_nd19 <- osprey_nd%>%
                                     filter(ID == 'IAB' & time >= '2019-05-05 06:00:00' & time <= '2019-06-10 14:00:00')

                            IAB_nd20 <- osprey_nd%>%
                                     filter(ID == 'IAB' & time >= '2020-03-16 00:00:00')

                  # Plot the non-breeding homerange
                           IAB_HR_plot <- 
                           ggplot(IAB_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IAB_nonb19_HR, mapping = aes(x=long, y=lat), fill = "pink") +
                           geom_polygon(IAB_nonb19_HRcore, mapping = aes(x=long, y=lat), fill = "purple") +
                           geom_polygon(IAB_nonb20_HR, mapping = aes(x=long, y=lat), fill = "light blue") +
                           geom_polygon(IAB_nonb20_HRcore, mapping = aes(x=long, y=lat), fill = "dark blue") +
                           geom_path(data = IAB, aes(x = x, y = y, colour = "Before dispersal track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IAB_nd19, aes(x = x, y = y, colour = "Natal dispersal 2019"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IAB_nd20, aes(x = x, y = y, colour = "Natal dispersal 2020"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IAB non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Before dispersal track" = "green",
                                                                          "Natal dispersal 2019" = "red",
                                                                          "Natal dispersal 2020" = "orange"))
         
                           IAB_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/IAB_HR_ND_plot.jpg", plot = IAB_HR_plot)

# "IAD" -> HR da rivedere, track rientro in Italia da aggiungere

                    # First define the non-breeding period
                            IAD_nonbHR1 <- osprey_nonb%>%
                                              dplyr::filter( ID == 'IAD' & time >= '2016-08-25 11:00:00' & time <= '2018-02-04 18:00:00')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IAD_nonbHR1$ID <- factor(IAD_nonbHR1$ID)

                            IAD_nonbHR4 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IAD' & time >= '2018-12-20 15:00:00' & time <= '2019-03-04 10:00:00')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IAD_nonbHR4$ID <- factor(IAD_nonbHR4$ID)


                            IAD_nonb18 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IAD', year == "2018")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IAD_nonb18$ID <- factor(IAD_nonb18$ID)

                            IAD_nonb19 <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IAD', year == "2019")%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IAD_nonb19$ID <- factor(IAD_nonb19$ID)
          
                   # Let's create a spatialPoint object
                            IAD_nonb_sp <- SpatialPointsDataFrame(IAD_nonb[,c("x", "y")], IAD_nonb)   
                            IAD_nonbHR1_sp <- SpatialPointsDataFrame(IAD_nonbHR1[,c("x", "y")], IAD_nonbHR1)   
                            IAD_nonbHR4_sp <- SpatialPointsDataFrame(IAD_nonbHR4[,c("x", "y")], IAD_nonbHR4)   
                            IAD_nonb18_sp <- SpatialPointsDataFrame(IAD_nonb18[,c("x", "y")], IAD_nonb18)   
                            IAD_nonb19_sp <- SpatialPointsDataFrame(IAD_nonb19[,c("x", "y")], IAD_nonb19)
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            IAD_nonb_kde <- kernelUD(IAD_nonb_sp[,1], h = "href") # h = "LSCV"
                            IAD_nonbHR1_kde <- kernelUD(IAD_nonbHR1_sp[,1], h = "href") # h = "LSCV"
                            IAD_nonbHR4_kde <- kernelUD(IAD_nonbHR4_sp[,1], h = "href") # h = "LSCV"
                            IAD_nonb18_kde <- kernelUD(IAD_nonb18_sp[,1], h = "href") # h = "LSCV"
                            IAD_nonb19_kde <- kernelUD(IAD_nonb19_sp[,1], h = "href") # h = "LSCV"

                 # get IAD non-breeding HR
                            IAD_nonb_HR <- getverticeshr(IAD_nonb_kde, percent = 95)
                            IAD_nonb_HR

                            IAD_nonb_HRcore <- getverticeshr(IAD_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAD_nonb_HRcore

                            IAD_nonbHR1_HR <- getverticeshr(IAD_nonbHR1_kde, percent = 95)
                            IAD_nonbHR1_HR

                            IAD_nonbHR1_HRcore <- getverticeshr(IAD_nonbHR1_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAD_nonbHR1_HRcore

                            IAD_nonbHR4_HR <- getverticeshr(IAD_nonbHR4_kde, percent = 95)
                            IAD_nonbHR4_HR

                            IAD_nonbHR4_HRcore <- getverticeshr(IAD_nonbHR4_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAD_nonbHR4_HRcore

                            IAD_nonb18_HR <- getverticeshr(IAD_nonb18_kde, percent = 95)
                            IAD_nonb18_HR

                            IAD_nonb18_HRcore <- getverticeshr(IAD_nonb18_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAD_nonb18_HRcore

                            IAD_nonb19_HR <- getverticeshr(IAD_nonb19_kde, percent = 95)
                            IAD_nonb19_HR

                            IAD_nonb19_HRcore <- getverticeshr(IAD_nonb19_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IAD_nonb19_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                            IAD_nonb_HR <- fortify(IAD_nonb_HR)
                            IAD_nonb_HRcore <- fortify(IAD_nonb_HRcore)

                            IAD_nonbHR1_HR <- fortify(IAD_nonbHR1_HR)
                            IAD_nonbHR1_HRcore <- fortify(IAD_nonbHR1_HRcore)

                            IAD_nonbHR4_HR <- fortify(IAD_nonbHR4_HR)
                            IAD_nonbHR4_HRcore <- fortify(IAD_nonbHR4_HRcore)

                            IAD_nonb18_HR <- fortify(IAD_nonb18_HR)
                            IAD_nonb18_HRcore <- fortify(IAD_nonb18_HRcore)

                            IAD_nonb19_HR <- fortify(IAD_nonb19_HR)
                            IAD_nonb19_HRcore <- fortify(IAD_nonb19_HRcore)

                            IAD <- osprey%>%
                                     filter(ID == 'IAD')

                            IAD_nd16 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2016-08-20 09:00:00' & time <= '2016-08-25 11:00:00')

                            IAD_nd18 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2018-02-04 18:00:00' & time <= '2018-02-07 19:00:00' | 
                                            ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00' |
                                            ID == 'IAD' & time >= '2018-12-15 00:00:00' & time <= '2018-12-20 15:00:00')

                            IAD_nd19 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-04-01 17:00:00' |
                                             ID == 'IAD' & time >= '2019-04-05 10:00:00' & time <= '2019-04-09 17:00:00' |
                                             ID == 'IAD' & time >= '2019-04-14 20:00:00' & time <= '2019-04-19 13:00:00' |
                                             ID == 'IAD' & time >= '2019-04-21 06:00:00' & time <= '2019-04-21 20:00:00' |
                                             ID == 'IAD' & time >= '2019-04-29 09:00:00' & time <= '2019-05-01 21:00:00' |
                                             ID == 'IAD' & time >= '2019-07-08 09:00:00' & time <= '2019-07-15 18:00:00')

            IAD_nd19t <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-04-14 20:00:00' & time <= '2019-04-19 13:00:00')

            IAD_nd1 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2016-08-20 09:00:00' & time <= '2016-08-25 11:00:00')
            IAD_nd2 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2018-02-04 18:00:00' & time <= '2018-02-07 19:00:00')
            IAD_nd3 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00')
            IAD_nd4 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2018-12-15 00:00:00' & time <= '2018-12-20 15:00:00')
            IAD_nd5 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-04-01 17:00:00')
            IAD_nd6 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-04-05 10:00:00' & time <= '2019-04-09 17:00:00')
            IAD_nd7 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-04-14 20:00:00' & time <= '2019-04-19 13:00:00')
            IAD_nd8 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-04-21 06:00:00' & time <= '2019-04-21 20:00:00')
            IAD_nd9 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-04-29 09:00:00' & time <= '2019-05-01 21:00:00')
            IAD_nd10 <- osprey_nd%>%
                                     filter(ID == 'IAD' & time >= '2019-07-08 09:00:00' & time <= '2019-07-15 18:00:00')


            IAD_nonb1 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2016-08-25 11:00:00' & time <= '2018-02-04 18:00:00')
            IAD_nonb2 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2018-02-07 19:00:00' & time <= '2018-03-28 08:00:00')
            IAD_nonb3 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2018-06-12 14:00:00' & time <= '2018-12-15 00:00:00')
            IAD_nonb4 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2018-12-20 15:00:00' & time <= '2019-03-04 10:00:00')
            IAD_nonb5 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2019-04-01 17:00:00' & time <= '2019-04-05 10:00:00')
            IAD_nonb6 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2019-04-09 17:00:00' & time <= '2019-04-14 20:00:00')
            IAD_nonb7 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2019-04-19 13:00:00' & time <= '2019-04-21 06:00:00')
            IAD_nonb8 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2019-04-21 20:00:00' & time <= '2019-04-29 09:00:00')
            IAD_nonb9 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2019-05-01 21:00:00' & time <= '2019-07-08 09:00:00')
            IAD_nonb10 <- osprey_nonb%>%
                                     filter(ID == 'IAD' & time >= '2019-07-15 18:00:00')

                  # Plot the non-breeding homerange  IAD_eu_utm
                           IAD_HR_plot <- 
                           ggplot() +
                           geom_spatvector()+
                           #geom_path(data = IAD_nonb, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = IAD_nd16, aes(x = x, y = y, colour = "Natal dispersal 2016"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = IAD_nd18, aes(x = x, y = y, colour = "Natal dispersal 2018"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = IAD_nd19, aes(x = x, y = y, colour = "Natal dispersal 2019"), linewidth = 0.5, lineend = "round") +
                           #geom_polygon(IAD_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                           #geom_polygon(IAD_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                           geom_polygon(IAD_nonbHR1_HR, mapping = aes(x=long, y=lat), fill = "orange") +
                           geom_polygon(IAD_nonbHR1_HRcore, mapping = aes(x=long, y=lat), fill = "red") +
                           geom_polygon(IAD_nonbHR4_HR, mapping = aes(x=long, y=lat), fill = "pink") +
                           geom_polygon(IAD_nonbHR4_HRcore, mapping = aes(x=long, y=lat), fill = "purple") +
                       #    geom_path(data = IAD_nonb, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IAD_nonb4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb8, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb9, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nonb10, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd1, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd2, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd3, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd4, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd5, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd6, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd7, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd8, aes(x = x, y = y, colour = "Natal dispersal 8th travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd9, aes(x = x, y = y, colour = "Natal dispersal 9th travel"), linewidth = 0.5, lineend = "round") +
                       #    geom_path(data = IAD_nd10, aes(x = x, y = y, colour = "Natal dispersal 10th travel"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IAD non-breeding HR and ND movements tracks") +
                           theme_minimal()+
                                     scale_color_manual(name = "Tracks", values = c("Non-Dispersal movements" = "green",
                                                                                    "Natal dispersal 1st travel" = "blue",
                                                                                    "Natal dispersal 2nd travel" = "orange",
                                                                                    "Natal dispersal 3rd travel" = "brown",
                                                                                    "Natal dispersal 4th travel" = "magenta",
                                                                                    "Natal dispersal 5th travel" = "pink",
                                                                                    "Natal dispersal 6th travel" = "light blue",
                                                                                    "Natal dispersal 7th travel" = "black",
                                                                                    "Natal dispersal 8th travel" = "grey",
                                                                                    "Natal dispersal 9th travel" = "purple",
                                                                                    "Natal dispersal 10th travel" = "red")) +
                                      scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "orange",
                                                                                        "Non-Breeding HR core area" = "red"))
         
                           IAD_HR_plot
 
                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/IAD_HR_ND_plot.jpg", plot = IAD_HR_plot)

# "IBH"

                    # First define the non-breeding period
                            IBH_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IBH')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IBH_nonb$ID <- factor(IBH_nonb$ID)
          
                   # Let's create a spatialPoint object
                            IBH_nonb_sp <- SpatialPointsDataFrame(IBH_nonb[,c("x", "y")], IBH_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            IBH_nonb_kde <- kernelUD(IBH_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get IBH non-breeding HR
                            IBH_nonb_HR <- getverticeshr(IBH_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            IBH_nonb_HR

                            IBH_nonb_HRcore <- getverticeshr(IBH_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IBH_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           IBH_nonb_HR <- fortify(IBH_nonb_HR)
                           IBH_nonb_HRcore <- fortify(IBH_nonb_HRcore)

                            IBH <- osprey%>%
                                     filter(ID == 'IBH')

                            IBH_nd <- osprey_nd%>%
                                     filter(ID == 'IBH')

                  # Plot the non-breeding homerange
                           IBH_HR_plot <- 
                           ggplot(IBH_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IBH_nonb_HR, mapping = aes(x=long, y=lat), fill = "orange") +
                           geom_polygon(IBH_nonb_HRcore, mapping = aes(x=long, y=lat), fill = "red") +
                           geom_path(data = IBH, aes(x = x, y = y, colour = "Before dispersal track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBH_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBH non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Before dispersal track" = "green",
                                                                          "Natal dispersal" = "blue"))
         
                           IBH_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/IBH_HR_ND_plot.jpg", plot = IBH_HR_plot)

# "IBI"

                    # First define the non-breeding period
                            IBI_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IBI')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IBI_nonb$ID <- factor(IBI_nonb$ID)
          
                   # Let's create a spatialPoint object
                            IBI_nonb_sp <- SpatialPointsDataFrame(IBI_nonb[,c("x", "y")], IBI_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            IBI_nonb_kde <- kernelUD(IBI_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get IBI non-breeding HR
                            IBI_nonb_HR <- getverticeshr(IBI_nonb_kde, percent = 95)
                            IBI_nonb_HR

                            IBI_nonb_HRcore <- getverticeshr(IBI_nonb_kde, percent = 50)
                            IBI_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           IBI_nonb_HR <- fortify(IBI_nonb_HR)
                           IBI_nonb_HRcore <- fortify(IBI_nonb_HRcore)


                            IBI <- osprey%>%
                                     filter(ID == 'IBI')

                            IBI_nd <- osprey_nd%>%
                                     filter(ID == 'IBI')

                  # Plot the non-breeding homerange
                           IBI_HR_plot <- 
                           ggplot(IBI_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IBI_nonb_HR, mapping = aes(x=long, y=lat), fill = "orange") +
                           geom_polygon(IBI_nonb_HRcore, mapping = aes(x=long, y=lat), fill = "red") +
                           geom_path(data = IBI, aes(x = x, y = y, colour = "Before dispersal track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBI_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBI non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Before dispersal track" = "green",
                                                                          "Natal dispersal" = "blue"))
         
                           IBI_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/IBI_HR_ND_plot.jpg", plot = IBI_HR_plot)

# "IBK"

                    # First define the non-breeding period
                            IBK_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IBK')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IBK_nonb$ID <- factor(IBK_nonb$ID)
          
                   # Let's create a spatialPoint object
                            IBK_nonb_sp <- SpatialPointsDataFrame(IBK_nonb[,c("x", "y")], IBK_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            IBK_nonb_kde <- kernelUD(IBK_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get IBK non-breeding HR
                            IBK_nonb_HR <- getverticeshr(IBK_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            IBK_nonb_HR

                            IBK_nonb_HRcore <- getverticeshr(IBK_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IBK_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           IBK_nonb_HR <- fortify(IBK_nonb_HR)
                           IBK_nonb_HRcore <- fortify(IBK_nonb_HRcore)

                            IBK <- osprey%>%
                                     filter(ID == 'IBK')

                            IBK_nd <- osprey_nd%>%
                                     filter(ID == 'IBK')

                  # Plot the non-breeding homerange
                           IBK_HR_plot <- 
                           ggplot(IBK_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IBK_nonb_HR, mapping = aes(x=long, y=lat), fill = "orange") +
                           geom_polygon(IBK_nonb_HRcore, mapping = aes(x=long, y=lat), fill = "red") +
                           geom_path(data = IBK, aes(x = x, y = y, colour = "Before dispersal track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBK_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBK non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Before dispersal track" = "green",
                                                                          "Natal dispersal" = "blue"))
         
                           IBK_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/IBK_HR_ND_plot.jpg", plot = IBK_HR_plot)

# "IBS" -> HR da sistemare, path a zig zag

                    # First define the non-breeding period
                            IBS_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'IBS')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IBS_nonb$ID <- factor(IBS_nonb$ID)
          
                   # Let's create a spatialPoint object
                            IBS_nonb_sp <- SpatialPointsDataFrame(IBS_nonb[,c("x", "y")], IBS_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            IBS_nonb_kde <- kernelUD(IBS_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get IBS non-breeding HR
                            IBS_nonb_HR <- getverticeshr(IBS_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            IBS_nonb_HR

                            IBS_nonb_HRcore <- getverticeshr(IBS_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            IBS_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           IBS_nonb_HR <- fortify(IBS_nonb_HR)
                           IBS_nonb_HRcore <- fortify(IBS_nonb_HRcore)

                            IBS <- osprey%>%
                                     filter(ID == 'IBS')

                            IBS_nd20 <- osprey_nd%>%
                                     filter(ID == "IBS"& time >= "2020-08-07 08:00:00" & time <= "2020-08-18 19:00:00")

                            IBS_nd21 <- osprey_nd%>%
                                     filter(ID == "IBS" & time >= "2021-02-15 09:00:00" & time <= "2021-05-01 16:00:00")

                            IBS_nd22 <- osprey_nd%>%
                                     filter(ID == 'IBS' & time > '2022-03-22 00:00:00' & time < '2022-06-04 15:00:00')

                            IBS_nd23 <- osprey_nd%>%
                                     filter(ID == "IBS" & time >= "2023-02-08 16:00:00" & time <= "2023-02-28 00:00:00")

                  # Plot the non-breeding homerange
                           IBS_HR_plot <- 
                           ggplot(IBS_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IBS_nonb_HR, mapping = aes(x=long, y=lat), fill = "orange") +
                           geom_polygon(IBS_nonb_HRcore, mapping = aes(x=long, y=lat), fill = "red") +
                           geom_path(data = IBS, aes(x = x, y = y, colour = "Before dispersal track"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = IBS_nd20, aes(x = x, y = y, colour = "Natal dispersal 2020"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = IBS_nd21, aes(x = x, y = y, colour = "Natal dispersal 2021"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = IBS_nd22, aes(x = x, y = y, colour = "Natal dispersal 2022"), linewidth = 0.5, lineend = "round") +
                           #geom_path(data = IBS_nd23, aes(x = x, y = y, colour = "Natal dispersal 2023"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBS non-breeding homerange and natal dispersal tracks") +
                          # facet_wrap(~year)+
                           theme_minimal()#+
                           #scale_color_manual(name = "Tracks", values = c("Before dispersal track" = "green",
                                                                          "Natal dispersal 2020" = "magenta",
                                                                          "Natal dispersal 2021" = "blue",
                                                                          "Natal dispersal 2022" = "orange",
                                                                          "Natal dispersal 2023" = "brown"))
         
                           IBS_HR_plot

                   # ggsave("C:/Tesi/R/osprey/images/20231214_TrackPlot/IBS_HR_ND_plot.jpg", plot = IBS_HR_plot)

# "ICZ"

                    # First define the non-breeding period
                            ICZ_nonb <- osprey_nonb%>%
                                              dplyr::filter(ID == 'ICZ')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            ICZ_nonb$ID <- factor(ICZ_nonb$ID)
          
                   # Let's create a spatialPoint object
                            ICZ_nonb_sp <- SpatialPointsDataFrame(ICZ_nonb[,c("x", "y")], ICZ_nonb)   
          
                   # Here I calculate the non-breeding homerange with a Kernel Density Estimation
                            ICZ_nonb_kde <- kernelUD(ICZ_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get ICZ non-breeding HR
                            ICZ_nonb_HR <- getverticeshr(ICZ_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            ICZ_nonb_HR

                            ICZ_nonb_HRcore <- getverticeshr(ICZ_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                            ICZ_nonb_HRcore

                  # fortify() function is needed to plot the non-breeding homerange with ggplot
                           ICZ_nonb_HR <- fortify(ICZ_nonb_HR)

                            ICZ <- osprey%>%
                                     filter(ID == 'ICZ')

                            ICZ_nd <- osprey_nd%>%
                                     filter(ID == 'ICZ')

                  # Plot the non-breeding homerange
                           ICZ_HR_plot <- 
                           ggplot(ICZ_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(ICZ_nonb_HR, mapping = aes(x=long, y=lat), fill = "orange") +
                           geom_polygon(ICZ_nonb_HRcore, mapping = aes(x=long, y=lat), fill = "red") +
                           geom_path(data = ICZ, aes(x = x, y = y, colour = "Before dispersal track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = ICZ_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "ICZ non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Before dispersal track" = "green",
                                                                          "Natal dispersal" = "blue"))
         
                           ICZ_HR_plot

                   # ggsave("ICZ_HR_ND_plot.jpg", plot = ICZ_HR_plot)



################################
#     Non-breeding HR stat     #
################################


                    osp_nonb_lt <- c(A7_nonb_lt, E7_nonb_lt, H7_nonb_lt, IFP_nonb_lt)
          
                    osp_nonb_df <- ld(osp_nonb_lt)

                    osp_nonb_df <- osp_nonb_df%>%
                                        mutate(day = as.Date(date),
                                               distKM = dist/1000)      


          # SUMMARY dfs

             # Summary table showing the minimum, mean and maximum distance travelled between successive GPS location every single day       
                   osp_nonb_summary_dist <- osp_nonb_df%>%
                                              group_by(id, day)%>%
                                              summarize(minDist = min(distKM),
                                                        meanDist = mean(distKM),
                                                        maxDist = max(distKM),
                                                        totDist = sum(distKM))

             # Summary table showing the minimum, mean and maximum distance travelled in a day
                   osp_nonb_summary_id <- osp_nonb_summary_dist%>%
                                              group_by(id)%>%
                                              summarize(minDistxDay = min(totDist, na.rm=T),
                                                        meanDistxDay = mean(totDist, na.rm=T),
                                                        maxDistxDay = max(totDist, na.rm=T))
