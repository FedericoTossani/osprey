
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


####################
# Homerange's List #
####################

# KernelUD al 95 e al 50 (è la core area) 

# Here I will calculate the homerange of Ospreys during the winter before the beginning of natal dispersal movements

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
          
         
# "Antares" 
          # Messy track, difficult to find separete travel

                 # get Antares winter HR
                            Antares_winter_HR <- getverticeshr(winter_HR$Antares, percent = 50) # 50% is the value to obtain the core area of the HR
                            Antares_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           Antares_winter_HR <- fortify(Antares_winter_HR)

                            Antares <- osprey%>%
                                     filter(ID == 'Antares')

                            Antares_nd15_1 <- osprey_nd%>%
                                     filter(ID == "Antares" & time >= "2015-08-15 11:00:00" & time <= "2015-08-18 19:00:00")

                            Antares_nd15_2 <- osprey_nd%>%
                                     filter(ID == "Antares" & time >= "2015-09-08 10:30:00" & time <= "2015-09-13 20:00:00")

                            Antares_nd16 <- osprey_nd%>%
                                     filter(ID == "Antares" & time >= "2016-03-19 13:00:00" & time <= "2016-06-28 18:00:00" )

                  # Plot the winter homerange
                           Antares_HR_plot <- 
                           ggplot(Antares_eu_utm) +
                           geom_spatvector()+
                           geom_path(data = Antares, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = Antares_nd15_1, aes(x = x, y = y, colour = "Natal dispersal 2015 first travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = Antares_nd15_2, aes(x = x, y = y, colour = "Natal dispersal 2015 second travel"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = Antares_nd16, aes(x = x, y = y, colour = "Natal dispersal 2016"), linewidth = 0.5, lineend = "round") +
                           geom_polygon(Antares_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           labs(x = " ", y = " ", title = "Antares winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal 2015 first travel" = "red", "Natal dispersal 2015 second travel" = "orange", "Natal dispersal 2016" = "yellow"))
         
                           Antares_HR_plot

                   # ggsave("Antares_HR_ND_plot.jpg", plot = Antares_HR_plot)

# "CAM"  

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

                   # ggsave("CAM_HR_ND_plot.jpg", plot = CAM_HR_plot)

# "CBK"     
          # Doesn't work: Errore in re[[i]] : subscript fuori limite

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
                           #geom_polygon(CBK_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = CBK, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CBK_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "CBK winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           CBK_HR_plot

                   # ggsave("CBK_HR_ND_plot.jpg", plot = CBK_HR_plot)

# "CIV"     
          # Doesn't work: Errore in re[[i]] : subscript fuori limite

                 # get CIV winter HR
                            CIV_winter_HR <- getverticeshr(winter_HR$CIV, percent = 50) # 50% is the value to obtain the core area of the HR
                            CIV_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           CIV_winter_HR <- fortify(CIV_winter_HR)

                            CIV <- osprey%>%
                                     filter(ID == 'CIV')

                            CIV_nd14 <- osprey_nd%>%
                                     filter(ID == "CIV" & time >= "2014-08-16 06:00:00" & time <= "2014-10-22 12:00:00")

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
                           geom_path(data = CIV_nd14, aes(x = x, y = y, colour = "Natal dispersal 2014"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd15, aes(x = x, y = y, colour = "Natal dispersal 2015"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = CIV_nd16, aes(x = x, y = y, colour = "Natal dispersal 2016"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "CIV winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal 2015" = "magenta", "Natal dispersal 2015" = "red", "Natal dispersal 2016" = "orange"))
         
                           CIV_HR_plot

                   # ggsave("CIV_HR_ND_plot.jpg", plot = CIV_HR_plot)



# "IAB"

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

                   # ggsave("IAB_HR_ND_plot.jpg", plot = IAB_HR_plot)

# "IAD"

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

                   # ggsave("IAD_HR_ND_plot.jpg", plot = IAD_HR_plot)

# "IBH"

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
                           #geom_polygon(IBH_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = IBH, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBH_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBH winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           IBH_HR_plot

                   # ggsave("IBH_HR_ND_plot.jpg", plot = IBH_HR_plot)

# "IBI"

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

                   # ggsave("IBI_HR_ND_plot.jpg", plot = IBI_HR_plot)

# "IBK"

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

                   # ggsave("IBK_HR_ND_plot.jpg", plot = IBK_HR_plot)

# "IBS"

                 # get IBS winter HR
                            IBS_winter_HR <- getverticeshr(winter_HR$IBS, percent = 50) # 50% is the value to obtain the core area of the HR
                            IBS_winter_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IBS_winter_HR <- fortify(IBS_winter_HR)

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

                  # Plot the winter homerange
                           IBS_HR_plot <- 
                           ggplot(IBS_eu_utm) +
                           geom_spatvector()+
                           geom_polygon(IBS_winter_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           geom_path(data = IBS, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd20, aes(x = x, y = y, colour = "Natal dispersal 2020"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd21, aes(x = x, y = y, colour = "Natal dispersal 2021"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd22, aes(x = x, y = y, colour = "Natal dispersal 2022"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd23, aes(x = x, y = y, colour = "Natal dispersal 2023"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBS winter homerange and tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal 2020" = "magenta", "Natal dispersal 2021" = "red", "Natal dispersal 2022" = "orange", "Natal dispersal 2023" = "yellow"))
         
                           IBS_HR_plot

                   # ggsave("IBS_HR_ND_plot.jpg", plot = IBS_HR_plot)

# "ICZ"

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

                   # ggsave("ICZ_HR_ND_plot.jpg", plot = ICZ_HR_plot)




###########################
#     Non-breeding HR     #
###########################

# "A7"
          # ANIMAL USED TO TEST A NEW APPROACH

                    # First define the non-breeding period
                            A7_nonb <- osprey%>%
                                              dplyr::filter(ID == "A7" & time >= '2015-08-17 20:00:00' & time <= '2017-02-20 00:00:00')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            A7_nonb$ID <- factor(A7_nonb$ID)
          
                   # Let's create a spatialPoint object
                            A7_nonb_sp <- SpatialPointsDataFrame(A7_nonb[,c("x", "y")], A7_nonb)   
          
                   # Here I calculate the winter homerange with a Kernel Density Estimation
                            A7_nonb_kde <- kernelUD(A7_nonb_sp[,1], h = "href") # h = "LSCV"


                  # get A7 winter HR
                           A7_nonb_HR <- getverticeshr(A7_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                           A7_nonb_HR

                           A7_nonb_HRcore <- getverticeshr(A7_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
                           A7_nonb_HRcore

                  # fortify() function is needed to plot the winter homerange with ggplot
                           A7_nonb_HR <- fortify(A7_nonb_HR)
                           A7_nonb_HRcore <- fortify(A7_nonb_HRcore)

                  A7<- osprey%>%
                           filter(ID == 'A7')

                  A7_nd<- osprey_nd%>%
                           filter(ID == 'A7')

                  # Plot the winter homerange
                           A7_HR_plot <- 
                           ggplot(A7_eu_utm) +
                           geom_spatvector()+
                           geom_path(data = A7, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = A7_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           geom_polygon(A7_nonb_HR, mapping = aes(x=long, y=lat), color = "white") +
                           geom_polygon(A7_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "red"), color = "white") +
                           labs(x = " ", y = " ", title = "A7 non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           A7_HR_plot

                   # ggsave("A7_HR_ND_plot.jpg", plot = A7_HR_plot)

          
          # Non-breeding ltraj object

                       # Let's create a ltraj object with UTM coordinates
                              
                            A7_nonb_lt <- osprey%>%
                                              dplyr::filter(ID == "A7" & time >= '2015-08-17 20:00:00' & time <= '2017-02-20 00:00:00')
         
                            A7_nonb_lt <- as.ltraj(A7_nonb_lt[, c("x", "y")],
                                                  date = A7_nonb_lt$time, 
                                                  id = A7_nonb_lt$ID,
                                                  typeII=TRUE)

# "E7"
          # ANIMAL USED TO TEST A NEW APPROACH

                    # First define the non-breeding period
                            E7_nonb <- osprey%>%
                                              dplyr::filter(ID == 'E7' & time >= '2014-08-27 12:30:00' & time <= '2016-03-10 05:00:00')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            E7_nonb$ID <- factor(E7_nonb$ID)
          
                   # Let's create a spatialPoint object
                            E7_nonb_sp <- SpatialPointsDataFrame(E7_nonb[,c("x", "y")], E7_nonb)   
          
                   # Here I calculate the winter homerange with a Kernel Density Estimation
                            E7_nonb_kde <- kernelUD(E7_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get E7 winter HR
                            E7_nonb_HR <- getverticeshr( E7_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            E7_nonb_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           E7_nonb_HR <- fortify(E7_nonb_HR)

                            E7 <- osprey%>%
                                     filter(ID == 'E7')

                            E7_nd <- osprey_nd%>%
                                     filter(ID == 'E7')

                  # Plot the winter homerange
                           E7_HR_plot <- 
                           ggplot(E7_eu_utm) +
                           geom_spatvector()+
                           geom_path(data = E7, aes(x = x, y = y, colour = "Complete track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = E7_nd, aes(x = x, y = y, colour = "Natal dispersal"), linewidth = 0.5, lineend = "round") +
                           geom_polygon(E7_nonb_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           labs(x = " ", y = " ", title = "E7 non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           E7_HR_plot

                   # ggsave("E7_HR_ND_plot.jpg", plot = E7_HR_plot)

          
          # Non-breeding ltraj object

                       # Let's create a ltraj object with UTM coordinates
                              
                            E7_nonb_lt <- osprey%>%
                                              dplyr::filter(ID == 'E7' & time >= '2014-08-27 12:30:00' & time <= '2016-03-10 05:00:00')
         
                            E7_nonb_lt <- as.ltraj(E7_nonb_lt[, c("x", "y")],
                                                  date = E7_nonb_lt$time, 
                                                  id = E7_nonb_lt$ID,
                                                  typeII=TRUE)

# "H7"
          # ANIMAL USED TO TEST A NEW APPROACH

                    # First define the non-breeding period
                            H7_nonb <- osprey%>%
                                              dplyr::filter(ID == 'H7' & time >= '2013-08-10 00:00:00' & time <= '2015-04-02 05:00:00')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            H7_nonb$ID <- factor(H7_nonb$ID)
          
                   # Let's create a spatialPoint object
                            H7_nonb_sp <- SpatialPointsDataFrame(H7_nonb[,c("x", "y")], H7_nonb)   
          
                   # Here I calculate the winter homerange with a Kernel Density Estimation
                            H7_nonb_kde <- kernelUD(H7_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get H7 winter HR
                            H7_nonb_HR <- getverticeshr(H7_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            H7_nonb_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           H7_nonb_HR <- fortify(H7_nonb_HR)

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
                           geom_polygon(H7_nonb_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           labs(x = " ", y = " ", title = "H7 non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal" = "red"))
         
                           H7_HR_plot

                   # ggsave("H7_HR_ND_plot.jpg", plot = H7_HR_plot)

          
          # Non-breeding ltraj object

                       # Let's create a ltraj object with UTM coordinates
                              
                            H7_nonb_lt <- osprey%>%
                                              dplyr::filter(ID == 'H7' & time >= '2013-08-10 00:00:00' & time <= '2015-04-02 05:00:00')
         
                            H7_nonb_lt <- as.ltraj(H7_nonb_lt[, c("x", "y")],
                                                  date = H7_nonb_lt$time, 
                                                  id = H7_nonb_lt$ID,
                                                  typeII=TRUE)

# "IFP"
          # ANIMAL USED TO TEST A NEW APPROACH

                    # First define the non-breeding period
                            IFP_nonb <- osprey%>%
                                              dplyr::filter(ID == 'IFP' & time >= '2022-08-15 12:00:00' & time <= '2023-04-24 00:00:00')%>%
                                              dplyr::select(ID, x, y)%>%
                                              filter_at(vars(x, y), all_vars(!is.na(.)))
          
                            IFP_nonb$ID <- factor(IFP_nonb$ID)
          
                   # Let's create a spatialPoint object
                            IFP_nonb_sp <- SpatialPointsDataFrame(IFP_nonb[,c("x", "y")], IFP_nonb)   
          
                   # Here I calculate the winter homerange with a Kernel Density Estimation
                            IFP_nonb_kde <- kernelUD(IFP_nonb_sp[,1], h = "href") # h = "LSCV"

                 # get IFP winter HR
                            IFP_nonb_HR <- getverticeshr(IFP_nonb_kde, percent = 95) # 50% is the value to obtain the core area of the HR
                            IFP_nonb_HR

                  # fortify() function is needed to plot the winter homerange with ggplot
                           IFP_nonb_HR <- fortify(IFP_nonb_HR)

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
                           geom_polygon(IFP_nonb_HR, mapping = aes(x=long, y=lat, fill = group), color = "white") +
                           labs(x = " ", y = " ", title = "IFP non-breeding homerange and natal dispersal tracks") +
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Complete track" = "green", "Natal dispersal first travel" = "red", "Natal dispersal second travel" = "orange"))
         
                           IFP_HR_plot

                   # ggsave("IFP_HR_ND_plot.jpg", plot = IFP_HR_plot)

          
          # Non-breeding ltraj object

                       # Let's create a ltraj object with UTM coordinates
                              
                            IFP_nonb_lt <- osprey%>%
                                              dplyr::filter(ID == 'IFP' & time >= '2022-08-15 12:00:00' & time <= '2023-04-24 00:00:00')
         
                            IFP_nonb_lt <- as.ltraj(IFP_nonb_lt[, c("x", "y")],
                                                  date = IFP_nonb_lt$time, 
                                                  id = IFP_nonb_lt$ID,
                                                  typeII=TRUE)


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





