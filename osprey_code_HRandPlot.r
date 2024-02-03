
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

########
# "A7" # OK
########

# First define the non-breeding period
        A7_wintering1 <- st_df%>%
                          dplyr::filter(stop_id == "A7_wintering1")%>%
                          dplyr::select(ID, x, y)%>%
                          filter_at(vars(x, y), all_vars(!is.na(.)))

        A7_wintering1$id <- factor(A7_wintering1$ID)

# Let"s create a spatialPoint object
        A7_HR_sp <- SpatialPointsDataFrame(A7_wintering1[,c("x", "y")], A7_wintering1)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
        A7_HR_kde <- kernelUD(A7_HR_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get A7 non-breeding HR
       A7_HR_HR <- getverticeshr(A7_HR_kde, percent = 95)
       A7_HR_HR

       A7_HR_HRcore <- getverticeshr(A7_HR_kde, percent = 50)
       A7_HR_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
       A7_HR_HR <- fortify(A7_HR_HR)
       A7_HR_HRcore <- fortify(A7_HR_HRcore)


A7_stop1 <- st_df%>%
       filter(stop_id == "A7_stop1")
A7_stop2 <- st_df%>%
       filter(top_id == "A7_stop2")
A7_stop3 <- st_df%>%
       filter(stop_id == "A7_stop3")
A7_stop4 <- st_df%>%
       filter(stop_id == "A7_stop4")
A7_stop5 <- st_df%>%
       filter(stop_id == "A7_stop5")
A7_stop6 <- st_df%>%
       filter(stop_id == "A7_stop6")
A7_stop7 <- st_df%>%
       filter(stop_id == "A7_stop7")
A7_stop8 <- st_df%>%
       filter(stop_id == "A7_stop8")
A7_stop9 <- st_df%>%
       filter(stop_id == "A7_stop9")
A7_nest <- st_df%>%
       filter(stop_id == "A7_nest")
A7_end <- st_df%>%
       filter(stop_id == "A7_end")

A7_nd1a <- nd_df%>%
       filter(track_id == "A7_nd1a")
A7_nd2a <- nd_df%>%
       filter(track_id == "A7_nd2a")
A7_nd3a <- nd_df%>%
       filter(track_id == "A7_nd3a")
A7_nd3b <- nd_df%>%
       filter(track_id == "A7_nd3b")
A7_nd4a <- nd_df%>%
       filter(track_id == "A7_nd4a")
A7_nd4b <- nd_df%>%
       filter(track_id == "A7_nd4b")
A7_nd4c <- nd_df%>%
       filter(track_id == "A7_nd4c")
A7_nd4d <- nd_df%>%
       filter(track_id == "A7_nd4d")
A7_nd4e <- nd_df%>%
       filter(track_id == "A7_nd4e")
A7_nd5a <- nd_df%>%
       filter(track_id == "A7_nd5a")
A7_nd6a <- nd_df%>%
       filter(track_id == "A7_nd6a")


# Plot the nonbreeding HR
A7_HR_plot <- 
        ggplot(A7_eu_utm) + 
        geom_spatvector()+
        geom_polygon(A7_HR_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(A7_HR_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_path(data = A7_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop8, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_stop9, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd4a, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd4b, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd4c, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd4d, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd4e, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd5a, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = A7_nd6a, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
        labs(x = " ", y = " ", title = "A7 non-breeding HR and ND movements tracks") +
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
        scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red")) 

A7_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/A7_HR_ND_plot.jpg", plot = A7_HR_plot)

#############
# "Antares" #
#############

# Traccaito difficile da analizzare, da rivedere

# First define the non-breeding period
Antares_wintering1 <- st_df%>%
                  dplyr::filter(stop_id == "Antares_wintering1")%>%
                  dplyr::select(ID, x, y)%>%
                  filter_at(vars(x, y), all_vars(!is.na(.)))

Antares_wintering1$ID <- factor(Antares_wintering1$ID)

# Let"s create a spatialPoint object
        Antares_HR_sp <- SpatialPointsDataFrame(Antares_wintering1[,c("x", "y")], Antares_wintering1)

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
        Antares_HR_kde <- kernelUD(Antares_HR_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get Antares non-breeding HR
        Antares_HR <- getverticeshr(Antares_HR_kde, percent = 95)
        Antares_HR

        Antares_HRcore <- getverticeshr(Antares_HR_kde, percent = 50) # 50% is the value to obtain the core area of the HR
        Antares_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
       Antares_HR <- fortify(Antares_HR)
       Antares_HRcore <- fortify(Antares_HRcore)

#Antares_nonb_HR_1 <- Antares_nonb_HR%>%
#        filter(piece == 1)

#Antares_nonb_HR_2 <- Antares_nonb_HR%>%
#        filter(piece == 2)

#Antares_nonb_HR_3 <- Antares_nonb_HR%>%
#        filter(piece == 3)

#Antares_nonb_HR_4 <- Antares_nonb_HR%>%
#        filter(piece == 4)


Antares_nest <- st_df%>%
       filter(stop_id == "Antares_nest")
Antares_wintering1 <- st_df%>%
       filter(stop_id == "Antares_wintering1")
Antares_stop1 <- st_df%>%
       filter(stop_id == "Antares_stop1")
Antares_stop2 <- st_df%>%
       filter(stop_id == "Antares_stop2")
Antares_stop3 <- st_df%>%
       filter(stop_id == "Antares_stop3")
Antares_stop4 <- st_df%>%
       filter(stop_id == "Antares_stop4")
Antares_stop5 <- st_df%>%
       filter(stop_id == "Antares_stop5")
Antares_stop6 <- st_df%>%
       filter(stop_id == "Antares_stop6")
Antares_stop7 <- st_df%>%
       filter(stop_id == "Antares_stop7")
Antares_stop8 <- st_df%>%
       filter(stop_id == "Antares_stop8")
Antares_stop9 <- st_df%>%
       filter(stop_id == "Antares_stop9")
Antares_end <- st_df%>%
       filter(stop_id == "Antares_end")

Antares_nd1a <- nd_df%>%
       filter(track_id == "Antares_nd1a")
Antares_nd2a <- nd_df%>%
       filter(track_id == "Antares_nd2a")
Antares_nd2b <- nd_df%>%
       filter(track_id == "Antares_nd2b")
Antares_nd3a <- nd_df%>%
       filter(track_id == "Antares_nd3a")
Antares_nd3b <- nd_df%>%
       filter(track_id == "Antares_nd3b")
Antares_nd3c <- nd_df%>%
       filter(track_id == "Antares_nd3c")
Antares_nd3d <- nd_df%>%
       filter(track_id == "Antares_nd3d")
Antares_nd4a <- nd_df%>%
       filter(track_id == "Antares_nd4a")
Antares_nd4b <- nd_df%>%
       filter(track_id == "Antares_nd4b")
Antares_nd5a <- nd_df%>%
       filter(track_id == "Antares_nd5a")


# Plot the non-breeding homerange
Antares_HR_plot <- 
       ggplot(Antares_eu_utm) +
       geom_spatvector()+
       geom_polygon(Antares_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
       geom_polygon(Antares_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
    #   geom_polygon(Antares_nonb_HR_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
    #   geom_polygon(Antares_nonb_HR_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
    #   geom_polygon(Antares_nonb_HR_3, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
    #   geom_polygon(Antares_nonb_HR_4, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
    #   geom_polygon(Antares_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
       geom_path(data = Antares_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop8, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_stop9, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd2b, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd3b, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd3c, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd3d, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd4a, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd4b, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
       geom_path(data = Antares_nd5a, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
       labs(x = " ", y = " ", title = "Antares non-breeding HR and ND movements tracks") +
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
  scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red")) 

       Antares_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/Antares_HR_ND_plot.jpg", plot = Antares_HR_plot)


#######
# CAM # OK
#######

# First define the non-breeding period
        CAM_wintering1 <- st_df%>%
                          dplyr::filter(stop_id == "CAM_wintering1")%>%
                          dplyr::select(ID, x, y)%>%
                          filter_at(vars(x, y), all_vars(!is.na(.)))

        CAM_wintering1$ID <- factor(CAM_wintering1$ID)

# Let"s create a spatialPoint object
        CAM_nonb_sp <- SpatialPointsDataFrame(CAM_wintering1[,c("x", "y")], CAM_wintering1)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
        CAM_nonb_kde <- kernelUD(CAM_nonb_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get CAM non-breeding HR
        CAM_nonb_HR <- getverticeshr(CAM_nonb_kde, percent = 95)
        CAM_nonb_HR

        CAM_nonb_HRcore <- getverticeshr(CAM_nonb_kde, percent = 50) # 50% is the value to obtain the core area of the HR
        CAM_nonb_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
       CAM_nonb_HR <- fortify(CAM_nonb_HR)
       CAM_nonb_HRcore <- fortify(CAM_nonb_HRcore)


CAM_nest <- st_df%>%
       filter(stop_id == "CAM_nest")
CAM_stop1 <- st_df%>%
       filter(stop_id == "CAM_stop1")
CAM_stop2 <- st_df%>%
       filter(stop_id == "CAM_stop2")
CAM_stop3 <- st_df%>%
       filter(stop_id == "CAM_stop3")
CAM_stop4 <- st_df%>%
       filter(stop_id == "CAM_stop4")
CAM_end <- st_df%>%
       filter(stop_id == "CAM_end")

CAM_nd1a <- nd_df%>%
       filter(track_id == "CAM_nd1a")
CAM_nd1b <- nd_df%>%
       filter(track_id == "CAM_nd1b")
CAM_nd2a <- nd_df%>%
       filter(track_id == "CAM_nd2a")
CAM_nd3a <- nd_df%>%
       filter(track_id == "CAM_nd3a")
CAM_nd4a <- nd_df%>%
       filter(track_id == "CAM_nd4a")

# Plot the non-breeding homerange
       CAM_HR_plot <- 
                 ggplot(CAM_eu_utm) +
                 geom_spatvector()+
               #  geom_polygon(CAM_nonb_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
               #  geom_polygon(CAM_nonb_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                 geom_path(data = CAM_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_nd1b, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CAM_nd4a, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
                 labs(x = " ", y = " ", title = "CAM non-breeding HR and ND movements tracks") +
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
  scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red")) 

       CAM_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/CAM_HR_ND_plot.jpg", plot = CAM_HR_plot)


#########
# "CBK" # OK
#########

# First define the non-breeding period
        CBK_wintering1 <- st_df%>%
                          dplyr::filter(stop_id == "CBK_wintering1")%>%
                          dplyr::select(ID, x, y)%>%
                          filter_at(vars(x, y), all_vars(!is.na(.)))

        CBK_wintering1$ID <- factor(CBK_wintering1$ID)

# Let"s create a spatialPoint object
        CBK_HR_sp <- SpatialPointsDataFrame(CBK_wintering1[,c("x", "y")], CBK_wintering1)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
        CBK_HR_kde <- kernelUD(CBK_HR_sp[,1], h = "href", grid = 200) # h = "LSCV"

# get CBK non-breeding HR
        CBK_HR <- getverticeshr(CBK_HR_kde, percent = 95) # 50% is the value to obtain the core area of the HR
        CBK_HR

        CBK_HRcore <- getverticeshr(CBK_HR_kde, percent = 50) # 50% is the value to obtain the core area of the HR
        CBK_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
       CBK_HR <- fortify(CBK_HR)
       CBK_HRcore <- fortify(CBK_HRcore)

CBK_nest <- st_df%>%
       filter(stop_id == "CBK_nest")
CBK_stop1a <- st_df%>%
       filter(stop_id == "CBK_stop1a")
CBK_stop2a <- st_df%>%
       filter(stop_id == "CBK_stop2a")
CBK_end <- st_df%>%
       filter(stop_id == "CBK_end")

CBK_nd1a <- nd_df%>%
       filter(track_id == "CBK_nd1a")
CBK_nd2a <- nd_df%>%
       filter(track_id == "CBK_nd2a")
CBK_nd3a <- nd_df%>%
       filter(track_id == "CBK_nd3a")

# Plot the non-breeding homerange
       CBK_HR_plot <- 
                 ggplot(CBK_eu_utm) +
                 geom_spatvector()+
                 geom_polygon(CBK_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                 geom_polygon(CBK_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                 geom_path(data = CBK_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CBK_stop1a, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CBK_stop2a, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CBK_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CBK_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CBK_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = CBK_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                 labs(x = " ", y = " ", title = "CBK non-breeding HR and ND movements tracks") +
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
  scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red")) 

       CBK_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/CBK_HR_ND_plot.jpg", plot = CBK_HR_plot)


#########
# "CIV" # OK
#########

# First define the non-breeding period
        CIV_wintering1 <- st_df%>%
                          dplyr::filter(stop_id == "CIV_wintering1")%>%
                          dplyr::select(ID, x, y)%>%
                          filter_at(vars(x, y), all_vars(!is.na(.)))

        CIV_wintering1$ID <- factor(CIV_wintering1$ID)

        CIV_wintering2 <- st_df%>%
                          dplyr::filter(stop_id == "CIV_wintering2")%>%
                          dplyr::select(ID, x, y)%>%
                          filter_at(vars(x, y), all_vars(!is.na(.)))

        CIV_wintering2$ID <- factor(CIV_wintering2$ID)

# Let"s create a spatialPoint object
        CIV_HR1_sp <- SpatialPointsDataFrame(CIV_wintering1[,c("x", "y")], CIV_wintering1)
        CIV_HR2_sp <- SpatialPointsDataFrame(CIV_wintering2[,c("x", "y")], CIV_wintering2)

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
        CIV_HR1_kde <- kernelUD(CIV_HR1_sp[,1], h = "href", grid = 400) # h = "LSCV"
        CIV_HR2_kde <- kernelUD(CIV_HR1_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get CIV non-breeding HR
        CIV_HR1 <- getverticeshr(CIV_HR1_kde, percent = 95) 
        CIV_HR1

        CIV_HR1core <- getverticeshr(CIV_HR1_kde, percent = 50) # 50% is the value to obtain the core area of the HR
        CIV_HR1core

        CIV_HR2 <- getverticeshr(CIV_HR2_kde, percent = 95) 
        CIV_HR2

        CIV_HR2core <- getverticeshr(CIV_HR2_kde, percent = 50) # 50% is the value to obtain the core area of the HR
        CIV_HR2core

# fortify() function is needed to plot the non-breeding homerange with ggplot
       CIV_HR1 <- fortify(CIV_HR1)
       CIV_HR1core <- fortify(CIV_HR1core)

       CIV_HR2 <- fortify(CIV_HR2)
       CIV_HR2core <- fortify(CIV_HR2core)

CIV_HR1_1 <- CIV_HR1%>%
        filter(group == "CIV.1")
CIV_HR1_2 <- CIV_HR1%>%
        filter(group == "CIV.2")
CIV_HR1_3 <- CIV_HR1%>%
        filter(group == "CIV.3")


CIV_HR2_1 <- CIV_HR2%>%
         filter(group == "CIV.1")
CIV_HR2_2 <- CIV_HR2%>%
        filter(group == "CIV.2")
CIV_HR2_3 <- CIV_HR2%>%
        filter(group == "CIV.3")


CIV_nest <- st_df%>%
       filter(stop_id == "CIV_nest")
CIV_stop1 <- st_df%>%
       filter(stop_id == "CIV_stop1")
CIV_stop2 <- st_df%>%
       filter(stop_id == "CIV_stop2")
CIV_wintering1 <- st_df%>%
       filter(stop_id == "CIV_wintering1")
CIV_stop3 <- st_df%>%
       filter(stop_id == "CIV_stop3")
CIV_stop4 <- st_df%>%
       filter(stop_id == "CIV_stop4")
CIV_stop5 <- st_df%>%
       filter(stop_id == "CIV_stop5")
CIV_wintering2 <- st_df%>%
       filter(stop_id == "CIV_wintering2")
CIV_stop6 <- st_df%>%
       filter(stop_id == "CIV_stop6")
CIV_stop7 <- st_df%>%
       filter(stop_id == "CIV_stop7")
CIV_end <- st_df%>%
       filter(stop_id == "CIV_end")

CIV_nd1a <- nd_df%>%
       filter(track_id == "CIV_nd1a")
CIV_nd2a <- nd_df%>%
       filter(track_id == "CIV_nd2a")
CIV_nd3a <- nd_df%>%
       filter(track_id == "CIV_nd3a")
CIV_nd4a <- nd_df%>%
       filter(track_id == "CIV_nd4a")
CIV_nd5a <- nd_df%>%
       filter(track_id == "CIV_nd5a")
CIV_nd6a <- nd_df%>%
       filter(track_id == "CIV_nd6a")
CIV_nd7a <- nd_df%>%
       filter(track_id == "CIV_nd7a")
CIV_nd8a <- nd_df%>%
       filter(track_id == "CIV_nd8a")
CIV_nd9a <- nd_df%>%
       filter(track_id == "CIV_nd9a")
CIV_nd10a <- nd_df%>%
       filter(track_id == "CIV_nd10a")


# Plot the non-breeding homerange    
CIV_HR_plot <- 
       ggplot(CIV_eu_utm) +
       geom_spatvector()+
#       geom_polygon(CIV_HR1_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
#       geom_polygon(CIV_HR1_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
#       geom_polygon(CIV_HR1_3, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
#       geom_polygon(CIV_HR1core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
       geom_polygon(CIV_HR2_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
       geom_polygon(CIV_HR2_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
       geom_polygon(CIV_HR2_3, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
       geom_polygon(CIV_HR2core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
#       geom_path(data = CIV_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_stop5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_wintering2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_stop6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_stop7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd4a, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd5a, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd6a, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd7a, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd8a, aes(x = x, y = y, colour = "Natal dispersal 8th travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd9a, aes(x = x, y = y, colour = "Natal dispersal 9th travel"), linewidth = 0.5, lineend = "round") +
#       geom_path(data = CIV_nd10a, aes(x = x, y = y, colour = "Natal dispersal 10th travel"), linewidth = 0.5, lineend = "round") +
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
        scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red")) 

CIV_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/CIV_HR_ND_plot.jpg", plot = CIV_HR_plot)


########
# "E7" # OK
########

# First define the non-breeding period
        E7_wintering1 <- st_df%>%
                          dplyr::filter(stop_id == "E7_wintering1")%>%
                          dplyr::select(ID, x, y)%>%
                          filter_at(vars(x, y), all_vars(!is.na(.)))

        E7_wintering1$ID <- factor(E7_wintering1$ID)

# Let"s create a spatialPoint object
        E7_HR_sp <- SpatialPointsDataFrame(E7_wintering1[,c("x", "y")], E7_wintering1)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
        E7_HR_kde <- kernelUD(E7_HR_sp[,1], h = "href", grid=400) # h = "LSCV"

# get E7 non-breeding HR
        E7_HR <- getverticeshr(E7_HR_kde, percent = 95) # 50% is the value to obtain the core area of the HR
        E7_HR

        E7_HRcore <- getverticeshr(E7_HR_kde, percent = 50) # 50% is the value to obtain the core area of the HR
        E7_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
       E7_HR <- fortify(E7_HR)
       E7_HRcore <- fortify(E7_HRcore)

E7_HR_1 <- E7_HR%>%
        filter(group == "E7.1")

E7_HR_2 <- E7_HR%>%
        filter(group == "E7.2")

E7_HR_3 <- E7_HR%>%
        filter(group == "E7.3")

E7_nest <- st_df%>%
         filter(stop_id == "E7_nest")
E7_stop1 <- st_df%>%
         filter(stop_id == "E7_stop1")
E7_stop2 <- st_df%>%
         filter(stop_id == "E7_stop2")
E7_stop3 <- st_df%>%
         filter(stop_id == "E7_stop3")
E7_stop4 <- st_df%>%
         filter(stop_id == "E7_stop4")
E7_stop5 <- st_df%>%
         filter(stop_id == "E7_stop5")
E7_stop6 <- st_df%>%
         filter(stop_id == "E7_stop6")
E7_stop7 <- st_df%>%
         filter(stop_id == "E7_stop7")
E7_stop8 <- st_df%>%
         filter(stop_id == "E7_stop8")
E7_stop9 <- st_df%>%
         filter(stop_id == "E7_stop9")
E7_stop10 <- st_df%>%
         filter(stop_id == "E7_stop10")
E7_end <- st_df%>%
         filter(stop_id == "E7_end")

E7_nd1a <- nd_df%>%
         filter(track_id == "E7_nd1a")
E7_nd2a <- nd_df%>%
         filter(track_id == "E7_nd2a")
E7_nd2b <- nd_df%>%
         filter(track_id == "E7_nd2b")
E7_nd2c <- nd_df%>%
         filter(track_id == "E7_nd2c")
E7_nd2d <- nd_df%>%
         filter(track_id == "E7_nd2d")
E7_nd2e <- nd_df%>%
         filter(track_id == "E7_nd2e")
E7_nd2f <- nd_df%>%
         filter(track_id == "E7_nd2f")
E7_nd2g <- nd_df%>%
         filter(track_id == "E7_nd2g")
E7_nd3a <- nd_df%>%
         filter(track_id == "E7_nd3a")
E7_nd3b <- nd_df%>%
         filter(track_id == "E7_nd3b")
E7_nd3c <- nd_df%>%
         filter(track_id == "E7_nd3c")
E7_nd3d <- nd_df%>%
         filter(track_id == "E7_nd3d")

# Plot the non-breeding homerange   
E7_HR_plot <- 
                 ggplot(E7_eu_utm) +
                 geom_spatvector()+
                 geom_polygon(E7_HR_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                 geom_polygon(E7_HR_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                 geom_polygon(E7_HR_3, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
                 geom_polygon(E7_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
                 geom_path(data = E7_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop8, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop9, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_stop10, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd2b, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd2c, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd2d, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd2e, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd2f, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd2g, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd3b, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd3c, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                 geom_path(data = E7_nd3d, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
                 labs(x = " ", y = " ", title = "E7 non-breeding HR and ND movements tracks") +
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
  scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red")) 

E7_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/E7_HR_ND_plot.jpg", plot = E7_HR_plot)






### RIPRENDI DA QUI




########
# "H7" # OK
########

# First define the non-breeding period
        H7_wintering1 <- st_df%>%
                          dplyr::filter(stop_id == "H7_wintering1")%>%
                          dplyr::select(ID, x, y)%>%
                          filter_at(vars(x, y), all_vars(!is.na(.)))

        H7_wintering1$ID <- factor(H7_wintering1$ID)

# Let"s create a spatialPoint object
        H7_HR_sp <- SpatialPointsDataFrame(H7_wintering1[,c("x", "y")], H7_wintering1)


# Here I calculate the non-breeding homerange with a Kernel Density Estimation
        H7_HR_kde <- kernelUD(H7_HR_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get H7 non-breeding HR
        H7_HR <- getverticeshr(H7_HR_kde, percent = 95)
        H7_HR

        H7_HRcore <- getverticeshr(H7_HR_kde, percent = 50) # 50% is the value to obtain the core area of the HR
        H7_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
       H7_HR <- fortify(H7_HR)
       H7_HRcore <- fortify(H7_HRcore)

H7_nest <- st_df%>%
        filter(stop_id == "H7_nest")
H7_wintering1 <- st_df%>%
        filter(stop_id == "H7_wintering1")
H7_stop1 <- st_df%>%
        filter(stop_id == "H7_stop1")
H7_stop2 <- st_df%>%
        filter(stop_id == "H7_stop2")
H7_end <- st_df%>%
        filter(stop_id == "H7_end")

H7_nd1a <- nd_df%>%
        filter(track_id == "H7_nd1a")
H7_nd2a <- nd_df%>%
        filter(track_id == "H7_nd2a")
H7_nd2b <- nd_df%>%
        filter(track_id == "H7_nd2b")
H7_nd3a <- nd_df%>%
        filter(track_id == "H7_nd3a")

# Plot the non-breeding homerange  -> H7_eu_utm
H7_HR_plot <- 
         ggplot(H7_eu_utm) +
         geom_spatvector()+
         geom_polygon(H7_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
         geom_polygon(H7_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
         geom_path(data = H7_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
         geom_path(data = H7_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
         geom_path(data = H7_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
         geom_path(data = H7_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
         geom_path(data = H7_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
         geom_path(data = H7_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
         geom_path(data = H7_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
         geom_path(data = H7_nd2b, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
         geom_path(data = H7_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
         labs(x = " ", y = " ", title = "H7 non-breeding HR and ND movements tracks") +
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
        scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red")) 

H7_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/H7_HR_ND_plot.jpg", plot = H7_HR_plot)



#########
# "IAB" # 
#########

# First define the non-breeding period
IAB_wintering1 <- st_df%>%
        dplyr::filter(stop_id == "IAB_wintering1")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IAB_wintering1$ID <- factor(IAB_wintering1$ID)

IAB_wintering2 <- st_df%>%
        dplyr::filter(stop_id == "IAB_wintering2")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IAB_wintering2$ID <- factor(IAB_wintering2$ID)

# Let"s create a spatialPoint object
IAB_HR1_sp <- SpatialPointsDataFrame(IAB_wintering1[,c("x", "y")], IAB_wintering1)
IAB_HR2_sp <- SpatialPointsDataFrame(IAB_wintering2[,c("x", "y")], IAB_wintering2)

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
IAB_HR1_kde <- kernelUD(IAB_HR1_sp[,1], h = "href", grid = 400) # h = "LSCV"
IAB_HR2_kde <- kernelUD(IAB_HR2_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get IAB non-breeding HR
IAB_HR1 <- getverticeshr(IAB_HR1_kde, percent = 95) 
IAB_HR1
IAB_HR1core <- getverticeshr(IAB_HR1_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IAB_HR1core

IAB_HR2 <- getverticeshr(IAB_HR2_kde, percent = 95) 
IAB_HR2
IAB_HR2core <- getverticeshr(IAB_HR2_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IAB_HR2core

# fortify() function is needed to plot the non-breeding homerange with ggplot
IAB_HR1 <- fortify(IAB_HR1)
IAB_HR1core <- fortify(IAB_HR1core)

IAB_HR2 <- fortify(IAB_HR2)
IAB_HR2core <- fortify(IAB_HR2core)

IAB_nest <- st_df%>%
         filter(stop_id == "IAB_nest")
IAB_wintering1 <- st_df%>%
         filter(stop_id == "IAB_wintering1")
IAB_stop1 <- st_df%>%
         filter(stop_id == "IAB_stop1")
IAB_stop2 <- st_df%>%
         filter(stop_id == "IAB_stop2")
IAB_stop3 <- st_df%>%
         filter(stop_id == "IAB_stop3")
IAB_stop4 <- st_df%>%
         filter(stop_id == "IAB_stop4")
IAB_stop5 <- st_df%>%
         filter(stop_id == "IAB_stop5")
IAB_stop6 <- st_df%>%
         filter(stop_id == "IAB_stop6")
IAB_stop7 <- st_df%>%
         filter(stop_id == "IAB_stop7")
IAB_stop8 <- st_df%>%
         filter(stop_id == "IAB_stop8")
IAB_stop9 <- st_df%>%
         filter(stop_id == "IAB_stop9")
IAB_stop10 <- st_df%>%
         filter(stop_id == "IAB_stop10")
IAB_wintering2 <- st_df%>%
         filter(stop_id == "IAB_wintering2")
IAB_end <- st_df%>%
         filter(stop_id == "IAB_end")

IAB_nd1a <- nd_df%>%
         filter(track_id == "IAB_nd1a")
IAB_nd2a <- nd_df%>%
         filter(track_id == "IAB_nd2a")
IAB_nd3a <- nd_df%>%
         filter(track_id == "IAB_nd3a")
IAB_nd3b <- nd_df%>%
         filter(track_id == "IAB_nd3b")
IAB_nd3c <- nd_df%>%
         filter(track_id == "IAB_nd3c")
IAB_nd4a <- nd_df%>%
         filter(track_id == "IAB_nd4a")
IAB_nd4b <- nd_df%>%
         filter(track_id == "IAB_nd4b")
IAB_nd5a <- nd_df%>%
         filter(track_id == "IAB_nd5a")
IAB_nd5b <- nd_df%>%
         filter(track_id == "IAB_nd5b")
IAB_nd6a <- nd_df%>%
         filter(track_id == "IAB_nd6a")
IAB_nd6b <- nd_df%>%
         filter(track_id == "IAB_nd6b")
IAB_nd7a <- nd_df%>%
         filter(track_id == "IAB_nd7a")


# Plot the non-breeding homerange
IAB_HR_plot <- 
        ggplot(IAB_eu_utm) +
        geom_spatvector()+
        geom_polygon(IAB_HR1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(IAB_HR1core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
#        geom_polygon(IAB_HR2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
#        geom_polygon(IAB_HR2core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
 #       geom_path(data = IAB_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop8, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop9, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_stop10, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_wintering2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd3b, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd4a, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd4b, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd5a, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd5b, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd6a, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd6b, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
 #       geom_path(data = IAB_nd7a, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
        labs(x = " ", y = " ", title = "IAB non-breeding HR and ND movements tracks") +
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
          scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                            "Non-Breeding HR core area" = "red")) 
IAB_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/IAB_HR_ND_plot.jpg", plot = IAB_HR_plot)


#########
# "IAD" # OK
#########

# First define the non-breeding period
IAD_wintering1 <- st_df%>%
                  dplyr::filter(stop_id == "IAD_wintering1")%>%
                  dplyr::select(ID, x, y)%>%
                  filter_at(vars(x, y), all_vars(!is.na(.)))

IAD_wintering1$ID <- factor(IAD_wintering1$ID)

IAD_wintering2 <- st_df%>%
                  dplyr::filter(stop_id == "IAD_wintering2")%>%
                  dplyr::select(ID, x, y)%>%
                  filter_at(vars(x, y), all_vars(!is.na(.)))

IAD_wintering2$ID <- factor(IAD_wintering2$ID)


# Let"s create a spatialPoint object
IAD_HR1_sp <- SpatialPointsDataFrame(IAD_wintering1[,c("x", "y")], IAD_wintering1)   
IAD_HR2_sp <- SpatialPointsDataFrame(IAD_wintering2[,c("x", "y")], IAD_wintering2)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
IAD_HR1_kde <- kernelUD(IAD_HR1_sp[,1], h = "href", grid = 400) # h = "LSCV"
IAD_HR2_kde <- kernelUD(IAD_HR2_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get IAD non-breeding HR
IAD_HR1 <- getverticeshr(IAD_HR1_kde, percent = 95)
IAD_HR1
IAD_HR1core <- getverticeshr(IAD_HR1_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IAD_HR1core

IAD_HR2 <- getverticeshr(IAD_HR2_kde, percent = 95)
IAD_HR2
IAD_HR2core <- getverticeshr(IAD_HR2_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IAD_HR2core

# fortify() function is needed to plot the non-breeding homerange with ggplot
IAD_HR1 <- fortify(IAD_HR1)
IAD_HR1core <- fortify(IAD_HR1core)

IAD_HR2 <- fortify(IAD_HR2)
IAD_HR2core <- fortify(IAD_HR2core)


IAD_nest <- st_df%>%
         filter(stop_id == "IAD_nest")
IAD_wintering1 <- st_df%>%
         filter(stop_id == "IAD_wintering1")
IAD_stop1 <- st_df%>%
         filter(stop_id == "IAD_stop1")
IAD_stop2 <- st_df%>%
         filter(stop_id == "IAD_stop2")
IAD_stop3 <- st_df%>%
         filter(stop_id == "IAD_stop3")
IAD_stop4 <- st_df%>%
         filter(stop_id == "IAD_stop4")
IAD_stop5 <- st_df%>%
         filter(stop_id == "IAD_stop5")
IAD_stop6 <- st_df%>%
         filter(stop_id == "IAD_stop6")
IAD_stop7 <- st_df%>%
         filter(stop_id == "IAD_stop7")
IAD_stop8 <- st_df%>%
         filter(stop_id == "IAD_stop8")
IAD_wintering2 <- st_df%>%
         filter(stop_id == "IAD_wintering2")
IAD_stop9 <- st_df%>%
         filter(stop_id == "IAD_stop9")
IAD_stop10 <- st_df%>%
         filter(stop_id == "IAD_stop10")
IAD_stop11 <- st_df%>%
         filter(stop_id == "IAD_stop11")
IAD_stop12 <- st_df%>%
         filter(stop_id == "IAD_stop12")
IAD_stop13 <- st_df%>%
         filter(stop_id == "IAD_stop13")
IAD_stop14 <- st_df%>%
         filter(stop_id == "IAD_stop14")
IAD_stop15 <- st_df%>%
         filter(stop_id == "IAD_stop15")
IAD_stop16 <- st_df%>%
         filter(stop_id == "IAD_stop16")
IAD_stop17 <- st_df%>%
         filter(stop_id == "IAD_stop17")
IAD_end <- st_df%>%
         filter(stop_id == "IAD_end")

IAD_nd1a <- nd_df%>%
         filter(track_id == "IAD_nd1a")
IAD_nd2a <- nd_df%>%
         filter(track_id == "IAD_nd2a")
IAD_nd3a <- nd_df%>%
         filter(track_id == "IAD_nd3a")
IAD_nd3b <- nd_df%>%
         filter(track_id == "IAD_nd3b")
IAD_nd3c <- nd_df%>%
         filter(track_id == "IAD_nd3c")
IAD_nd3d <- nd_df%>%
         filter(track_id == "IAD_nd3d")
IAD_nd4a <- nd_df%>%
         filter(track_id == "IAD_nd4a")
IAD_nd5a <- nd_df%>%
         filter(track_id == "IAD_nd5a")
IAD_nd5b <- nd_df%>%
         filter(track_id == "IAD_nd5b")
IAD_nd6a <- nd_df%>%
         filter(track_id == "IAD_nd6a")
IAD_nd7a <- nd_df%>%
         filter(track_id == "IAD_nd7a")
IAD_nd7b <- nd_df%>%
         filter(track_id == "IAD_nd7b")
IAD_nd7c <- nd_df%>%
         filter(track_id == "IAD_nd7c")
IAD_nd7d <- nd_df%>%
         filter(track_id == "IAD_nd7d")
IAD_nd7e <- nd_df%>%
         filter(track_id == "IAD_nd7e")
IAD_nd7f <- nd_df%>%
         filter(track_id == "IAD_nd7f")
IAD_nd7g <- nd_df%>%
         filter(track_id == "IAD_nd7g")
IAD_nd8a <- nd_df%>%
         filter(track_id == "IAD_nd8a")
IAD_nd9a <- nd_df%>%
         filter(track_id == "IAD_nd9a")
IAD_nd9b <- nd_df%>%
         filter(track_id == "IAD_nd9b")


# Plot the non-breeding homerange  
IAD_HR_plot <- 
        ggplot(IAD_eu_utm) +
        geom_spatvector()+
    #    geom_polygon(IAD_HR1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
    #    geom_polygon(IAD_HR1core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_polygon(IAD_HR2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(IAD_HR2core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
    #    geom_path(data = IAD_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop8, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_wintering2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop9, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop10, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop11, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop12, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop13, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop14, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop15, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop16, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_stop17, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd3b, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd3c, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd3d, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd4a, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd5a, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd5b, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd6a, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd7a, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd7b, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd7c, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd7d, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd7e, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd7f, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd7g, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd8a, aes(x = x, y = y, colour = "Natal dispersal 8th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd9a, aes(x = x, y = y, colour = "Natal dispersal 9th travel"), linewidth = 0.5, lineend = "round") +
    #    geom_path(data = IAD_nd9b, aes(x = x, y = y, colour = "Natal dispersal 9th travel"), linewidth = 0.5, lineend = "round") +
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
                                                        "Natal dispersal 8th travel" = "yellow",
                                                        "Natal dispersal 9th travel" = "purple",
                                                        "Natal dispersal 10th travel" = "red")) +
          scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                            "Non-Breeding HR core area" = "red"))

IAD_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/IAD_HR_ND_plot.jpg", plot = IAD_HR_plot)


#########
# "IBH" # OK
#########

# First define the non-breeding period
IBH_wintering1 <- st_df%>%
        dplyr::filter(stop_id == "IBH_wintering1")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IBH_wintering1$ID <- factor(IBH_wintering1$ID)

# Let"s create a spatialPoint object
IBH_HR_sp <- SpatialPointsDataFrame(IBH_wintering1[,c("x", "y")], IBH_wintering1)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
IBH_HR_kde <- kernelUD(IBH_HR_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get IBH non-breeding HR
IBH_HR <- getverticeshr(IBH_HR_kde, percent = 95) # 50% is the value to obtain the core area of the HR
IBH_HR

IBH_HRcore <- getverticeshr(IBH_HR_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IBH_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
IBH_HR <- fortify(IBH_HR)
IBH_HRcore <- fortify(IBH_HRcore)

IBH_HR_1 <- IBH_HR%>%
        filter(group == "IBH.1")
IBH_HR_2 <- IBH_HR%>%
        filter(group == "IBH.2")
IBH_HR_3 <- IBH_HR%>%
        filter(group == "IBH.3")


IBH_nest <- st_df%>%
        filter(stop_id == "IBH_nest")
IBH_stop1 <- st_df%>%
        filter(stop_id == "IBH_stop1")
IBH_stop2 <- st_df%>%
        filter(stop_id == "IBH_stop2")
IBH_wintering1 <- st_df%>%
        filter(stop_id == "IBH_wintering1")
IBH_stop3 <- st_df%>%
        filter(stop_id == "IBH_stop3")
IBH_stop4 <- st_df%>%
        filter(stop_id == "IBH_stop4")
IBH_stop5 <- st_df%>%
        filter(stop_id == "IBH_stop5")
IBH_stop6 <- st_df%>%
        filter(stop_id == "IBH_stop6")
IBH_stop7 <- st_df%>%
        filter(stop_id == "IBH_stop7")
IBH_stop8 <- st_df%>%
        filter(stop_id == "IBH_stop8")
IBH_end <- st_df%>%
        filter(stop_id == "IBH_end")


IBH_nd1a <- nd_df%>%
        filter(track_id == "IBH_nd1a")
IBH_nd1b <- nd_df%>%
        filter(track_id == "IBH_nd1b")
IBH_nd1c <- nd_df%>%
        filter(track_id == "IBH_nd1c")
IBH_nd2a <- nd_df%>%
        filter(track_id == "IBH_nd2a")
IBH_nd3a <- nd_df%>%
        filter(track_id == "IBH_nd3a")
IBH_nd3b <- nd_df%>%
        filter(track_id == "IBH_nd3b")
IBH_nd3c <- nd_df%>%
        filter(track_id == "IBH_nd3c")
IBH_nd3d <- nd_df%>%
        filter(track_id == "IBH_nd3d")
IBH_nd3e <- nd_df%>%
        filter(track_id == "IBH_nd3e")


# Plot the non-breeding homerange  
IBH_HR_plot <- 
        ggplot(IBH_eu_utm) +
        geom_spatvector()+
        geom_polygon(IBH_HR_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(IBH_HR_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(IBH_HR_3, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(IBH_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_path(data = IBH_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_stop5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_stop6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_stop7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_stop8, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_nd1b, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_nd3b, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_nd3c, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_nd3d, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBH_nd3e, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
        labs(x = " ", y = " ", title = "IBH non-breeding HR and ND movements tracks") +
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
        scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red"))

IBH_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/IBH_HR_ND_plot.jpg", plot = IBH_HR_plot)


#########
# "IBI" # OK
#########

#        # First define the non-breeding period
#        IBI_wintering1 <- st_df%>%
#                dplyr::filter(stop_id == "IBI_wintering1")%>%
#                dplyr::select(ID, x, y)%>%
#                filter_at(vars(x, y), all_vars(!is.na(.)))

#        IBI_wintering1$ID <- factor(IBI_wintering1$ID)

#        # Let"s create a spatialPoint object
#        IBI_HR_sp <- SpatialPointsDataFrame(IBI_wintering1[,c("x", "y")], IBI_wintering1)   

#        # Here I calculate the non-breeding homerange with a Kernel Density Estimation
#        IBI_HR_kde <- kernelUD(IBI_HR_sp[,1], h = "href", grid = 400) # h = "LSCV"

#        # get IBI non-breeding HR
#        IBI_HR <- getverticeshr(IBI_HR_kde, percent = 95)
#        IBI_HR
#        IBI_HRcore <- getverticeshr(IBI_HR_kde, percent = 50)
#        IBI_HRcore

#        # fortify() function is needed to plot the non-breeding homerange with ggplot
#        IBI_HR <- fortify(IBI_HR)
#        IBI_HRcore <- fortify(IBI_HRcore)


#        IBI_HR_1 <- IBI_HR%>%
#                filter(id == "IBI" & group == "IBI.1")
#        IBI_HR_2 <- IBI_HR%>%
#                filter(id == "IBI" & group == "IBI.2")
#        IBI_HR_3 <- IBI_HR%>%
#                filter(id == "IBI" & group == "IBI.3")

IBI_nest <- st_df%>%
        filter(stop_id == "IBI_nest")
IBI_stop1 <- st_df%>%
        filter(stop_id == "IBI_stop1")
IBI_stop2 <- st_df%>%
        filter(stop_id == "IBI_stop2")
IBI_end <- st_df%>%
        filter(stop_id == "IBI_end")

IBI_nd1a <- nd_df%>%
        filter(track_id == "IBI_nd1a")
IBI_nd1b <- nd_df%>%
        filter(track_id == "IBI_nd1b")
IBI_nd2a <- nd_df%>%
        filter(track_id == "IBI_nd2a")

# Plot the non-breeding homerange
IBI_HR_plot <- 
        ggplot(IBI_eu_utm) +
        geom_spatvector()+
       # geom_polygon(IBI_HR_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
       # geom_polygon(IBI_HR_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
       # geom_polygon(IBI_HR_3, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
       # geom_polygon(IBI_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_path(data = IBI_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBI_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBI_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBI_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBI_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBI_nd1b, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = IBI_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
        labs(x = " ", y = " ", title = "IBI non-breeding HR and ND tracks") +
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
        scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red"))
IBI_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/IBI_HR_ND_plot.jpg", plot = IBI_HR_plot)


#########
# "IBK" # OK
#########

# First define the non-breeding period
IBK_wintering1 <- st_df%>%
        dplyr::filter(stop_id == "IBK_wintering1")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IBK_wintering1$ID <- factor(IBK_wintering1$ID)

IBK_wintering2 <- st_df%>%
        dplyr::filter(stop_id == "IBK_wintering2")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IBK_wintering2$ID <- factor(IBK_wintering2$ID)

# Let"s create a spatialPoint object
IBK_HR1_sp <- SpatialPointsDataFrame(IBK_wintering1[,c("x", "y")], IBK_wintering1)
IBK_HR2_sp <- SpatialPointsDataFrame(IBK_wintering2[,c("x", "y")], IBK_wintering2)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
IBK_HR1_kde <- kernelUD(IBK_HR1_sp[,1], h = "href", grid = 400) # h = "LSCV"
IBK_HR2_kde <- kernelUD(IBK_HR2_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get IBK non-breeding HR
IBK_HR1 <- getverticeshr(IBK_HR1_kde, percent = 95) # 50% is the value to obtain the core area of the HR
IBK_HR1
IBK_HR1core <- getverticeshr(IBK_HR1_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IBK_HR1core

IBK_HR2 <- getverticeshr(IBK_HR2_kde, percent = 95) # 50% is the value to obtain the core area of the HR
IBK_HR2
IBK_HR2core <- getverticeshr(IBK_HR2_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IBK_HR2core

# fortify() function is needed to plot the non-breeding homerange with ggplot
IBK_HR1 <- fortify(IBK_HR1)
IBK_HR1core <- fortify(IBK_HR1core)

IBK_HR2 <- fortify(IBK_HR2)
IBK_HR2core <- fortify(IBK_HR2core)

IBK_HR1_1 <-IBK_HR1%>%
        filter(group == "IBK.1")
IBK_HR1_2 <-IBK_HR1%>%
        filter(group == "IBK.2")
IBK_HR1_3 <-IBK_HR1%>%
        filter(group == "IBK.3")

IBK_HR1core_1 <- IBK_HR1core%>%
        filter(group == "IBK.1")
IBK_HR1core_2 <- IBK_HR1core%>%
        filter(group == "IBK.2")

IBK_HR2core_1 <- IBK_HR2core%>%
        filter(group == "IBK.1")
IBK_HR2core_2 <- IBK_HR2core%>%
        filter(group == "IBK.2")
IBK_HR2core_3 <- IBK_HR2core%>%
        filter(group == "IBK.3")


IBK_nest <- st_df%>%
        filter(stop_id == "IBK_nest")
IBK_wintering1 <- st_df%>%
        filter(stop_id == "IBK_wintering1")
IBK_wintering2 <- st_df%>%
        filter(stop_id == "IBK_wintering2")
IBK_end <- st_df%>%
        filter(stop_id == "IBK_end")

IBK_nd1a <- nd_df%>%
        filter(track_id == "IBK_nd1a")
IBK_nd2a <- nd_df%>%
        filter(track_id == "IBK_nd2a")
IBK_nd3a <- nd_df%>%
        filter(track_id == "IBK_nd3a")

# Plot the non-breeding homerange
IBK_HR_plot <- 
        ggplot(IBK_eu_utm) +
        geom_spatvector()+
   #     geom_polygon(IBK_HR1_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
   #     geom_polygon(IBK_HR1_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
   #     geom_polygon(IBK_HR1_3, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
   #     geom_polygon(IBK_HR1core_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
   #     geom_polygon(IBK_HR1core_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_polygon(IBK_HR2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(IBK_HR2core_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_polygon(IBK_HR2core_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_polygon(IBK_HR2core_3, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
   #     geom_path(data = IBK_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBK_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBK_wintering2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBK_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBK_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBK_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBK_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
        labs(x = " ", y = " ", title = "IBK non-breeding HR and ND movements tracks") +
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
        scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                                    "Non-Breeding HR core area" = "red"))
IBK_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/IBK_HR_ND_plot.jpg", plot = IBK_HR_plot)


#########
# "IBS" # OK
#########

# First define the non-breeding period
IBS_wintering1 <- st_df%>%
        dplyr::filter(stop_id == "IBS_wintering1")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IBS_wintering1$ID <- factor(IBS_wintering1$ID)

IBS_wintering2 <- st_df%>%
        dplyr::filter(stop_id == "IBS_wintering2")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IBS_wintering2$ID <- factor(IBS_wintering2$ID)

IBS_wintering3 <- st_df%>%
        dplyr::filter(stop_id == "IBS_wintering3")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IBS_wintering3$ID <- factor(IBS_wintering3$ID)

# Let"s create a spatialPoint object
IBS_HR1_sp <- SpatialPointsDataFrame(IBS_wintering1[,c("x", "y")], IBS_wintering1)
IBS_HR2_sp <- SpatialPointsDataFrame(IBS_wintering2[,c("x", "y")], IBS_wintering2)  
IBS_HR3_sp <- SpatialPointsDataFrame(IBS_wintering3[,c("x", "y")], IBS_wintering3)  

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
IBS_HR1_kde <- kernelUD(IBS_HR1_sp[,1], h = "href", grid = 400) # h = "LSCV"
IBS_HR2_kde <- kernelUD(IBS_HR2_sp[,1], h = "href", grid = 400) # h = "LSCV"
IBS_HR3_kde <- kernelUD(IBS_HR3_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get IBS non-breeding HR
IBS_HR1 <- getverticeshr(IBS_HR1_kde, percent = 95) # 50% is the value to obtain the core area of the HR
IBS_HR1
IBS_HR1core <- getverticeshr(IBS_HR1_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IBS_HR1core

IBS_HR2 <- getverticeshr(IBS_HR2_kde, percent = 95) # 50% is the value to obtain the core area of the HR
IBS_HR2
IBS_HR2core <- getverticeshr(IBS_HR2_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IBS_HR2core

IBS_HR3 <- getverticeshr(IBS_HR3_kde, percent = 95) # 50% is the value to obtain the core area of the HR
IBS_HR3
IBS_HR3core <- getverticeshr(IBS_HR3_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IBS_HR3core

# fortify() function is needed to plot the non-breeding homerange with ggplot
IBS_HR1 <- fortify(IBS_HR1)
IBS_HR1core <- fortify(IBS_HR1core)

IBS_HR2 <- fortify(IBS_HR2)
IBS_HR2core <- fortify(IBS_HR2core)

IBS_HR3 <- fortify(IBS_HR3)
IBS_HR3core <- fortify(IBS_HR3core)


# 

IBS_HR2_1 <- IBS_HR2%>%
        filter(group == "IBS.1")
IBS_HR2_2 <- IBS_HR2%>%
        filter(group == "IBS.2")

IBS_HR3_1 <- IBS_HR3%>%
        filter(group == "IBS.1")
IBS_HR3_2 <- IBS_HR3%>%
        filter(group == "IBS.2")

IBS_nest <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_nest")
IBS_stop1 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop1")
IBS_wintering1 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_wintering1")
IBS_stop2 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop2")
IBS_wintering2 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_wintering2")
IBS_stop3 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop3")
IBS_stop4 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop4")
IBS_stop5 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop5")
IBS_stop6 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop6")
IBS_stop7 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop7")
IBS_stop8 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop8")
IBS_stop9 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop9")
IBS_stop10 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop10")
IBS_stop11 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop11")
IBS_wintering3 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_wintering3")
IBS_stop12 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop12")
IBS_stop13 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop13")
IBS_stop14 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop14")
IBS_stop15 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop15")
IBS_stop16 <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_stop16")
IBS_end <- st_df%>%
        arrange(time)%>%
        filter(stop_id == "IBS_end")

IBS_nd1a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd1a")
IBS_nd2a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd2a")
IBS_nd3a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd3a")
IBS_nd4a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd4a")
IBS_nd5a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd5a")
IBS_nd5b <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd5b")
IBS_nd5c <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd5c")
IBS_nd5d <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd5d")
IBS_nd5e <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd5e")
IBS_nd5f <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd5f")
IBS_nd6a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd6a")
IBS_nd6b <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd6b")
IBS_nd6c <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd6c")
IBS_nd7a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd7a")
IBS_nd8a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd8a")
IBS_nd8b <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd8b")
IBS_nd8c <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd8c")
IBS_nd9a <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd9a")
IBS_nd9b <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd9b")
IBS_nd9c <- nd_df%>%
        arrange(time)%>%
        filter(track_id == "IBS_nd9c")

# Plot the non-breeding homerange          
IBS_HR_plot <- 
        ggplot(IBS_eu_utm) +
        geom_spatvector()+
   #     geom_polygon(IBS_HR1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
   #     geom_polygon(IBS_HR2_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
   #     geom_polygon(IBS_HR2_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(IBS_HR3_1, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(IBS_HR3_2, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
   #     geom_polygon(IBS_HR1core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
   #     geom_polygon(IBS_HR2core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_polygon(IBS_HR3core, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
   #     geom_path(data = IBS_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_wintering2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop5, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop6, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop7, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop8, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop9, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop10, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop11, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_wintering3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop12, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop13, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop14, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop15, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_stop16, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd4a, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd5a, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd5b, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd5c, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd5d, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd5e, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd5f, aes(x = x, y = y, colour = "Natal dispersal 5th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd6a, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd6b, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd6c, aes(x = x, y = y, colour = "Natal dispersal 6th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd7a, aes(x = x, y = y, colour = "Natal dispersal 7th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd8a, aes(x = x, y = y, colour = "Natal dispersal 8th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd8b, aes(x = x, y = y, colour = "Natal dispersal 8th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd8c, aes(x = x, y = y, colour = "Natal dispersal 8th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd9a, aes(x = x, y = y, colour = "Natal dispersal 9th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd9b, aes(x = x, y = y, colour = "Natal dispersal 9th travel"), linewidth = 0.5, lineend = "round") +
   #     geom_path(data = IBS_nd9c, aes(x = x, y = y, colour = "Natal dispersal 9th travel"), linewidth = 0.5, lineend = "round") +
        labs(x = " ", y = " ", title = "IBS non-breeding HR and ND movements tracks") +
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
        scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                            "Non-Breeding HR core area" = "red"))
IBS_HR_plot          

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/IBS_HR_ND_plot.jpg", plot = IBS_HR_plot)


#########
# "ICZ" # OK
#########

# First define the non-breeding period
ICZ_wintering1 <- st_df%>%
        dplyr::filter(stop_id == "ICZ_wintering1")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

ICZ_wintering1$ID <- factor(ICZ_wintering1$ID)

# Let"s create a spatialPoint object
ICZ_HR_sp <- SpatialPointsDataFrame(ICZ_wintering1[,c("x", "y")], ICZ_wintering1)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
ICZ_HR_kde <- kernelUD(ICZ_HR_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get ICZ non-breeding HR
ICZ_HR <- getverticeshr(ICZ_HR_kde, percent = 95) # 50% is the value to obtain the core area of the HR
ICZ_HR

ICZ_HRcore <- getverticeshr(ICZ_HR_kde, percent = 50) # 50% is the value to obtain the core area of the HR
ICZ_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
ICZ_HR <- fortify(ICZ_HR)
ICZ_HRcore <- fortify(ICZ_HRcore)

ICZ_nest <- st_df%>%
        filter(stop_id == "ICZ_nest")
ICZ_winering1 <- st_df%>%
        filter(stop_id == "ICZ_winering1")
ICZ_stop1 <- st_df%>%
        filter(stop_id == "ICZ_stop1")
ICZ_end <- st_df%>%
        filter(stop_id == "ICZ_end")

ICZ_nd1a <- nd_df%>%
        filter(track_id == "ICZ_nd1a")
ICZ_nd2a <- nd_df%>%
        filter(track_id == "ICZ_nd2a")
ICZ_nd3a <- nd_df%>%
        filter(track_id == "ICZ_nd3a")


# Plot the non-breeding homerange
ICZ_HR_plot <- 
        ggplot(ICZ_eu_utm) +
        geom_spatvector()+
        geom_polygon(ICZ_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
        geom_polygon(ICZ_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
        geom_path(data = ICZ_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = ICZ_winering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = ICZ_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = ICZ_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
        geom_path(data = ICZ_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = ICZ_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
        geom_path(data = ICZ_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
        labs(x = " ", y = " ", title = "ICZ non-breeding HR and ND movements tracks") +
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
        scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                                "Non-Breeding HR core area" = "red"))

ICZ_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/ICZ_HR_ND_plot.jpg", plot = ICZ_HR_plot)


#########
# "IFP" # OK
#########

# First define the non-breeding period
IFP_wintering1 <- st_df%>%
        dplyr::filter(stop_id == "IFP_wintering1")%>%
        dplyr::select(ID, x, y)%>%
        filter_at(vars(x, y), all_vars(!is.na(.)))

IFP_wintering1$ID <- factor(IFP_wintering1$ID)

# Let"s create a spatialPoint object
IFP_HR_sp <- SpatialPointsDataFrame(IFP_wintering1[,c("x", "y")], IFP_wintering1)   

# Here I calculate the non-breeding homerange with a Kernel Density Estimation
IFP_HR_kde <- kernelUD(IFP_HR_sp[,1], h = "href", grid = 400) # h = "LSCV"

# get IFP non-breeding HR
IFP_HR <- getverticeshr(IFP_HR_kde, percent = 95) 
IFP_HR

IFP_HRcore <- getverticeshr(IFP_HR_kde, percent = 50) # 50% is the value to obtain the core area of the HR
IFP_HRcore

# fortify() function is needed to plot the non-breeding homerange with ggplot
IFP_HR <- fortify(IFP_HR)
IFP_HRcore <- fortify(IFP_HRcore)

IFP_nest <- st_df%>%
        filter(stop_id == "IFP_nest")
IFP_stop1 <- st_df%>%
        filter(stop_id == "IFP_stop1")
IFP_wintering1 <- st_df%>%
        filter(stop_id == "IFP_wintering1")
IFP_stop2 <- st_df%>%
        filter(stop_id == "IFP_stop2")
IFP_stop3 <- st_df%>%
        filter(stop_id == "IFP_stop3")
IFP_stop4 <- st_df%>%
        filter(stop_id == "IFP_stop4")
IFP_end <- st_df%>%
        filter(stop_id == "IFP_end")

IFP_nd1a <- nd_df%>%
        filter(track_id == "IFP_nd1a")
IFP_nd1b <- nd_df%>%
        filter(track_id == "IFP_nd1b")
IFP_nd2a <- nd_df%>%
        filter(track_id == "IFP_nd2a")
IFP_nd3a <- nd_df%>%
        filter(track_id == "IFP_nd3a")
IFP_nd3b <- nd_df%>%
        filter(track_id == "IFP_nd3b")
IFP_nd4a <- nd_df%>%
        filter(track_id == "IFP_nd4a")

# Plot the non-breeding homerange
IFP_HR_plot <- 
ggplot(IFP_eu_utm) +
geom_spatvector()+
#geom_polygon(IFP_HR, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR 95%")) +
#geom_polygon(IFP_HRcore, mapping = aes(x=long, y=lat, fill = "Non-Breeding HR core area")) +
geom_path(data = IFP_nest, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_stop1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_wintering1, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_stop2, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_stop3, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_stop4, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_end, aes(x = x, y = y, colour = "Non-Dispersal movements"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_nd1a, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_nd1b, aes(x = x, y = y, colour = "Natal dispersal 1st travel"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_nd2a, aes(x = x, y = y, colour = "Natal dispersal 2nd travel"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_nd3a, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_nd3b, aes(x = x, y = y, colour = "Natal dispersal 3rd travel"), linewidth = 0.5, lineend = "round") +
geom_path(data = IFP_nd4a, aes(x = x, y = y, colour = "Natal dispersal 4th travel"), linewidth = 0.5, lineend = "round") +
labs(x = " ", y = " ", title = "IFP non-breeding HR and ND movements tracks") +
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
scale_fill_manual(name = "Home Range", values = c("Non-Breeding HR 95%" = "yellow",
                    "Non-Breeding HR core area" = "red")) 

IFP_HR_plot

# ggsave("C:/Tesi/R/osprey/images/20240109_TrackPlot/IFP_HR_ND_plot.jpg", plot = IFP_HR_plot)

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
