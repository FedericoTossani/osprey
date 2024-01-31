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

#####################
#   COORD vs TIME   #
#####################
          
          # Longitude vs time
                   all_lon_time <-
                   ggplot(nd_df, aes(doy, x, col = id)) +
                   geom_point(size = 0.5) +
                   facet_wrap(~ id_y) +
                   geom_path()

all_lon_time <- all_lon_time + xlim(1,365)

all_lon_time


# Latitude vs time
         all_lat_time <-
         ggplot(nd_df, aes(doy, y, col = id)) +
         geom_point(size = 0.5) +
         facet_wrap(~ id_y) +
         geom_path()

all_lat_time <- all_lat_time + xlim(1, 365)

all_lat_time

         grid.arrange(all_lon_time, all_lat_time, nrow=23)

# COORD vs TIME per animal

  # H7 
        h7 <- nd_df%>%
            filter(id == "H7")

        h7_lat_time <-
        ggplot(h7, aes(doy, y)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(h7$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "H7 latitude variation during natal dispersal")

          h7_lat_time <- h7_lat_time + xlim(1, 365)

        h7_lon_time <-
        ggplot(h7, aes(doy, x)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(h7$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "H7 longitude variation during natal dispersal")

          h7_lon_time <- h7_lon_time + xlim(1, 365)

        h7_lon_lat <- ggarrange(h7_lon_time, h7_lat_time, ncol = 2, nrow = 1)
         
        h7_lon_lat

       # ggsave("h7_lon_lat.jpg", plot=h7_lon_lat)


  # CBK

# si sposta tra roost notturno e area di foraggiamento
# dalla latitudine si vede che si comporta come uno svernante normale

        cbk <- nd_df%>%
            filter(id == "CBK")

        cbk_lat_time <-
        ggplot(cbk, aes(doy, y)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(cbk$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

          cbk_lat_time <- cbk_lat_time + xlim(1, 365)

        cbk_lon_time <-
        ggplot(cbk, aes(doy, x)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(cbk$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "CBK longitude and latitude movement graph")

          cbk_lon_time <- cbk_lon_time + xlim(1, 365)

        cbk_lon_lat <- ggarrange(cbk_lon_time, cbk_lat_time, ncol = 2, nrow = 1)

          cbk_lon_lat

       # ggsave("cbk_lon_lat.jpg", plot=cbk_lon_lat)

        grid.arrange(cbk_lon_time, cbk_lat_time, nrow=1)

  # CIV

        civ <- osprey%>%
            filter(ID == "CIV")

        civ_lat_time <-
        ggplot(civ, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(civ$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        civ_lon_time <-
        ggplot(civ, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(civ$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "CIV longitude and latitude movement graph")

        civ_lon_lat <- ggarrange(civ_lon_time, civ_lat_time, ncol = 1, nrow = 2)

        civ_lon_lat

      #  ggsave("civ_lon_lat.jpg", plot=civ_lon_lat)

        grid.arrange(civ_lon_time, civ_lat_time, nrow=2)

  # E7
        e7 <- nd_df%>%
            filter(id == "E7")

        e7_lat_time <-
        ggplot(e7, aes(doy, y)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(e7$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

          e7_lat_time <- e7_lat_time + xlim(1, 365)

        e7_lon_time <-
        ggplot(e7, aes(doy, x)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(e7$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "E7 longitude and latitude movement graph")

          e7_lon_time <- e7_lon_time + xlim(1,365)

        e7_lon_lat <- ggarrange(e7_lon_time, e7_lat_time, ncol = 2, nrow = 1)

          e7_lon_lat
     
       # ggsave("e7_lon_lat.jpg", plot=e7_lon_lat)

        grid.arrange(e7_lon_time, e7_lat_time, nrow=2)

  # A7 
        a7 <- nd_df%>%
            filter(id == "A7")

        a7_lat_time <-
        ggplot(a7, aes(date, y)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(a7$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

          a7_lat_time <- a7_lat_time + xlim(1,365)

        a7_lon_time <-
        ggplot(a7, aes(date, x)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(a7$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "A7 longitude and latitude movement graph")

          a7_lon_time <- a7_lon_time + xlim(1,365)

        a7_lon_lat <- ggarrange(a7_lon_time, a7_lat_time, ncol = 2, nrow = 1)

          a7_lon_lat
     
       # ggsave("a7_lon_lat.jpg", plot=a7_lon_lat)

        grid.arrange(a7_lon_time, a7_lat_time, nrow=2)

  # Antares

# controlla come sono stati trattati gli individui residenti nell'articolo di IBIS
# calcola media dei movimenti e individua spostamento significativo
        antares <- osprey%>%
            filter(ID == "Antares")

        antares_lat_time <-
        ggplot(antares, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(antares$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        antares_lon_time <-
        ggplot(antares, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(antares$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "Antares longitude and latitude movement graph")

        antares_lon_lat <- ggarrange(antares_lon_time, antares_lat_time, ncol = 1, nrow = 2)
     
       # ggsave("antares_lon_lat.jpg", plot=antares_lon_lat)

        grid.arrange(antares_lon_time, antares_lat_time, nrow=2)

  # IAD
        iad <- osprey%>%
            filter(ID == "IAD")

        iad_lat_time <-
        ggplot(iad, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(iad$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        iad_lon_time <-
        ggplot(iad, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(iad$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "IAD longitude and latitude movement graph") 
        iad_lon_lat <- ggarrange(iad_lon_time, iad_lat_time, ncol = 1, nrow = 2)
     
      #  ggsave("iad_lon_lat.jpg", plot=iad_lon_lat)

        grid.arrange(iad_lon_time, iad_lat_time, nrow=2)

  # CAM 
# tienilo per ultimo
        cam <- osprey%>%
            filter(ID == "CAM")

        cam_lat_time <-
        ggplot(cam, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(cam$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        cam_lon_time <-
        ggplot(cam, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(cam$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "CAM longitude and latitude movement graph")

        cam_lon_lat <- grid.arrange(cam_lon_time, cam_lat_time, nrow=2)

      #  ggsave("cam_lon_lat.jpg", plot=cam_lon_lat)

  # IBI
# trattalo come residente

        ibi <- osprey%>%
            filter(ID == "IBI")

        ibi_lat_time <-
        ggplot(ibi, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(ibi$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        ibi_lon_time <-
        ggplot(ibi, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(ibi$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "IBI longitude and latitude movement graph")

        ibi_lon_lat <- grid.arrange(ibi_lon_time, ibi_lat_time, nrow=2)

      #  ggsave("ibi_lon_lat.jpg", plot=ibi_lon_lat)

  # IAB     
        iab <- osprey%>%
            filter(ID == "IAB")

        iab_lat_time <-
        ggplot(iab, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(iab$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        iab_lon_time <-
        ggplot(iab, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(iab$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "IAB longitude and latitude movement graph")

        iab_lon_lat <- grid.arrange(iab_lon_time, iab_lat_time, nrow=2)

        iab_lon_lat

      #  ggsave("iab_lon_lat.jpg", plot=iab_lon_lat)

  # ICZ  
        icz <- osprey%>%
            filter(ID == "ICZ")

        icz_lat_time <-
        ggplot(icz, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(icz$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        icz_lon_time <-
        ggplot(icz, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(icz$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "ICZ longitude and latitude movement graph")

        icz_lon_lat <- grid.arrange(icz_lon_time, icz_lat_time, nrow=2)

      #  ggsave("icz_lon_lat.jpg", plot=icz_lon_lat)

  # IBS  
                  ibs <- osprey_nd%>%
            filter(ID == "IBS")

        ibs_lat_time <-
        ggplot(ibs, aes(doy, y)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(ibs$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

ibs_lat_time <- ibs_lat_time + xlim(1,365)

        ibs_lon_time <-
        ggplot(ibs, aes(doy, x)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(ibs$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "IBS longitude and latitude movement graph")

          ibs_lon_time <- ibs_lon_time + xlim(1, 365)

        ibs_lon_lat <- grid.arrange(ibs_lon_time, ibs_lat_time, nrow=2)

      #  ggsave("ibs_lon_lat.jpg", plot=ibs_lon_lat)

  # IBH 
        ibh <- osprey%>%
            filter(ID == "IBH")

        ibh_lat_time <-
        ggplot(ibh, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(ibh$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        ibh_lon_time <-
        ggplot(ibh, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(ibh$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "IBH longitude and latitude movement graph")

        ibh_lon_lat <- grid.arrange(ibh_lon_time, ibh_lat_time, nrow=2)

      #  ggsave("ibh_lon_lat.jpg", plot=ibh_lon_lat)

  # IBK
        ibk <- osprey%>%
            filter(ID == "IBK")

        ibk_lat_time <-
        ggplot(ibk, aes(time, lat)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(ibk$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

        ibk_lon_time <-
        ggplot(ibk, aes(time, lon)) +
        geom_point(size = 0.5) +
        geom_hline(yintercept = mean(ibk$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "IBK longitude and latitude movement graph")

        ibk_lon_lat <- grid.arrange(ibk_lon_time, ibk_lat_time, nrow=2)

      #  ggsave("ibk_lon_lat.jpg", plot=ibk_lon_lat)


  # IFP 
        ifp <- nd_df%>%
            filter(id == "IFP")

        ifp_lat_time <-
        ggplot(ifp, aes(doy, y)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(ifp$lat), color = "red", linetype = "dashed", size = 1) +
        geom_path()

          ifp_lat_time <- ifp_lat_time + xlim(1,365)

        ifp_lon_time <-
        ggplot(ifp, aes(doy, x)) +
        geom_point(size = 0.5) +
        #geom_hline(yintercept = mean(ifp$lon), color = "red", linetype = "dashed", size = 1) +
        geom_path()+
        labs(title = "IFP longitude and latitude movement graph")

          ifp_lon_time <- ifp_lon_time + xlim(1,365)

        ifp_lon_lat <- ggarrange(ifp_lon_time, ifp_lat_time, ncol = 1, nrow = 2)
         
        ifp_lon_lat

       # ggsave("ifp_lon_lat.jpg", plot=ifp_lon_lat)


# Check this part!
         # Project to UTM
         llcoord <- st_as_sf(data[, c("lon", "lat")], coords = c("lon", "lat"),
         crs = CRS("+proj=longlat +datum=WGS84"))
         utmcoord <- st_transform(llcoord, crs = CRS("+proj=utm +zone=32 +datum=WGS84"))

         # Add Easting-Northing to data (in km)
         data[, c("x", "y")] <- st_coordinates(utmcoord)/1000

         # Plot Northing vs Easting
         ggplot(data, aes(x, y, col = ID)) +
         geom_point(size = 0.5) +
         geom_path() +
         coord_equal()

         # Table of time intervals in data
         plot(table(diff(data$time)), xlim = c(0, 1000),
         xlab = "time interval (min)", ylab = "count")



  # use this graph to check for departure dates  

natal dispersal travel
ID == "IFP" & time >= "2022-07-23 09:00:00" & time <= "2022-07-24 14:30:00" ~ "IFP_nd1a",
ID == "IFP" & time >= "2022-07-26 06:00:00" & time <= "2022-07-28 17:30:00" ~ "IFP_nd1b",
ID == "IFP" & time >= "2023-04-24 07:00:00" & time <= "2023-04-29 18:00:00" ~ "IFP_nd2a",
ID == "IFP" & time >= "2023-05-16 04:00:00" & time <= "2023-05-17 18:00:00" ~ "IFP_nd3a",
ID == "IFP" & time >= "2023-05-23 04:00:00" & time <= "2023-05-24 23:00:00" ~ "IFP_nd3b",
ID == "IFP" & time >= "2023-06-07 06:00:00" & time <= "2023-06-11 17:00:00"~ "IFP_nd4a"

stationary
ID == "IFP" & time <= "2022-07-23 09:00:00" ~ "IFP_nest",
ID == "IFP" & time >= "2022-07-24 14:30:00" & time <= "2022-07-26 06:00:00" ~ "IFP_stop1",
ID == "IFP" & time >= "2022-07-28 17:30:00" & time <= "2023-04-24 07:00:00" ~ "IFP_wintering1",
ID == "IFP" & time >= "2023-04-29 18:00:00" & time <= "2023-05-16 04:00:00" ~ "IFP_stop2",
ID == "IFP" & time >= "2023-05-17 18:00:00" & time <= "2023-05-23 04:00:00" ~ "IFP_stop3",
ID == "IFP" & time >= "2023-05-24 23:00:00" & time <= "2023-06-07 06:00:00" ~ "IFP_stop4",
ID == "IFP" & time >= "2023-06-11 17:00:00" ~ "IFP_end"


osp <- osprey%>%
arrange(time)%>%
filter(ID == "IFP" & time >= "2022-07-28 17:30:00" & time <= "2022-08-15 07:00:00")

osp_lat_time <-
ggplot(osp, aes(time, lat)) +
geom_point(size = 0.5) +
geom_path()

osp_lon_time <-
ggplot(osp, aes(time, lon)) +
geom_point(size = 0.5) +
geom_path()

osp_lon_lat <- ggarrange(osp_lon_time, osp_lat_time, ncol = 1, nrow = 2)

osp_lon_lat




# plto to check the route
ggplot(IFP_eu_utm) + 
          geom_spatvector()+
          geom_path(data = osp, aes(x = x, y = y, colour = "red"), linewidth = 1, lineend = "round")


          
          
          
