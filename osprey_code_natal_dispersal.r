# =============================== #
#        1.SetWD & packages       #
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


# ========================================== #
#          Let's try a new approach          #
# ========================================== #

          # Let's define our dataset of natal dispersal

osprey_nd_no_duplicates <- osprey_nd[!duplicated(osprey_nd[c("ID", "time")]) & !duplicated(osprey_nd[c("ID", "time")], fromLast = TRUE), ]


                  nd <- osprey_nd%>%
                           #filter(ID == 'A7' | ID == 'E7' | ID == 'H7' | ID == 'IFP')%>%
                              select(-c("date", "death_date", "season"))

          table(is.na(nd$lon))
          table(is.na(nd$lat))
          table(is.na(nd$x))
          table(is.na(nd$y))

nd_with_na <- nd[is.na(nd$x) & is.na(nd$y), ]

nd_with_na

          # convert it into an ltraj object
                  nd_lt <- as.ltraj(nd[, c("x", "y")],
                                        date = nd$time,
                                        id = nd$ID,
                                        typeII = T)

          # create a data frame
                  nd_df <- nd_lt%>%
                              ld()%>%
                              mutate(doy = yday(date),
                                     ymd = as.Date(date),
                                     year = year(date))%>%
                              tidyr::unite(burst, c(burst, year, doy), sep="_", remove = F)%>%
                              tidyr::unite(id_y, c(id, year), sep="_", remove = F)%>%
                              select(-c("pkey"))


nd$time <- as.POSIXct(strptime(as.character(nd$time),"%Y-%m-%d %H:%M"))

# Find duplicate dates
duplicate_dates <- osprey_nd[duplicated(osprey_nd$time) | duplicated(osprey_nd$time, fromLast = TRUE), ]

# Print the duplicate dates
print(duplicate_dates)

# daily track (= successive telemetry locations in each day)

          dailyDirections <- nd_df%>%
                    mutate(deg_turnAngle = rel.angle * (180/pi))%>%
                    group_by(id, ymd)%>%
                    summarize(meanDir = mean(deg_turnAngle),
                              medianDir = median(deg_turnAngle))

          dirBreaks <- c(22.5, 67.5,112.5, 157.5, 202.5, 247.5, 292.5, 337.5)


ggplot(daily) +
  geom_bar(aes(category)) +
  xlab("Angle of Flight") +
  ylab("Count of birds") +
  theme_light() 

ggplot(daily, aes(x = "", y = meanDir)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start = 0) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5)) +  # Change the legend label
  ggtitle("Pie Chart Example")



# long-distance event (= continuous, usually multiday, unidirectional movements of ??≥300 km?? from the initial location)



# Options:
# - change >90degree and continued moving in the new direction: daily track before was the last track of the LD event. However, if the point of the turn occurred more than halfway through the day was still included and was the last
# - change of >90degree, but within 8 days, resume the original direction, then tracks after the directional change were still included in the long-distance event.
# - change of >90degree, but due to a stopover of ≤7 days, after which the resumed travel in the original direction, then stopover tracks still included in the LD event
# - gap in the telemetry data, or a stopover, lasting >7 days, then the daily track before the gap or stopover was the last track of the long-distance event.




          # F I X  I T ! ! !
                    foo <- function(date) {
                                             da <- as.POSIXlt(nd$date, "UTC")
                                             ho <- da$hour
                                             return(ho)
                                        }

                   nd_lt <- cutltraj(nd_lt, criterion = "foo(date)")





#########################
# Natal dispersal track #
#########################
         
         # let's see all the tracks togheter

              osprey_track <- 
                  ggplot(osprey_eu_utm) +
                  geom_spatvector() + 
                   geom_path(data = nd_df, aes(x = x, y = y, color = id), 
                              linewidth = 0.5, lineend = "round") +
                  labs(x = " ", y = " ", title = "Inividual tracks") +
                  #facet_wrap(~ id_y) +
                  theme(legend.position="none") +
                  theme_minimal()
         
                  osprey_track

                 # save the plot to the working directory 
                  ggsave( "osprey_nd.jpg", plot = osprey_track)

# A7

                  A7_nd <- osprey_nd%>%
                           filter(ID == 'A7')%>%
                              select(-c("death_date", "season"))

                  A7 <- osprey%>%
                           filter(ID == 'A7')%>%
                              select(-c("death_date", "season"))


                  A7_nd_lt <- as.ltraj(A7_nd[, c("x", "y")],
                                        date = A7_nd$time,
                                        id = A7_nd$ID,
                                        typeII = T)

                  A7_lt <- as.ltraj(A7[, c("x", "y")],
                                        date = A7$time,
                                        id = A7$ID,
                                        typeII = T)

                  A7_nd_lt$day <- as.factor(as.Date(A7_nd_lt$date))

                  A7_nd_lt_df <- ld(A7_nd_lt) 

                  A7_nd_lt_df <- A7_nd_lt_df%>%
                              mutate(day = as.Date(date))%>%
                              mutate(day = as.factor(day))%>%
                              mutate(deg_turnAngle = rel.angle * (180/pi))

                    A7_nd_lt_df$diff_direction <- c(0, diff(A7_nd_lt_df$deg_turnAngle))

         # Daily track

                  foo <- function(dt){ 
                           return(dt>(1*3600*24))
                                   }

                  A7_nd_daily <- cutltraj(A7_nd_lt, "foo(dt)", nextr = T)

                  A7_nd_daily_df <- ld(A7_nd_daily)

                  A7_nd_daily_df <- A7_nd_daily_df%>%
                                        mutate(day = as.Date(date))


plotltr(A7_lt, which="R2n")

lav_A7_lt <- lavielle(A7_lt, Lmin=10, Kmax=10, type="mean", which="R2n")
chooseseg(lav_A7_lt)

seg_A7_lt <- findpath(lav_A7_lt, 3)

# plot of the segments
par(mfrow=c(1,3))
plot(seg_A7_lt[1])
plot(seg_A7_lt[2])
plot(seg_A7_lt[3])

end_point <- A7_nd_lt_df%>%
          filter(diff_direction > 90.0 | diff_direction < -90.0)



# NB. chech this URL for usefull help in analysis
          # http://www.r-gators.com/2018/01/31/wildlife-tracking-data-in-r/

          # track plot

nd_lt <- nd_lt%>%
ld()
                  osp_track <- 
                  ggplot(osprey_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = nd_lt, aes(x = x, y = y, col = burst), 
                              linewidth = 0.5, lineend = "round") +
                    labs(x = " ", y = " ", title = "A7 natal dispersal GPS track") +
                    facet_wrap(~id)+
                    theme_minimal() +
                    theme(legend.position = "none")

                  osp_track


                  A7_track <- 
                  ggplot(A7_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = A7_nd_daily_df, aes(x = x, y = y, col = day), 
                              linewidth = 0.5, lineend = "round") +
                    labs(x = " ", y = " ", title = "A7 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
                  
                  A7_track

                 # save the plot to the working directory 
                  ggsave( "a7_nd.jpg", plot = a7_track)

# Antares

                  antares_nd <- osprey_nd%>%
                           filter(ID == 'Antares')
         
                  antares_track <- 
                  ggplot(antares_eu) +
                    geom_spatvector()+
                    geom_path(data = antares_nd, aes(x = lon, y = lat), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "Antares natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  antares_track
         
         # save the plots!!
         
         ggsave("", plots = )

# CAM
         
                  cam_nd <- osprey_nd%>%
                           filter(ID == 'CAM')
         
                  cam_track <- 
                  ggplot(CAM_eu) +
                    geom_spatvector()+
                    geom_path(data = cam_nd, aes(x = lon, y = lat), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CAM natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
                  
                  cam_track

# CBK
         
                  cbk_nd <- osprey_nd%>%
                           filter(ID == 'CBK')
         
                  cbk_track <- 
                  ggplot(cbk_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = cbk_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CBK natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  cbk_track
         
                 # save the plot to the working directory 
                  ggsave( "cbk_nd.jpg", plot = cbk_track)

# CIV
         
                  civ_nd15 <- osprey_nd%>%
                           filter(ID == 'CIV' & time > '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00')
         
                  civ_track15 <- 
                  ggplot(CIV_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = civ_nd15, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CIV 2015 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  civ_track15

                 # save the plot to the working directory 
                  ggsave( "civ_nd15.jpg", plot = civ_track15)

                  civ_nd16 <- osprey_nd%>%
                           filter(ID == 'CIV' & time > '2016-03-29 00:01:00' & time < '2016-10-29 18:00:00')
         
                  civ_track16 <- 
                  ggplot(CIV_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = civ_nd16, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CIV 2016 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  civ_track16

                 # save the plot to the working directory 
                  ggsave( "civ_nd16.jpg", plot = civ_track16)
         
                  civ_all_nd <- ggarrange(civ_track15, civ_track16, ncol = 2, nrow = 1)
                  ggsave( "civ_all_nd.jpg", plot = civ_all_nd)

# E7
         
                  E7_nd <- osprey_nd%>%
                           filter(ID == 'E7')
         
                  E7 <- osprey%>%
                           filter(ID == 'E7')

                  E7_nd_lt <- as.ltraj(E7_nd[, c("x", "y")],
                                        date = E7_nd$time,
                                        id = E7_nd$ID,
                                        typeII = T)

                  E7_lt <- as.ltraj(E7[, c("x", "y")],
                                        date = E7$time,
                                        id = E7$ID,
                                        typeII = T)

#NetSquaredDisplacement path segmentation
plotltr(E7_lt, which="R2n")

lav_E7_lt <- lavielle(E7_lt, Lmin=10, Kmax=10, type="mean", which="R2n")
chooseseg(lav_E7_lt)

seg_E7_lt <- findpath(lav_E7_lt, 6)

# plot of the segments
par(mfrow=c(2,3))
plot(seg_E7_lt[1])
plot(seg_E7_lt[2])
plot(seg_E7_lt[3])
plot(seg_E7_lt[4])
plot(seg_E7_lt[5])
plot(seg_E7_lt[6])

          # plot Natal Dispersal track

                  E7_track <- 
                  ggplot(E7_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = E7_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "E7 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  E7_track


         
                 # save the plot to the working directory 
                  ggsave( "e7_nd.jpg", plot = e7_track)

# H7
         
                  H7_nd <- osprey_nd%>%
                           filter(ID == 'H7')

                  H7 <- osprey%>%
                           filter(ID == 'H7')

                  H7_nd_lt <- as.ltraj(H7_nd[, c("x", "y")],
                                        date = H7_nd$time,
                                        id = H7_nd$ID,
                                        typeII = T)

                  H7_lt <- as.ltraj(H7[, c("x", "y")],
                                        date = H7$time,
                                        id = H7$ID,
                                        typeII = T)

plotltr(H7_lt, which="R2n")

lav_H7_lt <- lavielle(H7_lt, Lmin=10, Kmax=10, type="mean", which="R2n")
chooseseg(lav_H7_lt)

seg_H7_lt <- findpath(lav_H7_lt, 3)

                  h7_track <- 
                  ggplot(h7_eu_utm)+
                  geom_spatvector()+
                  geom_path(data = h7_nd, aes(x = x, y = y), 
                            linewidth = 0.5, lineend = "round", col = 'red') +
                  labs(x = " ", y = " ", title = "H7 natal dispersal GPS track") +
                  theme_minimal() +
                  theme(legend.position = "none")
         
                  h7_track

                 # save the plot to the working directory 
                  ggsave( "h7_nd.jpg", plot = h7_track)

# IAB
         
                  iab_nd <- osprey_nd%>%
                           filter(ID == 'IAB')
         
                  iab_track <- 
                  ggplot(iab_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = iab_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IAB natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  iab_track
         
                 # save the plot to the working directory 
                  ggsave( "iab_nd.jpg", plot = iab_track)

# IAD
         
                  iad_nd18 <- osprey_nd%>%
                           filter(ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00')
         
                  iad_track18 <- 
                  ggplot(iad_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = iad_nd18, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IAD 2018 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  iad_track18
         
                 # save the plot to the working directory 
                  ggsave( "iad_nd18.jpg", plot = iad_track18)

                  # BERLIN
                  iad_nd19 <- osprey_nd%>%
                           filter(ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-05-07 15:00:00')
         
                  iad_track19 <- 
                  ggplot(iad_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = iad_nd19, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IAD 2019 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  iad_track19
         
                 # save the plot to the working directory 
                  ggsave( "iad_nd19.jpg", plot = iad_track19)

                  iad_all_nd <- ggarrange(iad_track18, iad_track19, ncol = 2, nrow = 1)
                  ggsave( "iad_all_nd.jpg", plot = iad_all_nd)

# IBH
         
                  ibh_nd <- osprey_nd%>%
                           filter(ID == 'IBH')
         
                  ibh_track <- 
                  ggplot(ibh_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = ibh_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IBH natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  ibh_track
         
                 # save the plot to the working directory 
                  ggsave( "ibh_nd.jpg", plot = ibh_track)

# IBI
         
                  ibi_nd <- osprey%>%
                           filter(ID == 'IBI')
         
                  ibi_track <- 
                  ggplot(ibi_eu) +
                    geom_spatvector()+
                    geom_path(data = ibi_nd, aes(x = lon, y = lat), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IBI natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  ibi_track

# IBK
         
                  ibk_nd <- osprey_nd%>%
                           filter(ID == 'IBK')
         
                  ibk_track <- 
                  ggplot(ibk_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = ibk_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IBK natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  ibk_track
         
                 # save the plot to the working directory 
                  ggsave( "ibk_nd.jpg", plot = ibk_track)

# IBS

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
                           geom_path(data = IBS, aes(x = x, y = y, colour = "Before dispersal track"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd20, aes(x = x, y = y, colour = "Natal dispersal 2020"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd21, aes(x = x, y = y, colour = "Natal dispersal 2021"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd22, aes(x = x, y = y, colour = "Natal dispersal 2022"), linewidth = 0.5, lineend = "round") +
                           geom_path(data = IBS_nd23, aes(x = x, y = y, colour = "Natal dispersal 2023"), linewidth = 0.5, lineend = "round") +
                           labs(x = " ", y = " ", title = "IBS non-breeding homerange and natal dispersal tracks") +
                          # facet_wrap(~year)+
                           theme_minimal()+
                           scale_color_manual(name = "Tracks", values = c("Before dispersal track" = "green",
                                                                          "Natal dispersal 2020" = "magenta",
                                                                          "Natal dispersal 2021" = "blue",
                                                                          "Natal dispersal 2022" = "orange",
                                                                          "Natal dispersal 2023" = "brown"))
         
                           IBS_HR_plot

         
                 # save the plot to the working directory 
                  ggsave( "ibs_nd.jpg", plot = ibs_track)

# ICZ
         
                  icz_nd <- osprey_nd%>%
                           filter(ID == 'ICZ')
         
                  icz_track <- 
                  ggplot(icz_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = icz_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "ICZ natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  icz_track
         
                 # save the plot to the working directory 
                  ggsave( "icz_nd.jpg", plot = icz_track)

# IFP
         
                  ifp_nd <- osprey_nd%>%
                           filter(ID == 'IFP')

                  ifp <- osprey%>%
                           filter(ID == 'IFP')

                  ifp_nd_lt <- as.ltraj(ifp_nd[, c("x", "y")],
                                        date = ifp_nd$time,
                                        id = ifp_nd$ID,
                                        typeII = T)

                  ifp_lt <- as.ltraj(ifp[, c("x", "y")],
                                        date = ifp$time,
                                        id = ifp$ID,
                                        typeII = T)

plotltr(ifp_lt, which="R2n")

lav_ifp_lt <- lavielle(ifp_lt, Lmin=10, Kmax=10, type="mean", which="R2n")
chooseseg(lav_ifp_lt)

seg_ifp_lt <- findpath(lav_ifp_lt, 5)


                  ifp_track <- 
                  ggplot(ifp_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = ifp_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "IFP natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  ifp_track
         
                 # save the plot to the working directory 
                  ggsave( "ifp_nd.jpg", plot = ifp_track)


lav_H7_lt <- lavielle(H7_lt, Lmin=10, Kmax=10, type="mean", which="R2n")
chooseseg(lav_H7_lt)

seg_H7_lt <- findpath(lav_H7_lt, 3)

#########################
#  Natal dispersal stat #
#########################
         
         # First let's create a ltraj object with UTM coordinates 
         
                  osp_ndlt <- as.ltraj(osprey_nd[, c("x", "y")],
                                        date = osprey_nd$time, 
                                        id = osprey_nd$ID,
                                        typeII=TRUE)
         
         # let's create a function that split bursts with multiple years         
                 
                  foo <- function(dt){ 
                           return(dt>(100*3600*24))
                                   }
                  
                  osp_ndlt2 <- cutltraj(osp_ndlt,"foo(dt)",nextr=TRUE)
         
                  a7lt <- ld(osp_ndlt2[[1]])
         
                  plotltr(a7lt)
         
                  plotltr(osp_ndlt2, "dist")
         
                  osp_ndlt_df <- ld(osp_ndlt2)

                  osp_ndlt_df <- osp_ndlt_df%>%
                                    mutate(day = as.Date(date),
                                           distKM = dist/1000)
         

# SUMMARY dfs

         summary_distance <- osp_ndlt_df%>%
                                    group_by(burst, day)%>%
                                    summarize(minDistxDay = min(dist),
                                              meanDistxDay = mean(dist),
                                              maxDistxDay = max(dist))

         summary_id_distance <- summary_distance%>%
                                    group_by(burst)%>%
                                    summarize(minDist = min(minDistxDay, na.rm=T),
                                              meanDist = mean(meanDistxDay, na.rm=T),
                                              maxDist = max(maxDistxDay, na.rm=T))


dist_plot <- ggplot(osp_ndlt_df, aes(x = osp_ndlt_df$distKM)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Distance (Km)") +
  ylab("Count") +
  ggtitle("Distribution of Distance")



         # export this table to tex
         
                  summary_id_distance %>%
                      kable(format = 'latex', booktabs = TRUE, digits = c(1, 2, 2, 2)) 


nd_duration <- osp_ndlt_df %>%
  group_by(burst) %>% 
  summarize(start = min(date), end = max(date)) %>%
  mutate(duration = round(difftime(end, start)))

summary_speed <- osp_ndlt_df%>%
  group_by(burst)%>%
  mutate(Speed = (dist/1000)/(dt/3600)) %>% 
  summarize(minSpeed = min(Speed, na.rm=T),
            meanSpeed = mean(Speed, na.rm=T),
            maxSpeed = max(Speed, na.rm=T)) 

osp_ndlt_df<- osp_ndlt_df%>%
  mutate(Speed = (dist/1000)/(dt/3600),
         deg_turnAngle = rel.angle * (180/pi))

                  
                  speed_plot <- ggplot(osp_ndlt_df, aes(x = Speed)) +
                    geom_histogram(binwidth = 1) +
                    labs(x = "Speed", y = "Frequency") +
                    theme_bw()

summary_dt <- osp_ndlt_df%>%
  group_by(burst)%>%
  mutate(dt = dt/3600) %>% 
  summarize(minDt = min(dt, na.rm=T),
            meanDt = mean(dt, na.rm=T),
            maxDt = max(dt, na.rm=T)) 


osp_ndlt_df <- osp_ndlt_df%>%
                  mutate(deg_turnAngle = rel.angle * (180/pi))
















                  # Create an empty list to store the summary data frames:
         
                           osp_summaries <- list()
         
                  # Use the split() function to split the larger data frame based on the unique IDs:
         
                           id_groups <- split(osp_ndlt_df, osp_ndlt_df$id)
         
                  # Iterate over each ID group using a for loop:
                           for (id in names(id_groups)) {
                             id_group <- id_groups[[id]]                   # Access the current ID group
                             summary_stats <- lapply(id_group, summary)    # Calculate summary statistics (e.g., mean, median) for each variable in the ID group
                             summary_df <- as.data.frame(summary_stats)    # Convert the summary statistics into a data frame
                             summary_dfs[[id]] <- summary_df               # Append the summary data frame to the list
                           }
         
