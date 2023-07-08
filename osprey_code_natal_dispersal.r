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


#########################
# Natal dispersal track #
#########################
         
         # let's see all the tracks togheter

              osprey_track <- 
                  ggplot(osprey_eu_utm) +
                  geom_spatvector() + 
                   geom_path(data = osprey_nd, aes(x = x, y = y, color = ID), 
                              linewidth = 0.5, lineend = "round") +
                  labs(x = " ", y = " ", title = "Inividual tracks") +
                  #facet_wrap(~ ID) +
                  theme(legend.position="none") +
                  theme_minimal()
         
                  osprey_track

                 # save the plot to the working directory 
                  ggsave( "osprey_nd.jpg", plot = osprey_track)


# H7
         
                  h7_nd <- osprey_nd%>%
                           filter(ID == 'H7')
         
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


# CIV
         
                  civ_nd15 <- osprey_nd%>%
                           filter(ID == 'CIV' & time > '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00')
         
                  civ_track15 <- 
                  ggplot(civ_eu_utm) +
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
                  ggplot(civ_eu_utm) +
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


# E7
         
                  e7_nd <- osprey_nd%>%
                           filter(ID == 'E7')
         
                  e7_track <- 
                  ggplot(e7_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = e7_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "E7 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
         
                  e7_track
         
                 # save the plot to the working directory 
                  ggsave( "e7_nd.jpg", plot = e7_track)


# A7
         
                  a7_nd <- osprey_nd%>%
                           filter(ID == 'A7')
         
                  a7_track <- 
                  ggplot(a7_eu_utm) +
                    geom_spatvector()+
                    geom_path(data = a7_nd, aes(x = x, y = y), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "A7 natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
                  
                  a7_track
         
                 # save the plot to the working directory 
                  ggsave( "a7_nd.jpg", plot = a7_track)


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

# IBS
         
                  ibs_nd <- osprey_nd%>%
                           filter(ID == 'IBS')
         
                  ibs_track <- ggplot(ibs_eu_utm) +
                             geom_spatvector()+
                             geom_path(data = ibs_nd, aes(x = x, y = y), 
                                       linewidth = 0.5, lineend = "round", col = 'red') +
                             labs(x = " ", y = " ", title = "IBS natal dispersal GPS track") +
                             theme_minimal() +
                             theme(legend.position = "none")
         
                  ibs_track
         
                 # save the plot to the working directory 
                  ggsave( "ibs_nd.jpg", plot = ibs_track)


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

# IFP
         
                  ifp_nd <- osprey_nd%>%
                           filter(ID == 'IFP')
         
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

         ########################
         # ND track da definire #
         ########################

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
         
# CAM
         
                  cam_nd <- osprey%>%
                           filter(ID == 'CAM')
         
                  cam_track <- 
                  ggplot(cam_eu) +
                    geom_spatvector()+
                    geom_path(data = cam_nd, aes(x = lon, y = lat), 
                              linewidth = 0.5, lineend = "round", col = 'red') +
                    labs(x = " ", y = " ", title = "CAM natal dispersal GPS track") +
                    theme_minimal() +
                    theme(legend.position = "none")
                  
                  cam_track
         
# Antares

                  antares_nd <- osprey%>%
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
         

         
         # ---------------------------------------------------------------  #
         
                 getMoveStats <- function(df){
                  # df - is a generic data frame that will contain X,Y and Time columns
                  Z <- df$x + 1i*df$y
                  Time <- df$time
                  Step <- c(NA, diff(Z)) # we add the extra NA because there is no step to the first location
                  dT <- c(NA, difftime(Time[-1], Time[-length(Time)], hours) %>% as.numeric)
         
                  SL <- Mod(Step)/1e3 # convert to km - so the speed is in km/hour
                  MR <- SL/dT # computing the movement rate
         
                  # this is what the function returns
                  data.frame(df, Z, dT, Step, SL, MR)
                           }
         
         
         
                  osprey_move_nd <- getMoveStats(osprey_nd) 
         
                  osprey_move_nd <- osprey_move_nd %>%
                                    dplyr::filter( ID != "IBS" & ID != "IBH")
                                             
                  ggplot(osprey_move_nd, aes(ID, MR)) +
                  geom_boxplot()
         
         
                  mr_summarystats <- osprey_move_nd %>%
                                    dplyr::filter( ID != "IBS")%>%
                                    plyr::ddply(c("ID", "season"), summarize,
                                     min = min(MR, na.rm = TRUE), max = max(MR, na.rm = TRUE),
                                     n = length(MR), NA.count = sum(is.na(MR)),
                                     Zero.count = sum(MR == 0, na.rm = TRUE))
         
                  SL_summarystats <- osprey_move_nd %>%
                                    plyr::ddply(c("ID", "season"), summarize,
                                     min = min(SL, na.rm = TRUE), max = max(SL, na.rm = TRUE),
                                     n = length(SL), NA.count = sum(is.na(SL)),
                                     Zero.count = sum(SL == 0, na.rm = TRUE))
         
         
         
                  
                  plot(osprey_move_nd$Step, asp=1, type="n")
                  arrows(rep(0, length(osprey_move_nd$Step)), rep(0, length(osprey_move_nd$Step)), Re(osprey_move_nd$Step), Im(osprey_move_nd$Step), col=rgb(0,0,0,.5), lwd=2, length=0.1)
         
         hist(osprey_move_nd$SL, col="grey", bor="darkgrey", freq=FALSE)
         lines(density(osprey_move_nd$SL), col=2, lwd=2)
         
         
         
