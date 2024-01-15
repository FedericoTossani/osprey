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
          year = year(date),
          burst = dplyr::case_when(
                    id == 'H7' & date >= '2013-08-04 09:30:00' & date <= '2013-08-09 15:30:00' ~ "H7_nd1",
                    id == 'H7' & date >= '2015-04-02 05:00:00' & date <= '2015-04-11 13:00:00' ~ "H7_nd2",
                    id == 'H7' & date >= '2015-04-20 13:00:00' & date <= '2015-04-30 16:00:00' |
                    id == 'H7' & date >= '2015-05-03 05:00:00' & date <= '2015-05-03 11:00:00' ~ "H7_nd3",
                    id == 'CIV' & date >= "2014-08-16 10:00:00" & date <= "2014-08-22 06:00:00" ~ "CIV_nd1",
                    id == 'CIV' & date >= "2014-09-11 08:00:00" & date <= "2014-09-13 17:00:00" ~ "CIV_nd2",
                    id == 'CIV' & date >= "2014-10-21 08:00:00" & date <= "2014-10-21 21:00:00" ~ "CIV_nd3",
                    id == 'CIV' & date >= "2015-03-21 02:00:00" & date <= "2015-03-21 24:00:00" ~ "CIV_nd4",
                    id == 'CIV' & date >= "2015-06-04 04:00:00" & date <= "2015-06-22 18:00:00" ~ "CIV_nd5",
                    id == 'CIV' & date >= "2015-08-13 06:00:00" & date <= "2015-08-14 00:00:00" ~ "CIV_nd6",
                    id == 'CIV' & date >= "2015-11-21 16:00:00" & date <= "2015-11-27 00:00:00" ~ "CIV_nd7",
                    id == 'CIV' & date >= '2016-03-28 18:00:00' & date <= '2016-04-01 06:00:00' ~ "CIV_nd8",
                    id == 'CIV' & date >= '2016-04-15 06:00:00' & date <= '2016-04-18 22:00:00' ~ "CIV_nd9",
                    id == 'CIV' & date >= '2016-10-28 06:00:00' & date <= '2016-10-30 06:00:00' ~ "CIV_nd10",
                    id == 'E7' & date >= '2014-08-21 09:00:00' & date <= '2014-08-27 13:00:00' ~ "E7_nd1",
                    id == 'E7' & date >= '2016-03-10 05:00:00' & date <= '2016-03-24 14:00:00' |
                    id == 'E7' & date >= '2016-03-30 09:00:00' & date <= '2016-04-22 20:00:00' |
                    id == 'E7' & date >= '2016-04-24 10:30:00' & date <= '2016-04-25 17:30:00' ~ "E7_nd2",
                    id == 'E7' & date >= '2016-05-04 10:00:00' & date <= '2016-05-07 17:00:00' |
                    id == 'E7' & date >= '2016-05-12 11:00:00' & date <= '2016-05-16 20:30:00' |
                    id == 'E7' & date >= '2016-05-20 12:00:00' & date <= '2016-05-27 18:30:00' ~ "E7_nd3",
                    id == 'A7' & date >= '2015-08-14 09:00:00' & date <= '2015-08-18 12:00:00' ~ "A7_nd1",
                    id == 'A7' & date >= '2017-02-20 00:00:00' & date <= '2017-03-01 20:00:00' ~ "A7_nd2",
                    id == 'A7' & date >= '2017-03-17 00:00:00' & date <= '2017-04-15 06:00:00' |
                    id == 'A7' & date >= '2017-04-16 24:00:00' & date <= '2017-04-21 08:00:00' ~ "A7_nd3",
                    id == 'A7' & date >= '2017-04-28 00:00:00' & date <= '2017-05-11 06:00:00' |
                    id == 'A7' & date >= '2017-05-14 00:00:00' & date <= '2017-05-21 18:00:00' |
                    id == 'A7' & date >= '2017-05-23 06:00:00' & date <= '2017-05-23 18:00:00' |
                    id == 'A7' & date >= '2017-05-26 00:00:00' & date <= '2017-05-28 24:00:00' ~ "A7_nd4",
                    id == 'A7' & date >= '2017-06-06 00:00:00' & date <= '2017-06-06 16:00:00' |
                    id == 'A7' & date >= '2017-06-10 20:00:00' & date <= '2017-06-12 20:00:00' |
                    id == 'A7' & date >= '2017-07-12 18:00:00' & date <= '2017-07-28 06:00:00' ~ "A7_nd5",
                    id == 'IAD' & date >= '2016-08-20 09:00:00' & date <= '2016-08-25 11:00:00' ~ "IAD_nd1",
                    id == 'IAD' & date >= '2018-02-04 18:00:00' & date <= '2018-02-07 19:00:00' ~ "IAD_nd2",
                    id == 'IAD' & date >= '2018-03-28 08:00:00' & date <= '2018-06-12 14:00:00' ~ "IAD_nd3",
                    id == 'IAD' & date >= '2018-12-15 00:00:00' & date <= '2018-12-20 15:00:00' ~ "IAD_nd4",
                    id == 'IAD' & date >= '2019-03-04 10:00:00' & date <= '2019-04-01 17:00:00' |
                    id == 'IAD' & date >= '2019-04-05 10:00:00' & date <= '2019-04-09 17:00:00' |
                    id == 'IAD' & date >= '2019-04-14 20:00:00' & date <= '2019-04-19 13:00:00' |
                    id == 'IAD' & date >= '2019-04-21 06:00:00' & date <= '2019-04-21 20:00:00' ~ "IAD_nd5",
                    id == 'IAD' & date >= '2019-04-29 09:00:00' & date <= '2019-05-01 21:00:00' ~ "IAD_nd6",
                    id == 'IAD' & date >= '2019-07-08 09:00:00' & date <= '2019-07-15 18:00:00' ~ "IAD_nd7",
                    id == 'IBS' & date >= '2020-07-27 00:00:00' & date <= '2020-07-28 19:00:00' ~ "IBS_nd1",
                    id == 'IBS' & date >= "2020-08-07 08:00:00" & date <= "2020-08-18 19:00:00" ~ "IBS_nd2",
                    id == 'IBS' & date >= "2021-01-14 06:00:00" & date <= "2021-01-15 16:00:00" ~ "IBS_nd3",
                    id == 'IBS' & date >= "2021-02-15 09:00:00" & date <= "2021-02-15 17:00:00" ~ "IBS_nd4",
                    id == 'IBS' & date >= "2021-04-26 11:00:00" & date <= "2021-05-01 16:00:00" ~ "IBS_nd5",
                    id == 'IBS' & date >= '2022-03-22 00:00:00' & date <= '2022-06-04 15:00:00' ~ "IBS_nd6",
                    id == 'IBS' & date >= '2023-02-08 16:00:00' & date <= '2023-02-15 00:00:00' |
                    id == "IBS" & date >= "2023-02-19 04:00:00" & date <= "2023-02-23 20:00:00" ~ "IBS_nd7",
                    id == "IBS" & date >= "2023-03-01 16:00:00" & date <= "2023-03-08 24:00:00" |
                    id == "IBS" & date >= "2023-03-13 04:00:00" & date <= "2023-03-16 17:00:00" ~ "IBS_nd8",
                    id == 'IBH' & date >= '2020-08-07 07:00:00' & date <= '2020-08-15 15:00:00' ~ "IBH_nd1",
                    id == 'IBH' & date >= '2022-04-09 06:00:00' & date <= '2022-04-11 19:00:00' ~ "IBH_nd2",
                    id == 'IBH' & date >= '2022-04-19 12:00:00' & date <= '2022-04-24 15:00:00' ~ "IBH_nd3",
                    id == 'IBH' & date >= '2022-04-29 09:00:00' & date <= '2022-05-01 18:00:00' ~ "IBH_nd4",
                    id == 'IBH' & date >= '2022-05-04 09:00:00' & date <= '2022-05-04 16:00:00' ~ "IBH_nd5",
                    id == 'IBH' & date >= '2022-05-10 09:00:00' & date <= '2022-05-13 17:00:00' ~ "IBH_nd6",
                    id == 'IBK' & date >= '2020-08-20 09:00:00' & date <= '2020-08-23 11:00:00' ~ "IBK_nd1",
                    id == 'IBK' & date >= '2021-06-27 09:00:00' & date <= '2021-07-02 12:00:00' ~ "IBK_nd2",
                    id == 'IBK' & date >= '2022-04-15 00:00:00' & date <= '2022-04-17 20:00:00' ~ "IBK_nd3",
                    id == 'IFP' & date >= '2022-07-23 06:00:00' & date <= '2022-08-15 00:00:00' ~ "IFP_nd1",
                    id == 'IFP' & date >= '2023-04-24 00:00:00' & date <= '2023-04-30 04:00:00' ~ "IFP_nd2",
                    id == "IFP" & date >= '2023-05-16 00:00:00' & date <= '2023-06-08 20:00:00' ~ "IFP_nd3",
                    id == 'ICZ' & date >= '2019-09-09 08:00:00' & date <= '2019-09-14 14:30:00' ~ "ICZ_nd1",
                    id == 'ICZ' & date >= '2020-04-11 10:00:00' & date <= '2020-04-24 00:00:00' ~ "ICZ_nd2",
                    id == 'ICZ' & date >= '2020-05-02 14:30:00' & date <= '2020-05-05 17:00:00' ~ "ICZ_nd3",
                    id == 'CBK' & date >= '2013-08-15 00:00:00' & date <= '2013-08-21 12:00:00' ~ "CBK_nd1",
                    id == 'CBK' & date >= '2014-03-20 04:30:00' & date <= '2014-03-22 10:30:00' ~ "CBK_nd2",
                    id == 'CBK' & date >= '2014-04-08 06:30:00' & date <= '2014-04-12 11:00:00' ~ "CBK_nd3",
                    id == 'IAB' & date >= '2018-08-07 11:00:00' & date <= '2018-08-12 20:00:00' ~ "IAB_nd1",
                    id == 'IAB' & date >= '2019-03-26 11:00:00' & date <= '2019-03-28 14:00:00' ~ "IAB_nd2",
                    id == 'IAB' & date >= '2019-04-19 00:00:00' & date <= '2019-04-23 24:00:00' ~ "IAB_nd3",
                    id == 'IAB' & date >= '2019-05-05 06:00:00' & date <= '2019-05-12 14:00:00' |
                    id == 'IAB' & date >= '2019-05-15 21:00:00' & date <= '2019-05-20 13:00:00' ~ "IAB_nd4",
                    id == 'IAB' & date >= '2019-05-29 15:00:00' & date <= '2019-06-02 16:00:00' |
                    id == 'IAB' & date >= '2019-06-08 09:00:00' & date <= '2019-06-10 17:00:00' ~ "IAB_nd5",
                    id == 'IAB' & date >= '2019-09-07 11:00:00' & date <= '2019-09-11 15:00:00' ~ "IAB_nd6",
                    id == 'IAB' & date >= '2019-10-29 08:00:00' & date <= '2019-10-30 09:00:00' ~ "IAB_nd7",
                    id == 'IAB' & date >= '2020-03-16 00:00:00' ~ "IAB_nd8",
                    id == 'CAM' & date >= "2016-04-14 06:00:00" & date <= "2016-04-19 20:00:00" ~ "CAM_nd1",
                    id == "CAM" & date >= "2016-05-03 08:00:00" & date <= "2016-05-06 18:00:00" ~ "CAM_nd2",
                    id == "CAM" & date >= "2016-05-20 05:00:00" & date <= "2016-05-24 18:00:00" ~ "CAM_nd3",
                    id == "CAM" & date >= "2016-07-02 23:00:00" & date <= "2016-07-06 20:00:00" ~ "CAM_nd4",
                    id == 'Antares' & date >= "2015-08-15 11:00:00" & date <= "2015-08-18 19:00:00" ~ "Antares_nd1",
                    id == "Antares" & date >= "2015-09-08 10:30:00" & date <= "2015-09-13 20:00:00" ~ "Antares_nd2",
                    id == "Antares" & date >= "2016-04-04 00:00:00" & date <= "2016-04-10 17:00:00" ~ "Antares_nd3",
                    id == "Antares" & date >= "2016-04-19 11:00:00" & date <= "2016-04-22 13:00:00" ~ "Antares_nd4",
                    id == "Antares" & date >= "2016-05-05 00:00:00" & date <= "2016-05-06 06:00:00" ~ "Antares_nd5",
                    id == "Antares" & date >= "2016-05-14 13:00:00" & date <= "2016-05-17 19:00:00" ~ "Antares_nd6",
                    id == "Antares" & date >= "2016-06-10 07:00:00" & date <= "2016-06-12 19:00:00" ~ "Antares_nd7",
                    id == 'IBI' & date >= "2017-07-21 05:00:00" & date <= "2017-07-27 20:00:00" ~ "IBI_nd1",
                    id == "IBI" & date >= "2017-08-19 08:00:00" & date <= "2017-08-20 15:00:00" ~ "IBI_nd2"),
          day = as.Date(date),
          distKM = dist/1000)%>%
          tidyr::unite(id_y, c(id, year), sep="_", remove = F)%>%
          select(-c("pkey"))


nd$time <- as.POSIXct(strptime(as.character(nd$time),"%Y-%m-%d %H:%M"))

# Find duplicate dates
duplicate_dates <- osprey_nd[duplicated(osprey_nd$time) | duplicated(osprey_nd$time, fromLast = TRUE), ]

duplicate_dates <- duplicate_dates%>%
          arrange(time)

# Print the duplicate dates
print(head(duplicate_dates, 40))

# daily track (= successive telemetry locations in each day)

          dailyDirections <- nd_df%>%
                    mutate(deg_turnAngle = rel.angle * (180/pi))%>%
                    group_by(id, ymd)%>%
                    summarize(meanDir = mean(deg_turnAngle),
                              medianDir = median(deg_turnAngle))

          dirBreaks <- c(22.5, 67.5,112.5, 157.5, 202.5, 247.5, 292.5, 337.5)


ggplot(dailyDirections) +
  geom_bar(aes(category)) +
  xlab("Angle of Flight") +
  ylab("Count of birds") +
  theme_light() 

ggplot(dailyDirections, aes(x = "", y = "meanDir")) + 
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



#########################
#  Natal dispersal stat #
#########################

# export tables to Latex, pay attention to digits arguments          , digits = c(0, 3, 3, 3)

lde_df %>%
  kable(format = 'latex', booktabs = TRUE) 

# 1. prima tabella: inizio fine e durata (n day) + aggiungere i paesi visitati

# Durations #

nd_duration <- nd_df %>%
          group_by(id, burst) %>% 
          summarize(start = min(day), end = max(day)) %>%
          mutate(duration = round(difftime(end, start, units = "days")))%>%
          summarize(mean_duration = sprintf("%.2f", mean(as.numeric(difftime(end, start, units = "days")))),
                    max_duration = sprintf("%.2f", max(as.numeric(difftime(end, start, units = "days")))),
                    sd_duration = sprintf("%.2f", sd(as.numeric(difftime(end, start, units = "hours"))))
                   )


nd_duration_id <- nd_df %>%
          group_by(id) %>% 
          summarize(start = min(day), end = max(day)) %>%
          mutate(duration = round(difftime(end, start, units = "days")))

 countries_vis<- nd_df%>%
          dplyr::filter(burst == "IBS_nd5")

 ggplot(osprey_eu_utm) + 
       geom_spatvector()+
       geom_path(data = countries_vis, aes(x = x, y = y, colour = "red"), linewidth = 1, lineend = "round")



# 2. seconda tabella: Long distance event numero di eventi +
#                    durata media e massima in giorni x lde +
#                    distanza media e massima per ogni individ +
#                    velocità media e massima +
#                    aggiungi paesi visitati

lde_stat <- nd_df %>%
          group_by(id) %>%
          summarise(lde_event = n_distinct(burst),
                    mean_dist = sprintf("%.2f", mean(distKM, na.rm = TRUE)),
                    max_dist = sprintf("%.2f", max(distKM, na.rm = TRUE)),
                    sd_dist = sprintf("%.2f", sd(distKM, na.rm = TRUE))
                   )


lde_df <- left_join(lde_stat, nd_duration, by = "id")

lde_df

mean_vel = , 
                   max_vel = 


# 3. terza tabella: descrizione aree di sosta (multi day) id +
#                    numero aree di sosta +
#                    numero medio di giorni di sosta per area +
#                    percentuale giorni di sosta in un’area protetta

GRAFICI 
# 1. primo plot: mappe gi`a fatte con evidenziati i lde intervellati dalle aree di sosta

# 2. secondo plot: grafico unico con tutti gli individui 


# Distances #

summary_distance <- nd_df%>%
      group_by(burst, day)%>%
      summarize(minDistxDay = min(distKM),
                meanDistxDay = mean(distKM),
                maxDistxDay = max(distKM))

summary_id_distance <- summary_distance%>%
      group_by(burst)%>%
      summarize(minDist = min(minDistxDay, na.rm=T),
                meanDist = mean(meanDistxDay, na.rm=T),
                maxDist = max(maxDistxDay, na.rm=T))

summary_id_distance <- summary_id_distance%>%
          arrange(burst)

dist_plot <- ggplot(osprey_ndlt_df, aes(x = osprey_ndlt_df$distKM)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Distance (Km)") +
  ylab("Count") +
  ggtitle("Distribution of Distance")



summary_speed <- osprey_ndlt_df%>%
  group_by(burst)%>%
  mutate(Speed = (dist/1000)/(dt/3600)) %>% 
  summarize(minSpeed = min(Speed, na.rm=T),
            meanSpeed = mean(Speed, na.rm=T),
            maxSpeed = max(Speed, na.rm=T)) 

osp_ndlt_df<- osprey_ndlt_df%>%
  mutate(Speed = (dist/1000)/(dt/3600),
         deg_turnAngle = rel.angle * (180/pi))

                  
                  speed_plot <- ggplot(osprey_ndlt_df, aes(x = Speed)) +
                    geom_histogram(binwidth = 1) +
                    labs(x = "Speed", y = "Frequency") +
                    theme_bw()

summary_dt <- osprey_ndlt_df%>%
  group_by(burst)%>%
  mutate(dt = dt/3600) %>% 
  summarize(minDt = min(dt, na.rm=T),
            meanDt = mean(dt, na.rm=T),
            maxDt = max(dt, na.rm=T)) 


osp_ndlt_df <- osprey_ndlt_df%>%
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
         
