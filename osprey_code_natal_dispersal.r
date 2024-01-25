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


#########################
#  Natal dispersal stat #
#########################

# export tables to Latex, pay attention to digits arguments

ndt_stat%>%
          kable(format = 'latex', booktabs = TRUE, digits = c(0, 1, 2, 2, 2, 2, 2, 2 )) 


#############
# Durations #
#############

# Request
# 1. prima tabella: inizio fine e durata (n day) + aggiungere i paesi visitati


# Code to check which countries are traversed during Natal Dispersal Travel

countries_vis<- ndtraj_df%>%
          dplyr::filter(NDT == "IBS_nd5")

ggplot(osprey_eu_utm) + 
          geom_spatvector()+
          geom_path(data = countries_vis, aes(x = x, y = y, colour = "red"), linewidth = 1, lineend = "round")

countries_id <- data.frame(id = c('A7', 'Antares', 'CAM', 'CBK', 'CIV', 'E7', 'H7', 'IAB', 'IAD', 'IBH', 'IBI', 'IBK', 'IBS', 'ICZ', 'IFP'),
                           countries = c('BIH, HRV, HUN, ITA, SVN', 'FRA, ITA', 'FRA, ITA', 'FRA, ITA', 'DZA, FRA, ITA, TUN', 'AUT, BIH, CHE, DEU, ESP, FRA, HUN, HRV, ITA', 'ESP, FRA, ITA', 'ITA', 'AUT, BEL, BIH, CHE, DEU, ESP, FRA, HUN, HRV, ITA', 'ITA', 'FRA, ITA', 'FRA, ITA', 'ALB, BOH, CHE, ESP, FRA, HVN, ITA, MNE, SVN', 'ITA', 'FRA, ITA'))


# Table reporting first and last day of Natal Dispersal period and whole durations of every animal

nd_duration_id <- ndtraj_df %>%
          group_by(id, NDT) %>% 
          summarize(start = min(day), end = max(day)) %>%
          mutate(duration = round(difftime(end, start, units = "days")))%>%
          group_by(id)%>%
          summarize(start = min(start), end = max(end), duration = sum(duration))%>%
          left_join(countries_id, by = "id")

nd_duration_id

# Table reporting mean, max duration and standard deviation of Natal Dispersal Travel duration of every animal

nd_duration <- ndtraj_df %>%
          group_by(id, NDT) %>% 
          summarize(start = min(date), end = max(date)) %>%
          mutate(duration = difftime(end, start, units = "days"))%>%
          summarize(mean_dur = mean(duration),
                    max_dur = max(duration),
                    sd_dur = sd(duration))
nd_duration


# Table reporting NDT start/finish date and duration of every animal

ndt_duration <- ndtraj_df %>%
          group_by(NDT) %>% 
          mutate(start = min(date), end = max(date))%>%
          mutate(duration = difftime(end, start, units = "days"))%>%
          select("start", "end", "duration")%>%
          unique()
ndt_duration


# Request

# 2. seconda tabella: Long distance event numero di eventi +
#                    durata media e massima in giorni x lde +
#                    distanza media e massima per ogni individ +
#                    velocità media e massima +
#                    aggiungi paesi visitati

# median_dist = sprintf("%.2f", median(distKM, na.rm = TRUE)), # add this line in the second summerize() funciton to have the median value


###################
# Daily distances #
###################


# DF reporting daily distances travelled by each animals

daily_dist_df <- ndtraj_df %>%
          group_by(id, track_id, day) %>%
          summarise(daily_dist = sum(distKM, na.rm = TRUE))
daily_dist_df


#  DF reporting mean\max and standard direction of daily distances travelled by each animals

daily_dist_stat <- daily_dist_df%>%
          group_by (id)%>%
          summarize(mean_dist = mean(daily_dist, na.rm = TRUE),
                    med_dist = median(daily_dist, na.rm = TRUE),
                    max_dist = max(daily_dist, na.rm = TRUE),
                    sd_dist = sd(daily_dist, na.rm = TRUE))
daily_dist_stat


# Total number of Natal Dispersal Travel made by each animal

ndt_ev <- ndtraj_df%>%
          group_by(id)%>%
          summarize(ndt_event = n_distinct(NDT))

ndt_stat <- left_join(ndt_ev, daily_dist_stat, by = "id")

ndt_stat <- left_join(ndt_stat, nd_duration, by = "id")

ndt_stat

speed_df <-  ndtraj_df %>%
          group_by(burst) %>%
          mutate(speed = distKM / (dt / 3600)) %>%
          summarise(mean_speed = mean(speed, na.rm = TRUE),
                    max_speed = max(speed, na.rm = TRUE)
          )


summary_dt <- nd1htraj_df%>%
  group_by(burst)%>%
  summarize(minDt = min(dt, na.rm=T),
            meanDt = mean(dt, na.rm=T),
            maxDt = max(dt, na.rm=T)) 
summary_dt

          


# 3. terza tabella: descrizione aree di sosta (multi day) id +
#                    numero aree di sosta +
#                    numero medio di giorni di sosta per area +
#                    percentuale giorni di sosta in un’area protetta


st_df_no_duplicates <- st_df[!duplicated(st_df[c("ID", "time")]) & !duplicated(st_df[c("ID", "time")], fromLast = TRUE), ]


 st <- st_df_no_duplicates%>%
           select(-c("date", "death_date", "season"))
 
 stationary_lt <- as.ltraj(st[, c("x", "y")],
                               date = st$time,
                               id = st$ID,
                               typeII = T)


foo <- function(dt) {
return(dt> (60*60*24))
}

# Cut the ltraj object based on the time gap criterion
cut_stationary_lt <- cutltraj(stationary_lt, "foo(dt)", nextr = TRUE)

cut_stationary_lt


 
 stationary_df <- stationary_lt%>%
           ld()%>%
           mutate(doy = yday(date),
                  ymd = as.Date(date),
                  year = year(date),
                  time = date,
                  ID = id,
                  burst = dplyr::case_when(ID == 'H7' & time >= '2013-08-09 15:30:00' & time <= '2015-04-02 05:00:00' ~ "H7_wintering",
                     ID == 'H7' & time >= '2015-04-11 13:00:00' & time <= '2015-04-20 13:00:00' ~ "H7_stop1",
                     ID == 'H7' & time >= '2015-04-30 16:00:00' & time <= '2015-05-03 05:00:00' ~ "H7_stop2",
                     ID == 'H7' & time >= '2015-05-03 11:00:0' ~ "H7_end",
                     ID == 'CIV' & time >= "2014-08-22 06:00:00" & time <= "2014-09-11 08:00:00" ~ "CIV_stop1",
                     ID == 'CIV' & time >= "2014-09-13 17:00:00" & time <= "2014-10-21 08:00:00" ~ "CIV_stop2",
                     ID == 'CIV' & time >= "2014-10-21 21:00:00" & time <= "2015-03-21 04:00:00" ~ "CIV_wintering1",
                     ID == 'CIV' & time >= "2015-03-21 24:00:00" & time <= "2015-06-04 04:00:00" ~ "CIV_stop3",
                     ID == 'CIV' & time >= "2015-06-22 18:00:00" & time <= "2015-08-13 06:00:00" ~ "CIV_stop4",
                     ID == 'CIV' & time >= "2015-08-14 00:00:00" & time <= "2015-11-21 16:00:00" ~ "CIV_stop5",
                     ID == 'CIV' & time >= "2015-11-27 00:00:00" & time <= "2016-03-28 18:00:00" ~ "CIV_wintering2",
                     ID == 'CIV' & time >= '2016-04-01 06:00:00' & time <= '2016-04-15 06:00:00' ~ "CIV_stop6",
                     ID == 'CIV' & time >= '2016-04-18 22:00:00' & time <= '2016-10-28 06:00:00' ~ "CIV_stop7",
                     ID == 'CIV' & time >= '2016-10-30 06:00:00' ~ "CIV_end",
                     ID == 'E7' & time >= '2014-08-27 13:00:00' & time <= '2016-03-10 05:00:00' ~ "E7_wintering",
                     ID == 'E7' & time >= '2016-03-24 14:00:00' & time <= '2016-03-30 09:00:00' ~ "E7_stop1",
                     ID == 'E7' & time >= '2016-04-22 20:00:00' & time <= '2016-04-24 10:30:00' ~ "E7_stop2",
                     ID == 'E7' & time >= '2016-04-25 17:30:00' & time <= '2016-05-04 10:00:00' ~ "E7_stop3",
                     ID == 'E7' & time >= '2016-05-07 17:00:00' & time <= '2016-05-12 11:00:00' ~ "E7_stop4",
                     ID == 'E7' & time >= '2016-05-16 20:30:00' & time <= '2016-05-20 12:00:00' ~ "E7_stop5",
                     ID == 'E7' & time >= '2016-05-27 18:30:00' ~ "E7_end",
                     ID == 'A7' & time >= '2015-08-18 12:00:00' & time <= '2017-02-20 00:00:00' ~ "A7_wintering",
                     ID == 'A7' & time >= '2017-03-01 20:00:00' & time <= '2017-03-17 00:00:00' ~ "A7_stop1",
                     ID == 'A7' & time >= '2017-04-15 06:00:00' & time <= '2017-04-16 24:00:00' ~ "A7_stop2",
                     ID == 'A7' & time >= '2017-04-21 08:00:00' & time <= '2017-04-28 00:00:00' ~ "A7_stop3",
                     ID == 'A7' & time >= '2017-05-11 06:00:00' & time <= '2017-05-14 00:00:00' ~ "A7_stop4",
                     ID == 'A7' & time >= '2017-05-21 18:00:00' & time <= '2017-05-23 06:00:00' ~ "A7_stop5",
                     ID == 'A7' & time >= '2017-05-23 18:00:00' & time <= '2017-05-26 00:00:00' ~ "A7_stop6",
                     ID == 'A7' & time >= '2017-05-28 24:00:00' & time <= '2017-06-06 00:00:00' ~ "A7_stop7",
                     ID == 'A7' & time >= '2017-06-06 16:00:00' & time <= '2017-06-10 20:00:00' ~ "A7_stop8",
                     ID == 'A7' & time >= '2017-06-12 20:00:00' & time <= '2017-07-12 18:00:00' ~ "A7_stop9",
                     ID == 'A7' & time >= '2017-07-28 06:00:00'~ "A7_end",
                     ID == 'IAD' & time >= '2016-08-25 11:00:00' & time <= '2018-02-04 18:00:00' ~ "IAD_wintering1",
                     ID == 'IAD' & time >= '2018-02-07 19:00:00' & time <= '2018-03-28 08:00:00' ~ "IAD_stop1",
                     ID == 'IAD' & time >= '2018-06-12 14:00:00' & time <= '2018-12-15 00:00:00' ~ "IAD_wintering2a",
                     ID == 'IAD' & time >= '2018-12-20 15:00:00' & time <= '2019-03-04 10:00:00' ~ "IAD_wintering2b",
                     ID == 'IAD' & time >= '2019-04-01 17:00:00' & time <= '2019-04-05 10:00:00' ~ "IAD_stop2",
                     ID == 'IAD' & time >= '2019-04-09 17:00:00' & time <= '2019-04-14 20:00:00' ~ "IAD_stop3",
                     ID == 'IAD' & time >= '2019-04-19 13:00:00' & time <= '2019-04-21 06:00:00' ~ "IAD_stop4",
                     ID == 'IAD' & time >= '2019-04-21 20:00:00' & time <= '2019-04-29 09:00:00' ~ "IAD_stop5",
                     ID == 'IAD' & time >= '2019-05-01 21:00:00' & time <= '2019-07-08 09:00:00' ~ "IAD_stop6",
                     ID == 'IAD' & time >= '2019-07-15 18:00:00' ~ "IAD_end",
                     ID == 'IBS' & time >= '2020-07-28 19:00:00' & time <= '2020-08-07 08:00:00' ~ "IBS_stop1",
                     ID == 'IBS' & time >= '2020-08-18 19:00:00' & time <= '2021-01-14 06:00:00' ~ "IBS_wintering1",
                     ID == 'IBS' & time >= '2021-01-15 16:00:00' & time <= '2021-02-15 09:00:00' ~ "IBS_stop2",
                     ID == 'IBS' & time >= '2021-02-15 17:00:00' & time <= '2021-04-26 11:00:00' ~ "IBS_stop3",
                     ID == 'IBS' & time >= '2021-05-01 16:00:00' & time <= '2022-03-22 00:00:00' ~ "IBS_wintering2",
                     ID == 'IBS' & time >= '2022-06-04 15:00:00' & time <= '2023-02-08 16:00:00' ~ "IBS_wintering3",
                     ID == 'IBS' & time >= '2023-02-15 00:00:00' & time <= '2023-02-19 04:00:00' ~ "IBS_stop4",
                     ID == 'IBS' & time >= '2023-02-23 20:00:00' & time <= '2023-03-01 16:00:00' ~ "IBS_stop5",
                     ID == 'IBS' & time >= '2023-03-08 24:00:00' & time <= '2023-03-13 04:00:00' ~ "IBS_stop6",
                     ID == 'IBS' & time >= '2023-03-16 17:00:00' ~ "IBS_end",
                     ID == 'IBH' & time >= '2020-08-15 15:00:00' & time <= '2022-04-09 06:00:00' ~ "IBH_wintering1",
                     ID == 'IBH' & time >= '2022-04-11 19:00:00' & time <= '2022-04-19 12:00:00' ~ "IBH_stop1",
                     ID == 'IBH' & time >= '2022-04-24 15:00:00' & time <= '2022-04-29 09:00:00' ~ "IBH_stop2",
                     ID == 'IBH' & time >= '2022-05-01 18:00:00' & time <= '2022-05-04 09:00:00' ~ "IBH_stop3",
                     ID == 'IBH' & time >= '2022-05-04 16:00:00' & time <= '2022-05-10 09:00:00' ~ "IBH_stop4",
                     ID == 'IBH' & time >= '2022-05-13 17:00:00' ~ "IBH_end",
                     ID == 'IBK' & time <= '2020-08-20 09:00:00' ~ "IBK_nest",
                     ID == 'IBK' & time >= '2020-08-23 11:00:00' & time <= '2021-06-27 09:00:00' ~ "IBK_wintering1",
                     ID == 'IBK' & time >= '2021-07-02 12:00:00' & time <= '2022-04-15 00:00:00' ~ "IBK_wintering2",
                     ID == 'IBK' & time >= '2022-04-17 20:00:00' ~ "IBK_end",
                     ID == 'IFP' & time >= '2022-08-15 00:00:00' & time <= '2023-04-24 00:00:00' ~ "IFP_wintering",
                     ID == 'IFP' & time >= '2023-04-30 04:00:00' & time <= '2023-05-16 00:00:00' ~ "IFP_stop1",
                     ID == "IFP" & time >= '2023-05-17 22:00:00' & time <= "2023-05-23 00:00:00" ~ "IFP_stop2",
                     ID == "IFP" & time >= '2023-05-25 00:00:00' & time <= "2023-06-07 00:00:00" ~ "IFP_stop3",
                     ID == "IFP" & time >= '2023-06-09 24:00:00' & time <= "2023-06-11 03:00:00" ~ "IFP_stop4",
                     ID == "IFP" & time >= "2023-06-11 18:00:00" ~ "IFP_end",
                     ID == 'ICZ' & time <= '2019-09-09 08:00:00' ~ "ICZ_nest",
                     ID == 'ICZ' & time >= '2019-09-14 14:30:00' & time <= '2020-04-11 10:00:00' ~ "ICZ_wintering1",
                     ID == 'ICZ' & time >= '2020-04-24 00:00:00' & time <= '2020-05-02 14:30:00' ~ "ICZ_stop1",
                     ID == 'ICZ' & time >= '2020-05-05 17:00:00' ~ "ICZ_end",
                     ID == 'CBK' & time >= '2013-08-21 12:00:00' & time <= '2014-03-20 04:30:00' ~ "CBK_wintering1",
                     ID == 'CBK' & time >= '2014-03-22 10:30:00' & time <= '2014-04-08 06:30:00' ~ "CBK_stop1",
                     ID == 'CBK' & time >= '2014-04-12 11:00:00' ~ "CBK_end",
                     ID == 'IAB' & time >= '2018-08-12 20:00:00' & time <= '2019-03-26 11:00:00' ~ "IAB_wintering1",
                     ID == 'IAB' & time >= '2019-03-28 14:00:00' & time <= '2019-04-19 00:00:00' ~ "IAB_stop1",
                     ID == 'IAB' & time >= '2019-04-23 24:00:00' & time <= '2019-05-05 06:00:00' ~ "IAB_stop2",
                     ID == 'IAB' & time >= '2019-05-12 14:00:00' & time <= '2019-05-15 21:00:00' ~ "IAB_stop3",
                     ID == 'IAB' & time >= '2019-05-20 13:00:00' & time <= '2019-05-29 15:00:00' ~ "IAB_stop4",
                     ID == 'IAB' & time >= '2019-06-02 16:00:00' & time <= '2019-06-08 09:00:00' ~ "IAB_stop5",
                     ID == 'IAB' & time >= '2019-06-10 17:00:00' & time <= '2019-09-07 11:00:00' ~ "IAB_stop6",
                     ID == 'IAB' & time >= '2019-09-11 15:00:00' & time <= '2019-10-29 08:00:00' ~ "IAB_stop7",
                     ID == 'IAB' & time >= '2019-10-30 09:00:00' & time <= '2020-03-16 00:00:00' ~ "IAB_wintering2",
                     ID == "CAM" & time <= "2016-04-14 06:00:00" ~ "CAM_nest",
                     ID == "CAM" & time >= "2016-04-19 20:00:00" & time <= "2016-05-03 08:00:00" ~ "CAM_stop1",
                     ID == "CAM" & time >= "2016-05-06 18:00:00" & time <= "2016-05-20 05:00:00" ~ "CAM_stop2",
                    ID == "CAM" & time >= "2016-05-24 18:00:00" & time <= "2016-07-02 23:00:00" ~ "CAM_stop3",
                     ID == "CAM" & time >= "2016-07-06 20:00:00" ~ "CAM_end",
                     ID == "IBI" & time >= "2017-07-27 20:00:00" & time <= "2017-08-19 08:00:00" ~ "IBI_stop1",
                    ID == "IBI" & time >= "2017-08-20 15:00:00" ~ "IBI_end",
                     ID == "Antares" & time >= "2015-08-18 19:00:00" & time <= "2015-09-08 10:30:00" ~ "Antares_stop1",
                     ID == "Antares" & time >= "2015-09-13 20:00:00" & time <= "2016-04-04 00:00:00" ~ "Antares_wintering",
                     ID == "Antares" & time >= "2016-04-10 17:00:00" & time <= "2016-04-19 11:00:00" ~ "Antares_stop2",
                     ID == "Antares" & time >= "2016-04-22 13:00:00" & time <= "2016-05-05 00:00:00" ~ "Antares_stop3",
                     ID == "Antares" & time >= "2016-05-06 06:00:00" & time <= "2016-05-14 13:00:00" ~ "Antares_stop4",
                     ID == "Antares" & time >= "2016-05-17 19:00:00" & time <= "2016-06-10 07:00:00" ~ "Antares_stop5",
                     ID == "Antares" & time >= "2016-06-12 19:00:00" ~ "Antares_end"),
           day = as.Date(date),
           distKM = dist/1000)%>%
           tidyr::unite(id_y, c(ID, year), sep="_", remove = F)%>%
           select(-c("pkey"))
 
 
 dist_control <- stationary_df%>%
           select("date", "burst", "distKM")%>%
           arrange(desc(distKM))
 
 
 
 stopover_df <- stationary_df %>%
   filter(!grepl("wintering", burst, ignore.case = TRUE))%>%
   filter(!grepl("nest", burst, ignore.case = TRUE))%>%
   filter(!grepl("end", burst, ignore.case = TRUE))
 
 stopover_duration <- stopover_df %>%
           group_by(ID, burst) %>% 
           summarize(start = min(date), end = max(date)) %>%
           mutate(duration = difftime(end, start, units = "day"))%>%
           summarize(min_dur = min(duration),
                     mean_dur = mean(duration),
                     max_dur = max(duration),
                     tot_dur = sum(duration),
                     sd_dur = sd(duration))


stopover <- stopover_df%>%
          group_by(ID)%>%
          summarize(tot_stopover = n_distinct(burst))

stopover_stat <- left_join(stopover, stopover_duration, by = "ID")

stopover_stat

stopoverPA_df <- read.csv("C:/Tesi/R/osprey/data/stopover_PA_32632.csv")

 stopoverPA_duration <- stopoverPA_df%>%
          rename(ID = "id")%>%
           group_by(ID, burst) %>% 
           summarize(start = min(date), end = max(date)) %>%
           mutate(duration = difftime(end, start, units = "day"))%>%
           summarize(min_dur = min(duration),
                     mean_dur = mean(duration),
                     max_dur = max(duration),
                     tot_durPA = sum(duration),
                     sd_dur = sd(duration))%>%
          select("ID", "tot_durPA")


stopoverPA <- stopoverPA_df%>%
          rename(ID = "id")%>%
          group_by(ID)%>%
          summarize(tot_stopover = n_distinct(burst))

stopoverPA_stat <- left_join(stopoverPA, stopoverPA_duration, by = "ID")

stopoverPA_stat

stopover_stat_tot <- left_join(stopover_stat, stopoverPA_duration, by = "ID")

stopover_stat_tot <- stopover_stat_tot%>%
          mutate()%>%
          mutate(perc_PA = (as.numeric(tot_durPA)/as.numeric(tot_dur))*100)%>%
          select(-"tot_durPA")

GRAFICI 
# 1. primo plot: mappe gi`a fatte con evidenziati i lde intervellati dalle aree di sosta

# 2. secondo plot: grafico unico con tutti gli individui 


# Distances #

summary_distance <- ndtraj_df%>%
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

dist_plot <- ggplot(nd_dflt_df, aes(x = nd_dflt_df$distKM)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Distance (Km)") +
  ylab("Count") +
  ggtitle("Distribution of Distance")



summary_speed <- nd_dflt_df%>%
  group_by(burst)%>%
  mutate(Speed = (dist/1000)/(dt/3600)) %>% 
  summarize(minSpeed = min(Speed, na.rm=T),
            meanSpeed = mean(Speed, na.rm=T),
            maxSpeed = max(Speed, na.rm=T)) 

osp_ndlt_df<- nd_dflt_df%>%
  mutate(Speed = (dist/1000)/(dt/3600),
         deg_turnAngle = rel.angle * (180/pi))

                  
                  speed_plot <- ggplot(nd_dflt_df, aes(x = Speed)) +
                    geom_histogram(binwidth = 1) +
                    labs(x = "Speed", y = "Frequency") +
                    theme_bw()




###############################################

##########   OLD CODE   #######################

###############################################




ndtraj_df$rel.angle.degrees <- ndtraj_df$rel.angle* (180 / pi)

nd_lt2 <- ndtraj_df%>%
          dl()

foo <- function(rel.angle.degrees) {
return(rel.angle.degrees>90)
}

nd_ltc <- cutltraj(nd_lt2, "foo(rel.angle.degrees)", nextr = TRUE)
nd_ltc

ndtraj_df2 <- nd_ltc%>%
          ld()

write.csv(ndtraj_df2, "ndtraj_df2.csv")

nd$time <- as.POSIXct(strptime(as.character(nd$time),"%Y-%m-%d %H:%M"))

# Find duplicate dates
duplicate_dates <- nd_df[duplicated(nd_df$time) | duplicated(nd_df$time, fromLast = TRUE), ]

duplicate_dates <- duplicate_dates%>%
          arrange(time)

# Print the duplicate dates
print(head(duplicate_dates, 40))

# daily track (= successive telemetry locations in each day)



main_direction <- ndtraj_df %>%
  group_by(burst) %>%
  summarize(main_angle = atan2(mean(sin(abs.angle)), mean(cos(abs.angle))))

# Print the result
print(main_direction)

dailyDirections <- ndtraj_df%>%
          mutate(deg_turnAngle = rel.angle * (180/pi))%>%
          group_by(id, day)%>%
          summarize(meanDir = mean(deg_turnAngle),
                    medianDir = median(deg_turnAngle))

dailyDirections_id <- dailyDirections%>%
          group_by(id)%>%
          summarize(meanDir_id = mean(meanDir),
                    medianDir_id = median(medianDir))


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




osp_ndlt_df <- nd_dflt_df%>%
                  mutate(deg_turnAngle = rel.angle * (180/pi))


