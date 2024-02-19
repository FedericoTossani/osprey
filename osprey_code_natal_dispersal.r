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

tab_appendix%>%
          kable(format = 'latex', booktabs = TRUE, digits = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)) 

  group_by(ID)%>%

osprey_lt <- as.ltraj(osprey[, c("x", "y")],
                    date = osprey$time,
                    id = osprey$ID,
                    typeII = T)
osprey_lt_df <- osprey_lt%>%
          ld()

summary_dt <- osprey_lt_df%>%
  summarize(minDt = min(dt, na.rm=T),
            meanDt = mean(dt, na.rm=T),
            maxDt = max(dt, na.rm=T)) 
summary_dt



#############
# Durations #
#############

# Request
# 1. prima tabella: inizio fine e durata (n day) + aggiungere i paesi visitati


# Code to check which countries are traversed during Natal Dispersal Travel

countries_vis<- sttraj_df%>%
          dplyr::filter(ID == "CIV")

ggplot(CIV_eu_utm) + 
          geom_spatvector()+
          geom_path(data = countries_vis, aes(x = x, y = y, colour = "red"), linewidth = 1, lineend = "round")+
          facet_wrap(~stop_id)

countries_id <- data.frame(ID = c('A7', 'Antares', 'CAM', 'CBK', 'CIV', 'E7', 'H7', 'IAB', 'IAD', 'IBH', 'IBI', 'IBK', 'IBS', 'ICZ', 'IFP'),
                           countries = c('BIH, HRV, HUN, ITA, SVN', 'FRA, ITA', 'FRA, ITA', 'FRA, ITA', 'DZA, FRA, ITA, TUN', 'AUT, BIH, CHE, DEU, ESP, FRA, HUN, HRV, ITA', 'ESP, FRA, ITA', 'ITA', 'AUT, BEL, BIH, CHE, DEU, ESP, FRA, HUN, HRV, ITA, MNE', 'ITA', 'FRA, ITA', 'FRA, ITA', 'ALB, BOH, CHE, ESP, FRA, HVN, ITA, MNE, SVN', 'ITA', 'FRA, ITA'))


# Table reporting first and last day of Natal Dispersal period and whole durations of every animal

nd_duration_id <- ndtraj_df %>%
          group_by(ID, NDT)%>% 
          summarize(start = min(day), end = max(day)) %>%
          mutate(duration = round(difftime(end, start, units = "days")))%>%
          group_by(ID)%>%
          summarize(start = min(start), end = max(end), duration = sum(duration))%>%
          left_join(countries_id, by = "ID")%>%
          mutate(num_countries = str_count(countries, ",") + 1)%>%
          select(ID, start, end, duration, num_countries, countries)
nd_duration_id

# Table reporting mean, max duration and standard deviation of Natal Dispersal Travel duration of every animal

nd_duration <- ndtraj_df %>%
          group_by(ID, NDT) %>% 
          summarize(start = min(time), end = max(time)) %>%
          mutate(duration = difftime(end, start, units = "days"))%>%
          summarize(mean_dur = mean(duration),
                    max_dur = max(duration),
                    sd_dur = sd(duration))
nd_duration

# Table reporting NDT start/finish date and duration of every animal

ndt_duration <- ndtraj_df %>%
          group_by(NDT) %>% 
          mutate(start = min(day), end = max(day))%>%
          mutate(duration = difftime(end, start, units = "days"))%>%
          select("start", "end", "duration")%>%
          unique()
ndt_duration

# Table reporting every single track (track_id) start/finish date, duration and countries visited by every animal

track_duration <- ndtraj_df %>%
          filter(ID == "CIV")%>%
          group_by(track_id) %>% 
          mutate(start = min(day), end = max(day))%>%
          mutate(duration = difftime(end, start, units = "days"))%>%
          select("start", "end", "duration")%>%
          unique()
track_duration

departure_date <- ndtraj_df %>%
          group_by(ID, NDT) %>% 
          summarize(start = min(doy))%>%
          ggplot(aes(x = ID, y = start))+
          geom_point()+
          theme_light()
departure_date


#### Request

# 2. seconda tabella: Long distance event numero di eventi +
#                    durata media e massima in giorni x lde +
#                    distanza media e massima per ogni individ +
#                    velocità media e massima +
#                    aggiungi paesi visitati

# median_dist = sprintf("%.2f", median(distKM, na.rm = TRUE)), # add this line in the second summerize() funciton to have the median value

#####################
### Mean distance ###
#####################

NDT_dist_df <- ndtraj_df %>%
          group_by(ID, NDT) %>%
          summarise(NDT_dist = sum(distKM, na.rm = TRUE))
NDT_dist_df

mean_dist <- NDT_dist_df%>%
          group_by(ID)%>%
          summarize(mean_dist = mean(NDT_dist, na.rm = TRUE),
                    max_dist = max(NDT_dist, na.rm = TRUE),
                    sd_dist = sd(NDT_dist, na.rm = TRUE))
mean_dist


# Mean distance travelled by each osprey

NDT_dist <- ndtraj_df%>%
          group_by(NDT)%>%
          summarise(tot_dist = sum(distKM, na.rm = TRUE))%>%
          arrange(tot_dist)
NDT_dist

mean_dist <- NDT_dist%>%
          group_by(ID)%>%
          summarise(mean_dist = mean(tot_dist),
                    sd_dist = sd(tot_dist))
mean_dist

plot_mean_dist <- ggplot(mean_dist, aes(x = ID, y = mean_dist, ymin = mean_dist - sd_dist, ymax = mean_dist + sd_dist)) +
          geom_pointrange() +
          geom_text(aes(label = round(mean_dist, 2)), hjust = -0.15, size = 5) +
          labs(x = "", y = "Mean distance (Km)") +
          theme_minimal()+
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))
plot_mean_dist

# ggsave("C:/Tesi/images/plot_sex.jpg", plot = plot_sex)


###################
# Daily distances #
###################

# DF reporting daily distances travelled by each animals                     NDT, day%>%
          arrange(daily_dist)

daily_dist_df <- ndtraj_df %>%
          group_by(ID, day) %>%
          summarise(daily_dist = sum(distKM, na.rm = TRUE))
daily_dist_df

ndt_dist_df <- ndtraj_df %>%
          group_by(ID, NDT) %>%
          summarise(tot_dist = sum(distKM, na.rm = TRUE))%>%
          arrange(tot_dist)
ndt_dist_df

plot_ndt_dist <- ggplot(ndt_dist_df, aes(x = NDT, y = tot_dist)) +
          geom_col() +
          labs(x = "Natal Dispersal Trips", y = "Total distance (Km)") +
          theme_minimal()+
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18),
                axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
plot_ndt_dist

plot_daily_dist <- ggplot(daily_dist_df, aes(x = ID, y = daily_dist)) +
          geom_boxplot() +
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          labs(x = "", y = "Daily distnace (Km)")
plot_daily_dist

#  DF reporting mean\max and standard direction of daily distances travelled by each animals

# med_dist = median(daily_dist, na.rm = TRUE),

daily_dist_stat <- daily_dist_df%>%
          group_by (ID)%>%
          summarize(mean_dist = mean(daily_dist, na.rm = TRUE),
                    med_dist = median(daily_dist, na.rm = TRUE),
                    max_dist = max(daily_dist, na.rm = TRUE),
                    sd_dist = sd(daily_dist, na.rm = TRUE))
daily_dist_stat


#############################
### Natal Dispersal Trips ###
#############################

# Total number of Natal Dispersal Trips made by each animal

ndt_ev <- ndtraj_df%>%
          group_by(ID)%>%
          summarize(ndt_event = n_distinct(NDT))

ndt_stat <- left_join(ndt_ev, daily_dist_stat, by = "ID")

ndt_stat <- left_join(ndt_stat, nd_duration, by = "ID")

ndt_stat

# NDT departure month

month_departure <- ndtraj_df%>%
          group_by(ID, NDT)%>%
          summarize(start = min(time))%>%
          mutate(month = month(start))
month_departure

month_departure <- month_departure%>%
          group_by(month)
month_departure

month_dep <- month_departure%>%
          group_by(month)%>%
          count(ID)


plot_month_dep <- ggplot(month_dep, aes(x = month, y = n))+
          geom_col()+
          labs(x = "Months", y = "Frequency of departures")+
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          scale_x_continuous(breaks = seq(1, 12, by = 1))
plot_month_dep


# ggsave("C:/Tesi/images/plot_sex.jpg", plot = plot_sex)


################
### Sex-stat ###
################

id <- unique(ndtraj_df$ID)
ID <- c("A7", "Antares", "CAM", "CBK", "CIV", "E7", "H7", "IAB", "IAD", "IBH", "IBI", "IBK", "IBS", "ICZ", "IFP")
sex <- c("F", "M", "F", "U", "U", "F", "M", "M", "F", "M", "M", "M", "F", "M", "M")

sex_id <- data.frame(ID = ID, sex = sex)
sex_id

ndtraj_df <- left_join(ndtraj_df, sex_id, by = "ID")

# Distances stat by SEX

id_daily <- ndtraj_df %>%
          group_by(ID, sex, day) %>%
          summarise(dailyDist= sum(distKM, na.rm = TRUE))%>%
          filter(sex != "U")
id_daily

s_dist <- id_daily%>%
          group_by(sex)%>%
          summarize(meandD_day = mean(dailyDist),
                   sdD_day = sd(dailyDist),
                   medianD_day = median(dailyDist),
                   maxD_day = max(dailyDist),
                   tot_dDay = sum(dailyDist))
s_dist$sdD_day

## A tibble: 3 × 6
#  sex   meandD_day sdD_day medianD_day maxD_day tot_dDay
#  <chr>      <dbl>   <dbl>       <dbl>    <dbl>    <dbl>
#1 F           153.    92.8        139.     419.   45955.
#2 M           171.   134.         146.    1052.   24497.
#3 U           186.   136.         185.     557.    6148.

# boxplot of the daily distance travelled by each sex
plot_s_dist <- ggplot(id_daily, aes(x = sex, y = dailyDist)) +
          geom_boxplot() +
          stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = 0, size = 5, color = "black") +
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          labs(x = "", y = "Daily distance (Km/day)", title = "d)")
plot_s_dist


# total distance travelled by each sex #

s_totDist <-ndtraj_df%>%
          group_by(sex)%>%
          summarize(totDist = sum(distKM, na.rm = T))%>%
          filter(sex != "U")
s_totDist

plot_s_totdist <- ggplot(s_totDist, aes(x = sex, y = totDist)) +
          geom_col()+
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          labs(x = "", y = "Total distance (Km)", title = "b)")
plot_s_totdist


# Duration stat by SEX 

s_ndt_dur <- ndtraj_df %>%
          group_by(NDT, sex) %>%
          summarise(Start= min(day, na.rm = TRUE),
                    End = max(day, na.rm = TRUE),
                    Duration = difftime(End, Start, unit = "days"))%>%
          mutate(Duration = as.numeric(Duration))%>%
          filter(sex != "U")
s_ndt_dur

plot_s_dur <- ggplot(s_ndt_dur, aes(x = sex, y = Duration)) +
          geom_boxplot() +
          stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -0.4, size = 5, color = "black") +
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          labs(x = "", y = "Duration (Days)", title = "c)")
plot_s_dur

s_dur <- sex_ndt_dur%>%
          group_by(sex)%>%
          summarize(meandDur = mean(Duration),
                   sdDur = sd(Duration),
                   medianDur = median(Duration),
                   maxDur = max(Duration),
                   tot_Dur = sum(Duration))
s_dur

## A tibble: 3 × 6
#  sex   meandDur       sdDur medianDur maxDur  tot_Dur 
#  <chr> <drtn>         <dbl> <drtn>    <drtn>  <drtn>  
#1 F     14.384615 days 16.0  6 days    49 days 374 days
#2 M      7.043478 days  5.94 4 days    23 days 162 days          
#3 U      2.300000 days  3.65 1 days    12 days  23 days


# sex rate

sex_rate <- ndtraj_df%>%
          group_by(sex)%>%
          count(ID)
sex_rate

sex_rate <- sex_rate%>%
          group_by(sex)%>%
          filter(sex != "U")
sex_rate

sex_count <- sex_rate %>% 
  count(sex)

# Calculate the total number of IDs
total_IDs <- nrow(sex_rate)

# Calculate the sex rate
sex_count$rate <- sex_count$n / total_IDs

# View the sex rate
print(sex_count)

## A tibble: 3 × 3
## Groups:   sex [3]
#  sex       n  rate
#  <chr> <int> <dbl>
#1 F         5 0.333
#2 M         8 0.533
#3 U         2 0.133


plot_s_rate <- ggplot(sex_count, aes(x = sex, y = rate)) +
          geom_col()+
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          labs(x = "", y = "Sex rate", title = "a)")
plot_s_rate

plot_sex <- grid.arrange(ncol = 2, plot_s_rate, plot_s_totdist, plot_s_dur, plot_s_dist)
plot_sex

#############
### Speed ###
#############

speed_df <-  ndtraj_df %>%
          group_by(burst) %>%
          mutate(speed = distKM / (dt / 3600)) %>%
          summarise(mean_speed = mean(speed, na.rm = TRUE),
                    max_speed = max(speed, na.rm = TRUE)
          )

# Mean flight speed during NDTs

# Convert dt from seconds to hours
ndtraj_df_redis$dt_hours <- ndtraj_df_redis$dt / 3600  # 3600 seconds in an hour

# Calculate speed
ndtraj_df_redis$speed <- ndtraj_df_redis$distKM / ndtraj_df_redis$dt_hours  # Speed in Km/hour

ndtraj_speed <- ndtraj_df_redis%>%
          filter(speed > 5)

speed_ndtstat <- ndtraj_speed%>%
          group_by(NDT)%>%
          summarise(mean_speed = mean(speed, na.rm = TRUE),
                   max_speed = max(speed, na.rm = TRUE),
                   sd_speed = sd(speed, na.rm = TRUE))
print(speed_ndtstat, n = 100)

speed_idstat <- ndtraj_speed%>%
          group_by(ID)%>%
          summarise(mean_speed = mean(speed, na.rm = TRUE),
                   max_speed = max(speed, na.rm = TRUE),
                   sd_speed = sd(speed, na.rm = TRUE))
print(speed_idstat, n = 100)

mean_speed <- ndtraj_speed %>%
  group_by(ID) %>%
  summarise(mean_speed = mean(speed, na.rm = TRUE))


plot_mean_speed <- ggplot(ndtraj_speed, aes(x = ID, y = speed)) +
          geom_boxplot() +
          stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -0.4, size = 5, color = "black") +
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          labs(x = "", y = "Speed (Km/h)")
plot_mean_speed


################
### Stopover ###
################

# 3. terza tabella: descrizione aree di sosta (multi day) id +
#                    numero aree di sosta +
#                    numero medio di giorni di sosta per area +
#                    percentuale giorni di sosta in un’area protetta


summary_st <- sttraj_df%>%
          group_by(stop_id)%>%
          summarize(start = min(date),
                    end = max(date))
print(summary_st, n = 217)


summary_nd <- nd_df%>%
          group_by(track_id)%>%
          summarize(start = min(time),
                    end = max(time))
print(summary_nd, n = 150)

summary_nd <- summary_nd%>%
          group_by(id)%>%
          mutate(duration = difftime(end, start, units = "hours"))%>%
          summarize(min_dur = min(duration))

print(summary_nd, n = 150)

 
 stopover_df <- sttraj_df %>%
   filter(!grepl("wintering", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("nest", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("end", stop_id, ignore.case = TRUE))

stopover_df%>%
          group_by(stop_id)%>%
          arrange(time)%>%
          head()


dist_stopover <- stopover_df%>%
          group_by(ID, stop_id, day) %>%
          summarize(daily_dist = sum(distKM, na.rm = TRUE))%>%
          group_by(ID)%>%
          summarize(mean_daily_dist = mean(daily_dist, na.rm = TRUE),
                    sd_daily_dist = sd(daily_dist, na.rm = TRUE))
dist_stopover

plot_stopover_dist <- ggplot(dist_stopover, aes(x = ID, y = daily_dist)) +
          geom_boxplot() +
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          labs(x = "", y = "Daily distance (Km/day)")
plot_stopover_dist


dist_stopover_all <- dist_stopover%>%
          group_by(ID)%>%
          summarize(min = min(daily_dist, na.rm = TRUE),
                    mean = mean(daily_dist, na.rm = TRUE),
                    median = median(daily_dist, na.rm = TRUE),
                    sd = sd(daily_dist, na.rm = TRUE),
                    max = max(daily_dist, na.rm = TRUE))
dist_stopover_all

# write.csv(stopover_df, "C:/Tesi/R/osprey/data/stopover_df.csv")

 stopover_duration <- stopover_df %>%
           group_by(ID, stop_id) %>% 
           summarize(start = min(time), end = max(time)) %>%
           mutate(duration = difftime(end, start, units = "day"))%>%
           summarize(min_dur = min(duration),
                     mean_dur = mean(duration),
                     max_dur = max(duration),
                     tot_dur = sum(duration),
                     sd_dur = sd(duration))
stopover_duration

plot_stopover_duration <- ggplot(stopover_duration, aes(x = ID, y = duration)) +
          geom_boxplot() +
          theme_minimal() +
          theme(axis.text = element_text(size = 16),  
                axis.title = element_text(size = 18))+
          labs(x = "", y = "Duration (Days)")
plot_stopover_duration

 +
          stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -0.5, size = 5, color = "black")

stopover <- stopover_df%>%
          group_by(ID)%>%
          summarize(tot_stopover = n_distinct(stop_id))

stopover_stat <- left_join(stopover, stopover_duration, by = "ID")

stopover_stat

stopoverPA_df <- read.csv("C:/Tesi/R/osprey/data/stopover_PA_32632.csv")

stopoverPA_duration <- stopoverPA_df%>%
           group_by(stop_id) %>% 
           summarize(start = min(time), end = max(time)) %>%
           mutate(duration = difftime(end, start, units = "day"))%>%
           group_by(stop_id)%>%
           summarize(min_dur = min(duration),
                     mean_dur = mean(duration),
                     max_dur = max(duration),
                     tot_durPA = sum(duration),
                     sd_dur = sd(duration))%>%
          select("stop_id", "tot_durPA")
stopoverPA_duration

stopoverPA <- stopoverPA_df%>%
          group_by(ID)%>%
          summarize(tot_stopover = n_distinct(stop_id))

stopoverPA_stat <- left_join(stopoverPA, stopoverPA_duration, by = "ID")
stopoverPA_stat

stopover_stat_tot <- left_join(stopover_stat, stopoverPA_duration, by = "ID")

stopover_stat_tot <- stopover_stat_tot%>%
          mutate()%>%
          mutate(perc_PA = (as.numeric(tot_durPA)/as.numeric(tot_dur))*100)%>%
          select(-"tot_durPA")
stopover_stat_tot


#################
### WINTERING ###
#################


wintering_df <- sttraj_df %>%
   filter(!grepl("stop", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("nest", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("wintering1", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("end", stop_id, ignore.case = TRUE))

# DF reporting daily distances travelled by each animals

winter_daily_dist_df <- wintering_df %>%
          group_by(stop_id, day) %>%
          summarise(daily_dist = sum(distKM, na.rm = TRUE))
winter_daily_dist_df


#  DF reporting mean\max and standard direction of daily distances travelled by each animals


winter_daily_dist_stat <- winter_daily_dist_df%>%
          group_by (stop_id)%>%
          summarize(mean_dist = mean(daily_dist, na.rm = TRUE),
                    max_dist = max(daily_dist, na.rm = TRUE),
                    sd_dist = sd(daily_dist, na.rm = TRUE))
          winter_daily_dist_stat

winter_duration <- wintering_df %>%
          group_by(stop_id)%>%
          summarize(start = min(time),
                    end = max(time),
                    duartion = difftime(end, start, units = "days"),
                    )
winter_duration


wintering_stat <- left_join(winter_daily_dist_stat, winter_duration, by = "stop_id")

wintering_stat <- wintering_stat%>%
          select(-c("start", "end"))
wintering_stat


################
### Abstract ###
################

ndt_abstract <- left_join(ndt_duration, NDT_dist, by = "NDT")
ndt_abstract <- ndt_abstract%>%
          select(NDT, duration, tot_dist)%>%
          mutate(duration = as.numeric(duration))

ndt_abstract <- as.data.frame(ndt_abstract)


ndt_abs_stat <-ndt_abstract%>%
          summarize(mean_dur = mean(duration),
                    sd_dur = sd(duration),
                    mean_dist = mean(tot_dist),
                    sd_dist = sd(tot_dist))
ndt_abs_stat

#  mean_dur   sd_dur mean_dist  sd_dist
#  9.474576 12.16603  1298.308 1401.273

# Stopover
il numero di stopovers
loro durata media durante la fase di dispersal

stopover_df%>%
summarize(tot_stopover = n_distinct(stop_id))
# 91 stopover
# mean_dur = 16.52 days
# sd_dur = 32.4


########################
### tabelle appendix ###
########################


stop_df <- sttraj_df %>%
   filter(!grepl("nest", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("wintering1", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("end", stop_id, ignore.case = TRUE))

tab_traj <- ndtraj_df%>%
#          filter(ID == "")%>%
          group_by(ID, NDT, track_id)%>%
          summarize(start = min(day),
                    end = max(day),
                    duration = difftime(end, start, units = "days"),
                    tot_dist = sum(distKM, na.rm = T))%>%
          mutate(stop_id = NA,
                 tot_durPA = NA,
                 Countries_visited = NA)
tab_traj

tab_stop <- stop_df%>%
#          filter(ID == "")%>%
          group_by(ID, stop_id)%>%
          summarize(start = min(day),
                    end = max(day),
                    duration = difftime(end, start, units = "days"))
tab_stop

tab_stopPA <- left_join(tab_stop, stopoverPA_duration, by = "stop_id")
tab_stopPA

tab_appendix <- bind_rows(tab_traj, tab_stopPA)
tab_appendix

tab_appendix <- tab_appendix%>%
          arrange(ID, start)%>%
          rename("Track_id" = "track_id",
                 "Stop_id" = "stop_id",
                 "Start" = "start",
                 "End" = "end",
                 "Duration" = "duration",
                 "Total_duration_PA" = "tot_durPA",
                 "Total_distance" = "tot_dist")%>%
          select(ID, NDT, Track_id, Stop_id, Start, End, Duration, Total_duration_PA, Total_distance, Countries_visited)
tab_appendix


############################
# 4. Main directions graph #
############################

### TRAVEL DF DIRECTIONS

ndtraj_df_redis <- ndtraj_df_redis%>% 
          mutate(abs.angle_deg = abs.angle * (180 / pi))

nd_not_na_indices <- !is.na(ndtraj_df_redis$abs.angle_deg)
ndtraj_df_redis$abs.angle_deg[nd_not_na_indices] <- ndtraj_df_redis$abs.angle_deg[nd_not_na_indices] %% 360

north_ndtraj <- ndtraj_df_redis%>%
          mutate(abs.angle_deg = if_else(abs.angle_deg >= 337.5, abs.angle_deg - 360, abs.angle_deg))

# Create a function to map angles to directions
angle_to_direction <- function(angle_degrees) {
  if (is.na(angle_degrees)) return("Unknown")  # Handle missing values
  if (angle_degrees >= 337.5 || angle_degrees < 22.5) return("N")
  if (angle_degrees >= 22.5 && angle_degrees < 67.5) return("NE")
  if (angle_degrees >= 67.5 && angle_degrees < 112.5) return("E")
  if (angle_degrees >= 112.5 && angle_degrees < 157.5) return("SE")
  if (angle_degrees >= 157.5 && angle_degrees < 202.5) return("S")
  if (angle_degrees >= 202.5 && angle_degrees < 247.5) return("SW")
  if (angle_degrees >= 247.5 && angle_degrees < 292.5) return("W")
  if (angle_degrees >= 292.5 && angle_degrees < 337.5) return("NW")
}

# Apply the function to create the direction variable
ndtraj_df_redis <- ndtraj_df_redis %>% 
          mutate(direction = sapply(abs.angle_deg, angle_to_direction))

ndtraj_df_redis$direction <- as.factor(ndtraj_df_redis$direction)

track_dir <- ndtraj_df_redis%>%
          group_by(ID, track_id)%>%
          summarize(mean_angle = mean(abs.angle_deg))%>% 
          mutate(direction = sapply(mean_angle, angle_to_direction))
track_dir

### STATIONARY DF DIRECTIONS

sttraj <- sttraj_df %>% 
          mutate(abs.angle_deg = abs.angle * (180 / pi))

st_not_na_indices <- !is.na(sttraj$abs.angle_deg)
sttraj$abs.angle_deg[st_not_na_indices] <- sttraj$abs.angle_deg[st_not_na_indices] %% 360

north_sttraj <- sttraj %>%
  mutate(abs.angle_deg = if_else(abs.angle_deg >= 337.5, abs.angle_deg - 360, abs.angle_deg))

# Apply the function to create the direction variable
sttraj <- sttraj %>% 
          mutate(direction = sapply(abs.angle_deg, angle_to_direction))

sttraj$direction <- as.factor(sttraj$direction)


st_dir <- sttraj%>%
          group_by(ID, stop_id)%>%
          summarize(mean_angle = mean(abs.angle_deg))%>% 
          mutate(direction = sapply(mean_angle, angle_to_direction))
st_dir

### ROSE DIAGRAM OF DIRECTIONS

id_nd <- north_ndtraj%>%
          filter(ID == "A7")

breaks <- seq(-22.5, 337.4, by = 45)
breaks

midpoints <- c(0, 45, 90, 135, 180, 225, 270, 315)

p_nd <- ggplot(north_ndtraj, aes(x = abs.angle_deg)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 45, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(x = "", y = "")+
  coord_polar(theta = "x", start = -0.3926991, direction = 1)+
  scale_x_continuous(breaks = midpoints, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
labs(title = "Directions during Natal Dispersal Trips")
p_nd

id_st <- north_sttraj%>%
          filter(ID == "A7")

breaks <- seq(-22.5, 337.4, by = 45)
breaks

midpoints <- c(0, 45, 90, 135, 180, 225, 270, 315)

p_st <- ggplot(north_sttraj, aes(x = abs.angle_deg)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 45, fill = "skyblue", color = "black") +
  theme_minimal() +
  coord_polar(theta = "x", start = -0.3926991, direction = 1)+
  scale_x_continuous(breaks = midpoints, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")) +
labs(title = "Directions during Stopover")
p_st

p_st_nd <- grid.arrange(ncol = 2, p_nd, p_st)

ggsave()

