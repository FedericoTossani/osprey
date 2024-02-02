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
          dplyr::filter(ID == "A7")

ggplot(osprey_eu_utm) + 
          geom_spatvector()+
          geom_path(data = countries_vis, aes(x = x, y = y, colour = "red"), linewidth = 1, lineend = "round")




countries_id <- data.frame(ID = c('A7', 'Antares', 'CAM', 'CBK', 'CIV', 'E7', 'H7', 'IAB', 'IAD', 'IBH', 'IBI', 'IBK', 'IBS', 'ICZ', 'IFP'),
                           countries = c('BIH, HRV, HUN, ITA, SVN', 'FRA, ITA', 'FRA, ITA', 'FRA, ITA', 'DZA, FRA, ITA, TUN', 'AUT, BIH, CHE, DEU, ESP, FRA, HUN, HRV, ITA', 'ESP, FRA, ITA', 'ITA', 'AUT, BEL, BIH, CHE, DEU, ESP, FRA, HUN, HRV, ITA, MNE', 'ITA', 'FRA, ITA', 'FRA, ITA', 'ALB, BOH, CHE, ESP, FRA, HVN, ITA, MNE, SVN', 'ITA', 'FRA, ITA'))


# Table reporting first and last day of Natal Dispersal period and whole durations of every animal

nd_duration_id <- ndtraj_df %>%
          group_by(ID, NDT) %>% 
          summarize(start = min(day), end = max(day)) %>%
          mutate(duration = round(difftime(end, start, units = "days")))%>%
          group_by(ID)%>%
          summarize(start = min(start), end = max(end), duration = sum(duration))%>%
          left_join(countries_id, by = "ID")

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
          filter(ID == "IFP")%>%
          group_by(track_id) %>% 
          mutate(start = min(day), end = max(day))%>%
          mutate(duration = difftime(end, start, units = "days"))%>%
          select("start", "end", "duration")%>%
          unique()
track_duration


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
          group_by(ID, track_id, day) %>%
          summarise(daily_dist = sum(distKM, na.rm = TRUE))
daily_dist_df


#  DF reporting mean\max and standard direction of daily distances travelled by each animals

                    med_dist = median(daily_dist, na.rm = TRUE),

daily_dist_stat <- daily_dist_df%>%
          group_by (ID)%>%
          summarize(mean_dist = mean(daily_dist, na.rm = TRUE),
                    max_dist = max(daily_dist, na.rm = TRUE),
                    sd_dist = sd(daily_dist, na.rm = TRUE))
daily_dist_stat


# Total number of Natal Dispersal Travel made by each animal

ndt_ev <- ndtraj_df%>%
          group_by(ID)%>%
          summarize(ndt_event = n_distinct(NDT))

ndt_stat <- left_join(ndt_ev, daily_dist_stat, by = "ID")

ndt_stat <- left_join(ndt_stat, nd_duration, by = "ID")

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

 
 dist_control <- st_df%>%
           select("date", "burst", "distKM")%>%
           arrange(desc(distKM))
 
 
 
 stopover_df <- st_df %>%
   filter(!grepl("wintering", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("nest", stop_id, ignore.case = TRUE))%>%
   filter(!grepl("end", stop_id, ignore.case = TRUE))


write.csv("C:/Tesi/R/osprey/data/stopover_df.csv")

 stopover_duration <- stopover_df %>%
           group_by(ID, stop_id) %>% 
           summarize(start = min(date), end = max(date)) %>%
           mutate(duration = difftime(end, start, units = "day"))%>%
           summarize(min_dur = min(duration),
                     mean_dur = mean(duration),
                     max_dur = max(duration),
                     tot_dur = sum(duration),
                     sd_dur = sd(duration))


stopover <- stopover_df%>%
          group_by(ID)%>%
          summarize(tot_stopover = n_distinct(stop_id))

stopover_stat <- left_join(stopover, stopover_duration, by = "ID")

stopover_stat

stopoverPA_df <- read.csv("C:/Tesi/R/osprey/data/stopover_PA_32632.csv")

stopoverPA_duration <- stopoverPA_df%>%
           group_by(ID, stop_id) %>% 
           summarize(start = min(date), end = max(date)) %>%
           mutate(duration = difftime(end, start, units = "day"))%>%
           summarize(min_dur = min(duration),
                     mean_dur = mean(duration),
                     max_dur = max(duration),
                     tot_durPA = sum(duration),
                     sd_dur = sd(duration))%>%
          select("ID", "tot_durPA")


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



############################
# 4. Main directions graph #
############################


#There are a few ways to do this. There is a "zero" argument for rose.diag in this package.

y <- scan() # paste in the values from the question and hit return twice
y <- circlar(y) # not necessary but prevents a warning
rose.diag(y, units = 'degrees', zero = pi/2) # units doesn't change the underlying units

#Alternatively you could have set properties of the circular object that you created.


%>% 
          mutate(abs.angle_deg = rel.angle * (180 / pi))


id <- ndtraj%>%
          filter(track_id == "A7_nd4e")

ndtraj_df <- ndtraj_df%>% 
          mutate(abs.angle_deg = abs.angle * (180 / pi))

y <- id$abs.angle_deg
y <- circular(y)
rose.diag(y, bins =8, zero = pi/2, units = 'degrees')


### TRAVEL DF DIRECTIONS

ndtraj <- ndtraj_df %>% 
          mutate(abs.angle_deg = abs.angle * (180 / pi))

nd_not_na_indices <- !is.na(ndtraj$abs.angle_deg)
ndtraj$abs.angle_deg[nd_not_na_indices] <- ndtraj$abs.angle_deg[nd_not_na_indices] %% 360

north_ndtraj <- ndtraj %>%
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
ndtraj <- ndtraj %>% 
          mutate(direction = sapply(abs.angle_deg, angle_to_direction))

ndtraj$direction <- as.factor(ndtraj$direction)

track_dir <- ndtraj%>%
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
  coord_polar(theta = "x", start = -0.3926991, direction = 1)+
  scale_x_continuous(breaks = midpoints, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))
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
  scale_x_continuous(breaks = midpoints, labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))
p_st

p_st_nd <- grid.arrange(ncol = 2, p_nd, p_st)

ggsave()


GRAFICI 
# 1. primo plot: mappe gi`a fatte con evidenziati i lde intervellati dalle aree di sosta

# 2. secondo plot: grafico unico con tutti gli individui 



ndtraj_df$rel.angle_rad <- ndtraj_df$rel.angle * (pi/180)

A7 <- ndtraj_df%>%
          filter(ID == "A7")

ggplot(A7, aes(x = rel.angle_rad)) +
  geom_bar(stat = "count") +
  coord_polar(start = 0) +
  labs(title = "Circular Plot of Main Direction by NDT", x = "Main Direction (Radians)", y = "Count") +
  theme_minimal()
















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


