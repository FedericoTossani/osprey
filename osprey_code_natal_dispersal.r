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
                                     year = year(date))%>%
                              tidyr::unite(burst, c(burst, year, doy), sep="_", remove = F)%>%
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
         
         # First let's create a ltraj object with UTM coordinates 
         
                  osprey_ndlt <- as.ltraj(osprey_nd[, c("x", "y")],
                                        date = osprey_nd$time, 
                                        id = osprey_nd$ID,
                                        typeII=TRUE)
         
         # let's create a function that split bursts with multiple years         
                 
                  foo <- function(dt){ 
                           return(dt>(100*3600*24))
                                   }
                  
                  osprey_ndlt2 <- cutltraj(osprey_ndlt,"foo(dt)",nextr=TRUE)
         
                  a7lt <- ld(osprey_ndlt2[[1]])
         
                  plotltr(a7lt)
         
                  plotltr(osprey_ndlt2, "dist")
         
                  osprey_ndlt_df <- ld(osprey_ndlt2)

                  osprey_ndlt_df <- osprey_ndlt_df%>%
                                    mutate(day = as.Date(date),
                                           distKM = dist/1000)
         

# SUMMARY dfs

         summary_distance <- osprey_ndlt_df%>%
                                    group_by(burst, day)%>%
                                    summarize(minDistxDay = min(distKM),
                                              meanDistxDay = mean(distKM),
                                              maxDistxDay = max(distKM))

         summary_id_distance <- summary_distance%>%
                                    group_by(burst)%>%
                                    summarize(minDist = min(minDistxDay, na.rm=T),
                                              meanDist = mean(meanDistxDay, na.rm=T),
                                              maxDist = max(maxDistxDay, na.rm=T))


dist_plot <- ggplot(osprey_ndlt_df, aes(x = osprey_ndlt_df$distKM)) +
  geom_histogram(binwidth = 0.1) +
  xlab("Distance (Km)") +
  ylab("Count") +
  ggtitle("Distribution of Distance")



         # export this table to tex
         
                  summary_id_distance %>%
                      kable(format = 'latex', booktabs = TRUE, digits = c(1, 2, 2, 2)) 


nd_duration <- osprey_ndlt_df %>%
  group_by(burst) %>% 
  summarize(start = min(date), end = max(date)) %>%
  mutate(duration = round(difftime(end, start)))

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
         
