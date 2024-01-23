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
          year = year(date),
          burst = dplyr::case_when(
                    id == 'A7' & date >= '2015-08-14 08:00:00' & date <= '2015-08-18 09:00:00' ~ "A7_nd1",
                    id == 'A7' & date >= '2017-02-20 06:00:00' & date <= '2017-03-01 12:00:00' ~ "A7_nd2",
                    id == 'A7' & date >= '2017-03-17 06:00:00' & date <= '2017-04-14 18:00:00' ~ "A7_nd3a",
                    id == 'A7' & date >= '2017-04-17 06:00:00' & date <= '2017-04-20 18:00:00' ~ "A7_nd3b",
                    id == 'A7' & date >= '2017-04-28 06:00:00' & date <= '2017-05-10 12:00:00' ~ "A7_nd4a",
                    id == 'A7' & date >= '2017-05-14 08:00:00' & date <= '2017-05-21 12:00:00' ~ "A7_nd4b",
                    id == 'A7' & date >= '2017-05-23 06:00:00' & date <= '2017-05-23 14:00:00' ~ "A7_nd4c",
                    id == 'A7' & date >= '2017-05-26 08:00:00' & date <= '2017-05-28 18:00:00' ~ "A7_nd4d",
                    id == 'A7' & date >= '2017-06-06 08:00:00' & date <= '2017-06-06 12:00:00' ~ "A7_nd5a",
                    id == 'A7' & date >= '2017-06-11 06:00:00' & date <= '2017-06-12 12:00:00' ~ "A7_nd5b",
                    id == 'A7' & date >= '2017-07-27 06:00:00' & date <= '2017-07-27 18:00:00' ~ "A7_nd6",
                    id == 'Antares' & date >= "2015-08-15 11:00:00" & date <= "2015-08-18 16:30:00" ~ "Antares_nd1",
                    id == "Antares" & date >= "2015-09-08 09:30:00" & date <= "2015-09-08 16:30:00" ~ "Antares_nd2a",
                    id == "Antares" & date >= "2015-09-13 08:00:00" & date <= "2015-09-13 12:00:00" ~ "Antares_nd2b",
                    id == "Antares" & date >= "2016-04-04 00:00:00" & date <= "2016-04-05 15:00:00" ~ "Antares_nd3a",
                    id == "Antares" & date >= "2016-04-09 08:00:00" & date <= "2016-04-10 14:00:00" ~ "Antares_nd3b",
                    id == "Antares" & date >= "2016-04-19 10:00:00" & date <= "2016-04-22 10:00:00" ~ "Antares_nd4",
                    id == "Antares" & date >= "2016-05-05 06:00:00" & date <= "2016-05-06 06:00:00" ~ "Antares_nd5",
                    id == "Antares" & date >= "2016-05-14 12:00:00" & date <= "2016-05-17 15:00:00" ~ "Antares_nd6",
                    id == "Antares" & date >= "2016-06-10 07:00:00" & date <= "2016-06-12 15:00:00" ~ "Antares_nd7",
                    id == 'CAM' & date >= "2016-04-14 07:00:00" & date <= "2016-04-15 11:00:00" ~ "CAM_nd1a",
                    id == 'CAM' & date >= "2016-04-19 10:00:00" & date <= "2016-04-19 17:00:00" ~ "CAM_nd1b",
                    id == "CAM" & date >= "2016-05-03 07:00:00" & date <= "2016-05-06 15:00:00" ~ "CAM_nd2",
                    id == "CAM" & date >= "2016-05-20 04:00:00" & date <= "2016-05-24 14:00:00" ~ "CAM_nd3",
                    id == "CAM" & date >= "2016-07-02 23:00:00" & date <= "2016-07-06 18:00:00" ~ "CAM_nd4",
                    id == 'CBK' & date >= '2013-08-15 06:00:00' & date <= '2013-08-21 08:00:00' ~ "CBK_nd1",
                    id == 'CBK' & date >= '2014-03-20 08:00:00' & date <= '2014-03-21 10:00:00' ~ "CBK_nd2",
                    id == 'CBK' & date >= '2014-04-08 07:30:00' & date <= '2014-04-12 08:00:00' ~ "CBK_nd3",
                    id == 'CIV' & date >= "2014-08-16 09:00:00" & date <= "2014-08-21 18:00:00" ~ "CIV_nd1",
                    id == 'CIV' & date >= "2014-09-11 11:00:00" & date <= "2014-09-13 13:00:00" ~ "CIV_nd2",
                    id == 'CIV' & date >= "2014-10-21 08:00:00" & date <= "2014-10-21 18:00:00" ~ "CIV_nd3",
                    id == 'CIV' & date >= "2015-03-21 02:00:00" & date <= "2015-03-21 20:00:00" ~ "CIV_nd4",
                    id == 'CIV' & date >= "2015-06-04 06:00:00" & date <= "2015-06-16 12:00:00" ~ "CIV_nd5a",
                    id == 'CIV' & date >= "2015-06-20 00:00:00" & date <= "2015-06-16 12:00:00" ~ "CIV_nd5b",
                    id == 'CIV' & date >= "2015-08-13 06:00:00" & date <= "2015-08-13 20:00:00" ~ "CIV_nd6",
                    id == 'CIV' & date >= "2015-11-22 00:01:00" & date <= "2015-11-25 16:01:00" ~ "CIV_nd7",
                    id == 'CIV' & date >= '2016-03-29 06:00:00' & date <= '2016-03-31 18:00:00' ~ "CIV_nd8",
                    id == 'CIV' & date >= '2016-04-15 06:00:00' & date <= '2016-04-18 20:00:00' ~ "CIV_nd9",
                    id == 'CIV' & date >= '2016-10-28 08:00:00' & date <= '2016-10-29 18:00:00' ~ "CIV_nd10",
                    id == 'E7' & date >= '2014-08-21 08:00:00' & date <= '2014-08-27 11:00:00' ~ "E7_nd1",
                    id == 'E7' & date >= '2016-03-10 06:00:00' & date <= '2016-03-15 15:30:00' ~ "E7_nd2a",
                    id == 'E7' & date >= '2016-03-17 10:30:00' & date <= '2016-03-22 13:30:00' ~ "E7_nd2b",
                    id == 'E7' & date >= '2016-03-24 07:30:00' & date <= '2016-03-24 12:30:00' ~ "E7_nd2c",
                    id == 'E7' & date >= '2016-03-30 07:30:00' & date <= '2016-04-10 10:00:00' ~ "E7_nd2d",
                    id == 'E7' & date >= '2016-04-12 10:30:00' & date <= '2016-04-17 09:30:00' ~ "E7_nd2e",
                    id == 'E7' & date >= '2016-04-19 09:00:00' & date <= '2016-04-22 16:30:00' ~ "E7_nd2e",
                    id == 'E7' & date >= '2016-04-24 11:30:00' & date <= '2016-04-25 15:00:00' ~ "E7_nd2f",
                    id == 'E7' & date >= '2016-05-04 08:30:00' & date <= '2016-05-07 12:30:00' ~ "E7_nd3a",
                    id == 'E7' & date >= '2016-05-12 10:00:00' & date <= '2016-05-16 17:30:00' ~ "E7_nd3b",
                    id == 'E7' & date >= '2016-05-20 10:00:00' & date <= '2016-05-21 16:00:00' ~ "E7_nd3c",
                    id == 'E7' & date >= '2016-05-23 07:30:00' & date <= '2016-05-27 16:30:00' ~ "E7_nd3d",
                    id == 'H7' & date >= '2013-08-04 08:00:00' & date <= '2013-08-09 12:30:00' ~ "H7_nd1",
                    id == 'H7' & date >= '2015-04-02 08:00:00' & date <= '2015-04-06 11:00:00' ~ "H7_nd2a",
                    id == 'H7' & date >= '2015-04-07 17:00:00' & date <= '2015-04-11 10:00:00' ~ "H7_nd2b",
                    id == 'H7' & date >= '2015-04-20 11:00:00' & date <= '2015-04-30 13:30:00' ~ "H7_nd3a",
                    id == 'H7' & date >= '2015-05-03 05:30:00' & date <= '2015-05-03 09:00:00' ~ "H7_nd3b",
                    id == 'IAB' & date >= '2018-08-07 11:00:00' & date <= '2018-08-12 16:00:00' ~ "IAB_nd1",
                    id == 'IAB' & date >= '2019-03-26 11:00:00' & date <= '2019-03-28 11:00:00' ~ "IAB_nd2",
                    id == 'IAB' & date >= '2019-04-19 08:00:00' & date <= '2019-04-19 16:00:00' ~ "IAB_nd3a",
                    id == 'IAB' & date >= '2019-04-21 08:00:00' & date <= '2019-04-22 07:00:00' ~ "IAB_nd3b",
                    id == 'IAB' & date >= '2019-04-23 06:00:00' & date <= '2019-04-23 15:00:00' ~ "IAB_nd3c",
                    id == 'IAB' & date >= '2019-05-05 07:00:00' & date <= '2019-05-12 11:00:00' ~ "IAB_nd4a",
                    id == 'IAB' & date >= '2019-05-16 06:00:00' & date <= '2019-05-20 10:00:00' ~ "IAB_nd4",
                    id == 'IAB' & date >= '2019-05-29 14:00:00' & date <= '2019-06-02 12:00:00' ~ "IAB_nd5a",
                    id == 'IAB' & date >= '2019-06-08 08:00:00' & date <= '2019-06-10 13:00:00' ~ "IAB_nd5b",
                    id == 'IAB' & date >= '2019-09-07 10:00:00' & date <= '2019-09-08 12:00:00' ~ "IAB_nd6a",
                    id == 'IAB' & date >= '2019-09-10 09:00:00' & date <= '2019-09-11 11:00:00' ~ "IAB_nd6b",
                    id == 'IAB' & date >= '2019-10-29 08:00:00' & date <= '2019-10-30 07:00:00' ~ "IAB_nd7",
                    id == 'IAB' & date >= '2020-03-16 08:00:00' & date <= '2020-03-19 16:00:00' ~ "IAB_nd8",
                    id == 'IAD' & date >= '2016-08-20 08:00:00' & date <= '2016-08-22 14:30:00' ~ "IAD_nd1",
                    id == 'IAD' & date >= '2018-02-05 06:00:00' & date <= '2018-02-07 16:00:00' ~ "IAD_nd2",
                    id == 'IAD' & date >= '2018-03-28 10:00:00' & date <= '2018-04-14 18:00:00' ~ "IAD_nd3a",
                    id == 'IAD' & date >= '2018-04-16 06:00:00' & date <= '2018-04-16 14:00:00' ~ "IAD_nd3b",
                    id == 'IAD' & date >= '2018-04-18 12:00:00' & date <= '2018-04-25 18:00:00' ~ "IAD_nd3c",
                    id == 'IAD' & date >= '2018-04-27 06:00:00' & date <= '2018-05-05 12:00:00' ~ "IAD_nd3d",
                    id == 'IAD' & date >= '2018-05-07 12:00:00' & date <= '2018-05-07 16:00:00' ~ "IAD_nd3e",
                    id == 'IAD' & date >= '2018-05-09 08:00:00' & date <= '2018-05-09 12:00:00' ~ "IAD_nd3f",
                    id == 'IAD' & date >= '2018-05-14 10:00:00' & date <= '2018-05-17 12:00:00' ~ "IAD_nd3g",
                    id == 'IAD' & date >= '2018-05-29 08:00:00' & date <= '2018-06-03 10:00:00' ~ "IAD_nd4",
                    id == 'IAD' & date >= '2018-06-09 08:00:00' & date <= '2018-06-12 12:00:00' ~ "IAD_nd4b",
                    id == 'IAD' & date >= '2018-12-15 09:00:00' & date <= '2018-12-18 14:00:00' ~ "IAD_nd5a",
                    id == 'IAD' & date >= '2018-12-20 11:00:00' & date <= '2018-12-20 13:00:00' ~ "IAD_nd5b",
                    id == 'IAD' & date >= '2019-03-04 10:00:00' & date <= '2019-03-05 13:00:00' ~ "IAD_nd6a",
                    id == 'IAD' & date >= '2019-03-07 06:00:00' & date <= '2019-03-13 12:00:00' ~ "IAD_nd6b",
                    id == 'IAD' & date >= '2019-03-15 08:00:00' & date <= '2019-03-15 14:00:00' ~ "IAD_nd6c",
                    id == 'IAD' & date >= '2019-03-21 10:00:00' & date <= '2019-04-01 15:00:00' ~ "IAD_nd6d",
                    id == 'IAD' & date >= '2019-04-05 11:00:00' & date <= '2019-04-09 14:00:00' ~ "IAD_nd6e",
                    id == 'IAD' & date >= '2019-04-15 06:00:00' & date <= '2019-04-18 16:00:00' ~ "IAD_nd6f",
                    id == 'IAD' & date >= '2019-04-21 06:00:00' & date <= '2019-04-21 17:00:00' ~ "IAD_nd6g",
                    id == 'IAD' & date >= '2019-04-29 08:00:00' & date <= '2019-05-01 14:00:00' ~ "IAD_nd7",
                    id == 'IAD' & date >= '2019-07-08 09:00:00' & date <= '2019-07-08 17:00:00' ~ "IAD_nd8a",
                    id == 'IAD' & date >= '2019-07-11 08:00:00' & date <= '2019-07-15 14:00:00' ~ "IAD_nd8b",
                    id == 'IBH' & date >= '2020-08-07 06:17:15' & date <= '2020-08-07 17:17:15' ~ "IBH_nd1a",
                    id == 'IBH' & date >= '2020-08-09 07:19:13' & date <= '2020-08-13 13:14:35' ~ "IBH_nd1b",
                    id == 'IBH' & date >= '2020-08-15 08:25:49' & date <= '2020-08-15 11:25:28' ~ "IBH_nd1c",
                    id == 'IBH' & date >= '2022-04-09 06:47:05' & date <= '2022-04-11 15:37:18' ~ "IBH_nd2",
                    id == 'IBH' & date >= '2022-04-20 06:31:21' & date <= '2022-04-21 04:27:29' ~ "IBH_nd3a",
                    id == 'IBH' & date >= '2022-04-22 11:24:14' & date <= '2022-04-23 06:24:43' ~ "IBH_nd3b",
                    id == 'IBH' & date >= '2022-04-29 09:18:06' & date <= '2022-05-01 14:15:15' ~ "IBH_nd4",
                    id == 'IBH' & date >= '2022-05-04 09:00:00' & date <= '2022-05-04 12:12:25' ~ "IBH_nd5",
                    id == 'IBH' & date >= '2022-05-10 09:00:00' & date <= '2022-05-13 08:58:13' ~ "IBH_nd6",
                    id == 'IBI' & date >= "2017-07-21 07:00:00" & date <= "2017-07-25 14:30:00" ~ "IBI_nd1a",
                    id == 'IBI' & date >= "2017-07-27 12:00:00" & date <= "2017-07-27 17:00:00" ~ "IBI_nd1b",
                    id == "IBI" & date >= "2017-08-19 07:00:00" & date <= "2017-08-20 12:00:00" ~ "IBI_nd2",
                    id == 'IBK' & date >= '2020-08-21 08:31:15' & date <= '2020-08-22 18:04:45' ~ "IBK_nd1",
                    id == 'IBK' & date >= '2021-06-27 08:43:09' & date <= '2021-06-30 12:45:35' ~ "IBK_nd2",
                    id == 'IBK' & date >= '2022-04-16 08:39:12' & date <= '2022-04-17 12:50:05' ~ "IBK_nd3",
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
                    id == 'IFP' & date >= '2022-07-23 06:00:00' & date <= '2022-08-15 00:00:00' ~ "IFP_nd1",
                    id == 'IFP' & date >= '2023-04-24 00:00:00' & date <= '2023-04-30 04:00:00' ~ "IFP_nd2",
                    id == "IFP" & date >= '2023-05-16 00:00:00' & date <= '2023-06-08 20:00:00' ~ "IFP_nd3",
                    id == 'ICZ' & date >= '2019-09-09 08:00:00' & date <= '2019-09-14 14:30:00' ~ "ICZ_nd1",
                    id == 'ICZ' & date >= '2020-04-11 10:00:00' & date <= '2020-04-24 00:00:00' ~ "ICZ_nd2",
                    id == 'ICZ' & date >= '2020-05-02 14:30:00' & date <= '2020-05-05 17:00:00' ~ "ICZ_nd3",),
          day = as.Date(date),
          distKM = dist/1000)%>%
          tidyr::unite(id_y, c(id, year), sep="_", remove = F)%>%
          select(-c("pkey"))

nd_df$rel.angle.degrees <- nd_df$rel.angle* (180 / pi)

nd_lt2 <- nd_df%>%
          dl()


# Create a new factor variable based on the condition
nd_lt2$split_factor <- ifelse(nd_lt2$rel.angle.degrees > 90 | nd_lt2$rel.angle.degrees < -90, "split", "no_split")

table(nd_lt2$split_factor)

# Split the ltraj object based on the new factor
split_ltraj <- split(nd_lt2, f = nd_lt2$split_factor)

# If you want each split to be a separate ltraj object, you can use list2df
split_ltraj_list <- list2df(split_ltraj)




foo <- function(rel.angle.degrees) {
return(rel.angle.degrees>90)
}

nd_ltc <- cutltraj(nd_lt2, "foo(rel.angle.degrees)", nextr = TRUE)
nd_ltc

nd_df2 <- nd_ltc%>%
          ld()

write.csv(nd_df2, "nd_df2.csv")

nd$time <- as.POSIXct(strptime(as.character(nd$time),"%Y-%m-%d %H:%M"))

# Find duplicate dates
duplicate_dates <- osprey_nd[duplicated(osprey_nd$time) | duplicated(osprey_nd$time, fromLast = TRUE), ]

duplicate_dates <- duplicate_dates%>%
          arrange(time)

# Print the duplicate dates
print(head(duplicate_dates, 40))

# daily track (= successive telemetry locations in each day)



main_direction <- nd_df %>%
  group_by(burst) %>%
  summarize(main_angle = atan2(mean(sin(abs.angle)), mean(cos(abs.angle))))

# Print the result
print(main_direction)

dailyDirections <- nd_df%>%
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



#########################
#  Natal dispersal stat #
#########################

# export tables to Latex, pay attention to digits arguments          

stopover_stat_tot %>%
  kable(format = 'latex', booktabs = TRUE, digits = c(0, 1, 2, 2, 2, 2, 2, 2)) 

# 1. prima tabella: inizio fine e durata (n day) + aggiungere i paesi visitati

# Durations #
# median_dur = sprintf("%.2f", median(as.numeric(duration))), # add this line in the second summerize() funciton to have the median value

nd_duration <- nd_df %>%
          group_by(id, burst) %>% 
          summarize(start = min(day), end = max(day)) %>%
          mutate(duration = difftime(end, start, units = "days"))%>%
          summarize(mean_dur = mean(duration),
                    max_dur = max(duration),
                    sd_dur = sd(duration))
                   


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

# median_dist = sprintf("%.2f", median(distKM, na.rm = TRUE)), # add this line in the second summerize() funciton to have the median value

# Daily distances

daily_dist_df <- nd_df %>%
  group_by(id, burst, day) %>%
  summarise(daily_dist = sum(distKM, na.rm = TRUE))

daily_dist_stat <- daily_dist_df%>%
          group_by (id)%>%
          summarize(mean_dist = mean(daily_dist, na.rm = TRUE),
                    max_dist = max(daily_dist, na.rm = TRUE),
                    sd_dist = sd(daily_dist, na.rm = TRUE))

lde_ev <- nd_df%>%
          group_by(id)%>%
          summarize(lde_event = n_distinct(burst))

lde_stat <- left_join(lde_ev, daily_dist_stat, by = "id")

lde_stat <- left_join(lde_stat, nd_duration, by = "id")

lde_stat

speed_df <-  nd_df %>%
          group_by(burst) %>%
          mutate(speed = distKM / (dt / 3600)) %>%
          summarise(mean_speed = mean(speed, na.rm = TRUE),
                    max_speed = max(speed, na.rm = TRUE)
          )


# 3. terza tabella: descrizione aree di sosta (multi day) id +
#                    numero aree di sosta +
#                    numero medio di giorni di sosta per area +
#                    percentuale giorni di sosta in un’area protetta

osprey_nonb_no_duplicates <- osprey_nonb[!duplicated(osprey_nonb[c("ID", "time")]) & !duplicated(osprey_nonb[c("ID", "time")], fromLast = TRUE), ]
 
 
 st <- osprey_nonb_no_duplicates%>%
           select(-c("date", "death_date", "season"))
 
 stationary_lt <- as.ltraj(st[, c("x", "y")],
                               date = st$time,
                               id = st$ID,
                               typeII = T)
 
 
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


