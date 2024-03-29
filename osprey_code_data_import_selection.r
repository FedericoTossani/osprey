
# ================================ #
#       1.SetWD and packages       #
# ================================ #

          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_WD_packages.r")

# ================================= #
#          2.Data import            #
# ================================= #

         list_csv <- list.files(pattern = "20")
         csv_allfile <- lapply(list_csv, read.csv)

         col_selected <- c("timestamp", "location.long", "location.lat", "individual.local.identifier")

         csv_file_sel_col <- lapply(csv_allfile, "[", , col_selected)
         osprey_raw <- dplyr::bind_rows(csv_file_sel_col)

         osprey_death <- read.csv("osprey_death.csv")


# ================================= #
#        3. Filtering data          #
# ================================= #

# dead ospreys

         osprey_dead <- c("Balearics2013_FOSP11-Juv_ringH7", "Corsica2013_FOSP17_Juv_ringCBK",
                          "Italy2017_FOSP43_juv_ringIBI_Agrippa", "Italy2018_FIOS45_juv_ringIAB_Anassagora")

# dataset to use for further analysis

         osprey <- osprey_raw %>%
                     dplyr::select("timestamp", "location.long", "location.lat", "individual.local.identifier")%>%
                     rename("lon" = "location.long",
                            "lat" = "location.lat",
                            "id" = "individual.local.identifier")%>%
                     mutate(id = as.factor(id),
                            time = as.POSIXct(timestamp, tz = "UTC"),
                            signal_interruption_cause = ifelse (id %in% osprey_dead, "Death", "GPS lifecycle"),
                            date = as_date(parse_date_time(timestamp, orders = c("%Y-%m-%d %H:%M:%S"))),
                            day = day(time),
                            month = month(time),
                            year = year(time))%>%
                     tidyr::unite(m_day, c(month, day), sep="/", remove = F)%>%
                     tidyr::unite(id_time, c(id, time), sep="_", remove = F)%>%
                     dplyr::left_join(osprey_death, by = c("id" = "id"))%>%
                     mutate( season = dplyr::case_when(
                                month %in% 10:12 ~ "Fall",
                                month %in%  1:3  ~ "Winter",
                                month %in%  4:6  ~ "Spring",
                                TRUE ~ "Summer"),
                             ID = dplyr::case_when(
                             id == "Balearics2013_FOSP11-Juv_ringH7" ~ "H7",
                             id == "Corsica2013_FOSP17_Juv_ringCBK" ~ "CBK",
                             id == "Corsica2014_FOSP21-Juv_ringCIV" ~ "CIV",
                             id == "Italy2014_FOSP27-Juv_ringE7_Edy" ~ "E7",
                             id == "Italy2015_FOSP30-juv-ringA7-Cook" ~ "A7",
                             id == "Italy2015_FOSP33-juv-Antares" ~ "Antares",
                             id == "Italy2016_FOSP28_juv_ringIAD_Ciccia" ~ "IAD",
                             id == "Italy2016_FOSP37-adult-ringCAM Mora" ~ "CAM",
                             id == "Italy2017_FOSP43_juv_ringIBI_Agrippa" ~ "IBI",
                             id == "Italy2018_FIOS45_juv_ringIAB_Anassagora" ~ "IAB",
                             id == "Italy2019_OrnitelaWhite_juv_ringICZ_Odaba" ~ "ICZ",
                             id == "Italy2020_FIOS21_juv_ringIBS_Mauna Loa" ~ "IBS",
                             id == "Italy2020_Ornitela_juv_ringIBH_Infiernillo" ~ "IBH",
                             id == "Italy2020_Ornitela_juv_ringIBK_Imbabura" ~ "IBK",
                             id == "Italy2022_OSPI08_juv_ringIFP_Ildebrando" ~ "IFP"))

         osprey <- osprey%>%
         select(-c("timestamp", "id"))%>%
         relocate("ID", "time", "date", "day", "month", "year", "m_day",
                              "death_date", "season", "lon", "lat", "signal_interruption_cause", "death_comment")%>%
         unique()


# Convert lon and lat columns to a spatial object with WGS84 coordinate system
osp_hr_v <- vect(osprey, geom = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

# Define the desired projected coordinate system in meters. You can choose a suitable CRS for your specific location.
proj_crs <- "+proj=utm +zone=32 +datum=WGS84 +ellps:WGS84 +units=m"

# Convert the lon-lat points to the projected coordinate system
osp_hr_utm <- terra::project(osp_hr_v, proj_crs)

utm_coord <- geom(osp_hr_utm, df = T)

# Extract the X and Y coordinates in meters from the projected points
osprey <- osprey%>%
                  mutate(x = utm_coord$x,
                         y = utm_coord$y)

# Subset the original data frame into one containing only natal dispersal travel
nd_df <- osprey%>%
          select(-c("day", "month", "year", "m_day", "signal_interruption_cause", "death_comment"))%>%
          unique()

nd_df <- nd_df%>%
          filter(
          ID == "A7" & time >= "2017-02-20 06:00:00" & time <= "2017-03-01 14:00:00" |
          ID == "A7" & time >= "2017-03-17 06:00:00" & time <= "2017-04-15 07:00:00" |
          ID == "A7" & time >= "2017-04-17 06:00:00" & time <= "2017-04-20 20:00:00" |
          ID == "A7" & time >= "2017-04-28 06:00:00" & time <= "2017-05-10 24:00:00" |
          ID == "A7" & time >= "2017-05-14 08:00:00" & time <= "2017-05-15 18:00:00" |
          ID == "A7" & time >= "2017-05-17 06:00:00" & time <= "2017-05-21 14:00:00" |
          ID == "A7" & time >= "2017-05-23 06:00:00" & time <= "2017-05-23 16:00:00" |
          ID == "A7" & time >= "2017-05-26 08:30:00" & time <= "2017-05-28 14:00:00" |
          ID == "A7" & time >= "2017-06-11 06:00:00" & time <= "2017-06-11 20:00:00" |
          ID == "A7" & time >= "2017-07-27 06:00:00" & time <= "2017-07-29 09:00:00" |
          ID == "Antares" & time >= "2016-04-04 13:30:00" & time <= "2016-04-05 18:30:00" |
          ID == "Antares" & time >= "2016-04-09 09:00:00" & time <= "2016-04-12 12:30:00" |
          ID == "Antares" & time >= "2016-04-14 08:00:00" & time <= "2016-04-16 13:30:00" |
          ID == "Antares" & time >= "2016-04-19 11:30:00" & time <= "2016-04-22 12:30:00" |
          ID == "Antares" & time >= "2016-05-05 07:30:00" & time <= "2016-05-08 19:00:00" |
          ID == "Antares" & time >= "2016-05-14 14:00:00" & time <= "2016-05-17 17:00:00" |
          ID == "Antares" & time >= "2016-06-10 09:00:00" & time <= "2016-06-12 17:00:00" |
          ID == "CAM" & time >= "2016-04-14 09:00:00" & time <= "2016-04-15 13:00:00" |
          ID == "CAM" & time >= "2016-04-19 09:00:00" & time <= "2016-04-19 19:00:00" |
          ID == "CAM" & time >= "2016-05-03 09:00:00" & time <= "2016-05-06 17:00:00" |
          ID == "CAM" & time >= "2016-05-20 06:00:00" & time <= "2016-05-24 16:00:00" |
          ID == "CAM" & time >= "2016-07-03 08:00:00" & time <= "2016-07-05 20:00:00" |
          ID == "CBK" & time >= "2014-03-20 09:00:00" & time <= "2014-03-21 11:00:00" |
          ID == "CBK" & time >= "2014-04-08 10:30:00" & time <= "2014-04-12 10:00:00" |
          ID == "CIV" & time >= "2015-03-21 06:00:00" & time <= "2015-03-21 20:00:00" |
          ID == "CIV" & time >= "2015-06-04 06:00:00" & time <= "2015-06-16 21:00:00" |
          ID == "CIV" & time >= "2015-08-13 08:00:00" & time <= "2015-08-13 23:00:00" |
          ID == "CIV" & time >= "2015-11-22 00:01:00" & time <= "2015-11-23 16:01:00" |
          ID == "CIV" & time >= "2016-03-29 06:00:00" & time <= "2016-03-30 20:00:00" |
          ID == "CIV" & time >= "2016-04-15 07:00:00" & time <= "2016-04-18 21:00:00" |
          ID == "CIV" & time >= "2016-05-05 09:30:00" & time <= "2016-05-05 20:00:00" |
          ID == "CIV" & time >= "2016-10-28 08:30:00" & time <= "2016-10-29 14:30:00" |
          ID == "E7" & time >= "2016-03-10 09:00:00" & time <= "2016-03-15 17:00:00" |
          ID == "E7" & time >= "2016-03-17 11:30:00" & time <= "2016-03-22 14:00:00" |
          ID == "E7" & time >= "2016-03-24 09:00:00" & time <= "2016-03-24 14:00:00" |
          ID == "E7" & time >= "2016-03-30 09:30:00" & time <= "2016-04-09 15:00:00" |
          ID == "E7" & time >= "2016-04-12 12:30:00" & time <= "2016-04-17 13:00:00" |
          ID == "E7" & time >= "2016-04-19 10:30:00" & time <= "2016-04-22 18:30:00" |
          ID == "E7" & time >= "2016-04-24 11:30:00" & time <= "2016-04-25 17:00:00" |
          ID == "E7" & time >= "2016-05-04 10:30:00" & time <= "2016-05-07 15:00:00" |
          ID == "E7" & time >= "2016-05-12 11:30:00" & time <= "2016-05-17 10:30:00" |
          ID == "E7" & time >= "2016-05-20 12:00:00" & time <= "2016-05-21 18:30:00" |
          ID == "E7" & time >= "2016-05-23 09:30:00" & time <= "2016-05-27 17:00:00" |
          ID == "H7" & time >= "2015-04-02 07:00:00" & time <= "2015-04-06 15:00:00" |
          ID == "H7" & time >= "2015-04-08 08:00:00" & time <= "2015-04-11 12:00:00" |
          ID == "H7" & time >= "2015-04-20 12:00:00" & time <= "2015-04-30 15:30:00" |
          ID == "IAB" & time >= "2019-03-26 11:30:00" & time <= "2019-03-28 14:00:00" |
          ID == "IAB" & time >= "2019-04-19 09:00:00" & time <= "2019-04-19 21:00:00" |
          ID == "IAB" & time >= "2019-04-21 10:00:00" & time <= "2019-04-21 21:00:00" |
          ID == "IAB" & time >= "2019-04-23 05:00:00" & time <= "2019-04-23 17:00:00" |
          ID == "IAB" & time >= "2019-05-05 09:00:00" & time <= "2019-05-12 13:00:00" |
          ID == "IAB" & time >= "2019-05-16 08:00:00" & time <= "2019-05-20 12:00:00" |
          ID == "IAB" & time >= "2019-05-29 15:30:00" & time <= "2019-06-02 14:00:00" |
          ID == "IAB" & time >= "2019-06-08 10:00:00" & time <= "2019-06-10 15:00:00" |
          ID == "IAB" & time >= "2019-09-07 12:00:00" & time <= "2019-09-08 14:00:00" |
          ID == "IAB" & time >= "2019-09-10 10:00:00" & time <= "2019-09-11 14:00:00" |
          ID == "IAB" & time >= "2019-10-29 09:00:00" & time <= "2019-10-30 09:00:00" |
          ID == "IAB" & time >= "2020-03-16 08:00:00" & time <= "2020-03-19 17:00:00" |
          ID == "IAD" & time >= "2018-02-05 06:00:00" & time <= "2018-02-07 12:00:00" |
          ID == "IAD" & time >= "2018-03-28 11:00:00" & time <= "2018-04-14 20:00:00" |
          ID == "IAD" & time >= "2018-04-16 06:00:00" & time <= "2018-04-16 16:00:00" |
          ID == "IAD" & time >= "2018-04-19 13:00:00" & time <= "2018-04-23 18:00:00" |
          ID == "IAD" & time >= "2018-04-27 08:00:00" & time <= "2018-05-05 17:00:00" |
          ID == "IAD" & time >= "2018-05-14 11:00:00" & time <= "2018-05-17 18:00:00" |
          ID == "IAD" & time >= "2018-05-29 09:30:00" & time <= "2018-06-03 12:00:00" |
          ID == "IAD" & time >= "2018-06-09 10:00:00" & time <= "2018-06-12 14:00:00" |
          ID == "IAD" & time >= "2018-12-15 10:00:00" & time <= "2018-12-18 15:00:00" |
          ID == "IAD" & time >= "2019-03-04 11:00:00" & time <= "2019-03-05 14:00:00" |
          ID == "IAD" & time >= "2019-03-07 06:00:00" & time <= "2019-03-13 16:00:00" |
          ID == "IAD" & time >= "2019-03-15 08:00:00" & time <= "2019-03-15 15:00:00" |
          ID == "IAD" & time >= "2019-03-21 10:30:00" & time <= "2019-04-01 17:30:00" |
          ID == "IAD" & time >= "2019-04-05 13:00:00" & time <= "2019-04-09 16:00:00" |
          ID == "IAD" & time >= "2019-04-15 06:00:00" & time <= "2019-04-18 19:00:00" |
          ID == "IAD" & time >= "2019-04-20 12:00:00" & time <= "2019-04-22 12:00:00" |
          ID == "IAD" & time >= "2019-04-29 10:00:00" & time <= "2019-05-01 22:00:00" |
          ID == "IAD" & time >= "2019-07-08 10:00:00" & time <= "2019-07-08 20:00:00" |
          ID == "IAD" & time >= "2019-07-11 10:00:00" & time <= "2019-07-15 16:00:00" |
          ID == "IBH" & time >= "2022-04-09 08:00:00" & time <= "2022-04-11 18:00:00" |
          ID == "IBH" & time >= "2022-04-20 08:30:00" & time <= "2022-04-20 20:00:00" |
          ID == "IBH" & time >= "2022-04-22 12:30:00" & time <= "2022-04-23 17:00:00" |
          ID == "IBH" & time >= "2022-04-29 11:00:00" & time <= "2022-05-01 13:00:00" |
          ID == "IBH" & time >= "2022-05-04 09:00:00" & time <= "2022-05-04 14:00:00" |
          ID == "IBH" & time >= "2022-05-10 09:00:00" & time <= "2022-05-13 13:00:00" |
          ID == "IBI" & time >= "2017-07-21 09:00:00" & time <= "2017-07-25 16:30:00" |
          ID == "IBI" & time >= "2017-07-27 09:30:00" & time <= "2017-07-27 19:00:00" |
          ID == "IBI" & time >= "2017-08-19 09:00:00" & time <= "2017-08-20 14:00:00" |
          ID == "IBK" & time >= "2021-06-27 10:30:00" & time <= "2021-07-01 10:00:00" |
          ID == "IBK" & time >= "2022-04-15 08:00:00" & time <= "2022-04-17 20:00:00" |
          ID == "IBS" & time >= "2021-01-14 10:00:00" & time <= "2021-01-15 15:00:00" |
          ID == "IBS" & time >= "2021-04-26 12:00:00" & time <= "2021-04-27 19:00:00" |
          ID == "IBS" & time >= "2022-03-22 07:00:00" & time <= "2022-03-26 17:00:00" |
          ID == "IBS" & time >= "2022-03-28 10:30:00" & time <= "2022-04-01 16:00:00" |
          ID == "IBS" & time >= "2022-04-05 06:00:00" & time <= "2022-04-09 20:30:00" |
          ID == "IBS" & time >= "2022-04-16 10:30:00" & time <= "2022-04-18 20:30:00" |
          ID == "IBS" & time >= "2022-04-22 06:00:00" & time <= "2022-04-28 14:30:00" |
          ID == "IBS" & time >= "2022-05-04 08:00:00" & time <= "2022-05-05 12:30:00" |
          ID == "IBS" & time >= "2022-05-07 13:30:00" & time <= "2022-05-17 14:30:00" |
          ID == "IBS" & time >= "2022-05-28 06:00:00" & time <= "2022-05-28 20:30:00" |
          ID == "IBS" & time >= "2022-05-30 08:00:00" & time <= "2022-05-30 20:30:00" |
          ID == "IBS" & time >= "2022-06-02 06:00:00" & time <= "2022-06-04 14:30:00" |
          ID == "IBS" & time >= "2023-02-10 14:00:00" & time <= "2023-02-14 21:00:00" |
          ID == "IBS" & time >= "2023-02-18 08:00:00" & time <= "2023-02-20 21:00:00" |
          ID == "IBS" & time >= "2023-02-23 04:00:00" & time <= "2023-02-23 21:00:00" |
          ID == "IBS" & time >= "2023-03-02 00:00:00" & time <= "2023-03-03 21:00:00" |
          ID == "IBS" & time >= "2023-03-06 07:00:00" & time <= "2023-03-08 16:30:00" |
          ID == "IBS" & time >= "2023-03-13 07:00:00" & time <= "2023-03-16 14:30:00" |
          ID == "ICZ" & time >= "2020-04-11 13:00:00" & time <= "2020-04-23 19:00:00" |
          ID == "ICZ" & time >= "2020-05-02 15:00:00" & time <= "2020-05-05 16:30:00" |
          ID == "IFP" & time >= "2023-04-24 07:00:00" & time <= "2023-04-29 18:00:00" |
          ID == "IFP" & time >= "2023-05-16 04:00:00" & time <= "2023-05-17 18:00:00" |
          ID == "IFP" & time >= "2023-05-23 04:00:00" & time <= "2023-05-24 23:00:00" |
          ID == "IFP" & time >= "2023-06-07 06:00:00" & time <= "2023-06-11 17:00:00"
          )

nd_df <- nd_df%>%
          mutate(
          track_id = dplyr::case_when(
                    ID == "A7" & time >= "2017-02-20 06:00:00" & time <= "2017-03-01 14:00:00" ~ "A7_nd1a",
                    ID == "A7" & time >= "2017-03-17 06:00:00" & time <= "2017-04-15 07:00:00" ~ "A7_nd2a",
                    ID == "A7" & time >= "2017-04-17 06:00:00" & time <= "2017-04-20 20:00:00" ~ "A7_nd2b",
                    ID == "A7" & time >= "2017-04-28 06:00:00" & time <= "2017-05-10 24:00:00" ~ "A7_nd3a",
                    ID == "A7" & time >= "2017-05-14 08:00:00" & time <= "2017-05-15 18:00:00" ~ "A7_nd3b",
                    ID == "A7" & time >= "2017-05-17 06:00:00" & time <= "2017-05-21 14:00:00" ~ "A7_nd3c",
                    ID == "A7" & time >= "2017-05-23 06:00:00" & time <= "2017-05-23 16:00:00" ~ "A7_nd3d",
                    ID == "A7" & time >= "2017-05-26 08:30:00" & time <= "2017-05-28 14:00:00" ~ "A7_nd3e",
                    ID == "A7" & time >= "2017-06-11 06:00:00" & time <= "2017-06-11 20:00:00" ~ "A7_nd4a",
                    ID == "A7" & time >= "2017-07-27 06:00:00" & time <= "2017-07-29 09:00:00" ~ "A7_nd5a",
                    ID == "Antares" & time >= "2016-04-04 13:30:00" & time <= "2016-04-05 18:30:00" ~ "Antares_nd1a",
                    ID == "Antares" & time >= "2016-04-09 09:00:00" & time <= "2016-04-12 12:30:00" ~ "Antares_nd1b",
                    ID == "Antares" & time >= "2016-04-14 08:00:00" & time <= "2016-04-16 13:30:00" ~ "Antares_nd1c",
                    ID == "Antares" & time >= "2016-04-19 11:30:00" & time <= "2016-04-22 12:30:00" ~ "Antares_nd1d",
                    ID == "Antares" & time >= "2016-05-05 07:30:00" & time <= "2016-05-08 19:00:00" ~ "Antares_nd2a",
                    ID == "Antares" & time >= "2016-05-14 14:00:00" & time <= "2016-05-17 17:00:00" ~ "Antares_nd2b",
                    ID == "Antares" & time >= "2016-06-10 09:00:00" & time <= "2016-06-12 17:00:00" ~ "Antares_nd3a",
                    ID == "CAM" & time >= "2016-04-14 09:00:00" & time <= "2016-04-15 13:00:00" ~ "CAM_nd1a",
                    ID == "CAM" & time >= "2016-04-19 09:00:00" & time <= "2016-04-19 19:00:00" ~ "CAM_nd1b",
                    ID == "CAM" & time >= "2016-05-03 09:00:00" & time <= "2016-05-06 17:00:00" ~ "CAM_nd2a",
                    ID == "CAM" & time >= "2016-05-20 06:00:00" & time <= "2016-05-24 16:00:00" ~ "CAM_nd3a",
                    ID == "CAM" & time >= "2016-07-03 08:00:00" & time <= "2016-07-05 20:00:00" ~ "CAM_nd4a",
                    ID == "CBK" & time >= "2014-03-20 09:00:00" & time <= "2014-03-21 11:00:00" ~ "CBK_nd1a",
                    ID == "CBK" & time >= "2014-04-08 10:30:00" & time <= "2014-04-12 10:00:00" ~ "CBK_nd2a",
                    ID == "CIV" & time >= "2015-03-21 06:00:00" & time <= "2015-03-21 20:00:00" ~ "CIV_nd1a",
                    ID == "CIV" & time >= "2015-06-04 06:00:00" & time <= "2015-06-16 21:00:00" ~ "CIV_nd2a",
                    ID == "CIV" & time >= "2015-08-13 08:00:00" & time <= "2015-08-13 23:00:00" ~ "CIV_nd3a",
                    ID == "CIV" & time >= "2015-11-22 00:01:00" & time <= "2015-11-23 16:01:00" ~ "CIV_nd4a",
                    ID == "CIV" & time >= "2016-03-29 06:00:00" & time <= "2016-03-30 20:00:00" ~ "CIV_nd5a",
                    ID == "CIV" & time >= "2016-04-15 07:00:00" & time <= "2016-04-18 21:00:00" ~ "CIV_nd6a",
                    ID == "CIV" & time >= "2016-05-05 09:30:00" & time <= "2016-05-05 20:00:00" ~ "CIV_nd7a",
                    ID == "CIV" & time >= "2016-10-28 08:30:00" & time <= "2016-10-29 14:30:00" ~ "CIV_nd8a",
                    ID == "E7" & time >= "2016-03-10 09:00:00" & time <= "2016-03-15 17:00:00" ~ "E7_nd1a",
                    ID == "E7" & time >= "2016-03-17 11:30:00" & time <= "2016-03-22 14:00:00" ~ "E7_nd1b",
                    ID == "E7" & time >= "2016-03-24 09:00:00" & time <= "2016-03-24 14:00:00" ~ "E7_nd1c",
                    ID == "E7" & time >= "2016-03-30 09:30:00" & time <= "2016-04-09 15:00:00" ~ "E7_nd1d",
                    ID == "E7" & time >= "2016-04-12 12:30:00" & time <= "2016-04-17 13:00:00" ~ "E7_nd1e",
                    ID == "E7" & time >= "2016-04-19 10:30:00" & time <= "2016-04-22 18:30:00" ~ "E7_nd1f",
                    ID == "E7" & time >= "2016-04-24 11:30:00" & time <= "2016-04-25 17:00:00" ~ "E7_nd1g",
                    ID == "E7" & time >= "2016-05-04 10:30:00" & time <= "2016-05-07 15:00:00" ~ "E7_nd2a",
                    ID == "E7" & time >= "2016-05-12 11:30:00" & time <= "2016-05-17 10:30:00" ~ "E7_nd2b",
                    ID == "E7" & time >= "2016-05-20 12:00:00" & time <= "2016-05-21 18:30:00" ~ "E7_nd2c",
                    ID == "E7" & time >= "2016-05-23 09:30:00" & time <= "2016-05-27 17:00:00" ~ "E7_nd2d",
                    ID == "H7" & time >= "2015-04-02 07:00:00" & time <= "2015-04-06 15:00:00" ~ "H7_nd1a",
                    ID == "H7" & time >= "2015-04-08 08:00:00" & time <= "2015-04-11 12:00:00" ~ "H7_nd1b",
                    ID == "H7" & time >= "2015-04-20 12:00:00" & time <= "2015-04-30 15:30:00" ~ "H7_nd2a",
                    ID == "IAB" & time >= "2019-03-26 11:30:00" & time <= "2019-03-28 14:00:00" ~ "IAB_nd1a",
                    ID == "IAB" & time >= "2019-04-19 09:00:00" & time <= "2019-04-19 21:00:00" ~ "IAB_nd2a",
                    ID == "IAB" & time >= "2019-04-21 10:00:00" & time <= "2019-04-21 21:00:00" ~ "IAB_nd2b",
                    ID == "IAB" & time >= "2019-04-23 05:00:00" & time <= "2019-04-23 17:00:00" ~ "IAB_nd2c",
                    ID == "IAB" & time >= "2019-05-05 09:00:00" & time <= "2019-05-12 13:00:00" ~ "IAB_nd3a",
                    ID == "IAB" & time >= "2019-05-16 08:00:00" & time <= "2019-05-20 12:00:00" ~ "IAB_nd3b",
                    ID == "IAB" & time >= "2019-05-29 15:30:00" & time <= "2019-06-02 14:00:00" ~ "IAB_nd4a",
                    ID == "IAB" & time >= "2019-06-08 10:00:00" & time <= "2019-06-10 15:00:00" ~ "IAB_nd4b",
                    ID == "IAB" & time >= "2019-09-07 12:00:00" & time <= "2019-09-08 14:00:00" ~ "IAB_nd5a",
                    ID == "IAB" & time >= "2019-09-10 10:00:00" & time <= "2019-09-11 14:00:00" ~ "IAB_nd5b",
                    ID == "IAB" & time >= "2019-10-29 09:00:00" & time <= "2019-10-30 09:00:00" ~ "IAB_nd6a",
                    ID == "IAB" & time >= "2020-03-16 08:00:00" & time <= "2020-03-19 17:00:00" ~ "IAB_nd7a",
                    ID == "IAD" & time >= "2018-02-05 06:00:00" & time <= "2018-02-07 12:00:00" ~ "IAD_nd1a",
                    ID == "IAD" & time >= "2018-03-28 11:00:00" & time <= "2018-04-14 20:00:00" ~ "IAD_nd2a",
                    ID == "IAD" & time >= "2018-04-16 06:00:00" & time <= "2018-04-16 16:00:00" ~ "IAD_nd2b",
                    ID == "IAD" & time >= "2018-04-19 13:00:00" & time <= "2018-04-23 18:00:00" ~ "IAD_nd2c",
                    ID == "IAD" & time >= "2018-04-27 08:00:00" & time <= "2018-05-05 17:00:00" ~ "IAD_nd2d",
                    ID == "IAD" & time >= "2018-05-14 11:00:00" & time <= "2018-05-17 18:00:00" ~ "IAD_nd3a",
                    ID == "IAD" & time >= "2018-05-29 09:30:00" & time <= "2018-06-03 12:00:00" ~ "IAD_nd4a",
                    ID == "IAD" & time >= "2018-06-09 10:00:00" & time <= "2018-06-12 14:00:00" ~ "IAD_nd5b",
                    ID == "IAD" & time >= "2018-12-15 10:00:00" & time <= "2018-12-18 15:00:00" ~ "IAD_nd5a",
                    ID == "IAD" & time >= "2019-03-04 11:00:00" & time <= "2019-03-05 14:00:00" ~ "IAD_nd6a",
                    ID == "IAD" & time >= "2019-03-07 06:00:00" & time <= "2019-03-13 16:00:00" ~ "IAD_nd6b",
                    ID == "IAD" & time >= "2019-03-15 08:00:00" & time <= "2019-03-15 15:00:00" ~ "IAD_nd6c",
                    ID == "IAD" & time >= "2019-03-21 10:30:00" & time <= "2019-04-01 17:30:00" ~ "IAD_nd6d",
                    ID == "IAD" & time >= "2019-04-05 13:00:00" & time <= "2019-04-09 16:00:00" ~ "IAD_nd6e",
                    ID == "IAD" & time >= "2019-04-15 06:00:00" & time <= "2019-04-18 19:00:00" ~ "IAD_nd6f",
                    ID == "IAD" & time >= "2019-04-20 12:00:00" & time <= "2019-04-22 12:00:00" ~ "IAD_nd6g",
                    ID == "IAD" & time >= "2019-04-29 10:00:00" & time <= "2019-05-01 22:00:00" ~ "IAD_nd7a",
                    ID == "IAD" & time >= "2019-07-08 10:00:00" & time <= "2019-07-08 20:00:00" ~ "IAD_nd8a",
                    ID == "IAD" & time >= "2019-07-11 10:00:00" & time <= "2019-07-15 16:00:00" ~ "IAD_nd8b",
                    ID == "IBH" & time >= "2022-04-09 08:00:00" & time <= "2022-04-11 18:00:00" ~ "IBH_nd1a",
                    ID == "IBH" & time >= "2022-04-20 08:30:00" & time <= "2022-04-20 20:00:00" ~ "IBH_nd2a",
                    ID == "IBH" & time >= "2022-04-22 12:30:00" & time <= "2022-04-23 17:00:00" ~ "IBH_nd2b",
                    ID == "IBH" & time >= "2022-04-29 11:00:00" & time <= "2022-05-01 13:00:00" ~ "IBH_nd2c",
                    ID == "IBH" & time >= "2022-05-04 09:00:00" & time <= "2022-05-04 14:00:00" ~ "IBH_nd2d",
                    ID == "IBH" & time >= "2022-05-10 09:00:00" & time <= "2022-05-13 13:00:00" ~ "IBH_nd2e",
                    ID == "IBI" & time >= "2017-07-21 09:00:00" & time <= "2017-07-25 16:30:00" ~ "IBI_nd1a",
                    ID == "IBI" & time >= "2017-07-27 09:30:00" & time <= "2017-07-27 19:00:00" ~ "IBI_nd1b",
                    ID == "IBI" & time >= "2017-08-19 09:00:00" & time <= "2017-08-20 14:00:00" ~ "IBI_nd2a",
                    ID == "IBK" & time >= "2021-06-27 10:30:00" & time <= "2021-07-01 10:00:00" ~ "IBK_nd1a",
                    ID == "IBK" & time >= "2022-04-15 08:00:00" & time <= "2022-04-17 20:00:00" ~ "IBK_nd2a",
                    ID == "IBS" & time >= "2021-01-14 10:00:00" & time <= "2021-01-15 15:00:00" ~ "IBS_nd1a",
                    ID == "IBS" & time >= "2021-04-26 12:00:00" & time <= "2021-04-27 19:00:00" ~ "IBS_nd2a",
                    ID == "IBS" & time >= "2022-03-22 07:00:00" & time <= "2022-03-26 17:00:00" ~ "IBS_nd3a",
                    ID == "IBS" & time >= "2022-03-28 10:30:00" & time <= "2022-04-01 16:00:00" ~ "IBS_nd3b",
                    ID == "IBS" & time >= "2022-04-05 06:00:00" & time <= "2022-04-09 20:30:00" ~ "IBS_nd3c",
                    ID == "IBS" & time >= "2022-04-16 10:30:00" & time <= "2022-04-18 20:30:00" ~ "IBS_nd3d",
                    ID == "IBS" & time >= "2022-04-22 06:00:00" & time <= "2022-04-28 14:30:00" ~ "IBS_nd3e",
                    ID == "IBS" & time >= "2022-05-04 08:00:00" & time <= "2022-05-05 12:30:00" ~ "IBS_nd3f",
                    ID == "IBS" & time >= "2022-05-07 13:30:00" & time <= "2022-05-17 14:30:00" ~ "IBS_nd4a",
                    ID == "IBS" & time >= "2022-05-28 06:00:00" & time <= "2022-05-28 20:30:00" ~ "IBS_nd4b",
                    ID == "IBS" & time >= "2022-05-30 08:00:00" & time <= "2022-05-30 20:30:00" ~ "IBS_nd4c",
                    ID == "IBS" & time >= "2022-06-02 06:00:00" & time <= "2022-06-04 14:30:00" ~ "IBS_nd5a",
                    ID == "IBS" & time >= "2023-02-10 14:00:00" & time <= "2023-02-14 21:00:00" ~ "IBS_nd6a",
                    ID == "IBS" & time >= "2023-02-18 08:00:00" & time <= "2023-02-20 21:00:00" ~ "IBS_nd6b",
                    ID == "IBS" & time >= "2023-02-23 04:00:00" & time <= "2023-02-23 21:00:00" ~ "IBS_nd6c",
                    ID == "IBS" & time >= "2023-03-02 00:00:00" & time <= "2023-03-03 21:00:00" ~ "IBS_nd7a",
                    ID == "IBS" & time >= "2023-03-06 07:00:00" & time <= "2023-03-08 16:30:00" ~ "IBS_nd7b",
                    ID == "IBS" & time >= "2023-03-13 07:00:00" & time <= "2023-03-16 14:30:00" ~ "IBS_nd7c",
                    ID == "ICZ" & time >= "2020-04-11 13:00:00" & time <= "2020-04-23 19:00:00" ~ "ICZ_nd1a",
                    ID == "ICZ" & time >= "2020-05-02 15:00:00" & time <= "2020-05-05 16:30:00" ~ "ICZ_nd2a",
                    ID == "IFP" & time >= "2023-04-24 07:00:00" & time <= "2023-04-29 18:00:00" ~ "IFP_nd1a",
                    ID == "IFP" & time >= "2023-05-16 04:00:00" & time <= "2023-05-17 18:00:00" ~ "IFP_nd2a",
                    ID == "IFP" & time >= "2023-05-23 04:00:00" & time <= "2023-05-24 23:00:00" ~ "IFP_nd2b",
                    ID == "IFP" & time >= "2023-06-07 06:00:00" & time <= "2023-06-11 17:00:00"~ "IFP_nd3a"),
          NDT = dplyr::case_when(
                    ID == "A7" & time >= "2017-02-20 06:00:00" & time <= "2017-03-01 14:00:00" ~ "A7_nd1",
                    ID == "A7" & time >= "2017-03-17 06:00:00" & time <= "2017-04-20 20:00:00" ~ "A7_nd2",
                    ID == "A7" & time >= "2017-04-28 06:00:00" & time <= "2017-05-28 14:00:00" ~ "A7_nd3",
                    ID == "A7" & time >= "2017-06-11 06:00:00" & time <= "2017-06-11 20:00:00" ~ "A7_nd4",
                    ID == "A7" & time >= "2017-07-27 06:00:00" & time <= "2017-07-29 09:00:00" ~ "A7_nd5",
                    ID == "Antares" & time >= "2016-04-04 13:30:00" & time <= "2016-04-22 12:30:00" ~ "Antares_nd1",
                    ID == "Antares" & time >= "2016-05-05 07:30:00" & time <= "2016-05-17 17:00:00" ~ "Antares_nd2",
                    ID == "Antares" & time >= "2016-06-10 09:00:00" & time <= "2016-06-12 17:00:00" ~ "Antares_nd3",
                    ID == "CAM" & time >= "2016-04-14 09:00:00" & time <= "2016-04-19 19:00:00" ~ "CAM_nd1",
                    ID == "CAM" & time >= "2016-05-03 09:00:00" & time <= "2016-05-06 17:00:00" ~ "CAM_nd2",
                    ID == "CAM" & time >= "2016-05-20 06:00:00" & time <= "2016-05-24 16:00:00" ~ "CAM_nd3",
                    ID == "CAM" & time >= "2016-07-03 08:00:00" & time <= "2016-07-05 20:00:00" ~ "CAM_nd4",
                    ID == "CBK" & time >= "2014-03-20 09:00:00" & time <= "2014-03-21 11:00:00" ~ "CBK_nd1",
                    ID == "CBK" & time >= "2014-04-08 10:30:00" & time <= "2014-04-12 10:00:00" ~ "CBK_nd2",
                    ID == "CIV" & time >= "2015-03-21 06:00:00" & time <= "2015-03-21 20:00:00" ~ "CIV_nd1",
                    ID == "CIV" & time >= "2015-06-04 06:00:00" & time <= "2015-06-16 21:00:00" ~ "CIV_nd2",
                    ID == "CIV" & time >= "2015-08-13 08:00:00" & time <= "2015-08-13 23:00:00" ~ "CIV_nd3",
                    ID == "CIV" & time >= "2015-11-22 00:01:00" & time <= "2015-11-23 16:01:00" ~ "CIV_nd4",
                    ID == "CIV" & time >= "2016-03-29 06:00:00" & time <= "2016-03-30 20:00:00" ~ "CIV_nd5",
                    ID == "CIV" & time >= "2016-04-15 07:00:00" & time <= "2016-04-18 21:00:00" ~ "CIV_nd6",
                    ID == "CIV" & time >= "2016-05-05 09:30:00" & time <= "2016-05-05 20:00:00" ~ "CIV_nd7",
                    ID == "CIV" & time >= "2016-10-28 08:30:00" & time <= "2016-10-29 14:30:00" ~ "CIV_nd8",
                    ID == "E7" & time >= "2016-03-10 09:00:00" & time <= "2016-04-25 17:00:00" ~ "E7_nd1",
                    ID == "E7" & time >= "2016-05-04 10:30:00" & time <= "2016-05-27 17:00:00" ~ "E7_nd2",
                    ID == "H7" & time >= "2015-04-02 07:00:00" & time <= "2015-04-11 12:00:00" ~ "H7_nd1",
                    ID == "H7" & time >= "2015-04-20 12:00:00" & time <= "2015-04-30 15:30:00" ~ "H7_nd2",
                    ID == "IAB" & time >= "2019-03-26 11:30:00" & time <= "2019-03-28 14:00:00" ~ "IAB_nd1",
                    ID == "IAB" & time >= "2019-04-19 09:00:00" & time <= "2019-04-23 17:00:00" ~ "IAB_nd2",
                    ID == "IAB" & time >= "2019-05-05 09:00:00" & time <= "2019-05-20 12:00:00" ~ "IAB_nd3",
                    ID == "IAB" & time >= "2019-05-29 15:30:00" & time <= "2019-06-10 15:00:00" ~ "IAB_nd4",
                    ID == "IAB" & time >= "2019-09-07 12:00:00" & time <= "2019-09-11 14:00:00" ~ "IAB_nd5",
                    ID == "IAB" & time >= "2019-10-29 09:00:00" & time <= "2019-10-30 09:00:00" ~ "IAB_nd6",
                    ID == "IAB" & time >= "2020-03-16 08:00:00" & time <= "2020-03-19 17:00:00" ~ "IAB_nd7",
                    ID == "IAD" & time >= "2018-02-05 06:00:00" & time <= "2018-02-07 12:00:00" ~ "IAD_nd1",
                    ID == "IAD" & time >= "2018-03-28 11:00:00" & time <= "2018-05-05 17:00:00" ~ "IAD_nd2",
                    ID == "IAD" & time >= "2018-05-14 11:00:00" & time <= "2018-05-17 18:00:00" ~ "IAD_nd3",
                    ID == "IAD" & time >= "2018-05-29 09:30:00" & time <= "2018-06-12 14:00:00" ~ "IAD_nd4",
                    ID == "IAD" & time >= "2018-12-15 10:00:00" & time <= "2018-12-18 15:00:00" ~ "IAD_nd5",
                    ID == "IAD" & time >= "2019-03-04 11:00:00" & time <= "2019-04-22 12:00:00" ~ "IAD_nd6",
                    ID == "IAD" & time >= "2019-04-29 10:00:00" & time <= "2019-05-01 22:00:00" ~ "IAD_nd7",
                    ID == "IAD" & time >= "2019-07-08 10:00:00" & time <= "2019-07-15 16:00:00" ~ "IAD_nd8",
                    ID == "IBH" & time >= "2022-04-09 08:00:00" & time <= "2022-04-11 18:00:00" ~ "IBH_nd1",
                    ID == "IBH" & time >= "2022-04-20 08:30:00" & time <= "2022-05-13 13:00:00" ~ "IBH_nd2",
                    ID == "IBI" & time >= "2017-07-21 09:00:00" & time <= "2017-07-27 19:00:00" ~ "IBI_nd1",
                    ID == "IBI" & time >= "2017-08-19 09:00:00" & time <= "2017-08-20 14:00:00" ~ "IBI_nd2",
                    ID == "IBK" & time >= "2021-06-27 10:30:00" & time <= "2021-07-01 10:00:00" ~ "IBK_nd1",
                    ID == "IBK" & time >= "2022-04-15 08:00:00" & time <= "2022-04-17 20:00:00" ~ "IBK_nd2",
                    ID == "IBS" & time >= "2021-01-14 10:00:00" & time <= "2021-01-15 15:00:00" ~ "IBS_nd1",
                    ID == "IBS" & time >= "2021-04-26 12:00:00" & time <= "2021-04-27 19:00:00" ~ "IBS_nd2",
                    ID == "IBS" & time >= "2022-03-22 07:00:00" & time <= "2022-05-05 12:30:00" ~ "IBS_nd3",
                    ID == "IBS" & time >= "2022-05-07 13:30:00" & time <= "2022-05-30 20:30:00" ~ "IBS_nd4",
                    ID == "IBS" & time >= "2022-06-02 06:00:00" & time <= "2022-06-04 14:30:00" ~ "IBS_nd5",
                    ID == "IBS" & time >= "2023-02-10 14:00:00" & time <= "2023-02-23 21:00:00" ~ "IBS_nd6",
                    ID == "IBS" & time >= "2023-03-02 00:00:00" & time <= "2023-03-16 14:30:00" ~ "IBS_nd7",
                    ID == "ICZ" & time >= "2020-04-11 13:00:00" & time <= "2020-04-23 19:00:00" ~ "ICZ_nd1",
                    ID == "ICZ" & time >= "2020-05-02 15:00:00" & time <= "2020-05-05 16:30:00" ~ "ICZ_nd2",
                    ID == "IFP" & time >= "2023-04-24 07:00:00" & time <= "2023-04-29 18:00:00" ~ "IFP_nd1",
                    ID == "IFP" & time >= "2023-05-16 04:00:00" & time <= "2023-05-24 23:00:00" ~ "IFP_nd2",
                    ID == "IFP" & time >= "2023-06-07 06:00:00" & time <= "2023-06-11 17:00:00" ~ "IFP_nd3"))



#############################################
# nd_df processing for statistical analysis #
#############################################

# remove any duplicates

nd_df_no_duplicates <- nd_df[!duplicated(nd_df[c("ID", "time")]) & !duplicated(nd_df[c("ID", "time")], fromLast = TRUE), ]


# remove variable not needed

nd <- nd_df_no_duplicates%>%
          select(-c("date", "death_date", "season"))


# check if there is any NA values

table(is.na(nd$lon))
table(is.na(nd$lat))
table(is.na(nd$x))
table(is.na(nd$y))

nd_with_na <- nd[is.na(nd$x) & is.na(nd$y), ]
nd_with_na


# Convert it into an ltraj object
nd_lt <- as.ltraj(nd[, c("x", "y")],
                    date = nd$time,
                    id = nd$ID,
                    burst = nd$track_id,
                    typeII = T)


# Create a function to cut every tracks where there is a minimum of 24h gap in dt
foo <- function(dt) {
return(dt> (60*60*24))
}

# Cut the ltraj object based on the time gap
# cut_nd_lt <- cutltraj(nd_lt, "foo(dt)", nextr = TRUE)


# create a data frame with trajectory informations
ndtraj_df <- nd_lt%>%
          ld()%>%
          rename("time" = "date",
                 "ID" = "id",
                 "track_id" = "burst")%>%
          mutate(doy = yday(time),
          year = year(time),
          day = as.Date(time),
          distKM = dist/1000)%>%
          tidyr::unite(id_y, c(ID, year), sep="_", remove = F)%>%
          select(-c("pkey"))%>%
          mutate(NDT = dplyr::case_when(
                    ID == "A7" & time >= "2017-02-20 06:00:00" & time <= "2017-03-01 14:00:00" ~ "A7_nd1",
                    ID == "A7" & time >= "2017-03-17 06:00:00" & time <= "2017-04-20 20:00:00" ~ "A7_nd2",
                    ID == "A7" & time >= "2017-04-28 06:00:00" & time <= "2017-05-28 14:00:00" ~ "A7_nd3",
                    ID == "A7" & time >= "2017-06-11 06:00:00" & time <= "2017-06-11 20:00:00" ~ "A7_nd4",
                    ID == "A7" & time >= "2017-07-27 06:00:00" & time <= "2017-07-29 09:00:00" ~ "A7_nd5",
                    ID == "Antares" & time >= "2016-04-04 13:30:00" & time <= "2016-04-22 12:30:00" ~ "Antares_nd1",
                    ID == "Antares" & time >= "2016-05-05 07:30:00" & time <= "2016-05-17 17:00:00" ~ "Antares_nd2",
                    ID == "Antares" & time >= "2016-06-10 09:00:00" & time <= "2016-06-12 17:00:00" ~ "Antares_nd3",
                    ID == "CAM" & time >= "2016-04-14 09:00:00" & time <= "2016-04-19 19:00:00" ~ "CAM_nd1",
                    ID == "CAM" & time >= "2016-05-03 09:00:00" & time <= "2016-05-06 17:00:00" ~ "CAM_nd2",
                    ID == "CAM" & time >= "2016-05-20 06:00:00" & time <= "2016-05-24 16:00:00" ~ "CAM_nd3",
                    ID == "CAM" & time >= "2016-07-03 08:00:00" & time <= "2016-07-05 20:00:00" ~ "CAM_nd4",
                    ID == "CBK" & time >= "2014-03-20 09:00:00" & time <= "2014-03-21 11:00:00" ~ "CBK_nd1",
                    ID == "CBK" & time >= "2014-04-08 10:30:00" & time <= "2014-04-12 10:00:00" ~ "CBK_nd2",
                    ID == "CIV" & time >= "2015-03-21 06:00:00" & time <= "2015-03-21 20:00:00" ~ "CIV_nd1",
                    ID == "CIV" & time >= "2015-06-04 06:00:00" & time <= "2015-06-16 21:00:00" ~ "CIV_nd2",
                    ID == "CIV" & time >= "2015-08-13 08:00:00" & time <= "2015-08-13 23:00:00" ~ "CIV_nd3",
                    ID == "CIV" & time >= "2015-11-22 00:01:00" & time <= "2015-11-23 16:01:00" ~ "CIV_nd4",
                    ID == "CIV" & time >= "2016-03-29 06:00:00" & time <= "2016-03-30 20:00:00" ~ "CIV_nd5",
                    ID == "CIV" & time >= "2016-04-15 07:00:00" & time <= "2016-04-18 21:00:00" ~ "CIV_nd6",
                    ID == "CIV" & time >= "2016-05-05 09:30:00" & time <= "2016-05-05 20:00:00" ~ "CIV_nd7",
                    ID == "CIV" & time >= "2016-10-28 08:30:00" & time <= "2016-10-29 14:30:00" ~ "CIV_nd8",
                    ID == "E7" & time >= "2016-03-10 09:00:00" & time <= "2016-04-25 17:00:00" ~ "E7_nd1",
                    ID == "E7" & time >= "2016-05-04 10:30:00" & time <= "2016-05-27 17:00:00" ~ "E7_nd2",
                    ID == "H7" & time >= "2015-04-02 07:00:00" & time <= "2015-04-11 12:00:00" ~ "H7_nd1",
                    ID == "H7" & time >= "2015-04-20 12:00:00" & time <= "2015-04-30 15:30:00" ~ "H7_nd2",
                    ID == "IAB" & time >= "2019-03-26 11:30:00" & time <= "2019-03-28 14:00:00" ~ "IAB_nd1",
                    ID == "IAB" & time >= "2019-04-19 09:00:00" & time <= "2019-04-23 17:00:00" ~ "IAB_nd2",
                    ID == "IAB" & time >= "2019-05-05 09:00:00" & time <= "2019-05-20 12:00:00" ~ "IAB_nd3",
                    ID == "IAB" & time >= "2019-05-29 15:30:00" & time <= "2019-06-10 15:00:00" ~ "IAB_nd4",
                    ID == "IAB" & time >= "2019-09-07 12:00:00" & time <= "2019-09-11 14:00:00" ~ "IAB_nd5",
                    ID == "IAB" & time >= "2019-10-29 09:00:00" & time <= "2019-10-30 09:00:00" ~ "IAB_nd6",
                    ID == "IAB" & time >= "2020-03-16 08:00:00" & time <= "2020-03-19 17:00:00" ~ "IAB_nd7",
                    ID == "IAD" & time >= "2018-02-05 06:00:00" & time <= "2018-02-07 12:00:00" ~ "IAD_nd1",
                    ID == "IAD" & time >= "2018-03-28 11:00:00" & time <= "2018-05-05 17:00:00" ~ "IAD_nd2",
                    ID == "IAD" & time >= "2018-05-14 11:00:00" & time <= "2018-05-17 18:00:00" ~ "IAD_nd3",
                    ID == "IAD" & time >= "2018-05-29 09:30:00" & time <= "2018-06-12 14:00:00" ~ "IAD_nd4",
                    ID == "IAD" & time >= "2018-12-15 10:00:00" & time <= "2018-12-18 15:00:00" ~ "IAD_nd5",
                    ID == "IAD" & time >= "2019-03-04 11:00:00" & time <= "2019-04-22 12:00:00" ~ "IAD_nd6",
                    ID == "IAD" & time >= "2019-04-29 10:00:00" & time <= "2019-05-01 22:00:00" ~ "IAD_nd7",
                    ID == "IAD" & time >= "2019-07-08 10:00:00" & time <= "2019-07-15 16:00:00" ~ "IAD_nd8",
                    ID == "IBH" & time >= "2022-04-09 08:00:00" & time <= "2022-04-11 18:00:00" ~ "IBH_nd1",
                    ID == "IBH" & time >= "2022-04-20 08:30:00" & time <= "2022-05-13 13:00:00" ~ "IBH_nd2",
                    ID == "IBI" & time >= "2017-07-21 09:00:00" & time <= "2017-07-27 19:00:00" ~ "IBI_nd1",
                    ID == "IBI" & time >= "2017-08-19 09:00:00" & time <= "2017-08-20 14:00:00" ~ "IBI_nd2",
                    ID == "IBK" & time >= "2021-06-27 10:30:00" & time <= "2021-07-01 10:00:00" ~ "IBK_nd1",
                    ID == "IBK" & time >= "2022-04-15 08:00:00" & time <= "2022-04-17 20:00:00" ~ "IBK_nd2",
                    ID == "IBS" & time >= "2021-01-14 10:00:00" & time <= "2021-01-15 15:00:00" ~ "IBS_nd1",
                    ID == "IBS" & time >= "2021-04-26 12:00:00" & time <= "2021-04-27 19:00:00" ~ "IBS_nd2",
                    ID == "IBS" & time >= "2022-03-22 07:00:00" & time <= "2022-05-05 12:30:00" ~ "IBS_nd3",
                    ID == "IBS" & time >= "2022-05-07 13:30:00" & time <= "2022-05-30 20:30:00" ~ "IBS_nd4",
                    ID == "IBS" & time >= "2022-06-02 06:00:00" & time <= "2022-06-04 14:30:00" ~ "IBS_nd5",
                    ID == "IBS" & time >= "2023-02-10 14:00:00" & time <= "2023-02-23 21:00:00" ~ "IBS_nd6",
                    ID == "IBS" & time >= "2023-03-02 00:00:00" & time <= "2023-03-16 14:30:00" ~ "IBS_nd7",
                    ID == "ICZ" & time >= "2020-04-11 13:00:00" & time <= "2020-04-23 19:00:00" ~ "ICZ_nd1",
                    ID == "ICZ" & time >= "2020-05-02 15:00:00" & time <= "2020-05-05 16:30:00" ~ "ICZ_nd2",
                    ID == "IFP" & time >= "2023-04-24 07:00:00" & time <= "2023-04-29 18:00:00" ~ "IFP_nd1",
                    ID == "IFP" & time >= "2023-05-16 04:00:00" & time <= "2023-05-24 23:00:00" ~ "IFP_nd2",
                    ID == "IFP" & time >= "2023-06-07 06:00:00" & time <= "2023-06-11 17:00:00" ~ "IFP_nd3"))

# create a data frame with trajectory informations
nd_lt_redis <- redisltraj(nd_lt, 3600, type="time")

ndtraj_df_redis <- nd_lt_redis%>%
          ld()%>%
          rename("time" = "date",
                 "ID" = "id",
                 "track_id" = "burst")%>%
          mutate(doy = yday(time),
          year = year(time),
          day = as.Date(time),
          distKM = dist/1000)%>%
          tidyr::unite(id_y, c(ID, year), sep="_", remove = F)%>%
          select(-c("pkey"))%>%
          mutate(NDT = dplyr::case_when(
                    ID == "A7" & time >= "2017-02-20 06:00:00" & time <= "2017-03-01 14:00:00" ~ "A7_nd1",
                    ID == "A7" & time >= "2017-03-17 06:00:00" & time <= "2017-04-20 20:00:00" ~ "A7_nd2",
                    ID == "A7" & time >= "2017-04-28 06:00:00" & time <= "2017-05-28 14:00:00" ~ "A7_nd3",
                    ID == "A7" & time >= "2017-06-11 06:00:00" & time <= "2017-06-11 20:00:00" ~ "A7_nd4",
                    ID == "A7" & time >= "2017-07-27 06:00:00" & time <= "2017-07-29 09:00:00" ~ "A7_nd5",
                    ID == "Antares" & time >= "2016-04-04 13:30:00" & time <= "2016-04-22 12:30:00" ~ "Antares_nd1",
                    ID == "Antares" & time >= "2016-05-05 07:30:00" & time <= "2016-05-17 17:00:00" ~ "Antares_nd2",
                    ID == "Antares" & time >= "2016-06-10 09:00:00" & time <= "2016-06-12 17:00:00" ~ "Antares_nd3",
                    ID == "CAM" & time >= "2016-04-14 09:00:00" & time <= "2016-04-19 19:00:00" ~ "CAM_nd1",
                    ID == "CAM" & time >= "2016-05-03 09:00:00" & time <= "2016-05-06 17:00:00" ~ "CAM_nd2",
                    ID == "CAM" & time >= "2016-05-20 06:00:00" & time <= "2016-05-24 16:00:00" ~ "CAM_nd3",
                    ID == "CAM" & time >= "2016-07-03 08:00:00" & time <= "2016-07-05 20:00:00" ~ "CAM_nd4",
                    ID == "CBK" & time >= "2014-03-20 09:00:00" & time <= "2014-03-21 11:00:00" ~ "CBK_nd1",
                    ID == "CBK" & time >= "2014-04-08 10:30:00" & time <= "2014-04-12 10:00:00" ~ "CBK_nd2",
                    ID == "CIV" & time >= "2015-03-21 06:00:00" & time <= "2015-03-21 20:00:00" ~ "CIV_nd1",
                    ID == "CIV" & time >= "2015-06-04 06:00:00" & time <= "2015-06-16 21:00:00" ~ "CIV_nd2",
                    ID == "CIV" & time >= "2015-08-13 08:00:00" & time <= "2015-08-13 23:00:00" ~ "CIV_nd3",
                    ID == "CIV" & time >= "2015-11-22 00:01:00" & time <= "2015-11-23 16:01:00" ~ "CIV_nd4",
                    ID == "CIV" & time >= "2016-03-29 06:00:00" & time <= "2016-03-30 20:00:00" ~ "CIV_nd5",
                    ID == "CIV" & time >= "2016-04-15 07:00:00" & time <= "2016-04-18 21:00:00" ~ "CIV_nd6",
                    ID == "CIV" & time >= "2016-05-05 09:30:00" & time <= "2016-05-05 20:00:00" ~ "CIV_nd7",
                    ID == "CIV" & time >= "2016-10-28 08:30:00" & time <= "2016-10-29 14:30:00" ~ "CIV_nd8",
                    ID == "E7" & time >= "2016-03-10 09:00:00" & time <= "2016-04-25 17:00:00" ~ "E7_nd1",
                    ID == "E7" & time >= "2016-05-04 10:30:00" & time <= "2016-05-27 17:00:00" ~ "E7_nd2",
                    ID == "H7" & time >= "2015-04-02 07:00:00" & time <= "2015-04-11 12:00:00" ~ "H7_nd1",
                    ID == "H7" & time >= "2015-04-20 12:00:00" & time <= "2015-04-30 15:30:00" ~ "H7_nd2",
                    ID == "IAB" & time >= "2019-03-26 11:30:00" & time <= "2019-03-28 14:00:00" ~ "IAB_nd1",
                    ID == "IAB" & time >= "2019-04-19 09:00:00" & time <= "2019-04-23 17:00:00" ~ "IAB_nd2",
                    ID == "IAB" & time >= "2019-05-05 09:00:00" & time <= "2019-05-20 12:00:00" ~ "IAB_nd3",
                    ID == "IAB" & time >= "2019-05-29 15:30:00" & time <= "2019-06-10 15:00:00" ~ "IAB_nd4",
                    ID == "IAB" & time >= "2019-09-07 12:00:00" & time <= "2019-09-11 14:00:00" ~ "IAB_nd5",
                    ID == "IAB" & time >= "2019-10-29 09:00:00" & time <= "2019-10-30 09:00:00" ~ "IAB_nd6",
                    ID == "IAB" & time >= "2020-03-16 08:00:00" & time <= "2020-03-19 17:00:00" ~ "IAB_nd7",
                    ID == "IAD" & time >= "2018-02-05 06:00:00" & time <= "2018-02-07 12:00:00" ~ "IAD_nd1",
                    ID == "IAD" & time >= "2018-03-28 11:00:00" & time <= "2018-05-05 17:00:00" ~ "IAD_nd2",
                    ID == "IAD" & time >= "2018-05-14 11:00:00" & time <= "2018-05-17 18:00:00" ~ "IAD_nd3",
                    ID == "IAD" & time >= "2018-05-29 09:30:00" & time <= "2018-06-12 14:00:00" ~ "IAD_nd4",
                    ID == "IAD" & time >= "2018-12-15 10:00:00" & time <= "2018-12-18 15:00:00" ~ "IAD_nd5",
                    ID == "IAD" & time >= "2019-03-04 11:00:00" & time <= "2019-04-22 12:00:00" ~ "IAD_nd6",
                    ID == "IAD" & time >= "2019-04-29 10:00:00" & time <= "2019-05-01 22:00:00" ~ "IAD_nd7",
                    ID == "IAD" & time >= "2019-07-08 10:00:00" & time <= "2019-07-15 16:00:00" ~ "IAD_nd8",
                    ID == "IBH" & time >= "2022-04-09 08:00:00" & time <= "2022-04-11 18:00:00" ~ "IBH_nd1",
                    ID == "IBH" & time >= "2022-04-20 08:30:00" & time <= "2022-05-13 13:00:00" ~ "IBH_nd2",
                    ID == "IBI" & time >= "2017-07-21 09:00:00" & time <= "2017-07-27 19:00:00" ~ "IBI_nd1",
                    ID == "IBI" & time >= "2017-08-19 09:00:00" & time <= "2017-08-20 14:00:00" ~ "IBI_nd2",
                    ID == "IBK" & time >= "2021-06-27 10:30:00" & time <= "2021-07-01 10:00:00" ~ "IBK_nd1",
                    ID == "IBK" & time >= "2022-04-15 08:00:00" & time <= "2022-04-17 20:00:00" ~ "IBK_nd2",
                    ID == "IBS" & time >= "2021-01-14 10:00:00" & time <= "2021-01-15 15:00:00" ~ "IBS_nd1",
                    ID == "IBS" & time >= "2021-04-26 12:00:00" & time <= "2021-04-27 19:00:00" ~ "IBS_nd2",
                    ID == "IBS" & time >= "2022-03-22 07:00:00" & time <= "2022-05-05 12:30:00" ~ "IBS_nd3",
                    ID == "IBS" & time >= "2022-05-07 13:30:00" & time <= "2022-05-30 20:30:00" ~ "IBS_nd4",
                    ID == "IBS" & time >= "2022-06-02 06:00:00" & time <= "2022-06-04 14:30:00" ~ "IBS_nd5",
                    ID == "IBS" & time >= "2023-02-10 14:00:00" & time <= "2023-02-23 21:00:00" ~ "IBS_nd6",
                    ID == "IBS" & time >= "2023-03-02 00:00:00" & time <= "2023-03-16 14:30:00" ~ "IBS_nd7",
                    ID == "ICZ" & time >= "2020-04-11 13:00:00" & time <= "2020-04-23 19:00:00" ~ "ICZ_nd1",
                    ID == "ICZ" & time >= "2020-05-02 15:00:00" & time <= "2020-05-05 16:30:00" ~ "ICZ_nd2",
                    ID == "IFP" & time >= "2023-04-24 07:00:00" & time <= "2023-04-29 18:00:00" ~ "IFP_nd1",
                    ID == "IFP" & time >= "2023-05-16 04:00:00" & time <= "2023-05-24 23:00:00" ~ "IFP_nd2",
                    ID == "IFP" & time >= "2023-06-07 06:00:00" & time <= "2023-06-11 17:00:00" ~ "IFP_nd3"))

# Here there is the DataFrame with observations regularized to 1 hour intervall between fixs

nd1h <- nd_df %>%
          group_by(group1h = cut(time, "60 min"))%>% 
          distinct(ID, group1h, .keep_all = TRUE)

# convert it into an ltraj object
nd1h_lt <- as.ltraj(nd1h[, c("x", "y")],
                    date = nd1h$time,
                    id = nd1h$ID,
                    typeII = T)

foo <- function(dt) {
return(dt> (60*60*24))
}

# Cut the ltraj object based on the time gap criterion
nd1h_lt <- cutltraj(nd1h_lt, "foo(dt)", nextr = TRUE)

nd1htraj_df <- nd1h_lt%>%
          ld()


#########################
# Stationary Data Frame #
#########################

# Create a df with informationof stationary moments

st_df <- osprey%>%
          filter(ID == 'A7' & time <= "2015-08-14 09:00:00" |
                    ID == "A7" & time >= "2015-08-17 20:00:00" & time <= "2017-02-20 06:00:00" |
                    ID == "A7" & time >= "2017-03-01 14:00:00" & time <= "2017-03-17 06:00:00" |
                    ID == "A7" & time >= "2017-04-15 07:00:00" & time <= "2017-04-17 06:00:00" |
                    ID == "A7" & time >= "2017-04-20 20:00:00" & time <= "2017-04-28 06:00:00" |
                    ID == "A7" & time >= "2017-05-10 24:00:00" & time <= "2017-05-14 08:00:00" |
                    ID == "A7" & time >= "2017-05-15 18:00:00" & time <= "2017-05-17 06:00:00" |
                    ID == "A7" & time >= "2017-05-21 14:00:00" & time <= "2017-05-23 06:00:00" |
                    ID == "A7" & time >= "2017-05-23 16:00:00" & time <= "2017-05-26 08:30:00" |
                    ID == "A7" & time >= "2017-05-28 14:00:00" & time <= "2017-06-11 06:00:00" |
                    ID == "A7" & time >= "2017-06-11 20:00:00" & time <= "2017-07-27 06:00:00" |
                    ID == "A7" & time >= "2017-07-29 09:00:00" |
                    ID == "Antares" & time <= "2015-08-15 12:30:00" |
                    ID == "Antares" & time >= "2015-09-13 14:00:00" & time <= "2016-04-04 13:30:00" |
                    ID == "Antares" & time >= "2016-04-05 18:30:00" & time <= "2016-04-09 09:00:00" |
                    ID == "Antares" & time >= "2016-04-12 12:30:00" & time <= "2016-04-14 08:00:00" |
                    ID == "Antares" & time >= "2016-04-16 13:30:00" & time <= "2016-04-19 11:30:00" |
                    ID == "Antares" & time >= "2016-04-22 12:30:00" & time <= "2016-05-05 07:30:00" |
                    ID == "Antares" & time >= "2016-05-08 19:00:00" & time <= "2016-05-14 14:00:00" |
                    ID == "Antares" & time >= "2016-05-17 17:00:00" & time <= "2016-06-10 09:00:00" |
                    ID == "Antares" & time >= "2016-06-12 17:00:00" |
                    ID == "CAM" & time <= "2016-04-14 09:00:00" |
                    ID == "CAM" & time >= "2016-04-15 13:00:00" & time <= "2016-04-19 09:00:00" |
                    ID == "CAM" & time >= "2016-04-19 19:00:00" & time <= "2016-05-03 09:00:00" |
                    ID == "CAM" & time >= "2016-05-06 17:00:00" & time <= "2016-05-20 06:00:00" |
                    ID == "CAM" & time >= "2016-05-24 16:00:00" & time <= "2016-07-03 08:00:00" |
                    ID == "CAM" & time >= "2016-07-05 20:00:00" |
                    ID == "CBK" & time <= "2013-08-15 06:00:00" |
                    ID == "CBK" & time >= "2013-08-19 13:00:00" & time <= "2014-03-20 09:00:00" |
                    ID == "CBK" & time >= "2014-03-21 11:00:00" & time <= "2014-04-08 10:30:00" |
                    ID == "CBK" & time >= "2014-04-12 10:00:00" |
                    ID == "CIV" & time <= "2014-08-16 10:30:00" |
                    ID == "CIV" & time >= "2014-10-21 20:00:00" & time <= "2015-03-21 06:00:00" |
                    ID == "CIV" & time >= "2015-03-21 20:00:00" & time <= "2015-06-04 06:00:00" |
                    ID == "CIV" & time >= "2015-06-16 21:00:00" & time <= "2015-08-13 08:00:00" |
                    ID == "CIV" & time >= "2015-08-13 23:00:00" & time <= "2015-11-22 00:01:00" |
                    ID == "CIV" & time >= "2015-11-23 16:01:00" & time <= "2016-03-29 06:00:00" |
                    ID == "CIV" & time >= "2016-03-30 20:00:00" & time <= "2016-04-15 07:00:00" |
                    ID == "CIV" & time >= "2016-04-18 21:00:00" & time <= "2016-05-05 09:30:00" |
                    ID == "CIV" & time >= "2016-05-05 20:00:00" & time <= "2016-10-28 08:30:00" |
                    ID == "CIV" & time >= "2016-10-29 14:30:00"|
                    ID == "E7" & time <= "2014-08-21 10:00:00" |
                    ID == "E7" & time >= "2014-08-27 12:30:00" & time <= "2016-03-10 09:00:00" |
                    ID == 'E7' & time >= '2016-03-15 17:00:00' & time <= '2016-03-17 11:30:00' |
                    ID == "E7" & time >= "2016-03-22 14:00:00" & time <= "2016-03-24 09:00:00" |
                    ID == "E7" & time >= "2016-03-24 14:00:00" & time <= "2016-03-30 09:30:00" |
                    ID == "E7" & time >= "2016-04-09 15:00:00" & time <= "2016-04-12 12:30:00" |
                    ID == "E7" & time >= "2016-04-17 13:00:00" & time <= "2016-04-19 10:30:00" |
                    ID == "E7" & time >= "2016-04-22 18:30:00" & time <= "2016-04-24 11:30:00" |
                    ID == "E7" & time >= "2016-04-25 17:00:00" & time <= "2016-05-04 10:30:00" |
                    ID == "E7" & time >= "2016-05-07 15:00:00" & time <= "2016-05-12 11:30:00" |
                    ID == "E7" & time >= "2016-05-17 10:30:00" & time <= "2016-05-20 12:00:00" |
                    ID == "E7" & time >= "2016-05-21 18:30:00" & time <= "2016-05-23 09:30:00" |
                    ID == "E7" & time >= "2016-05-27 17:00:00" |
                    ID == "H7" & time <= "2013-08-04 10:00:00" |
                    ID == "H7" & time >= "2013-08-09 15:00:00" & time <= "2015-04-02 07:00:00" |
                    ID == "H7" & time >= "2015-04-06 15:00:00" & time <= "2015-04-08 08:00:00" |
                    ID == "H7" & time >= "2015-04-11 12:00:00" & time <= "2015-04-20 12:00:00" |
                    ID == "H7" & time >= "2015-04-30 15:30:00" |
                    ID == "IAB" & time <= "2018-08-07 14:30:00" |
                    ID == "IAB" & time >= "2018-08-12 18:00:00" & time <= "2019-03-26 11:30:00" |
                    ID == "IAB" & time >= "2019-03-28 14:00:00" & time <= "2019-04-19 09:00:00" |
                    ID == "IAB" & time >= "2019-04-19 21:00:00" & time <= "2019-04-21 10:00:00" |
                    ID == "IAB" & time >= "2019-04-21 21:00:00" & time <= "2019-04-23 05:00:00" |
                    ID == "IAB" & time >= "2019-04-23 17:00:00" & time <= "2019-05-05 09:00:00" |
                    ID == "IAB" & time >= "2019-05-12 13:00:00" & time <= "2019-05-16 08:00:00" |
                    ID == "IAB" & time >= "2019-05-20 12:00:00" & time <= "2019-05-29 15:30:00" |
                    ID == "IAB" & time >= "2019-06-02 14:00:00" & time <= "2019-06-08 10:00:00" |
                    ID == "IAB" & time >= "2019-06-10 15:00:00" & time <= "2019-09-07 12:00:00" |
                    ID == "IAB" & time >= "2019-09-08 14:00:00" & time <= "2019-09-10 10:00:00" |
                    ID == "IAB" & time >= "2019-09-11 14:00:00" & time <= "2019-10-29 09:00:00" |
                    ID == "IAB" & time >= "2019-10-30 09:00:00" & time <= "2020-03-16 08:00:00" |
                    ID == "IAB" & time >= "2020-03-19 17:00:00" |
                    ID == "IAD" & time <= "2016-08-20 09:30:00" |
                    ID == "IAD" & time >= "2016-08-22 17:30:00" & time <= "2018-02-05 06:00:00" |
                    ID == "IAD" & time >= "2018-02-07 12:00:00" & time <= "2018-03-28 11:00:00" |
                    ID == "IAD" & time >= "2018-04-14 20:00:00" & time <= "2018-04-16 06:00:00" |
                    ID == "IAD" & time >= "2018-04-16 16:00:00" & time <= "2018-04-19 13:00:00" |
                    ID == "IAD" & time >= "2018-04-23 18:00:00" & time <= "2018-04-27 08:00:00" |
                    ID == "IAD" & time >= "2018-05-05 17:00:00" & time <= "2018-05-14 11:00:00" |
                    ID == "IAD" & time >= "2018-05-17 18:00:00" & time <= "2018-05-29 09:30:00" |
                    ID == "IAD" & time >= "2018-06-03 12:00:00" & time <= "2018-06-09 10:00:00" |
                    ID == "IAD" & time >= "2018-06-12 14:00:00" & time <= "2018-12-15 10:00:00" |
                    ID == "IAD" & time >= "2018-12-18 15:00:00" & time <= "2019-03-04 11:00:00" |
                    ID == "IAD" & time >= "2019-03-05 14:00:00" & time <= "2019-03-07 06:00:00" |
                    ID == "IAD" & time >= "2019-03-13 16:00:00" & time <= "2019-03-15 08:00:00" |
                    ID == "IAD" & time >= "2019-03-15 15:00:00" & time <= "2019-03-21 10:30:00" |
                    ID == "IAD" & time >= "2019-04-01 17:30:00" & time <= "2019-04-05 13:00:00" |
                    ID == "IAD" & time >= "2019-04-09 16:00:00" & time <= "2019-04-15 06:00:00" |
                    ID == "IAD" & time >= "2019-04-18 19:00:00" & time <= "2019-04-20 12:00:00" |
                    ID == "IAD" & time >= "2019-04-22 12:00:00" & time <= "2019-04-29 10:00:00" |
                    ID == "IAD" & time >= "2019-05-01 22:00:00" & time <= "2019-07-08 10:00:00" |
                    ID == "IAD" & time >= "2019-07-08 20:00:00" & time <= "2019-07-11 10:00:00" |
                    ID == "IAD" & time >= "2019-07-15 16:00:00" |
                    ID == "IBH" & time <= "2020-08-07 08:00:00" | 
                    ID == "IBH" & time >= "2020-08-15 14:00:00" & time <= "2022-04-09 08:00:00" |
                    ID == "IBH" & time >= "2022-04-11 18:00:00" & time <= "2022-04-20 08:30:00" |
                    ID == "IBH" & time >= "2022-04-20 20:00:00" & time <= "2022-04-22 12:30:00" |
                    ID == "IBH" & time >= "2022-04-23 17:00:00" & time <= "2022-04-29 11:00:00" |
                    ID == "IBH" & time >= "2022-05-01 13:00:00" & time <= "2022-05-04 09:00:00" |
                    ID == "IBH" & time >= "2022-05-04 14:00:00" & time <= "2022-05-10 09:00:00" |
                    ID == "IBH" & time >= "2022-05-13 13:00:00" |
                    ID == "IBI" & time <= "2017-07-21 09:00:00" |
                    ID == "IBI" & time >= "2017-07-25 16:30:00" & time <= "2017-07-27 09:30:00" |
                    ID == "IBI" & time >= "2017-07-27 19:00:00" & time <= "2017-08-19 09:00:00" |
                    ID == "IBI" & time >= "2017-08-20 14:00:00" |
                    ID == "IBK" & time <= "2020-08-21 10:00:00" |
                    ID == "IBK" & time >= "2020-08-22 18:00:00" & time <= "2021-06-27 10:30:00" |
                    ID == "IBK" & time >= "2021-07-01 10:00:00" & time <= "2022-04-15 08:00:00" |
                    ID == "IBK" & time >= "2022-04-17 20:00:00" |
                    ID == "IBS" & time <= "2020-07-27 08:00:00" |
                    ID == "IBS" & time >= "2020-08-18 17:00:00" & time <= "2021-01-14 10:00:00" |
                    ID == "IBS" & time >= "2021-01-15 15:00:00" & time <= "2021-04-26 12:00:00" |
                    ID == "IBS" & time >= "2021-04-27 19:00:00" & time <= "2022-03-22 07:00:00" |
                    ID == "IBS" & time >= "2022-03-26 17:00:00" & time <= "2022-03-28 10:30:00" |
                    ID == "IBS" & time >= "2022-04-01 16:00:00" & time <= "2022-04-05 06:00:00" |
                    ID == "IBS" & time >= "2022-04-09 20:30:00" & time <= "2022-04-16 10:30:00" |
                    ID == "IBS" & time >= "2022-04-18 20:30:00" & time <= "2022-04-22 06:00:00" |
                    ID == "IBS" & time >= "2022-04-28 14:30:00" & time <= "2022-05-04 08:00:00" |
                    ID == "IBS" & time >= "2022-05-05 12:30:00" & time <= "2022-05-07 13:30:00" |
                    ID == "IBS" & time >= "2022-05-17 14:30:00" & time <= "2022-05-28 06:00:00" |
                    ID == "IBS" & time >= "2022-05-28 20:30:00" & time <= "2022-05-30 08:00:00" |
                    ID == "IBS" & time >= "2022-05-30 20:30:00" & time <= "2022-06-02 06:00:00" |
                    ID == "IBS" & time >= "2022-06-04 14:30:00" & time <= "2023-02-10 14:00:00" |
                    ID == "IBS" & time >= "2023-02-14 21:00:00" & time <= "2023-02-18 08:00:00" |
                    ID == "IBS" & time >= "2023-02-20 21:00:00" & time <= "2023-02-23 04:00:00" |
                    ID == "IBS" & time >= "2023-02-23 21:00:00" & time <= "2023-03-02 00:00:00" |
                    ID == "IBS" & time >= "2023-03-03 21:00:00" & time <= "2023-03-06 07:00:00" |
                    ID == "IBS" & time >= "2023-03-08 16:30:00" & time <= "2023-03-13 07:00:00" |
                    ID == "IBS" & time >= "2023-03-16 14:30:00" |          
                    ID == "ICZ" & time <= "2019-09-09 10:00:00" |
                    ID == "ICZ" & time >= "2019-09-14 14:00:00" & time <= "2020-04-11 13:00:00" |
                    ID == "ICZ" & time >= "2020-04-23 19:00:00" & time <= "2020-05-02 15:00:00" |
                    ID == "ICZ" & time >= "2020-05-05 16:30:00" |
                    ID == "IFP" & time <= "2022-07-23 09:00:00" |
                    ID == "IFP" & time >= "2022-07-28 17:30:00" & time <= "2023-04-24 07:00:00" |
                    ID == "IFP" & time >= "2023-04-29 18:00:00" & time <= "2023-05-16 04:00:00" |
                    ID == "IFP" & time >= "2023-05-17 18:00:00" & time <= "2023-05-23 04:00:00" |
                    ID == "IFP" & time >= "2023-05-24 23:00:00" & time <= "2023-06-07 06:00:00" |
                    ID == "IFP" & time >= "2023-06-11 17:00:00")%>%
          mutate(stop_id = dplyr::case_when(
                    ID == 'A7' & time <= "2015-08-14 09:00:00"  ~ "A7_nest",
                    ID == "A7" & time >= "2015-08-17 20:00:00" & time <= "2017-02-20 06:00:00" ~ "A7_wintering1",
                    ID == "A7" & time >= "2017-03-01 14:00:00" & time <= "2017-03-17 06:00:00" ~ "A7_stop1",
                    ID == "A7" & time >= "2017-04-15 07:00:00" & time <= "2017-04-17 06:00:00" ~ "A7_stop2",
                    ID == "A7" & time >= "2017-04-20 20:00:00" & time <= "2017-04-28 06:00:00" ~ "A7_stop3",
                    ID == "A7" & time >= "2017-05-10 24:00:00" & time <= "2017-05-14 08:00:00" ~ "A7_stop4",
                    ID == "A7" & time >= "2017-05-15 18:00:00" & time <= "2017-05-17 06:00:00" ~ "A7_stop5",
                    ID == "A7" & time >= "2017-05-21 14:00:00" & time <= "2017-05-23 06:00:00" ~ "A7_stop6",
                    ID == "A7" & time >= "2017-05-23 16:00:00" & time <= "2017-05-26 08:30:00" ~ "A7_stop7",
                    ID == "A7" & time >= "2017-05-28 14:00:00" & time <= "2017-06-11 06:00:00" ~ "A7_stop8",
                    ID == "A7" & time >= "2017-06-11 20:00:00" & time <= "2017-07-27 06:00:00" ~ "A7_stop9",
                    ID == "A7" & time >= "2017-07-29 09:00:00" ~ "A7_end",
                    ID == "Antares" & time <= "2015-08-15 12:30:00" ~ "Antares_nest",
                    ID == "Antares" & time >= "2015-09-13 14:00:00" & time <= "2016-04-04 13:30:00" ~ "Antares_wintering1",
                    ID == "Antares" & time >= "2016-04-05 18:30:00" & time <= "2016-04-09 09:00:00" ~ "Antares_stop1",
                    ID == "Antares" & time >= "2016-04-12 12:30:00" & time <= "2016-04-14 08:00:00" ~ "Antares_stop2",
                    ID == "Antares" & time >= "2016-04-16 13:30:00" & time <= "2016-04-19 11:30:00" ~ "Antares_stop3",
                    ID == "Antares" & time >= "2016-04-22 12:30:00" & time <= "2016-05-05 07:30:00" ~ "Antares_stop4",
                    ID == "Antares" & time >= "2016-05-08 19:00:00" & time <= "2016-05-14 14:00:00" ~ "Antares_stop5",
                    ID == "Antares" & time >= "2016-05-17 17:00:00" & time <= "2016-06-10 09:00:00" ~ "Antares_stop6",
                    ID == "Antares" & time >= "2016-06-12 17:00:00" ~ "Antares_end",
                    ID == "CAM" & time <= "2016-04-14 09:00:00" ~ "CAM_nest",
                    ID == "CAM" & time >= "2016-04-15 13:00:00" & time <= "2016-04-19 09:00:00" ~ "CAM_stop1",
                    ID == "CAM" & time >= "2016-04-19 19:00:00" & time <= "2016-05-03 09:00:00" ~ "CAM_stop2",
                    ID == "CAM" & time >= "2016-05-06 17:00:00" & time <= "2016-05-20 06:00:00" ~ "CAM_stop3",
                    ID == "CAM" & time >= "2016-05-24 16:00:00" & time <= "2016-07-03 08:00:00" ~ "CAM_stop4",
                    ID == "CAM" & time >= "2016-07-05 20:00:00" ~ "CAM_end",
                    ID == "CBK" & time <= "2013-08-15 06:00:00" ~ "CBK_nest",
                    ID == "CBK" & time >= "2013-08-19 13:00:00" & time <= "2014-03-20 09:00:00" ~ "CBK_wintering1",
                    ID == "CBK" & time >= "2014-03-21 11:00:00" & time <= "2014-04-08 10:30:00" ~ "CBK_stop1",
                    ID == "CBK" & time >= "2014-04-12 10:00:00" ~ "CBK_end",
                    ID == "CIV" & time <= "2014-08-16 10:30:00" ~ "CIV_nest",
                    ID == "CIV" & time >= "2014-10-21 20:00:00" & time <= "2015-03-21 06:00:00" ~ "CIV_wintering1",
                    ID == "CIV" & time >= "2015-03-21 20:00:00" & time <= "2015-06-04 06:00:00" ~ "CIV_stop1",
                    ID == "CIV" & time >= "2015-06-16 21:00:00" & time <= "2015-08-13 08:00:00" ~ "CIV_stop2",
                    ID == "CIV" & time >= "2015-08-13 23:00:00" & time <= "2015-11-22 00:01:00" ~ "CIV_stop3",
                    ID == "CIV" & time >= "2015-11-23 16:01:00" & time <= "2016-03-29 06:00:00" ~ "CIV_wintering2",
                    ID == "CIV" & time >= "2016-03-30 20:00:00" & time <= "2016-04-15 07:00:00" ~ "CIV_stop4",
                    ID == "CIV" & time >= "2016-04-18 21:00:00" & time <= "2016-05-05 09:30:00" ~ "CIV_stop5",
                    ID == "CIV" & time >= "2016-05-05 20:00:00" & time <= "2016-10-28 08:30:00" ~ "CIV_stop6",
                    ID == "CIV" & time >= "2016-10-29 14:30:00"~ "CIV_end",
                    ID == "E7" & time <= "2014-08-21 10:00:00" ~ "E7_nest",
                    ID == "E7" & time >= "2014-08-27 12:30:00" & time <= "2016-03-10 09:00:00" ~ "E7_wintering1",
                    ID == 'E7' & time >= '2016-03-15 17:00:00' & time <= '2016-03-17 11:30:00' ~ "E7_stop1",
                    ID == "E7" & time >= "2016-03-22 14:00:00" & time <= "2016-03-24 09:00:00" ~ "E7_stop2",
                    ID == "E7" & time >= "2016-03-24 14:00:00" & time <= "2016-03-30 09:30:00" ~ "E7_stop3",
                    ID == "E7" & time >= "2016-04-09 15:00:00" & time <= "2016-04-12 12:30:00" ~ "E7_stop4",
                    ID == "E7" & time >= "2016-04-17 13:00:00" & time <= "2016-04-19 10:30:00" ~ "E7_stop5",
                    ID == "E7" & time >= "2016-04-22 18:30:00" & time <= "2016-04-24 11:30:00" ~ "E7_stop6",
                    ID == "E7" & time >= "2016-04-25 17:00:00" & time <= "2016-05-04 10:30:00" ~ "E7_stop7",
                    ID == "E7" & time >= "2016-05-07 15:00:00" & time <= "2016-05-12 11:30:00" ~ "E7_stop8",
                    ID == "E7" & time >= "2016-05-17 10:30:00" & time <= "2016-05-20 12:00:00" ~ "E7_stop9",
                    ID == "E7" & time >= "2016-05-21 18:30:00" & time <= "2016-05-23 09:30:00" ~ "E7_stop10",
                    ID == "E7" & time >= "2016-05-27 17:00:00" ~ "E7_end",
                    ID == "H7" & time <= "2013-08-04 10:00:00" ~ "H7_nest",
                    ID == "H7" & time >= "2013-08-09 15:00:00" & time <= "2015-04-02 07:00:00" ~ "H7_wintering1",
                    ID == "H7" & time >= "2015-04-06 15:00:00" & time <= "2015-04-08 08:00:00" ~ "H7_stop1",
                    ID == "H7" & time >= "2015-04-11 12:00:00" & time <= "2015-04-20 12:00:00" ~ "H7_stop2",
                    ID == "H7" & time >= "2015-04-30 15:30:00" ~ "H7_end",
                    ID == "IAB" & time <= "2018-08-07 14:30:00" ~ "IAB_nest",
                    ID == "IAB" & time >= "2018-08-12 18:00:00" & time <= "2019-03-26 11:30:00" ~ "IAB_wintering1",
                    ID == "IAB" & time >= "2019-03-28 14:00:00" & time <= "2019-04-19 09:00:00" ~ "IAB_stop1",
                    ID == "IAB" & time >= "2019-04-19 21:00:00" & time <= "2019-04-21 10:00:00" ~ "IAB_stop2",
                    ID == "IAB" & time >= "2019-04-21 21:00:00" & time <= "2019-04-23 05:00:00" ~ "IAB_stop3",
                    ID == "IAB" & time >= "2019-04-23 17:00:00" & time <= "2019-05-05 09:00:00" ~ "IAB_stop4",
                    ID == "IAB" & time >= "2019-05-12 13:00:00" & time <= "2019-05-16 08:00:00" ~ "IAB_stop5",
                    ID == "IAB" & time >= "2019-05-20 12:00:00" & time <= "2019-05-29 15:30:00" ~ "IAB_stop6",
                    ID == "IAB" & time >= "2019-06-02 14:00:00" & time <= "2019-06-08 10:00:00" ~ "IAB_stop7",
                    ID == "IAB" & time >= "2019-06-10 15:00:00" & time <= "2019-09-07 12:00:00" ~ "IAB_stop8",
                    ID == "IAB" & time >= "2019-09-08 14:00:00" & time <= "2019-09-10 10:00:00" ~ "IAB_stop9",
                    ID == "IAB" & time >= "2019-09-11 14:00:00" & time <= "2019-10-29 09:00:00" ~ "IAB_stop10",
                    ID == "IAB" & time >= "2019-10-30 09:00:00" & time <= "2020-03-16 08:00:00" ~ "IAB_wintering2",
                    ID == "IAB" & time >= "2020-03-19 17:00:00" ~ "IAB_end",
                    ID == "IAD" & time <= "2016-08-20 09:30:00" ~ "IAD_nest",
                    ID == "IAD" & time >= "2016-08-22 17:30:00" & time <= "2018-02-05 06:00:00" ~ "IAD_wintering1",
                    ID == "IAD" & time >= "2018-02-07 12:00:00" & time <= "2018-03-28 11:00:00" ~ "IAD_stop1",
                    ID == "IAD" & time >= "2018-04-14 20:00:00" & time <= "2018-04-16 06:00:00" ~ "IAD_stop2",
                    ID == "IAD" & time >= "2018-04-16 16:00:00" & time <= "2018-04-19 13:00:00" ~ "IAD_stop3",
                    ID == "IAD" & time >= "2018-04-23 18:00:00" & time <= "2018-04-27 08:00:00" ~ "IAD_stop4",
                    ID == "IAD" & time >= "2018-05-05 17:00:00" & time <= "2018-05-14 11:00:00" ~ "IAD_stop5",
                    ID == "IAD" & time >= "2018-05-17 18:00:00" & time <= "2018-05-29 09:30:00" ~ "IAD_stop6",
                    ID == "IAD" & time >= "2018-06-03 12:00:00" & time <= "2018-06-09 10:00:00" ~ "IAD_stop7",
                    ID == "IAD" & time >= "2018-06-12 14:00:00" & time <= "2018-12-15 10:00:00" ~ "IAD_stop8",
                    ID == "IAD" & time >= "2018-12-18 15:00:00" & time <= "2019-03-04 11:00:00" ~ "IAD_wintering2",
                    ID == "IAD" & time >= "2019-03-05 14:00:00" & time <= "2019-03-07 06:00:00" ~ "IAD_stop9",
                    ID == "IAD" & time >= "2019-03-13 16:00:00" & time <= "2019-03-15 08:00:00" ~ "IAD_stop10",
                    ID == "IAD" & time >= "2019-03-15 15:00:00" & time <= "2019-03-21 10:30:00" ~ "IAD_stop11",
                    ID == "IAD" & time >= "2019-04-01 17:30:00" & time <= "2019-04-05 13:00:00" ~ "IAD_stop12",
                    ID == "IAD" & time >= "2019-04-09 16:00:00" & time <= "2019-04-15 06:00:00" ~ "IAD_stop13",
                    ID == "IAD" & time >= "2019-04-18 19:00:00" & time <= "2019-04-20 12:00:00" ~ "IAD_stop14",
                    ID == "IAD" & time >= "2019-04-22 12:00:00" & time <= "2019-04-29 10:00:00" ~ "IAD_stop15",
                    ID == "IAD" & time >= "2019-05-01 22:00:00" & time <= "2019-07-08 10:00:00" ~ "IAD_stop16",
                    ID == "IAD" & time >= "2019-07-08 20:00:00" & time <= "2019-07-11 10:00:00" ~ "IAD_stop17",
                    ID == "IAD" & time >= "2019-07-15 16:00:00" ~ "IAD_end",
                    ID == "IBH" & time <= "2020-08-07 08:00:00" ~ "IBH_nest",
                    ID == "IBH" & time >= "2020-08-15 14:00:00" & time <= "2022-04-09 08:00:00" ~ "IBH_wintering1",
                    ID == "IBH" & time >= "2022-04-11 18:00:00" & time <= "2022-04-20 08:30:00" ~ "IBH_stop1",
                    ID == "IBH" & time >= "2022-04-20 20:00:00" & time <= "2022-04-22 12:30:00" ~ "IBH_stop2",
                    ID == "IBH" & time >= "2022-04-23 17:00:00" & time <= "2022-04-29 11:00:00" ~ "IBH_stop3",
                    ID == "IBH" & time >= "2022-05-01 13:00:00" & time <= "2022-05-04 09:00:00" ~ "IBH_stop4",
                    ID == "IBH" & time >= "2022-05-04 14:00:00" & time <= "2022-05-10 09:00:00" ~ "IBH_stop5",
                    ID == "IBH" & time >= "2022-05-13 13:00:00" ~ "IBH_end",
                    ID == "IBI" & time <= "2017-07-21 09:00:00" ~ "IBI_nest",
                    ID == "IBI" & time >= "2017-07-25 16:30:00" & time <= "2017-07-27 09:30:00" ~ "IBI_stop1",
                    ID == "IBI" & time >= "2017-07-27 19:00:00" & time <= "2017-08-19 09:00:00" ~ "IBI_stop2",
                    ID == "IBI" & time >= "2017-08-20 14:00:00" ~ "IBI_end",
                    ID == "IBK" & time <= "2020-08-21 10:00:00" ~ "IBK_nest",
                    ID == "IBK" & time >= "2020-08-22 18:00:00" & time <= "2021-06-27 10:30:00" ~ "IBK_wintering1",
                    ID == "IBK" & time >= "2021-07-01 10:00:00" & time <= "2022-04-15 08:00:00" ~ "IBK_wintering2",
                    ID == "IBK" & time >= "2022-04-17 20:00:00" ~ "IBK_end",
                    ID == "IBS" & time <= "2020-07-27 08:00:00" ~ "IBS_nest",
                    ID == "IBS" & time >= "2020-08-18 17:00:00" & time <= "2021-01-14 10:00:00" ~ "IBS_wintering1",
                    ID == "IBS" & time >= "2021-01-15 15:00:00" & time <= "2021-04-26 12:00:00" ~ "IBS_stop1",
                    ID == "IBS" & time >= "2021-04-27 19:00:00" & time <= "2022-03-22 07:00:00" ~ "IBS_wintering2",
                    ID == "IBS" & time >= "2022-03-26 17:00:00" & time <= "2022-03-28 10:30:00" ~ "IBS_stop2",
                    ID == "IBS" & time >= "2022-04-01 16:00:00" & time <= "2022-04-05 06:00:00" ~ "IBS_stop3",
                    ID == "IBS" & time >= "2022-04-09 20:30:00" & time <= "2022-04-16 10:30:00" ~ "IBS_stop4",
                    ID == "IBS" & time >= "2022-04-18 20:30:00" & time <= "2022-04-22 06:00:00" ~ "IBS_stop5",
                    ID == "IBS" & time >= "2022-04-28 14:30:00" & time <= "2022-05-04 08:00:00" ~ "IBS_stop6",
                    ID == "IBS" & time >= "2022-05-05 12:30:00" & time <= "2022-05-07 13:30:00" ~ "IBS_stop7",
                    ID == "IBS" & time >= "2022-05-17 14:30:00" & time <= "2022-05-28 06:00:00" ~ "IBS_stop8",
                    ID == "IBS" & time >= "2022-05-28 20:30:00" & time <= "2022-05-30 08:00:00" ~ "IBS_stop9",
                    ID == "IBS" & time >= "2022-05-30 20:30:00" & time <= "2022-06-02 06:00:00" ~ "IBS_stop10",
                    ID == "IBS" & time >= "2022-06-04 14:30:00" & time <= "2023-02-10 14:00:00" ~ "IBS_wintering3",
                    ID == "IBS" & time >= "2023-02-14 21:00:00" & time <= "2023-02-18 08:00:00" ~ "IBS_stop11",
                    ID == "IBS" & time >= "2023-02-20 21:00:00" & time <= "2023-02-23 04:00:00" ~ "IBS_stop12",
                    ID == "IBS" & time >= "2023-02-23 21:00:00" & time <= "2023-03-02 00:00:00" ~ "IBS_stop13",
                    ID == "IBS" & time >= "2023-03-03 21:00:00" & time <= "2023-03-06 07:00:00" ~ "IBS_stop14",
                    ID == "IBS" & time >= "2023-03-08 16:30:00" & time <= "2023-03-13 07:00:00" ~ "IBS_stop15",
                    ID == "IBS" & time >= "2023-03-16 14:30:00" ~ "IBS_nest",
                    ID == "ICZ" & time <= "2019-09-09 10:00:00" ~ "ICZ_nest",
                    ID == "ICZ" & time >= "2019-09-14 14:00:00" & time <= "2020-04-11 13:00:00" ~ "ICZ_wintering1",
                    ID == "ICZ" & time >= "2020-04-23 19:00:00" & time <= "2020-05-02 15:00:00" ~ "ICZ_stop1",
                    ID == "ICZ" & time >= "2020-05-05 16:30:00" ~ "ICZ_end",
                    ID == "IFP" & time <= "2022-07-23 09:00:00" ~ "IFP_nest",
                    ID == "IFP" & time >= "2022-07-28 17:30:00" & time <= "2023-04-24 07:00:00" ~ "IFP_wintering1",
                    ID == "IFP" & time >= "2023-04-29 18:00:00" & time <= "2023-05-16 04:00:00" ~ "IFP_stop1",
                    ID == "IFP" & time >= "2023-05-17 18:00:00" & time <= "2023-05-23 04:00:00" ~ "IFP_stop2",
                    ID == "IFP" & time >= "2023-05-24 23:00:00" & time <= "2023-06-07 06:00:00" ~ "IFP_stop3",
                    ID == "IFP" & time >= "2023-06-11 17:00:00" ~ "IFP_end"))

#############################################
# st_df processing for statistical analysis #
#############################################

# remove any duplicates

st_df_no_duplicates <- st_df[!duplicated(st_df[c("ID", "time")]) & !duplicated(st_df[c("ID", "time")], fromLast = TRUE), ]

# remove variable not needed

st <- st_df_no_duplicates%>%
          select(-c("date", "death_date", "season"))


# check if there is any NA values

table(is.na(st$lon))
table(is.na(st$lat))
table(is.na(st$x))
table(is.na(st$y))

st_with_na <- st[is.na(st$x) & is.na(st$y), ]
st_with_na


# Convert it into an ltraj object
st_lt <- as.ltraj(st[, c("x", "y")],
                    date = st$time,
                    id = st$ID,
                    burst = st$stop_id,
                    typeII = T)


# Create a function to cut every tracks where there is a minimum of 24h gap in dt
foo <- function(dt) {
return(dt> (60*60*24))
}

# Cut the ltraj object based on the time gap
# cut_st_lt <- cutltraj(st_lt, "foo(dt)", nextr = TRUE)

# transform it into a trajectories data frame

sttraj_df <- st_lt%>%
          ld()%>%
          rename("time" = "date",
                 "ID" = "id",
                 "stop_id" = "burst")%>%
          mutate(doy = yday(time),
                    ymd = as.Date(time),
                    year = year(time),
                    day = as.Date(time),
                    distKM = dist/1000)%>%
          tidyr::unite(id_y, c(ID, year), sep="_", remove = F)%>%
          select(-c("pkey"))

