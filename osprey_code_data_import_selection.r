
# ================================ #
#       1.SetWD and packages       #
# ================================ #

          source("https://raw.githubusercontent.com/FedericoTossani/osprey/main/osprey_code_WD_packages.r")

# ================================= #
#          2.Data import            #
# ================================= #

         list_csv <- list.files(pattern = "20")
         csv_allfile <- lapply(list_csv, read.csv)

         col_selected <- c("timestamp", "location.long", "location.lat", "external.temperature", 
                           "individual.local.identifier")

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
                     dplyr::select("timestamp", "location.long", "location.lat", "external.temperature", 
                                   "individual.local.identifier")%>%
                     rename("lon"="location.long",
                            "lat"="location.lat",
                            "ext_temp"="external.temperature",
                            "id"="individual.local.identifier")%>%
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
                              "death_date", "season", "ext_temp", "lon", "lat", "signal_interruption_cause", "death_comment")%>%
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

# ltraj object
osprey_no_duplicates <- osprey[!duplicated(osprey[c("ID", "time")]) & !duplicated(osprey[c("ID", "time")], fromLast = TRUE), ]

osprey_lt <- as.ltraj(osprey_no_duplicates[, c("x", "y")],
                                        date = osprey_no_duplicates$time,
                                        id = osprey_no_duplicates$ID,
                                        typeII = T)

osp_lt_df <- ld(osprey_lt)

nd_df <- osprey%>%
                  select(-c('day', 'month', 'year', 'm_day', 'ext_temp', 'signal_interruption_cause', 'death_comment'))%>%
                  unique()

nd_df <- nd_df%>%
                    filter( ID == 'A7' & time >= '2015-08-14 08:00:00' & time <= '2015-08-18 09:00:00' |
                     ID == 'A7' & time >= '2017-02-20 06:00:00' & time <= '2017-03-01 12:00:00' |
                     ID == 'A7' & time >= '2017-03-17 06:00:00' & time <= '2017-04-14 18:00:00'  |
                     ID == 'A7' & time >= '2017-04-17 06:00:00' & time <= '2017-04-20 18:00:00'  |
                     ID == 'A7' & time >= '2017-04-28 06:00:00' & time <= '2017-05-10 12:00:00'  |
                     ID == 'A7' & time >= '2017-05-14 08:00:00' & time <= '2017-05-21 12:00:00'  |
                     ID == 'A7' & time >= '2017-05-26 08:00:00' & time <= '2017-05-28 18:00:00'  |
                     ID == 'A7' & time >= '2017-06-06 08:00:00' & time <= '2017-06-06 12:00:00'  |
                     ID == 'A7' & time >= '2017-06-11 06:00:00' & time <= '2017-06-12 12:00:00'  |
                     ID == 'A7' & time >= '2017-07-27 06:00:00' & time <= '2017-07-27 18:00:00'  |
                     ID == 'Antares' & time >= '2015-08-15 11:00:00' & time <= '2015-08-18 16:30:00'  |
                     ID == 'Antares' & time >= '2015-09-08 09:30:00' & time <= '2015-09-08 16:30:00'  |
                     ID == 'Antares' & time >= '2015-09-13 08:00:00' & time <= '2015-09-13 12:00:00'  |
                     ID == 'Antares' & time >= '2016-04-04 00:00:00' & time <= '2016-04-05 15:00:00'  |
                     ID == 'Antares' & time >= '2016-04-09 08:00:00' & time <= '2016-04-10 14:00:00'  |
                     ID == 'Antares' & time >= '2016-04-19 10:00:00' & time <= '2016-04-22 10:00:00'  |
                     ID == 'Antares' & time >= '2016-05-05 06:00:00' & time <= '2016-05-06 06:00:00' |
                     ID == 'Antares' & time >= '2016-05-14 12:00:00' & time <= '2016-05-17 15:00:00' |
                     ID == 'Antares' & time >= '2016-06-10 07:00:00' & time <= '2016-06-12 15:00:00' |
                     ID == 'CAM' & time >= '2016-04-14 07:00:00' & time <= '2016-04-15 11:00:00' |
                     ID == 'CAM' & time >= '2016-04-19 10:00:00' & time <= '2016-04-19 17:00:00' |
                     ID == 'CAM' & time >= '2016-05-03 07:00:00' & time <= '2016-05-06 15:00:00' |
                     ID == 'CAM' & time >= '2016-05-20 04:00:00' & time <= '2016-05-24 14:00:00' |
                     ID == 'CAM' & time >= '2016-07-02 23:00:00' & time <= '2016-07-06 18:00:00' |
                     ID == 'CBK' & time >= '2013-08-15 06:00:00' & time <= '2013-08-21 08:00:00' |
                     ID == 'CBK' & time >= '2014-03-20 08:00:00' & time <= '2014-03-21 10:00:00' |
                     ID == 'CBK' & time >= '2014-04-08 07:30:00' & time <= '2014-04-12 08:00:00' |
                     ID == 'CIV' & time >= '2014-08-16 09:00:00' & time <= '2014-08-21 18:00:00' |
                     ID == 'CIV' & time >= '2014-09-11 11:00:00' & time <= '2014-09-13 13:00:00' |
                     ID == 'CIV' & time >= '2014-10-21 08:00:00' & time <= '2014-10-21 18:00:00' |
                     ID == 'CIV' & time >= '2015-03-21 02:00:00' & time <= '2015-03-21 20:00:00' |
                     ID == 'CIV' & time >= '2015-06-04 06:00:00' & time <= '2015-06-16 12:00:00' |
                     ID == 'CIV' & time >= '2015-06-20 00:00:00' & time <= '2015-06-22 14:00:00' |
                     ID == 'CIV' & time >= '2015-08-13 06:00:00' & time <= '2015-08-13 20:00:00' |
                     ID == 'CIV' & time >= '2015-11-22 00:01:00' & time <= '2015-11-25 16:01:00' |
                     ID == 'CIV' & time >= '2016-03-29 06:00:00' & time <= '2016-03-31 18:00:00' |
                     ID == 'CIV' & time >= '2016-04-15 06:00:00' & time <= '2016-04-18 20:00:00' |
                     ID == 'CIV' & time >= '2016-10-28 08:00:00' & time <= '2016-10-29 18:00:00' |
                     ID == 'E7' & time >= '2014-08-21 08:00:00' & time <= '2014-08-27 11:00:00' |
                     ID == 'E7' & time >= '2016-03-10 06:00:00' & time <= '2016-03-15 15:30:00' |
                     ID == 'E7' & time >= '2016-03-17 10:30:00' & time <= '2016-03-22 13:30:00' |
                     ID == 'E7' & time >= '2016-03-24 07:30:00' & time <= '2016-03-24 12:30:00' |
                     ID == 'E7' & time >= '2016-03-30 07:30:00' & time <= '2016-04-10 10:00:00' |
                     ID == 'E7' & time >= '2016-04-12 10:30:00' & time <= '2016-04-17 09:30:00' |
                     ID == 'E7' & time >= '2016-04-19 09:00:00' & time <= '2016-04-22 16:30:00' |
                     ID == 'E7' & time >= '2016-04-24 11:30:00' & time <= '2016-04-25 15:00:00' |
                     ID == 'E7' & time >= '2016-05-04 08:30:00' & time <= '2016-05-07 12:30:00' |
                     ID == 'E7' & time >= '2016-05-12 10:00:00' & time <= '2016-05-16 17:30:00' |
                     ID == 'E7' & time >= '2016-05-20 10:00:00' & time <= '2016-05-21 16:00:00' |
                     ID == 'E7' & time >= '2016-05-23 07:30:00' & time <= '2016-05-27 16:30:00' |
                     ID == 'H7' & time >= '2013-08-04 08:00:00' & time <= '2013-08-09 12:30:00' |
                     ID == 'H7' & time >= '2015-04-02 08:00:00' & time <= '2015-04-06 11:00:00' |
                     ID == 'H7' & time >= '2015-04-07 17:00:00' & time <= '2015-04-11 10:00:00' |
                     ID == 'H7' & time >= '2015-04-20 11:00:00' & time <= '2015-04-30 13:30:00' |
                     ID == 'H7' & time >= '2015-05-03 05:30:00' & time <= '2015-05-03 09:00:00' |
                     ID == 'IAB' & time >= '2018-08-07 11:00:00' & time <= '2018-08-12 16:00:00' |
                     ID == 'IAB' & time >= '2019-03-26 11:00:00' & time <= '2019-03-28 11:00:00' |
                     ID == 'IAB' & time >= '2019-04-19 08:00:00' & time <= '2019-04-19 16:00:00' |
                     ID == 'IAB' & time >= '2019-04-21 08:00:00' & time <= '2019-04-22 07:00:00' |
                     ID == 'IAB' & time >= '2019-04-23 06:00:00' & time <= '2019-04-23 15:00:00' |
                     ID == 'IAB' & time >= '2019-05-05 07:00:00' & time <= '2019-05-12 11:00:00' |
                     ID == 'IAB' & time >= '2019-05-16 06:00:00' & time <= '2019-05-20 10:00:00' |
                     ID == 'IAB' & time >= '2019-05-29 14:00:00' & time <= '2019-06-02 12:00:00' |
                     ID == 'IAB' & time >= '2019-06-08 08:00:00' & time <= '2019-06-10 13:00:00' |
                     ID == 'IAB' & time >= '2019-09-07 10:00:00' & time <= '2019-09-08 12:00:00' |
                     ID == 'IAB' & time >= '2019-09-10 09:00:00' & time <= '2019-09-11 11:00:00' |
                     ID == 'IAB' & time >= '2019-10-29 08:00:00' & time <= '2019-10-30 07:00:00' |
                     ID == 'IAB' & time >= '2020-03-16 08:00:00' & time <= '2020-03-19 16:00:00' |
                     ID == 'IAD' & time >= '2016-08-20 08:00:00' & time <= '2016-08-22 14:30:00' |
                     ID == 'IAD' & time >= '2018-02-05 06:00:00' & time <= '2018-02-07 16:00:00' |
                     ID == 'IAD' & time >= '2018-03-28 10:00:00' & time <= '2018-04-14 18:00:00' |
                     ID == 'IAD' & time >= '2018-04-16 06:00:00' & time <= '2018-04-16 14:00:00' |
                     ID == 'IAD' & time >= '2018-04-18 12:00:00' & time <= '2018-04-25 18:00:00' |
                     ID == 'IAD' & time >= '2018-04-27 06:00:00' & time <= '2018-05-05 12:00:00' |
                     ID == 'IAD' & time >= '2018-05-07 12:00:00' & time <= '2018-05-07 16:00:00' |
                     ID == 'IAD' & time >= '2018-05-09 08:00:00' & time <= '2018-05-09 12:00:00' |
                     ID == 'IAD' & time >= '2018-05-14 10:00:00' & time <= '2018-05-17 12:00:00' |
                     ID == 'IAD' & time >= '2018-05-29 08:00:00' & time <= '2018-06-03 10:00:00' |
                     ID == 'IAD' & time >= '2018-06-09 08:00:00' & time <= '2018-06-12 12:00:00' |
                     ID == 'IAD' & time >= '2018-12-15 09:00:00' & time <= '2018-12-18 14:00:00' |
                     ID == 'IAD' & time >= '2018-12-20 11:00:00' & time <= '2018-12-20 13:00:00' |
                     ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-03-05 13:00:00' |
                     ID == 'IAD' & time >= '2019-03-07 06:00:00' & time <= '2019-03-13 12:00:00' |
                     ID == 'IAD' & time >= '2019-03-15 08:00:00' & time <= '2019-03-15 14:00:00' |
                     ID == 'IAD' & time >= '2019-03-21 10:00:00' & time <= '2019-04-01 15:00:00' |
                     ID == 'IAD' & time >= '2019-04-05 11:00:00' & time <= '2019-04-09 14:00:00' |
                     ID == 'IAD' & time >= '2019-04-15 06:00:00' & time <= '2019-04-18 16:00:00' |
                     ID == 'IAD' & time >= '2019-04-21 06:00:00' & time <= '2019-04-21 17:00:00' |
                     ID == 'IAD' & time >= '2019-04-29 08:00:00' & time <= '2019-05-01 14:00:00' |
                     ID == 'IAD' & time >= '2019-07-08 09:00:00' & time <= '2019-07-08 17:00:00' |
                     ID == 'IAD' & time >= '2019-07-11 08:00:00' & time <= '2019-07-15 14:00:00' |
                     ID == 'IBH' & time >= '2020-08-07 06:17:15' & time <= '2020-08-07 17:17:15' |
                     ID == 'IBH' & time >= '2020-08-09 07:19:13' & time <= '2020-08-13 13:14:35' |
                     ID == 'IBH' & time >= '2020-08-15 08:25:49' & time <= '2020-08-15 11:25:28' |
                     ID == 'IBH' & time >= '2022-04-09 06:47:05' & time <= '2022-04-11 15:37:18' |
                     ID == 'IBH' & time >= '2022-04-20 06:31:21' & time <= '2022-04-21 04:27:29' |
                     ID == 'IBH' & time >= '2022-04-22 11:24:14' & time <= '2022-04-23 06:24:43' |
                     ID == 'IBH' & time >= '2022-04-29 09:18:06' & time <= '2022-05-01 14:15:15' |
                     ID == 'IBH' & time >= '2022-05-04 09:00:00' & time <= '2022-05-04 12:12:25' |
                     ID == 'IBH' & time >= '2022-05-10 09:00:00' & time <= '2022-05-13 08:58:13' |
                     ID == 'IBI' & time >= '2017-07-21 07:00:00' & time <= '2017-07-25 14:30:00' |
                     ID == 'IBI' & time >= '2017-07-27 12:00:00' & time <= '2017-07-27 17:00:00' |
                     ID == 'IBI' & time >= '2017-08-19 07:00:00' & time <= '2017-08-20 12:00:00' |
                     ID == 'IBK' & time >= '2020-08-21 08:31:15' & time <= '2020-08-22 18:04:45' |
                     ID == 'IBK' & time >= '2021-06-27 08:43:09' & time <= '2021-06-30 12:45:35' |
                     ID == 'IBK' & time >= '2022-04-16 08:39:12' & time <= '2022-04-17 12:50:05' |
                     ID == 'IBS' & time >= '2020-07-27 06:00:00' & time <= '2020-07-28 15:00:00' |
                     ID == 'IBS' & time >= '2020-08-07 08:00:00' & time <= '2020-08-18 15:00:00' |
                     ID == 'IBS' & time >= '2021-01-14 08:00:00' & time <= '2021-01-15 12:00:00' |
                     ID == 'IBS' & time >= '2021-02-15 09:00:00' & time <= '2021-02-15 15:00:00' |
                     ID == 'IBS' & time >= '2021-04-26 10:00:33' & time <= '2021-04-27 16:00:11' |
                     ID == 'IBS' & time >= '2021-04-30 14:00:28' & time <= '2021-05-01 11:00:08' |
                     ID == 'IBS' & time >= '2022-03-22 06:00:34' & time <= '2022-03-26 12:00:11' |
                     ID == 'IBS' & time >= '2022-03-28 06:00:17' & time <= '2022-04-01 09:00:18' |
                     ID == 'IBS' & time >= '2022-04-05 06:00:52' & time <= '2022-04-09 18:00:19' |
                     ID == 'IBS' & time >= '2022-04-16 06:00:13' & time <= '2022-04-18 18:00:11' |
                     ID == 'IBS' & time >= '2022-04-22 06:00:17' & time <= '2022-04-28 12:00:04' |
                     ID == 'IBS' & time >= '2022-05-04 06:00:27' & time <= '2022-05-05 09:00:31' |
                     ID == 'IBS' & time >= '2022-05-07 12:00:17' & time <= '2022-05-17 12:00:18' |
                     ID == 'IBS' & time >= '2022-05-28 06:00:15' & time <= '2022-05-28 18:00:53' |
                     ID == 'IBS' & time >= '2022-05-30 06:00:17' & time <= '2022-05-30 18:00:15' |
                     ID == 'IBS' & time >= '2022-06-02 06:00:29' & time <= '2022-06-04 12:00:18' |
                     ID == 'IBS' & time >= '2023-02-10 13:03:31' & time <= '2023-02-14 21:00:21' |
                     ID == 'IBS' & time >= '2023-02-19 04:00:00' & time <= '2023-02-20 12:00:25' |
                     ID == 'IBS' & time >= '2023-02-23 06:00:34' & time <= '2023-02-23 15:00:34' |
                     ID == 'IBS' & time >= '2023-03-02 06:00:37' & time <= '2023-03-03 12:00:18' |
                     ID == 'IBS' & time >= '2023-03-06 06:00:15' & time <= '2023-03-08 15:00:17' |
                     ID == 'IBS' & time >= '2023-03-13 06:00:12' & time <= '2023-03-16 12:00:04' |
                     ID == 'ICZ' & time >= '2019-09-09 08:15:55' & time <= '2019-09-14 11:50:14' |
                     ID == 'ICZ' & time >= '2020-04-11 11:13:23' & time <= '2020-04-21 06:28:21' |
                     ID == 'ICZ' & time >= '2020-04-23 08:53:32' & time <= '2020-04-23 14:53:33' |
                     ID == 'ICZ' & time >= '2020-05-02 13:09:29' & time <= '2020-05-05 14:06:04' |
                     ID == 'IFP' & time >= '2022-07-23 09:00:31' & time <= '2022-07-24 12:00:07' |
                     ID == 'IFP' & time >= '2022-07-26 06:00:32' & time <= '2022-07-28 15:00:22' |
                     ID == 'IFP' & time >= '2023-04-24 06:00:31' & time <= '2023-04-29 12:00:37' |
                     ID == 'IFP' & time >= '2023-05-16 00:00:00' & time <= '2023-05-17 18:00:37' |
                     ID == 'IFP' & time >= '2023-05-23 03:01:01' & time <= '2023-05-24 21:00:32' |
                     ID == 'IFP' & time >= '2023-06-07 00:01:27' & time <= '2023-06-08 15:01:02'
                          )


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
                    typeII = T)


# Create a function to cut every tracks where there is a minimum of 24h gap in dt
foo <- function(dt) {
return(dt> (60*60*24))
}

# Cut the ltraj object based on the time gap
cut_nd_lt <- cutltraj(nd_lt, "foo(dt)", nextr = TRUE)


# create a data frame with trajectory informations
ndtraj_df <- cut_nd_lt%>%
          ld()%>%
          mutate(doy = yday(date),
          year = year(date),
          track_id = dplyr::case_when(
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
                    id == 'IBS' & date >= '2020-07-27 06:00:00' & date <= '2020-07-28 15:00:00' ~ "IBS_nd1",
                    id == 'IBS' & date >= "2020-08-07 08:00:00" & date <= "2020-08-18 15:00:00" ~ "IBS_nd2",
                    id == 'IBS' & date >= "2021-01-14 08:00:00" & date <= "2021-01-15 12:00:00" ~ "IBS_nd3",
                    id == 'IBS' & date >= "2021-02-15 09:00:00" & date <= "2021-02-15 15:00:00" ~ "IBS_nd4",
                    id == 'IBS' & date >= "2021-04-26 10:00:33" & date <= "2021-04-27 16:00:11" ~ "IBS_nd5a",
                    id == 'IBS' & date >= "2021-04-30 14:00:28" & date <= "2021-05-01 11:00:08" ~ "IBS_nd5b",
                    id == 'IBS' & date >= '2022-03-22 06:00:34' & date <= '2022-03-26 12:00:11' ~ "IBS_nd6a",
                    id == 'IBS' & date >= '2022-03-28 06:00:17' & date <= '2022-04-01 09:00:18' ~ "IBS_nd6b",
                    id == 'IBS' & date >= '2022-04-05 06:00:52' & date <= '2022-04-09 18:00:19' ~ "IBS_nd6c",
                    id == 'IBS' & date >= '2022-04-16 06:00:13' & date <= '2022-04-18 18:00:11' ~ "IBS_nd6d",
                    id == 'IBS' & date >= '2022-04-22 06:00:17' & date <= '2022-04-28 12:00:04' ~ "IBS_nd6e",
                    id == 'IBS' & date >= '2022-05-04 06:00:27' & date <= '2022-05-05 09:00:31' ~ "IBS_nd6f",
                    id == 'IBS' & date >= '2022-05-07 12:00:17' & date <= '2022-05-17 12:00:18' ~ "IBS_nd6g",
                    id == 'IBS' & date >= '2022-05-28 06:00:15' & date <= '2022-05-28 18:00:53' ~ "IBS_nd7a",
                    id == 'IBS' & date >= '2022-05-30 06:00:17' & date <= '2022-05-30 18:00:15' ~ "IBS_nd7b",
                    id == 'IBS' & date >= '2022-06-02 06:00:29' & date <= '2022-06-04 12:00:18' ~ "IBS_nd7c",
                    id == 'IBS' & date >= '2023-02-10 13:03:31' & date <= '2023-02-14 21:00:21' ~ "IBS_nd8a",
                    id == "IBS" & date >= "2023-02-19 04:00:00" & date <= "2023-02-20 12:00:25" ~ "IBS_nd8b",
                    id == "IBS" & date >= "2023-02-23 06:00:34" & date <= "2023-02-23 15:00:34" ~ "IBS_nd8c",
                    id == "IBS" & date >= "2023-03-02 06:00:37" & date <= "2023-03-03 12:00:18" ~ "IBS_nd9a",
                    id == "IBS" & date >= "2023-03-06 06:00:15" & date <= "2023-03-08 15:00:17" ~ "IBS_nd9b",
                    id == "IBS" & date >= "2023-03-13 06:00:12" & date <= "2023-03-16 12:00:04" ~ "IBS_nd9c",
                    id == 'ICZ' & date >= '2019-09-09 08:15:55' & date <= '2019-09-14 11:50:14' ~ "ICZ_nd1",
                    id == 'ICZ' & date >= '2020-04-11 11:13:23' & date <= '2020-04-21 06:28:21' ~ "ICZ_nd2a",
                    id == 'ICZ' & date >= '2020-04-23 08:53:32' & date <= '2020-04-23 14:53:33' ~ "ICZ_nd2b",
                    id == 'ICZ' & date >= '2020-05-02 13:09:29' & date <= '2020-05-05 14:06:04' ~ "ICZ_nd3",
                    id == 'IFP' & date >= '2022-07-23 09:00:31' & date <= '2022-07-24 12:00:07' ~ "IFP_nd1a",
                    id == 'IFP' & date >= '2022-07-26 06:00:32' & date <= '2022-07-28 15:00:22' ~ "IFP_nd1b",
                    id == 'IFP' & date >= '2023-04-24 06:00:31' & date <= '2023-04-29 12:00:37' ~ "IFP_nd2",
                    id == "IFP" & date >= '2023-05-16 00:00:00' & date <= '2023-05-17 18:00:37' ~ "IFP_nd3a",
                    id == "IFP" & date >= '2023-05-23 03:01:01' & date <= '2023-05-24 21:00:32' ~ "IFP_nd3b",
                    id == "IFP" & date >= '2023-06-07 00:01:27' & date <= '2023-06-08 15:01:02' ~ "IFP_nd3c"),
          day = as.Date(date),
          distKM = dist/1000,
          NDT = dplyr::case_when(
                     id == 'A7' & date >= '2015-08-14 08:00:00' & date <= '2015-08-18 09:00:00' ~ 'A7_nd1',
                     id == 'A7' & date >= '2017-02-20 06:00:00' & date <= '2017-03-01 12:00:00' ~ 'A7_nd2',
                     id == 'A7' & date >= '2017-03-17 06:00:00' & date <= '2017-04-20 18:00:00' ~ 'A7_nd3',
                     id == 'A7' & date >= '2017-04-28 06:00:00' & date <= '2017-05-28 18:00:00' ~ 'A7_nd4',
                     id == 'A7' & date >= '2017-06-06 08:00:00' & date <= '2017-06-12 12:00:00' ~ 'A7_nd5',
                     id == 'A7' & date >= '2017-07-27 06:00:00' & date <= '2017-07-27 18:00:00' ~ 'A7_nd6',
                     id == 'Antares' & date >= '2015-08-15 11:00:00' & date <= '2015-08-18 16:30:00' ~ 'Antares_nd1',
                     id == 'Antares' & date >= '2015-09-08 09:30:00' & date <= '2015-09-13 12:00:00' ~ 'Antares_nd2',
                     id == 'Antares' & date >= '2016-04-04 00:00:00' & date <= '2016-04-10 14:00:00' ~ 'Antares_nd3',
                     id == 'Antares' & date >= '2016-04-19 10:00:00' & date <= '2016-04-22 10:00:00' ~ 'Antares_nd4',
                     id == 'Antares' & date >= '2016-05-05 06:00:00' & date <= '2016-05-06 06:00:00' ~ 'Antares_nd5',
                     id == 'Antares' & date >= '2016-05-14 12:00:00' & date <= '2016-05-17 15:00:00' ~ 'Antares_nd6',
                     id == 'Antares' & date >= '2016-06-10 07:00:00' & date <= '2016-06-12 15:00:00' ~ 'Antares_nd7',
                     id == 'CAM' & date >= '2016-04-14 07:00:00' & date <= '2016-04-19 17:00:00' ~ 'CAM_nd1',
                     id == 'CAM' & date >= '2016-05-03 07:00:00' & date <= '2016-05-06 15:00:00' ~ 'CAM_nd2',
                     id == 'CAM' & date >= '2016-05-20 04:00:00' & date <= '2016-05-24 14:00:00' ~ 'CAM_nd3',
                     id == 'CAM' & date >= '2016-07-02 23:00:00' & date <= '2016-07-06 18:00:00' ~ 'CAM_nd4',
                     id == 'CBK' & date >= '2013-08-15 06:00:00' & date <= '2013-08-21 08:00:00' ~ 'CBK_nd1',
                     id == 'CBK' & date >= '2014-03-20 08:00:00' & date <= '2014-03-21 10:00:00' ~ 'CBK_nd2',
                     id == 'CBK' & date >= '2014-04-08 07:30:00' & date <= '2014-04-12 08:00:00' ~ 'CBK_nd3',
                     id == 'CIV' & date >= '2014-08-16 09:00:00' & date <= '2014-08-21 18:00:00' ~ 'CIV_nd1',
                     id == 'CIV' & date >= '2014-09-11 11:00:00' & date <= '2014-09-13 13:00:00' ~ 'CIV_nd2',
                     id == 'CIV' & date >= '2014-10-21 08:00:00' & date <= '2014-10-21 18:00:00' ~ 'CIV_nd3',
                     id == 'CIV' & date >= '2015-03-21 02:00:00' & date <= '2015-03-21 20:00:00' ~ 'CIV_nd4',
                     id == 'CIV' & date >= '2015-06-04 06:00:00' & date <= '2015-06-22 14:00:00' ~ 'CIV_nd5',
                     id == 'CIV' & date >= '2015-08-13 06:00:00' & date <= '2015-08-13 20:00:00' ~ 'CIV_nd6',
                     id == 'CIV' & date >= '2015-11-22 00:01:00' & date <= '2015-11-25 16:01:00' ~ 'CIV_nd7',
                     id == 'CIV' & date >= '2016-03-29 06:00:00' & date <= '2016-03-31 18:00:00' ~ 'CIV_nd8',
                     id == 'CIV' & date >= '2016-04-15 06:00:00' & date <= '2016-04-18 20:00:00' ~ 'CIV_nd9',
                     id == 'CIV' & date >= '2016-10-28 08:00:00' & date <= '2016-10-29 18:00:00' ~ 'CIV_nd10',
                     id == 'E7' & date >= '2014-08-21 08:00:00' & date <= '2014-08-27 11:00:00' ~ 'E7_nd1',
                     id == 'E7' & date >= '2016-03-10 06:00:00' & date <= '2016-04-25 15:00:00' ~ 'E7_nd2',
                     id == 'E7' & date >= '2016-05-04 08:30:00' & date <= '2016-05-27 16:30:00' ~ 'E7_nd3',
                     id == 'H7' & date >= '2013-08-04 08:00:00' & date <= '2013-08-09 12:30:00' ~ 'H7_nd1',
                     id == 'H7' & date >= '2015-04-02 08:00:00' & date <= '2015-04-11 10:00:00' ~ 'H7_nd2',
                     id == 'H7' & date >= '2015-04-20 11:00:00' & date <= '2015-05-03 09:00:00' ~ 'H7_nd3',
                     id == 'IAB' & date >= '2018-08-07 11:00:00' & date <= '2018-08-12 16:00:00' ~ 'IAB_nd1',
                     id == 'IAB' & date >= '2019-03-26 11:00:00' & date <= '2019-03-28 11:00:00' ~ 'IAB_nd2',
                     id == 'IAB' & date >= '2019-04-19 08:00:00' & date <= '2019-04-23 15:00:00' ~ 'IAB_nd3',
                     id == 'IAB' & date >= '2019-05-05 07:00:00' & date <= '2019-05-20 10:00:00' ~ 'IAB_nd4',
                     id == 'IAB' & date >= '2019-05-29 14:00:00' & date <= '2019-06-10 13:00:00' ~ 'IAB_nd5',
                     id == 'IAB' & date >= '2019-09-07 10:00:00' & date <= '2019-09-11 11:00:00' ~ 'IAB_nd6',
                     id == 'IAB' & date >= '2019-10-29 08:00:00' & date <= '2019-10-30 07:00:00' ~ 'IAB_nd7',
                     id == 'IAB' & date >= '2020-03-16 08:00:00' & date <= '2020-03-19 16:00:00' ~ 'IAB_nd8',
                     id == 'IAD' & date >= '2016-08-20 08:00:00' & date <= '2016-08-22 14:30:00' ~ 'IAD_nd1',
                     id == 'IAD' & date >= '2018-02-05 06:00:00' & date <= '2018-02-07 16:00:00' ~ 'IAD_nd2',
                     id == 'IAD' & date >= '2018-03-28 10:00:00' & date <= '2018-05-17 12:00:00' ~ 'IAD_nd3',
                     id == 'IAD' & date >= '2018-05-29 08:00:00' & date <= '2018-06-12 12:00:00' ~ 'IAD_nd4',
                     id == 'IAD' & date >= '2018-12-15 09:00:00' & date <= '2018-12-20 13:00:00' ~ 'IAD_nd5',
                     id == 'IAD' & date >= '2019-03-04 10:00:00' & date <= '2019-04-21 17:00:00' ~ 'IAD_nd6',
                     id == 'IAD' & date >= '2019-04-29 08:00:00' & date <= '2019-05-01 14:00:00' ~ 'IAD_nd7',
                     id == 'IAD' & date >= '2019-07-08 09:00:00' & date <= '2019-07-15 14:00:00' ~ 'IAD_nd8',
                     id == 'IBH' & date >= '2020-08-07 06:17:15' & date <= '2020-08-15 11:25:28' ~ 'IBH_nd1',
                     id == 'IBH' & date >= '2022-04-09 06:47:05' & date <= '2022-04-11 15:37:18' ~ 'IBH_nd2',
                     id == 'IBH' & date >= '2022-04-20 06:31:21' & date <= '2022-04-23 06:24:43' ~ 'IBH_nd3',
                     id == 'IBH' & date >= '2022-04-29 09:18:06' & date <= '2022-05-01 14:15:15' ~ 'IBH_nd4',
                     id == 'IBH' & date >= '2022-05-04 09:00:00' & date <= '2022-05-04 12:12:25' ~ 'IBH_nd5',
                     id == 'IBH' & date >= '2022-05-10 09:00:00' & date <= '2022-05-13 08:58:13' ~ 'IBH_nd6',
                     id == 'IBI' & date >= '2017-07-21 07:00:00' & date <= '2017-07-27 17:00:00' ~ 'IBI_nd1',
                     id == 'IBI' & date >= '2017-08-19 07:00:00' & date <= '2017-08-20 12:00:00' ~ 'IBI_nd2',
                     id == 'IBK' & date >= '2020-08-21 08:31:15' & date <= '2020-08-22 18:04:45' ~ 'IBK_nd1',
                     id == 'IBK' & date >= '2021-06-27 08:43:09' & date <= '2021-06-30 12:45:35' ~ 'IBK_nd2',
                     id == 'IBK' & date >= '2022-04-16 08:39:12' & date <= '2022-04-17 12:50:05' ~ 'IBK_nd3',
                     id == 'IBS' & date >= '2020-07-27 06:00:00' & date <= '2020-07-28 15:00:00' ~ 'IBS_nd1',
                     id == 'IBS' & date >= '2020-08-07 08:00:00' & date <= '2020-08-18 15:00:00' ~ 'IBS_nd2',
                     id == 'IBS' & date >= '2021-01-14 08:00:00' & date <= '2021-01-15 12:00:00' ~ 'IBS_nd3',
                     id == 'IBS' & date >= '2021-02-15 09:00:00' & date <= '2021-02-15 15:00:00' ~ 'IBS_nd4',
                     id == 'IBS' & date >= '2021-04-26 10:00:33' & date <= '2021-05-01 11:00:08' ~ 'IBS_nd5',
                     id == 'IBS' & date >= '2022-03-22 06:00:34' & date <= '2022-05-17 12:00:18' ~ 'IBS_nd6',
                     id == 'IBS' & date >= '2022-05-28 06:00:15' & date <= '2022-06-04 12:00:18' ~ 'IBS_nd7',
                     id == 'IBS' & date >= '2023-02-10 13:03:31' & date <= '2023-02-23 15:00:34' ~ 'IBS_nd8',
                     id == 'IBS' & date >= '2023-03-02 06:00:37' & date <= '2023-03-16 12:00:04' ~ 'IBS_nd9',
                     id == 'ICZ' & date >= '2019-09-09 08:15:55' & date <= '2019-09-14 11:50:14' ~ 'ICZ_nd1',
                     id == 'ICZ' & date >= '2020-04-11 11:13:23' & date <= '2020-04-23 14:53:33' ~ 'ICZ_nd2',
                     id == 'ICZ' & date >= '2020-05-02 13:09:29' & date <= '2020-05-05 14:06:04' ~ 'ICZ_nd3',
                     id == 'IFP' & date >= '2022-07-23 09:00:31' & date <= '2022-07-28 15:00:22' ~ 'IFP_nd1',
                     id == 'IFP' & date >= '2023-04-24 06:00:31' & date <= '2023-04-29 12:00:37' ~ 'IFP_nd2',
                     id == 'IFP' & date >= '2023-05-16 00:00:00' & date <= '2023-06-08 15:01:02' ~ 'IFP_nd3'))%>%
          tidyr::unite(id_y, c(id, year), sep="_", remove = F)%>%
          select(-c("pkey"))

# Here there is the DataFrame with observations regularized to 1 hour intervall between fixs

nd1h_df <- nd_df %>%
          group_by(group1h = cut(time, "60 min"))%>% 
          distinct(ID, group1h, .keep_all = TRUE)

# convert it into an ltraj object
nd1h_lt <- as.ltraj(nd1h_df[, c("x", "y")],
                    date = nd1h_df$time,
                    id = nd1h_df$ID,
                    typeII = T)

foo <- function(dt) {
return(dt> (60*60*24))
}

# Cut the ltraj object based on the time gap criterion
nd1h_lt <- cutltraj(nd1h_lt, "foo(dt)", nextr = TRUE)




#########################
# Stationary Data Frame #
#########################

# Create a df with informationof stationary moments
# To do this we are using the anti_join funtion which extract from the original dataset the observation non included in the Natal Dispersal df

st_df <- anti_join(osprey, nd_df, by = c("id_time" = "id_time"))
