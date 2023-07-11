
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

osprey_nd <- osprey%>%
                  select(-c("day", "month", "year", "m_day", "ext_temp", "signal_interruption_cause", "death_comment"))%>%
                  unique()

         osprey_nd <- osprey_nd%>%
                  filter(ID == 'H7' & time >= '2015-04-02 05:00:00' & time <= '2015-05-10 00:00:00' |
                         ID == "CIV" & time >= "2014-08-16 06:00:00" & time <= "2014-10-22 12:00:00" | ID == 'CIV' & time >= '2015-06-04 03:00:00' & time <= '2015-11-26 24:00:00' | ID == 'CIV' & time >= '2016-03-29 00:01:00' & time <= '2016-10-29 18:00:00' | 
                         ID == 'E7' & time >= '2016-03-10 05:00:00' |
                         ID == 'A7' & time >= '2017-02-20 00:00:00' |
                         ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00' | ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-05-07 15:00:00' | 
                         ID == "IBS"& time >= "2020-08-07 08:00:00" & time <= "2020-08-18 19:00:00" | ID == "IBS" & time >= "2021-02-15 09:00:00" & time <= "2021-05-01 16:00:00" | ID == 'IBS' & time >= '2022-03-22 00:00:00' & time <= '2022-06-04 15:00:00' | ID == "IBS" & time >= "2023-02-08 16:00:00" & time <= "2023-02-28 00:00:00" |
                         ID == 'IBH' & time >= '2022-04-09 06:00:00' |
                         ID == 'IBK' & time >= '2022-01-24 06:44:48' |
                         ID == 'IFP' & time > '2023-04-24 00:00:00' & time < '2023-04-30 04:00:00' | ID == "IFP" & time >= '2023-05-16 00:00:00' & time <= '2023-06-08 20:00:00' |
                         ID == 'ICZ' & time >= '2020-04-11 10:45:00' & time <= '2020-04-25 00:00:00' |
                         ID == 'CBK' & time >= '2014-04-08 06:30:00' & time <= '2014-04-12 11:00:00' |
                         ID == 'IAB' & time >= '2019-05-05 06:00:00' & time <= '2019-06-10 14:00:00' | ID == 'IAB' & time >= '2020-03-16 00:00:00' |
                         ID == "CAM" & time >= "2016-04-14 06:00:00" & time <= "2016-04-19 20:00:00" | ID == "CAM" & time >= "2016-05-03 08:00:00" & time <= "2016-05-06 18:00:00" | ID == "CAM" & time >= "2016-05-20 05:00:00" & time <= "2016-05-24 18:00:00" | ID == "CAM" & time >= "2016-07-02 23:00:00" & time <= "2016-07-06 20:00:00" |
                         ID == "Antares" & time >= "2016-03-19 00:00:00" & time <= "2016-06-29 00:00:00" |
                         ID == "IBI" & time >= "2017-07-21 05:00:00" & time <= "2017-07-27 20:00:00" |
                         ID == "Antares" & time >= "2015-08-15 11:00:00" & time <= "2015-08-18 19:00:00" | ID == "Antares" & time >= "2015-09-08 10:30:00" & time <= "2015-09-13 20:00:00" | ID == "Antares" & time >= "2016-03-19 13:00:00" & time <= "2016-06-28 18:00:00" 
                        )


