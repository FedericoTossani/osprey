osprey_nd_no_duplicates <- osprey_nd[!duplicated(osprey_nd[c(' ID', 'time')]) & !duplicated(osprey_nd[c(' ID', 'time')], fromLast = TRUE), ]

                  nd <- osprey_nd%>%
                              select(-c('date', 'death_date', 'season'))

          table(is.na(nd$lon))
          table(is.na(nd$lat))
          table(is.na(nd$x))
          table(is.na(nd$y))

nd_with_na <- nd[is.na(nd$x) & is.na(nd$y), ]

nd_with_na

# convert it into an ltraj object
        nd_lt <- as.ltraj(nd[, c('x', 'y')],
                              date = nd$time,
                              id = nd$ID,
                              typeII = T)

foo <- function(dt) {
return(dt> (60*60*24))
}

# Cut the ltraj object based on the time gap criterion
cut_nd_lt <- cutltraj(nd_lt, "foo(dt)", nextr = TRUE)

# create a data frame
ndtraj_df <- nd_lt%>%
          ld()%>%
          mutate(ID = id)

ndtraj_df <- ndtraj_df%>%
          mutate(doy = yday(date),
          year = year(date),
          track_id = dplyr::case_when(
                     ID == 'A7' & date >= '2015-08-14 08:00:00' & date <= '2015-08-18 09:00:00' ~ 'A7_nd1',
                     ID == 'A7' & date >= '2017-02-20 06:00:00' & date <= '2017-03-01 12:00:00' ~ 'A7_nd2',
                     ID == 'A7' & date >= '2017-03-17 06:00:00' & date <= '2017-04-14 18:00:00' ~ 'A7_nd3a',
                     ID == 'A7' & date >= '2017-04-17 06:00:00' & date <= '2017-04-20 18:00:00' ~ 'A7_nd3b',
                     ID == 'A7' & date >= '2017-04-28 06:00:00' & date <= '2017-05-10 12:00:00' ~ 'A7_nd4a',
                     ID == 'A7' & date >= '2017-05-14 08:00:00' & date <= '2017-05-21 12:00:00' ~ 'A7_nd4b',
                     ID == 'A7' & date >= '2017-05-23 06:00:00' & date <= '2017-05-23 14:00:00' ~ 'A7_nd4c',
                     ID == 'A7' & date >= '2017-05-26 08:00:00' & date <= '2017-05-28 18:00:00' ~ 'A7_nd4d',
                     ID == 'A7' & date >= '2017-06-06 08:00:00' & date <= '2017-06-06 12:00:00' ~ 'A7_nd5a',
                     ID == 'A7' & date >= '2017-06-11 06:00:00' & date <= '2017-06-12 12:00:00' ~ 'A7_nd5b',
                     ID == 'A7' & date >= '2017-07-27 06:00:00' & date <= '2017-07-27 18:00:00' ~ 'A7_nd6',
                     ID == 'Antares' & date >= '2015-08-15 11:00:00' & date <= '2015-08-18 16:30:00' ~ 'Antares_nd1',
                     ID == 'Antares' & date >= '2015-09-08 09:30:00' & date <= '2015-09-08 16:30:00' ~ 'Antares_nd2a',
                     ID == 'Antares' & date >= '2015-09-13 08:00:00' & date <= '2015-09-13 12:00:00' ~ 'Antares_nd2b',
                     ID == 'Antares' & date >= '2016-04-04 00:00:00' & date <= '2016-04-05 15:00:00' ~ 'Antares_nd3a',
                     ID == 'Antares' & date >= '2016-04-09 08:00:00' & date <= '2016-04-10 14:00:00' ~ 'Antares_nd3b',
                     ID == 'Antares' & date >= '2016-04-19 10:00:00' & date <= '2016-04-22 10:00:00' ~ 'Antares_nd4',
                     ID == 'Antares' & date >= '2016-05-05 06:00:00' & date <= '2016-05-06 06:00:00' ~ 'Antares_nd5',
                     ID == 'Antares' & date >= '2016-05-14 12:00:00' & date <= '2016-05-17 15:00:00' ~ 'Antares_nd6',
                     ID == 'Antares' & date >= '2016-06-10 07:00:00' & date <= '2016-06-12 15:00:00' ~ 'Antares_nd7',
                     ID == 'CAM' & date >= '2016-04-14 07:00:00' & date <= '2016-04-15 11:00:00' ~ 'CAM_nd1a',
                     ID == 'CAM' & date >= '2016-04-19 10:00:00' & date <= '2016-04-19 17:00:00' ~ 'CAM_nd1b',
                     ID == 'CAM' & date >= '2016-05-03 07:00:00' & date <= '2016-05-06 15:00:00' ~ 'CAM_nd2',
                     ID == 'CAM' & date >= '2016-05-20 04:00:00' & date <= '2016-05-24 14:00:00' ~ 'CAM_nd3',
                     ID == 'CAM' & date >= '2016-07-02 23:00:00' & date <= '2016-07-06 18:00:00' ~ 'CAM_nd4',
                     ID == 'CBK' & date >= '2013-08-15 06:00:00' & date <= '2013-08-21 08:00:00' ~ 'CBK_nd1',
                     ID == 'CBK' & date >= '2014-03-20 08:00:00' & date <= '2014-03-21 10:00:00' ~ 'CBK_nd2',
                     ID == 'CBK' & date >= '2014-04-08 07:30:00' & date <= '2014-04-12 08:00:00' ~ 'CBK_nd3',
                     ID == 'CIV' & date >= '2014-08-16 09:00:00' & date <= '2014-08-21 18:00:00' ~ 'CIV_nd1',
                     ID == 'CIV' & date >= '2014-09-11 11:00:00' & date <= '2014-09-13 13:00:00' ~ 'CIV_nd2',
                     ID == 'CIV' & date >= '2014-10-21 08:00:00' & date <= '2014-10-21 18:00:00' ~ 'CIV_nd3',
                     ID == 'CIV' & date >= '2015-03-21 02:00:00' & date <= '2015-03-21 20:00:00' ~ 'CIV_nd4',
                     ID == 'CIV' & date >= '2015-06-04 06:00:00' & date <= '2015-06-16 12:00:00' ~ 'CIV_nd5a',
                     ID == 'CIV' & date >= '2015-06-20 00:00:00' & date <= '2015-06-22 14:00:00' ~ 'CIV_nd5b',
                     ID == 'CIV' & date >= '2015-08-13 06:00:00' & date <= '2015-08-13 20:00:00' ~ 'CIV_nd6',
                     ID == 'CIV' & date >= '2015-11-22 00:01:00' & date <= '2015-11-25 16:01:00' ~ 'CIV_nd7',
                     ID == 'CIV' & date >= '2016-03-29 06:00:00' & date <= '2016-03-31 18:00:00' ~ 'CIV_nd8',
                     ID == 'CIV' & date >= '2016-04-15 06:00:00' & date <= '2016-04-18 20:00:00' ~ 'CIV_nd9',
                     ID == 'CIV' & date >= '2016-10-28 08:00:00' & date <= '2016-10-29 18:00:00' ~ 'CIV_nd10',
                     ID == 'E7' & date >= '2014-08-21 08:00:00' & date <= '2014-08-27 11:00:00' ~ 'E7_nd1',
                     ID == 'E7' & date >= '2016-03-10 06:00:00' & date <= '2016-03-15 15:30:00' ~ 'E7_nd2a',
                     ID == 'E7' & date >= '2016-03-17 10:30:00' & date <= '2016-03-22 13:30:00' ~ 'E7_nd2b',
                     ID == 'E7' & date >= '2016-03-24 07:30:00' & date <= '2016-03-24 12:30:00' ~ 'E7_nd2c',
                     ID == 'E7' & date >= '2016-03-30 07:30:00' & date <= '2016-04-10 10:00:00' ~ 'E7_nd2d',
                     ID == 'E7' & date >= '2016-04-12 10:30:00' & date <= '2016-04-17 09:30:00' ~ 'E7_nd2e',
                     ID == 'E7' & date >= '2016-04-19 09:00:00' & date <= '2016-04-22 16:30:00' ~ 'E7_nd2f',
                     ID == 'E7' & date >= '2016-04-24 11:30:00' & date <= '2016-04-25 15:00:00' ~ 'E7_nd2g',
                     ID == 'E7' & date >= '2016-05-04 08:30:00' & date <= '2016-05-07 12:30:00' ~ 'E7_nd3a',
                     ID == 'E7' & date >= '2016-05-12 10:00:00' & date <= '2016-05-16 17:30:00' ~ 'E7_nd3b',
                     ID == 'E7' & date >= '2016-05-20 10:00:00' & date <= '2016-05-21 16:00:00' ~ 'E7_nd3c',
                     ID == 'E7' & date >= '2016-05-23 07:30:00' & date <= '2016-05-27 16:30:00' ~ 'E7_nd3d',
                     ID == 'H7' & date >= '2013-08-04 08:00:00' & date <= '2013-08-09 12:30:00' ~ 'H7_nd1',
                     ID == 'H7' & date >= '2015-04-02 08:00:00' & date <= '2015-04-06 11:00:00' ~ 'H7_nd2a',
                     ID == 'H7' & date >= '2015-04-07 17:00:00' & date <= '2015-04-11 10:00:00' ~ 'H7_nd2b',
                     ID == 'H7' & date >= '2015-04-20 11:00:00' & date <= '2015-04-30 13:30:00' ~ 'H7_nd3a',
                     ID == 'H7' & date >= '2015-05-03 05:30:00' & date <= '2015-05-03 09:00:00' ~ 'H7_nd3b',
                     ID == 'IAB' & date >= '2018-08-07 11:00:00' & date <= '2018-08-12 16:00:00' ~ 'IAB_nd1',
                     ID == 'IAB' & date >= '2019-03-26 11:00:00' & date <= '2019-03-28 11:00:00' ~ 'IAB_nd2',
                     ID == 'IAB' & date >= '2019-04-19 08:00:00' & date <= '2019-04-19 16:00:00' ~ 'IAB_nd3a',
                     ID == 'IAB' & date >= '2019-04-21 08:00:00' & date <= '2019-04-22 07:00:00' ~ 'IAB_nd3b',
                     ID == 'IAB' & date >= '2019-04-23 06:00:00' & date <= '2019-04-23 15:00:00' ~ 'IAB_nd3c',
                     ID == 'IAB' & date >= '2019-05-05 07:00:00' & date <= '2019-05-12 11:00:00' ~ 'IAB_nd4a',
                     ID == 'IAB' & date >= '2019-05-16 06:00:00' & date <= '2019-05-20 10:00:00' ~ 'IAB_nd4b',
                     ID == 'IAB' & date >= '2019-05-29 14:00:00' & date <= '2019-06-02 12:00:00' ~ 'IAB_nd5a',
                     ID == 'IAB' & date >= '2019-06-08 08:00:00' & date <= '2019-06-10 13:00:00' ~ 'IAB_nd5b',
                     ID == 'IAB' & date >= '2019-09-07 10:00:00' & date <= '2019-09-08 12:00:00' ~ 'IAB_nd6a',
                     ID == 'IAB' & date >= '2019-09-10 09:00:00' & date <= '2019-09-11 11:00:00' ~ 'IAB_nd6b',
                     ID == 'IAB' & date >= '2019-10-29 08:00:00' & date <= '2019-10-30 07:00:00' ~ 'IAB_nd7',
                     ID == 'IAB' & date >= '2020-03-16 08:00:00' & date <= '2020-03-19 16:00:00' ~ 'IAB_nd8',
                     ID == 'IAD' & date >= '2016-08-20 08:00:00' & date <= '2016-08-22 14:30:00' ~ 'IAD_nd1',
                     ID == 'IAD' & date >= '2018-02-05 06:00:00' & date <= '2018-02-07 16:00:00' ~ 'IAD_nd2',
                     ID == 'IAD' & date >= '2018-03-28 10:00:00' & date <= '2018-04-14 18:00:00' ~ 'IAD_nd3a',
                     ID == 'IAD' & date >= '2018-04-16 06:00:00' & date <= '2018-04-16 14:00:00' ~ 'IAD_nd3b',
                     ID == 'IAD' & date >= '2018-04-18 12:00:00' & date <= '2018-04-25 18:00:00' ~ 'IAD_nd3c',
                     ID == 'IAD' & date >= '2018-04-27 06:00:00' & date <= '2018-05-05 12:00:00' ~ 'IAD_nd3d',
                     ID == 'IAD' & date >= '2018-05-07 12:00:00' & date <= '2018-05-07 16:00:00' ~ 'IAD_nd3e',
                     ID == 'IAD' & date >= '2018-05-09 08:00:00' & date <= '2018-05-09 12:00:00' ~ 'IAD_nd3f',
                     ID == 'IAD' & date >= '2018-05-14 10:00:00' & date <= '2018-05-17 12:00:00' ~ 'IAD_nd3g',
                     ID == 'IAD' & date >= '2018-05-29 08:00:00' & date <= '2018-06-03 10:00:00' ~ 'IAD_nd4a',
                     ID == 'IAD' & date >= '2018-06-09 08:00:00' & date <= '2018-06-12 12:00:00' ~ 'IAD_nd4b',
                     ID == 'IAD' & date >= '2018-12-15 09:00:00' & date <= '2018-12-18 14:00:00' ~ 'IAD_nd5a',
                     ID == 'IAD' & date >= '2018-12-20 11:00:00' & date <= '2018-12-20 13:00:00' ~ 'IAD_nd5b',
                     ID == 'IAD' & date >= '2019-03-04 10:00:00' & date <= '2019-03-05 13:00:00' ~ 'IAD_nd6a',
                     ID == 'IAD' & date >= '2019-03-07 06:00:00' & date <= '2019-03-13 12:00:00' ~ 'IAD_nd6b',
                     ID == 'IAD' & date >= '2019-03-15 08:00:00' & date <= '2019-03-15 14:00:00' ~ 'IAD_nd6c',
                     ID == 'IAD' & date >= '2019-03-21 10:00:00' & date <= '2019-04-01 15:00:00' ~ 'IAD_nd6d',
                     ID == 'IAD' & date >= '2019-04-05 11:00:00' & date <= '2019-04-09 14:00:00' ~ 'IAD_nd6e',
                     ID == 'IAD' & date >= '2019-04-15 06:00:00' & date <= '2019-04-18 16:00:00' ~ 'IAD_nd6f',
                     ID == 'IAD' & date >= '2019-04-21 06:00:00' & date <= '2019-04-21 17:00:00' ~ 'IAD_nd6g',
                     ID == 'IAD' & date >= '2019-04-29 08:00:00' & date <= '2019-05-01 14:00:00' ~ 'IAD_nd7',
                     ID == 'IAD' & date >= '2019-07-08 09:00:00' & date <= '2019-07-08 17:00:00' ~ 'IAD_nd8a',
                     ID == 'IAD' & date >= '2019-07-11 08:00:00' & date <= '2019-07-15 14:00:00' ~ 'IAD_nd8b',
                     ID == 'IBH' & date >= '2020-08-07 06:17:15' & date <= '2020-08-07 17:17:15' ~ 'IBH_nd1a',
                     ID == 'IBH' & date >= '2020-08-09 07:19:13' & date <= '2020-08-13 13:14:35' ~ 'IBH_nd1b',
                     ID == 'IBH' & date >= '2020-08-15 08:25:49' & date <= '2020-08-15 11:25:28' ~ 'IBH_nd1c',
                     ID == 'IBH' & date >= '2022-04-09 06:47:05' & date <= '2022-04-11 15:37:18' ~ 'IBH_nd2',
                     ID == 'IBH' & date >= '2022-04-20 06:31:21' & date <= '2022-04-21 04:27:29' ~ 'IBH_nd3a',
                     ID == 'IBH' & date >= '2022-04-22 11:24:14' & date <= '2022-04-23 06:24:43' ~ 'IBH_nd3b',
                     ID == 'IBH' & date >= '2022-04-29 09:18:06' & date <= '2022-05-01 14:15:15' ~ 'IBH_nd4',
                     ID == 'IBH' & date >= '2022-05-04 09:00:00' & date <= '2022-05-04 12:12:25' ~ 'IBH_nd5',
                     ID == 'IBH' & date >= '2022-05-10 09:00:00' & date <= '2022-05-13 08:58:13' ~ 'IBH_nd6',
                     ID == 'IBI' & date >= '2017-07-21 07:00:00' & date <= '2017-07-25 14:30:00' ~ 'IBI_nd1a',
                     ID == 'IBI' & date >= '2017-07-27 12:00:00' & date <= '2017-07-27 17:00:00' ~ 'IBI_nd1b',
                     ID == 'IBI' & date >= '2017-08-19 07:00:00' & date <= '2017-08-20 12:00:00' ~ 'IBI_nd2',
                     ID == 'IBK' & date >= '2020-08-21 08:31:15' & date <= '2020-08-22 18:04:45' ~ 'IBK_nd1',
                     ID == 'IBK' & date >= '2021-06-27 08:43:09' & date <= '2021-06-30 12:45:35' ~ 'IBK_nd2',
                     ID == 'IBK' & date >= '2022-04-16 08:39:12' & date <= '2022-04-17 12:50:05' ~ 'IBK_nd3',
                     ID == 'IBS' & date >= '2020-07-27 06:00:00' & date <= '2020-07-28 15:00:00' ~ 'IBS_nd1',
                     ID == 'IBS' & date >= '2020-08-07 08:00:00' & date <= '2020-08-18 15:00:00' ~ 'IBS_nd2',
                     ID == 'IBS' & date >= '2021-01-14 08:00:00' & date <= '2021-01-15 12:00:00' ~ 'IBS_nd3',
                     ID == 'IBS' & date >= '2021-02-15 09:00:00' & date <= '2021-02-15 15:00:00' ~ 'IBS_nd4',
                     ID == 'IBS' & date >= '2021-04-26 10:00:33' & date <= '2021-04-27 16:00:11' ~ 'IBS_nd5a',
                     ID == 'IBS' & date >= '2021-04-30 14:00:28' & date <= '2021-05-01 11:00:08' ~ 'IBS_nd5b',
                     ID == 'IBS' & date >= '2022-03-22 06:00:34' & date <= '2022-03-26 12:00:11' ~ 'IBS_nd6a',
                     ID == 'IBS' & date >= '2022-03-28 06:00:17' & date <= '2022-04-01 09:00:18' ~ 'IBS_nd6b',
                     ID == 'IBS' & date >= '2022-04-05 06:00:52' & date <= '2022-04-09 18:00:19' ~ 'IBS_nd6c',
                     ID == 'IBS' & date >= '2022-04-16 06:00:13' & date <= '2022-04-18 18:00:11' ~ 'IBS_nd6d',
                     ID == 'IBS' & date >= '2022-04-22 06:00:17' & date <= '2022-04-28 12:00:04' ~ 'IBS_nd6e',
                     ID == 'IBS' & date >= '2022-05-04 06:00:27' & date <= '2022-05-05 09:00:31' ~ 'IBS_nd6f',
                     ID == 'IBS' & date >= '2022-05-07 12:00:17' & date <= '2022-05-17 12:00:18' ~ 'IBS_nd6g',
                     ID == 'IBS' & date >= '2022-05-28 06:00:15' & date <= '2022-05-28 18:00:53' ~ 'IBS_nd7a',
                     ID == 'IBS' & date >= '2022-05-30 06:00:17' & date <= '2022-05-30 18:00:15' ~ 'IBS_nd7b',
                     ID == 'IBS' & date >= '2022-06-02 06:00:29' & date <= '2022-06-04 12:00:18' ~ 'IBS_nd7c',
                     ID == 'IBS' & date >= '2023-02-10 13:03:31' & date <= '2023-02-14 21:00:21' ~ 'IBS_nd8a',
                     ID == 'IBS' & date >= '2023-02-19 04:00:00' & date <= '2023-02-20 12:00:25' ~ 'IBS_nd8b',
                     ID == 'IBS' & date >= '2023-02-23 06:00:34' & date <= '2023-02-23 15:00:34' ~ 'IBS_nd8c',
                     ID == 'IBS' & date >= '2023-03-02 06:00:37' & date <= '2023-03-03 12:00:18' ~ 'IBS_nd9a',
                     ID == 'IBS' & date >= '2023-03-06 06:00:15' & date <= '2023-03-08 15:00:17' ~ 'IBS_nd9b',
                     ID == 'IBS' & date >= '2023-03-13 06:00:12' & date <= '2023-03-16 12:00:04' ~ 'IBS_nd9c',
                     ID == 'ICZ' & date >= '2019-09-09 08:15:55' & date <= '2019-09-14 11:50:14' ~ 'ICZ_nd1',
                     ID == 'ICZ' & date >= '2020-04-11 11:13:23' & date <= '2020-04-21 06:28:21' ~ 'ICZ_nd2a',
                     ID == 'ICZ' & date >= '2020-04-23 08:53:32' & date <= '2020-04-23 14:53:33' ~ 'ICZ_nd2b',
                     ID == 'ICZ' & date >= '2020-05-02 13:09:29' & date <= '2020-05-05 14:06:04' ~ 'ICZ_nd3',
                     ID == 'IFP' & date >= '2022-07-23 09:00:31' & date <= '2022-07-24 12:00:07' ~ 'IFP_nd1a',
                     ID == 'IFP' & date >= '2022-07-26 06:00:32' & date <= '2022-07-28 15:00:22' ~ 'IFP_nd1b',
                     ID == 'IFP' & date >= '2023-04-24 06:00:31' & date <= '2023-04-29 12:00:37' ~ 'IFP_nd2',
                     ID == 'IFP' & date >= '2023-05-16 00:00:00' & date <= '2023-05-17 18:00:37' ~ 'IFP_nd3a',
                     ID == 'IFP' & date >= '2023-05-23 03:01:01' & date <= '2023-05-24 21:00:32' ~ 'IFP_nd3b',
                     ID == 'IFP' & date >= '2023-06-07 00:01:27' & date <= '2023-06-08 15:01:02' ~ 'IFP_nd3c'),
          day = as.Date(date),
          distKM = dist/1000
          tidyr::unite( ID_y, c( ID, year), sep='_', remove = F)%>%
          select(-c('pkey'))

### Old osprey_nd

osprey_nd <- osprey%>%
                  select(-c("day", "month", "year", "m_day", "ext_temp", "signal_interruption_cause", "death_comment"))%>%
                  unique()

osprey_nd <- osprey_nd%>%
                    filter(
                    ID == 'A7' & time >= '2015-08-14 08:00:00' & time <= '2015-08-18 09:00:00' |
                    ID == 'A7' & time >= '2017-02-20 06:00:00' & time <= '2017-03-01 12:00:00' |
                    ID == 'A7' & time >= '2017-03-17 00:00:00' & time <= '2017-04-15 06:00:00' |
                    ID == 'A7' & time >= '2017-04-16 24:00:00' & time <= '2017-04-21 08:00:00' |
                    ID == 'A7' & time >= '2017-04-28 00:00:00' & time <= '2017-05-11 06:00:00' |
                    ID == 'A7' & time >= '2017-05-14 00:00:00' & time <= '2017-05-21 18:00:00' |
                    ID == 'A7' & time >= '2017-05-23 06:00:00' & time <= '2017-05-23 18:00:00' |
                    ID == 'A7' & time >= '2017-05-26 00:00:00' & time <= '2017-05-28 24:00:00' |
                    ID == 'A7' & time >= '2017-06-06 00:00:00' & time <= '2017-06-06 16:00:00' |
                    ID == 'A7' & time >= '2017-06-10 20:00:00' & time <= '2017-06-12 20:00:00' |
                    ID == 'A7' & time >= '2017-07-12 18:00:00' & time <= '2017-07-28 06:00:00' |
                    ID == 'Antares' & time >= "2015-08-15 11:00:00" & time <= "2015-08-18 19:00:00" |
                    ID == "Antares" & time >= "2015-09-08 10:30:00" & time <= "2015-09-13 20:00:00" |
                    ID == "Antares" & time >= "2016-04-04 00:00:00" & time <= "2016-04-10 17:00:00" |
                    ID == "Antares" & time >= "2016-04-19 11:00:00" & time <= "2016-04-22 13:00:00" |
                    ID == "Antares" & time >= "2016-05-05 00:00:00" & time <= "2016-05-06 06:00:00" |
                    ID == "Antares" & time >= "2016-05-14 13:00:00" & time <= "2016-05-17 19:00:00" |
                    ID == "Antares" & time >= "2016-06-10 07:00:00" & time <= "2016-06-12 19:00:00" |
                    ID == 'CAM' & time >= "2016-04-14 06:00:00" & time <= "2016-04-19 20:00:00" |
                    ID == "CAM" & time >= "2016-05-03 08:00:00" & time <= "2016-05-06 18:00:00" |
                    ID == "CAM" & time >= "2016-05-20 05:00:00" & time <= "2016-05-24 18:00:00" | 
                    ID == "CAM" & time >= "2016-07-02 23:00:00" & time <= "2016-07-06 20:00:00" |
                    ID == 'CBK' & time >= '2013-08-15 00:00:00' & time <= '2013-08-21 12:00:00' |
                    ID == 'CBK' & time >= '2014-03-20 04:30:00' & time <= '2014-03-22 10:30:00' |
                    ID == 'CBK' & time >= '2014-04-08 06:30:00' & time <= '2014-04-12 11:00:00' |
                    ID == 'CIV' & time >= "2014-08-16 10:00:00" & time <= "2014-08-22 06:00:00" |
                    ID == 'CIV' & time >= "2014-09-11 08:00:00" & time <= "2014-09-13 17:00:00" |
                    ID == 'CIV' & time >= "2014-10-21 08:00:00" & time <= "2014-10-21 21:00:00" |
                    ID == 'CIV' & time >= "2015-03-21 02:00:00" & time <= "2015-03-21 24:00:00" |
                    ID == 'CIV' & time >= "2015-06-04 04:00:00" & time <= "2015-06-22 18:00:00" |
                    ID == 'CIV' & time >= "2015-08-13 06:00:00" & time <= "2015-08-14 00:00:00" |
                    ID == 'CIV' & time >= "2015-11-21 16:00:00" & time <= "2015-11-27 00:00:00" |
                    ID == 'CIV' & time >= '2016-03-28 18:00:00' & time <= '2016-04-01 06:00:00' |
                    ID == 'CIV' & time >= '2016-04-15 06:00:00' & time <= '2016-04-18 22:00:00' |
                    ID == 'CIV' & time >= '2016-10-28 06:00:00' & time <= '2016-10-30 06:00:00' |
                    ID == 'E7' & time >= '2014-08-21 09:00:00' & time <= '2014-08-27 13:00:00' |
                    ID == 'E7' & time >= '2016-03-10 05:00:00' & time <= '2016-03-24 14:00:00' |
                    ID == 'E7' & time >= '2016-03-30 09:00:00' & time <= '2016-04-22 20:00:00' |
                    ID == 'E7' & time >= '2016-04-24 10:30:00' & time <= '2016-04-25 17:30:00' |
                    ID == 'E7' & time >= '2016-05-04 10:00:00' & time <= '2016-05-07 17:00:00' |
                    ID == 'E7' & time >= '2016-05-12 11:00:00' & time <= '2016-05-16 20:30:00' |
                    ID == 'E7' & time >= '2016-05-20 12:00:00' & time <= '2016-05-27 18:30:00' |
                    ID == 'H7' & time >= '2013-08-04 09:30:00' & time <= '2013-08-09 15:30:00' |
                    ID == 'H7' & time >= '2015-04-02 05:00:00' & time <= '2015-04-11 13:00:00' |
                    ID == 'H7' & time >= '2015-04-20 13:00:00' & time <= '2015-04-30 16:00:00' |
                    ID == 'H7' & time >= '2015-05-03 05:00:00' & time <= '2015-05-03 11:00:00' |
                    ID == 'IAB' & time >= '2018-08-07 11:00:00' & time <= '2018-08-12 20:00:00' |
                    ID == 'IAB' & time >= '2019-03-26 11:00:00' & time <= '2019-03-28 14:00:00' |
                    ID == 'IAB' & time >= '2019-04-19 00:00:00' & time <= '2019-04-23 24:00:00' |
                    ID == 'IAB' & time >= '2019-05-05 06:00:00' & time <= '2019-05-12 14:00:00' |
                    ID == 'IAB' & time >= '2019-05-15 21:00:00' & time <= '2019-05-20 13:00:00' |
                    ID == 'IAB' & time >= '2019-05-29 15:00:00' & time <= '2019-06-02 16:00:00' |
                    ID == 'IAB' & time >= '2019-06-08 09:00:00' & time <= '2019-06-10 17:00:00' |
                    ID == 'IAB' & time >= '2019-09-07 11:00:00' & time <= '2019-09-11 15:00:00' |
                    ID == 'IAB' & time >= '2019-10-29 08:00:00' & time <= '2019-10-30 09:00:00' |
                    ID == 'IAB' & time >= '2020-03-16 00:00:00' |
                    ID == 'IAD' & time >= '2016-08-20 09:00:00' & time <= '2016-08-25 11:00:00' |
                    ID == 'IAD' & time >= '2018-02-04 18:00:00' & time <= '2018-02-07 19:00:00' |
                    ID == 'IAD' & time >= '2018-03-28 08:00:00' & time <= '2018-06-12 14:00:00' |
                    ID == 'IAD' & time >= '2018-12-15 00:00:00' & time <= '2018-12-20 15:00:00' |
                    ID == 'IAD' & time >= '2019-03-04 10:00:00' & time <= '2019-04-01 17:00:00' |
                    ID == 'IAD' & time >= '2019-04-05 10:00:00' & time <= '2019-04-09 17:00:00' |
                    ID == 'IAD' & time >= '2019-04-14 20:00:00' & time <= '2019-04-19 13:00:00' |
                    ID == 'IAD' & time >= '2019-04-21 06:00:00' & time <= '2019-04-21 20:00:00' |
                    ID == 'IAD' & time >= '2019-04-29 09:00:00' & time <= '2019-05-01 21:00:00' |
                    ID == 'IAD' & time >= '2019-07-08 09:00:00' & time <= '2019-07-15 18:00:00' |
                    ID == 'IBH' & time >= '2020-08-07 07:00:00' & time <= '2020-08-15 15:00:00' |
                    ID == 'IBH' & time >= '2022-04-09 06:00:00' & time <= '2022-04-11 19:00:00' |
                    ID == 'IBH' & time >= '2022-04-19 12:00:00' & time <= '2022-04-24 15:00:00' |
                    ID == 'IBH' & time >= '2022-04-29 09:00:00' & time <= '2022-05-01 18:00:00' |
                    ID == 'IBH' & time >= '2022-05-04 09:00:00' & time <= '2022-05-04 16:00:00' |
                    ID == 'IBH' & time >= '2022-05-10 09:00:00' & time <= '2022-05-13 17:00:00' |
                    ID == 'IBI' & time >= "2017-07-21 05:00:00" & time <= "2017-07-27 20:00:00" |
                    ID == "IBI" & time >= "2017-08-19 08:00:00" & time <= "2017-08-20 15:00:00" |
                    ID == 'IBK' & time >= '2020-08-20 09:00:00' & time <= '2020-08-23 11:00:00' |
                    ID == 'IBK' & time >= '2021-06-27 09:00:00' & time <= '2021-07-02 12:00:00' |
                    ID == 'IBK' & time >= '2022-04-15 00:00:00' & time <= '2022-04-17 20:00:00' |
                    ID == 'IBS' & time >= '2020-07-27 00:00:00' & time <= '2020-07-28 19:00:00' |
                    ID == 'IBS' & time >= "2020-08-07 08:00:00" & time <= "2020-08-18 19:00:00" |
                    ID == 'IBS' & time >= "2021-01-14 06:00:00" & time <= "2021-01-15 16:00:00" |
                    ID == 'IBS' & time >= "2021-02-15 09:00:00" & time <= "2021-02-15 17:00:00" |
                    ID == 'IBS' & time >= "2021-04-26 11:00:00" & time <= "2021-05-01 16:00:00" |
                    ID == 'IBS' & time >= '2022-03-22 00:00:00' & time <= '2022-06-04 15:00:00' |
                    ID == 'IBS' & time >= '2023-02-08 16:00:00' & time <= '2023-02-15 00:00:00' |
                    ID == "IBS" & time >= "2023-02-19 04:00:00" & time <= "2023-02-23 20:00:00" |
                    ID == "IBS" & time >= "2023-03-01 16:00:00" & time <= "2023-03-08 24:00:00" |
                    ID == "IBS" & time >= "2023-03-13 04:00:00" & time <= "2023-03-16 17:00:00" |
                    ID == 'ICZ' & time >= '2019-09-09 08:00:00' & time <= '2019-09-14 14:30:00' |
                    ID == 'ICZ' & time >= '2020-04-11 10:00:00' & time <= '2020-04-24 00:00:00' |
                    ID == 'ICZ' & time >= '2020-05-02 14:30:00' & time <= '2020-05-05 17:00:00' |
                    ID == 'IFP' & time >= '2022-07-23 06:00:00' & time <= '2022-08-15 00:00:00' |
                    ID == 'IFP' & time >= '2023-04-24 00:00:00' & time <= '2023-04-30 04:00:00' |
                    ID == "IFP" & time >= '2023-05-16 00:00:00' & time <= "2023-05-17 22:00:00" |
                    ID == "IFP" & time >= '2023-05-23 00:00:00' & time <= "2023-05-25 00:00:00" |
                    ID == "IFP" & time >= '2023-06-07 00:00:00' & time <= "2023-06-09 24:00:00" |
                    ID == "IFP" & time >= '2023-06-11 03:00:00' & time <= "2023-06-11 18:00:00" 
                    )
                 
### Old Osprey_nonb


# {osprey_nonb <- osprey%>%
          filter(ID == 'H7' & time >= '2013-08-09 15:30:00' & time <= '2015-04-02 05:00:00' |
                    ID == 'H7' & time >= '2015-04-11 13:00:00' & time <= '2015-04-20 13:00:00' |
                    ID == 'H7' & time >= '2015-04-30 16:00:00' & time <= '2015-05-03 05:00:00' |
                    ID == 'H7' & time >= '2015-05-03 11:00:0' |
                    ID == 'CIV' & time >= "2014-08-22 06:00:00" & time <= "2014-09-11 08:00:00" |
                    ID == 'CIV' & time >= "2014-09-13 17:00:00" & time <= "2014-10-21 08:00:00" |
                    ID == 'CIV' & time >= "2014-10-21 21:00:00" & time <= "2015-03-21 04:00:00" |
                    ID == 'CIV' & time >= "2015-03-21 24:00:00" & time <= "2015-06-04 04:00:00" |
                    ID == 'CIV' & time >= "2015-06-22 18:00:00" & time <= "2015-08-13 06:00:00" |
                    ID == 'CIV' & time >= "2015-08-14 00:00:00" & time <= "2015-11-21 16:00:00" |
                    ID == 'CIV' & time >= "2015-11-27 00:00:00" & time <= "2016-03-28 18:00:00" |
                    ID == 'CIV' & time >= '2016-04-01 06:00:00' & time <= '2016-04-15 06:00:00' |
                    ID == 'CIV' & time >= '2016-04-18 22:00:00' & time <= '2016-10-28 06:00:00' |
                    ID == 'CIV' & time >= '2016-10-30 06:00:00' |
                    ID == 'E7' & time >= '2014-08-27 13:00:00' & time <= '2016-03-10 05:00:00' |
                    ID == 'E7' & time >= '2016-03-24 14:00:00' & time <= '2016-03-30 09:00:00' |
                    ID == 'E7' & time >= '2016-04-22 20:00:00' & time <= '2016-04-24 10:30:00' |
                    ID == 'E7' & time >= '2016-04-25 17:30:00' & time <= '2016-05-04 10:00:00' |
                    ID == 'E7' & time >= '2016-05-07 17:00:00' & time <= '2016-05-12 11:00:00' |
                    ID == 'E7' & time >= '2016-05-16 20:30:00' & time <= '2016-05-20 12:00:00' |
                    ID == 'E7' & time >= '2016-05-27 18:30:00' |
                    ID == 'A7' & time >= '2015-08-18 12:00:00' & time <= '2017-02-20 00:00:00' |
                    ID == 'A7' & time >= '2017-03-01 20:00:00' & time <= '2017-03-17 00:00:00' |
                    ID == 'A7' & time >= '2017-04-15 06:00:00' & time <= '2017-04-16 24:00:00' |
                    ID == 'A7' & time >= '2017-04-21 08:00:00' & time <= '2017-04-28 00:00:00' |
                    ID == 'A7' & time >= '2017-05-11 06:00:00' & time <= '2017-05-14 00:00:00' |
                    ID == 'A7' & time >= '2017-05-21 18:00:00' & time <= '2017-05-23 06:00:00' |
                    ID == 'A7' & time >= '2017-05-23 18:00:00' & time <= '2017-05-26 00:00:00' |
                    ID == 'A7' & time >= '2017-05-28 24:00:00' & time <= '2017-06-06 00:00:00' |
                    ID == 'A7' & time >= '2017-06-06 16:00:00' & time <= '2017-06-10 20:00:00' |
                    ID == 'A7' & time >= '2017-06-12 20:00:00' & time <= '2017-07-12 18:00:00' |
                    ID == 'A7' & time >= '2017-07-28 06:00:00'|
                    ID == 'IAD' & time >= '2016-08-25 11:00:00' & time <= '2018-02-04 18:00:00' |
                    ID == 'IAD' & time >= '2018-02-07 19:00:00' & time <= '2018-03-28 08:00:00' |
                    ID == 'IAD' & time >= '2018-06-12 14:00:00' & time <= '2018-12-15 00:00:00' |
                    ID == 'IAD' & time >= '2018-12-20 15:00:00' & time <= '2019-03-04 10:00:00' |
                    ID == 'IAD' & time >= '2019-04-01 17:00:00' & time <= '2019-04-05 10:00:00' |
                    ID == 'IAD' & time >= '2019-04-09 17:00:00' & time <= '2019-04-14 20:00:00' |
                    ID == 'IAD' & time >= '2019-04-19 13:00:00' & time <= '2019-04-21 06:00:00' |
                    ID == 'IAD' & time >= '2019-04-21 20:00:00' & time <= '2019-04-29 09:00:00' |
                    ID == 'IAD' & time >= '2019-05-01 21:00:00' & time <= '2019-07-08 09:00:00' |
                    ID == 'IAD' & time >= '2019-07-15 18:00:00' |
                    ID == 'IBS' & time >= '2020-07-28 19:00:00' & time <= '2020-08-07 08:00:00' |
                    ID == 'IBS' & time >= '2020-08-18 19:00:00' & time <= '2021-01-14 06:00:00' |
                    ID == 'IBS' & time >= '2021-01-15 16:00:00' & time <= '2021-02-15 09:00:00' |
                    ID == 'IBS' & time >= '2021-02-15 17:00:00' & time <= '2021-04-26 11:00:00' |
                    ID == 'IBS' & time >= '2021-05-01 16:00:00' & time <= '2022-03-22 00:00:00' |
                    ID == 'IBS' & time >= '2022-06-04 15:00:00' & time <= '2023-02-08 16:00:00' |
                    ID == 'IBS' & time >= '2023-02-15 00:00:00' & time <= '2023-02-19 04:00:00' |
                    ID == 'IBS' & time >= '2023-02-23 20:00:00' & time <= '2023-03-01 16:00:00' |
                    ID == 'IBS' & time >= '2023-03-08 24:00:00' & time <= '2023-03-13 04:00:00' |
                    ID == 'IBS' & time >= '2023-03-16 17:00:00' |
                    ID == 'IBH' & time >= '2020-08-15 15:00:00' & time <= '2022-04-09 06:00:00' |
                    ID == 'IBH' & time >= '2022-04-11 19:00:00' & time <= '2022-04-19 12:00:00' |
                    ID == 'IBH' & time >= '2022-04-24 15:00:00' & time <= '2022-04-29 09:00:00' |
                    ID == 'IBH' & time >= '2022-05-01 18:00:00' & time <= '2022-05-04 09:00:00' |
                    ID == 'IBH' & time >= '2022-05-04 16:00:00' & time <= '2022-05-10 09:00:00' |
                    ID == 'IBH' & time >= '2022-05-13 17:00:00' |
                    ID == 'IBK' & time <= '2020-08-20 09:00:00' |
                    ID == 'IBK' & time >= '2020-08-23 11:00:00' & time <= '2021-06-27 09:00:00' |
                    ID == 'IBK' & time >= '2021-07-02 12:00:00' & time <= '2022-04-15 00:00:00' |
                    ID == 'IBK' & time >= '2022-04-17 20:00:00' |
                    ID == 'IFP' & time >= '2022-08-15 00:00:00' & time <= '2023-04-24 00:00:00' |
                    ID == 'IFP' & time >= '2023-04-30 04:00:00' & time <= '2023-05-16 00:00:00' |       
                    ID == "IFP" & time >= '2023-05-17 22:00:00' & time <= "2023-05-23 00:00:00" |
                    ID == "IFP" & time >= '2023-05-25 00:00:00' & time <= "2023-06-07 00:00:00" |
                    ID == "IFP" & time >= '2023-06-09 24:00:00' & time <= "2023-06-11 03:00:00" |
                    ID == "IFP" & time >= "2023-06-11 18:00:00" |
                    ID == 'ICZ' & time <= '2019-09-09 08:00:00' |
                    ID == 'ICZ' & time >= '2019-09-14 14:30:00' & time <= '2020-04-11 10:00:00' |
                    ID == 'ICZ' & time >= '2020-04-24 00:00:00' & time <= '2020-05-02 14:30:00' |
                    ID == 'ICZ' & time >= '2020-05-05 17:00:00' |
                    ID == 'CBK' & time >= '2013-08-21 12:00:00' & time <= '2014-03-20 04:30:00' |
                    ID == 'CBK' & time >= '2014-03-22 10:30:00' & time <= '2014-04-08 06:30:00' |
                    ID == 'CBK' & time >= '2014-04-12 11:00:00' |
                    ID == 'IAB' & time >= '2018-08-12 20:00:00' & time <= '2019-03-26 11:00:00' |
                    ID == 'IAB' & time >= '2019-03-28 14:00:00' & time <= '2019-04-19 00:00:00' |
                    ID == 'IAB' & time >= '2019-04-23 24:00:00' & time <= '2019-05-05 06:00:00' |
                    ID == 'IAB' & time >= '2019-05-12 14:00:00' & time <= '2019-05-15 21:00:00' |
                    ID == 'IAB' & time >= '2019-05-20 13:00:00' & time <= '2019-05-29 15:00:00' |
                    ID == 'IAB' & time >= '2019-06-02 16:00:00' & time <= '2019-06-08 09:00:00' |
                    ID == 'IAB' & time >= '2019-06-10 17:00:00' & time <= '2019-09-07 11:00:00' |
                    ID == 'IAB' & time >= '2019-09-11 15:00:00' & time <= '2019-10-29 08:00:00' |
                    ID == 'IAB' & time >= '2019-10-30 09:00:00' & time <= '2020-03-16 00:00:00' |
                    ID == "CAM" & time <= "2016-04-14 06:00:00" |
                    ID == "CAM" & time >= "2016-04-19 20:00:00" & time <= "2016-05-03 08:00:00" |
                    ID == "CAM" & time >= "2016-05-06 18:00:00" & time <= "2016-05-20 05:00:00" |
                    ID == "CAM" & time >= "2016-05-24 18:00:00" & time <= "2016-07-02 23:00:00" |
                    ID == "CAM" & time >= "2016-07-06 20:00:00" |
                    ID == "IBI" & time >= "2017-07-27 20:00:00" & time <= "2017-08-19 08:00:00" |
                    ID == "IBI" & time >= "2017-08-20 15:00:00" |
                    ID == "Antares" & time >= "2015-08-18 19:00:00" & time <= "2015-09-08 10:30:00" |
                    ID == "Antares" & time >= "2015-09-13 20:00:00" & time <= "2016-04-04 00:00:00" |
                    ID == "Antares" & time >= "2016-04-10 17:00:00" & time <= "2016-04-19 11:00:00" |
                    ID == "Antares" & time >= "2016-04-22 13:00:00" & time <= "2016-05-05 00:00:00" |
                    ID == "Antares" & time >= "2016-05-06 06:00:00" & time <= "2016-05-14 13:00:00" |
                    ID == "Antares" & time >= "2016-05-17 19:00:00" & time <= "2016-06-10 07:00:00" |
                    ID == "Antares" & time >= "2016-06-12 19:00:00")%>%
                    tidyr::unite(id_y, c(ID, year), sep="_", remove = F)}
                    
          

                 
