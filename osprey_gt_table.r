       

# gt_table code

              data_blank <- osprey_summary1

              data_blank <- sapply(data_blank, as.character)
              data_blank[is.na(data_blank)] <- ""                     # Replace NA with blank
              data_blank  

              # constants ----
              n = 0
              c_col = c("#1e3048", "#274060", "#2f5375", "#4073a0", "#5088b9")
              c_col_light_blue = c("#edf2fb", "#e2eafc", "#d7e3fc", "#ccdbfd", "#c1d3fe")
              c_container_width = px(800)
              c_table_width = px(650)
              c_rn = 30
              c_save = TRUE
              c_format = "pdf"

              # Questo pezzo di codice serve per organizzare i dati in base ai valori di una colonna
              # in questo caso raggruppa i falchi pescatori per nome

              gt_season <- osprey_summary1 %>% 
                arrange(id) %>% 
                gt(
                  groupname_col = "id",
                  rowname_col = "season"
                )

              # Usa il codice seguente per modicificare il nome delle colonne

              gt_season <- gt_season %>%
                tab_header(
                  title = md("Number of fix per season per individual"),
                  subtitle = md("Project: Osprey in Mediterranean (Corsica, Italy, Balearics)"))%>%
                cols_label(
                  id = md("Name"),
                  season = md("Season"),
                  n = md("Number of fix")
                )

              gt_season <- gt_season %>% 
                #fmt_date(
                # columns = c("start", "end"),
                #) %>% 
                fmt_number(
                  columns = c("n"),
                  suffixing = F,
                  decimals = 0,
                  sep_mark = ""
                ) %>% 
                cols_align(
                  align = "center",
                  columns = vars(c("n"))
                ) %>% 
                cols_align(
                  align = "right",
                  columns = vars(c("season"))
                ) %>% 
                # cols_align(
                #   align = "left",
                #   columns = vars(c("))
                # ) %>% 
                cols_width(
                  #vars(c("")) ~ px(150),
                  vars("n") ~ px(200),
                  vars("season") ~ px(300),
                  vars("id") ~ px(370),
                ) %>%
                opt_row_striping()

              # let's add summary statistics
              gt_season <- gt_season%>%
                summary_rows(
                  groups = T,
                  columns = vars("n"),
                  fns = list(AVERAGE = "mean"),
                  formatter = fmt_number,
                  decimals = 0,
                  sep_mark = ""
                )


              gt_season <- gt_season %>%
                tab_options(
                  table.font.name = "Calibri",
                  table.font.color = c_col[1],
                  table.border.top.style = "none",
                  table.border.bottom.style = "solid",
                  table.border.bottom.color = c_col[2],
                  table.border.bottom.width = px(3),
                  column_labels.border.top.color = "white",
                  column_labels.border.top.width = px(3),
                  column_labels.border.bottom.color = c_col[2],
                  column_labels.border.bottom.width = px(3),
                  data_row.padding = px(10)
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(38),
                      weight = "normal",
                      align = "left",
                      font = "Calibri"
                    )
                  ),
                  locations = list(
                    cells_title(groups = "title")
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(18),
                      align = "left"
                    )
                  ),
                  locations = list(
                    cells_title(groups = "subtitle")
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(17)
                    ),
                    cell_borders(
                      sides = c("bottom", "top"),
                      color = c_col[1],
                      weight = px(1)
                    )
                  ),
                  locations = list(
                    cells_body(gt::everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      color = "#000000",
                      weight = "bold",
                      font = "Calibri"
                    )
                  ),
                  locations = list(
                    cells_column_labels(everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      color = "#000000",
                      font = "Calibri"
                    ),
                    cell_borders(
                      sides = c("bottom"),
                      style = "solid",
                      color = c_col[1],
                      weight = px(2)
                    )
                  ),
                  locations = list(
                    cells_row_groups(gt::everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      align = "right",
                      weight = "normal",
                      color = "#000000",
                      font = "Calibri"
                    ),
                    cell_borders(
                      sides = c("bottom", "right"),
                      style = "solid",
                      color = "white",
                      weight = px(1)
                    )
                  ),
                  locations = list(
                    cells_stub(gt::everything()),
                    cells_stubhead()
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      font = "Calibri", size = px(10), 
                      color = "#000000")
                  ),
                  location = list(
                    cells_body(columns = vars(id))
                  )
                ) 

gt_season

gtsave(gt_season, "seasons_fix_gt.html")

# create a table that shows monitoring's duration per individual grouped by signal interrumption cause
osprey_summary2 <- osprey %>%
  group_by(id, signal_interruption_cause) %>% 
  summarize(start = min(timestamp), end = max(timestamp)) %>%
  mutate(duration = round(difftime(end, start)))%>%
  arrange(desc(signal_interruption_cause))

osprey_summary2$duration <- as.numeric(osprey_summary2$duration)

# gt_table code

              data_blank <- osprey_summary2

              data_blank <- sapply(data_blank, as.character)
              data_blank[is.na(data_blank)] <- ""                     # Replace NA with blank
              data_blank  

              # constants ----
              n = 0
              c_col = c("#1e3048", "#274060", "#2f5375", "#4073a0", "#5088b9")
              c_col_light_blue = c("#edf2fb", "#e2eafc", "#d7e3fc", "#ccdbfd", "#c1d3fe")
              c_container_width = px(800)
              c_table_width = px(650)
              c_rn = 30
              c_save = TRUE
              c_format = "pdf"

              # Questo pezzo di codice serve per organizzare i dati in base ai valori di una colonna
              # in questo caso raggruppa i falchi pescatori per la causa di interruzione del segnale GPS

              gt_duration <- osprey_summary2 %>% 
                arrange(id) %>% 
                gt(
                  groupname_col = "signal_interruption_cause",
                  rowname_col = "id"
                )

              # Usa il codice seguente per modicificare il nome delle colonne

              gt_duration <- gt_duration %>%
                tab_header(
                  title = md("Monitoring's duration per individual"),
                  subtitle = md("Osprey in Mediterranean (Corsica, Italy, Balearics)"))%>%
               cols_label(
                 id = md("Name"),
                 signal_interruption_cause = md("Signal interruption cause"),
                 start = md("First location date"),
                 end = md("Last location date"),
                 duration = md("Total days of monitoring")
               )

              gt_duration <- gt_duration %>% 
                #fmt_date(
                # columns = c("start", "end"),
                #) %>% 
                fmt_number(
                  columns = c("duration"),
                  suffixing = F,
                  decimals = 0,
                  sep_mark = ""
                ) %>% 
                cols_align(
                  align = "center",
                  columns = vars(c("start", "end",
                                   "signal_interruption_cause", "duration"))
                ) %>% 
                cols_align(
                 align = "right",
                 columns = vars(c("id"))
                ) %>% 
                # cols_align(
                #   align = "left",
                #   columns = vars(c("))
                # ) %>% 
                cols_width(
                  #vars(c("")) ~ px(150),
                  vars("start", "end", "signal_interruption_cause") ~ px(200),
                  vars("duration") ~ px(200),
                  vars("id") ~ px(370),
                ) %>%
                opt_row_striping()

              # let's add summary statistics
              gt_duration <- gt_duration%>%
                  summary_rows(
                    groups = T,
                    columns = vars("duration"),
                  fns = list(AVERAGE = "mean"),
                  formatter = fmt_number,
                  decimals = 0
                  )


              gt_duration <- gt_duration %>%
                tab_options(
                  table.font.name = "Calibri",
                  table.font.color = c_col[1],
                  table.border.top.style = "none",
                  table.border.bottom.style = "solid",
                  table.border.bottom.color = c_col[2],
                  table.border.bottom.width = px(3),
                  column_labels.border.top.color = "white",
                  column_labels.border.top.width = px(3),
                  column_labels.border.bottom.color = c_col[2],
                  column_labels.border.bottom.width = px(3),
                  data_row.padding = px(10)
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(38),
                      weight = "normal",
                      align = "left",
                      font = "Calibri"
                    )
                  ),
                  locations = list(
                    cells_title(groups = "title")
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(18),
                      align = "left"
                    )
                  ),
                  locations = list(
                    cells_title(groups = "subtitle")
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      size = px(17)
                    ),
                    cell_borders(
                      sides = c("bottom", "top"),
                      color = c_col[1],
                      weight = px(1)
                    )
                  ),
                  locations = list(
                    cells_body(gt::everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      color = "#000000",
                      weight = "bold",
                      font = "Calibri"
                    )
                  ),
                  locations = list(
                    cells_column_labels(everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      color = "#000000",
                      font = "Calibri"
                    ),
                    cell_borders(
                      sides = c("bottom"),
                      style = "solid",
                      color = c_col[1],
                      weight = px(2)
                    )
                  ),
                  locations = list(
                    cells_row_groups(gt::everything())
                  )
                ) %>% 
                tab_style(
                  style = list( 
                    cell_text(
                      size = px(18),
                      align = "right",
                      weight = "normal",
                      color = "#000000",
                      font = "Calibri"
                    ),
                    cell_borders(
                      sides = c("bottom", "right"),
                      style = "solid",
                      color = "white",
                      weight = px(1)
                    )
                  ),
                  locations = list(
                    cells_stub(gt::everything()),
                    cells_stubhead()
                  )
                ) %>% 
                tab_style(
                  style = list(
                    cell_text(
                      font = "Calibri", size = px(10), 
                      color = "#000000")
                  ),
                  location = list(
                    cells_body(columns = vars(signal_interruption_cause))
                  )
                ) 

gt_duration

gtsave(gt_duration, "duration_monit_gt.html")
