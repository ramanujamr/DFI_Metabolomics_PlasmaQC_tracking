# Plasma QC tracking Server

server <- function(input, output, session) {
  
  df_categories = read.csv("categories.csv") %>% 
    mutate(compound_name = tolower(compound_name))
  
  file_mnt = list.files(wddir, pattern="*Maintenance*")[[1]]
  df_mnt = read.csv(file.path(wddir, file_mnt)) %>%
    mutate(sampleid = "MNT",
           date_run = Date,
           batch = NA,
           compound_name = "MNT",
           peakarea = -10,
           category=NA) %>%
    mutate(label = paste0(Operator, " - ", Instrument)) %>%
    dplyr::select(sampleid, date_run, batch, compound_name, peakarea, category, label)
  
  df_plasmaqc = data.frame(matrix(ncol = 6, nrow = 0))
  colnames(df_plasmaqc) = c("sampleid", "date_run", "batch", "compound_name", "peakarea", "category")

  files_tms = list.files(wddir, pattern="*TMS*")
  for(filename in files_tms)
  {
    filename = file.path(wddir, filename)
    temp = Function_readin_csv_2(filename, 1000) %>%
      filter(conc=="concentrated") %>%
      filter(grepl("plasma", sampleid, ignore.case = T)) %>%
      dplyr::select(sampleid, date_run, batch, compound_name, peakarea) %>%
      mutate(compound_name = gsub("_.*", "", compound_name)) %>%
      right_join(df_categories, by="compound_name")

    df_plasmaqc = rbind(df_plasmaqc, temp)
  }


  df_plasmaqc = df_plasmaqc %>%
    arrange(date_run) %>% 
    mutate(label = paste0(batch, " - ", sampleid))

  fig_tms1 = plot_ly(rbind(df_plasmaqc %>% filter(category == "High Abundance"), df_mnt %>% mutate(category = "High Abundance")),
                     x = ~date_run, y = ~peakarea, text = ~label, color = ~compound_name,
                     type = 'scatter', mode = 'lines+markers') %>% 
    layout(title = 'High Abundance')
                  
  
  fig_tms2 = plot_ly(rbind(df_plasmaqc %>% filter(category == "Medium Abundance"), df_mnt %>% mutate(category = "Medium Abundance")),
                 x = ~date_run, y = ~peakarea, text = ~label, color = ~compound_name,
                 type = 'scatter', mode = 'markers', name = 'Medium Abundance') %>% 
    layout(title = 'Medium Abundance')

  fig_tms3 = plot_ly(rbind(df_plasmaqc %>% filter(category=="Low Abundance"), df_mnt %>% mutate(category = "Low Abundance")),
                 x = ~date_run, y = ~peakarea, text = ~label, color = ~compound_name,
                 type = 'scatter', mode = 'lines+markers', name = 'Low Abundance') %>% 
    layout(title = 'Low Abundance')
                  


  output$plot_tms1 <- renderPlotly({fig_tms1})
  output$plot_tms2 <- renderPlotly({fig_tms2})
  output$plot_tms3 <- renderPlotly({fig_tms3})
      
}