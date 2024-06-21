#'@importFrom magrittr "%>%"
#'@export
rth_feedback <- function (path, pid, first) {
  
  filenames = intersect(list.files(path = path, pattern = "scored sleep", full.names = T, recursive = F), 
                        list.files(path = path, pattern = ".csv", full.names = T, recursive = F))
  
  dat <- read.csv(filenames, header = T)
  
  dat %>% 
    dplyr::mutate(sleepeff = round(Sleep.Percent.Onset.to.Offset, 1),
           latency = Sleep.Onset.Latency..mins.,
           startdate_adj = dplyr::case_when(as.Date(End.Date, "%m/%d/%Y") -as.Date(Start.Date, "%m/%d/%Y")==0~ as.Date(Start.Date, "%m/%d/%Y")-1,
                                     TRUE~ as.Date(Start.Date, "%m/%d/%Y")),
           awakening = round(Mean.Awakening..mins.*Number.of.Awakenings, 2),
           duration = round(Sleep.Time..mins./60, 1)) -> dat
  
  # summary table
  dat %>%
    dplyr::select(., Total.Time..mins., Sleep.Time..mins., sleepeff, Number.of.Awakenings, latency) %>%
    psych::describe(.) %>%
    as.data.frame(.) %>% 
    dplyr::select(., mean, min, max) %>% 
    t() %>% 
    as.data.frame(.) %>% 
    dplyr::mutate(`Hours in bed` = Total.Time..mins./60,
           `Hours of actual sleep` = Sleep.Time..mins./60) %>% 
    dplyr::rename(`Sleep efficiency: Percent of time sleeping while in bed` = sleepeff,
           `Number of awakenings` = Number.of.Awakenings,
           `Number of minutes it took to fall asleep` = latency) %>% 
    dplyr::select(., `Hours in bed` , `Hours of actual sleep`, `Sleep efficiency: Percent of time sleeping while in bed`, 
           `Number of minutes it took to fall asleep`, `Number of awakenings`) %>%
    t() %>% 
    as.data.frame(.) %>%
    tibble::rownames_to_column(.) %>% 
    dplyr::rename(`Sleep characteristics` = 1) %>% 
    dplyr::mutate_at(vars(mean, min, max),
              list(~round(., 1))) %>%
    dplyr::rename(`Average across days` = mean,
           `Minimum value across days` = min,
           `Maximum value across days` = max)-> summary
  
  formattable(summary, 
              align ="l", 
              list(`Sleep characteristics` = formatter(
                "span", style = ~ style(color = "grey",font.weight = "bold")) 
              )) -> table
  
  export_formattable <- function(f, file, width = "60%", height = NULL, 
                                 background = "white", delay = 0.2)
  {
    w <- as.htmlwidget(f, width = width, height = height)
    path <- html_print(w, background = background, viewer = NULL)
    url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
    webshot(url,
            file = file,
            selector = ".formattable_widget",
            delay = delay)
  }
  
  
  if (file.exists(paste(path, "/", pid, sep=""))){
    
    export_formattable(table, paste(path, "/", pid, "/table.pdf", sep=""))
    
  } else {
    
    dir.create(paste(path, "/", pid, sep=""))
    export_formattable(table, paste(path, "/", pid, "/table.pdf", sep=""))
    
  }
  
  
  
  
  # export_formattable(table,"/Users/phoebelam/Library/CloudStorage/GoogleDrive-phoebela@andrew.cmu.edu/My Drive/3_obs/students/committee/yuxi/figures/table.pdf")
  
  # figures
  # sleep duration
  dat %>% 
    ggplot2::ggplot( ggplot2::aes(x=startdate_adj, y=duration)) +
    ggplot2::geom_line(color="#A9A9A9", ggplot2::aes(group=1), linewidth = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.8) +
    ggplot2::geom_text(ggplot2::aes(label = duration), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Sleep duration: Hours of actual sleep") +
    ggplot2::ylab('') +
    ggplot2::ylim(min(dat$duration, na.rm=T)-1, max(dat$duration, na.rm=T)+1) +
    ggplot2::scale_x_date("", date_breaks = "days" , date_labels = "%b-%d")+
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", size = 15)) -> a
  
  
  # sleep efficiency
  dat %>% 
    ggplot2::ggplot( ggplot2::aes(x=startdate_adj, y=sleepeff)) +
    ggplot2::geom_line(color="#A9A9A9", ggplot2::aes(group=1), linewidth = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.8) +
    ggplot2::geom_text(ggplot2::aes(label = sleepeff), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Sleep efficiency: Percent of time sleeping while in bed") +
    ggplot2::ylab('') +
    ggplot2::ylim(min(dat$sleepeff, na.rm=T)-3, 105) +
    ggplot2::scale_x_date("", date_breaks = "days" , date_labels = "%b-%d")+
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", size = 15)) -> b
  
  #latency
  dat %>% 
    ggplot2::ggplot( ggplot2::aes(x=startdate_adj, y=latency)) +
    ggplot2::geom_line(color="#A9A9A9", ggplot2::aes(group=1), size = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(ggplot2::aes(label = latency), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Number of minutes it took to fall asleep") +
    ggplot2::ylab('') +
    ggplot2::ylim(min(dat$latency, na.rm=T)-3, max(dat$latency, na.rm=T)+3)+
    ggplot2::scale_x_date("", date_breaks = "days" , date_labels = "%b-%d")+
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", size = 15)) -> c
  
  #awakening minutes total
  # dat %>% 
  #   ggplot2::ggplot( ggplot2::aes(x=startdate_adj, y=awakening)) +
  #   ggplot2::geom_line(color="#A9A9A9", ggplot2::aes(group=1), size = 1) +
  #   ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
  #   ggplot2::geom_text(ggplot2::aes(label = awakening), hjust=0.5, vjust=-1, size = 4) +
  #   ggplot2::theme_minimal() + 
  #   ggplot2::ggtitle("Awakenings (in minutes)") +
  #   ggplot2::ylab('') +
  #   ggplot2::ylim(min(dat$awakening)-3, max(dat$awakening)+3) +
  #   ggplot2::scale_x_date("In-Bed Date", date_breaks = "days" , date_labels = "%b-%d")
  
  #awakening number
  dat %>% 
    ggplot2::ggplot( ggplot2::aes(x=startdate_adj, y=Number.of.Awakenings)) +
    ggplot2::geom_line(color="#A9A9A9", ggplot2::aes(group=1), size = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(ggplot2::aes(label = Number.of.Awakenings), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Number of awakenings") +
    ggplot2::xlab('') +
    ggplot2::ylab('') +
    ggplot2::ylim(min(dat$Number.of.Awakenings, na.rm=T)-3, max(dat$Number.of.Awakenings, na.rm=T)+3) +
    ggplot2::scale_x_date("\nIn-Bed Date", date_breaks = "days" , date_labels = "%b-%d") +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", size = 15))-> d
  
  # pdf("/Users/phoebelam/Library/CloudStorage/GoogleDrive-phoebela@andrew.cmu.edu/My Drive/3_obs/students/committee/yuxi/figures/fig1.pdf",
  #     width = 9.75, height = 10.64)
  
  pdf(paste(path, "/", pid, "/fig1.pdf", sep=""),
      width = 9.75, height = 10.64)
  print(ggpubr::ggarrange(a, b, c, d, ncol=1))
  dev.off()
  
  #sleep onset and offset time
  dat$First.Onset.Time2 = as.POSIXct(paste(dat$startdate_adj, dat$First.Onset.Time),
                                     format="%Y-%m-%d %H:%M:%S")
  dat$Last.Offset.Time2 = as.POSIXct(paste(dat$startdate_adj,dat$Last.Offset.Time),
                                     format="%Y-%m-%d %H:%M:%S")
  
  labelling_fn <- function(x) {
    (as.numeric(x + 43200) %% 86400) %>%
      as_hms()                      %>%
      substr(1, 5)
  }
  
  dat %>%
    dplyr::select(startdate_adj, First.Onset.Time2, Last.Offset.Time2) %>%
    tidyr::pivot_longer(.,
                        cols = c(First.Onset.Time2, Last.Offset.Time2),
                        names_to = "type",
                        values_to = "time") %>%
    dplyr::mutate(time_r = hms::as_hms(time - lubridate::hours(12)),
                  time_r2 = as.numeric(time_r +1)) -> long
  
  long %>% 
    ggplot2::ggplot(ggplot2::aes(x=startdate_adj, y=time_r, group = type)) +
    ggplot2::geom_line(ggplot2::aes(group = startdate_adj), size = 1, color = '#A9A9A9') +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(ggplot2::aes(label = format(time, format = "%H:%M")), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Sleep Schedule: Actual asleep and awake times") +
    ggplot2::scale_y_time(labels = labelling_fn) +
    ggplot2::scale_x_date("\nIn-Bed Date", date_breaks = "days" , date_labels = "%b-%d")+
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", size = 15),
                   axis.text.y=ggplot2::element_blank(), 
                   axis.ticks.y=ggplot2::element_blank()) +
    ggplot2::labs(y = "") + 
    ggplot2::scale_y_reverse() +
    ggplot2::ylim(min(long$time_r2, na.rm=T)-4000, max(long$time_r2, na.rm=T)+4000)-> e
  
  # pdf("/Users/phoebelam/Library/CloudStorage/GoogleDrive-phoebela@andrew.cmu.edu/My Drive/3_obs/students/committee/yuxi/figures/fig2.pdf",
  #     width = 9.75, height = 5.32)
  
  pdf(paste(path, "/", pid, "/fig2.pdf", sep=""),
       width = 9.75, height = 5.32)
  print(e)
  dev.off()
  
  
  # night diaries
  # path = '/Users/phoebelam/Desktop/yuxi'
  # id = 101
  # first = '2024-02-20'
  log <- data.frame(id = 999,
                    qualtrics_day = 999, 
                    reportdate = as.Date('2024-01-01', '%Y-%m-%d'),
                    stressor_sum = 999,
                    stressor_dc = 999,
                    negemo_pomp = 999,
                    posemo_pomp = 999)
  
  saveRDS(log, paste(path, "night.rds", sep="/"))
  
  filenames = intersect(list.files(path = path, pattern = "RTH_ya_daily", full.names = T, recursive = F), 
                        list.files(path = path, pattern = ".csv", full.names = T, recursive = F))
  
  # file <- read.csv("/Users/phoebelam/Desktop/rth/RTH_ya_daily_1_June+20,+2024_17.05.csv")
  
  
  # loop
  for (f in filenames) {
    
    print (f)
    
    file<-read.csv(f)
    
    basename(f) %>%
      gsub ("RTH_ya_daily_", "", .) %>%
      substr(., 1, 2) %>%
      gsub("_", "", .)-> daynum
    
    # basename("/Users/phoebelam/Desktop/rth/RTH_ya_daily_1_June+20,+2024_17.05.csv") %>%
    #   gsub ("RTH_ya_daily_", "", .) %>%
    #   substr(., 1, 2) %>%
    #   gsub("_", "", .)-> daynum
    
    file %>%
      dplyr::mutate(qualtrics_day = daynum) %>%
      dplyr::filter (ExternalReference == pid) %>% 
      dplyr::select(., ExternalReference, qualtrics_day, FI_3:FI_11, NFDI_2:NFDI_8,
                    SI_1:SI_6, WI_1:WI_8, DI_1, DI_2, 
                    PANAS_1:PANAS_23, EndDate) %>%
      dplyr::rename(id = ExternalReference) -> file

    
    # rename and score variables
    file %>%
      dplyr::mutate_at(vars(FI_3:FI_11, NFDI_2:NFDI_8,
                            SI_1:SI_6, WI_1:WI_8, DI_1, DI_2),
                       list(~as.numeric(as.character(.)))) %>%
      dplyr::mutate_at(vars(FI_3:FI_11, NFDI_2:NFDI_8,
                            SI_1:SI_6, WI_1:WI_8, DI_1, DI_2),
                       list(~case_when(is.na(EndDate)==F & is.na(.)==T~ 0,
                                       TRUE~.))) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(stressor_sum = hablar::sum_(c(FI_3, FI_4, FI_5, FI_6, FI_7,
                                                  FI_8, FI_9, FI_10, FI_11, NFDI_2, 
                                                  NFDI_3, NFDI_4, NFDI_5, NFDI_6, NFDI_7,
                                                  NFDI_8, SI_1, SI_2, SI_3, SI_4, SI_5, SI_6,
                                                  WI_1, WI_2, WI_3, WI_4, WI_5, WI_6, WI_7,
                                                  WI_8, DI_1, DI_2))) %>%
      ungroup() %>%
      dplyr::mutate(stressor_dc = case_when(stressor_sum > 0 ~ 1,
                                            TRUE~ 0)) -> file
    
    # emotion
    file %>%
      dplyr::mutate_at(vars(PANAS_1:PANAS_23),
                       list(~as.numeric(as.character(.)))) %>%
      dplyr::mutate_at(vars(PANAS_1:PANAS_23),
                       list(~case_when(.==9~ NA_real_,
                                       TRUE~.))) %>%
      dplyr::mutate(negemo_mean = rowMeans(select(., PANAS_1, PANAS_2, PANAS_3, PANAS_7, PANAS_11, PANAS_12, PANAS_13,
                                                  PANAS_15, PANAS_16, PANAS_18, PANAS_20, PANAS_21, PANAS_23), na.rm=T),
                    posemo_mean = rowMeans(select(., PANAS_4, PANAS_5, PANAS_6, PANAS_8, PANAS_9, PANAS_10, PANAS_14, 
                                                  PANAS_17, PANAS_19, PANAS_22), na.rm=T)) %>%
      dplyr::mutate(negemo_pomp = 100*(negemo_mean - 1)/(5-1),
                    posemo_pomp = 100*(posemo_mean - 1)/(5-1))-> file
    

    file %>% tidyr::separate (EndDate, c("eDate", "eTime"), " ", fill = "right", remove= FALSE) %>% 
      dplyr::rename(reportdate = eDate) -> file
    
    
    # trim
    file %>% dplyr::select(., id, qualtrics_day, reportdate, stressor_sum, stressor_dc, negemo_pomp, posemo_pomp) -> file

    
    # consolidate
    log <- readRDS (paste(path, "/night.rds", sep=""))
    log <- rbind(log, file)
    saveRDS (log, paste(path, "/night.rds", sep=""))
    
  }
  
  
  log <- readRDS (paste(path, "/night.rds", sep="")) [-1,-1] %>%
    dplyr::mutate(qualtrics_day = as.numeric( gsub('-', '', qualtrics_day, ignore.case = T))) %>% arrange(qualtrics_day)
  
  # use of "should" date instead of actual report date in case of late entries
  should <- c(as.Date(first), as.Date(first)+1, as.Date(first)+2, as.Date(first)+3, as.Date(first)+4, as.Date(first)+5, 
              as.Date(first)+6, as.Date(first)+7, as.Date(first)+8, as.Date(first)+9, as.Date(first)+10, as.Date(first)+11, 
              as.Date(first)+12, as.Date(first)+13, as.Date(first)+14)
  
  data.frame(qualtrics_day = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
             shoulddate = should) -> should
  
  merge(log, should, by = 'qualtrics_day', all=T) -> log

  
  # number of stressors
  log %>% 
    ggplot2::ggplot(ggplot2::aes(x=shoulddate, y=stressor_sum)) +
    ggplot2::geom_line(color="#A9A9A9", ggplot2::aes(group=1), size = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(ggplot2::aes(label = stressor_sum), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Number of stressors") +
    ggplot2::xlab('') +
    ggplot2::ylab('') +
    ggplot2::ylim(min(log$stressor_sum, na.rm=T)-2, max(log$stressor_sum, na.rm=T)+2) +
    ggplot2::scale_x_date("\nDate", date_breaks = "days" , date_labels = "%b-%d") +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", size = 15)) -> g
  
  pdf(paste(path, "/", pid, "/fig3.pdf", sep=""),
      width = 9.75, height = 5.32)
  print(g)
  dev.off()

  
  # positive and negative emotions 
  log %>%
    mutate_at(vars(negemo_pomp, posemo_pomp),
              list(~round(., 0))) -> log
  
  log %>% 
    ggplot2::ggplot(ggplot2::aes(x=shoulddate, y=posemo_pomp)) +
    ggplot2::geom_line(color="#A9A9A9", ggplot2::aes(group=1), size = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(ggplot2::aes(label = posemo_pomp), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Pleasant emotions (0-100%)") +
    ggplot2::xlab('') +
    ggplot2::ylab('') +
    ggplot2::ylim(0, 110) +
    ggplot2::scale_x_date("\nDate", date_breaks = "1 day" , date_labels = "%b-%d") +
    ggplot2::geom_hline(yintercept=mean(log$posemo_pomp), linetype="dashed", color = "#637A9F", size = .8) +
    ggplot2::annotate("text", x= log$shoulddate[nrow(log)]-0.3, y = mean(log$posemo_pomp)-3,
                      label = paste("Your average"), color = '#637A9F', size = 4, fontface = 'bold') +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", size = 15)) -> h
  
  
  log %>% 
    ggplot2::ggplot(ggplot2::aes(x=shoulddate, y=negemo_pomp)) +
    ggplot2::geom_line(color="#A9A9A9", ggplot2::aes(group=1), size = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(ggplot2::aes(label = negemo_pomp), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Unpleasant emotions (0-100%)") +
    ggplot2::xlab('') +
    ggplot2::ylab('') +
    ggplot2::ylim(0, 110) +
    ggplot2::scale_x_date("\nDate", date_breaks = "days" , date_labels = "%b-%d") +
    ggplot2::geom_hline(yintercept=mean(log$negemo_pomp), linetype="dashed", color = "#637A9F", size = .8) +
    ggplot2::annotate("text", x= log$shoulddate[nrow(log)]-0.3, y = mean(log$negemo_pomp)-3,
             label = paste("Your average"), color = '#637A9F', size = 4, fontface = 'bold') +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold", size = 15)) ->i
  
  
  pdf(paste(path, "/", pid, "/fig4.pdf", sep=""),
      width = 9.75, height = 10.64)
  print(ggpubr::ggarrange(h, i, ncol=1))
  dev.off()
  
  #stress and sleep merge
  #startdate_adj from sleep
  #should date from stress
  merge(dplyr::select(dat, startdate_adj, sleepeff, latency, awakening, duration, Number.of.Awakenings), dplyr::select(log, shoulddate, everything()), 
        by.x = 'startdate_adj', by.y= 'shoulddate', all=T) -> new
  
  new %>%
    dplyr::mutate(negemo_pomp_10 = negemo_pomp/10) -> new
  
  sleepcorr <- data.frame(type = c('Sleep duration:\nMinutes of actual sleep', 
                                   'Sleep efficiency: Percent of\ntime sleeping while in bed', 
                                   'Number of minutes\nit took to fall asleep', 
                                   'Number of awakenings'),
                          coef = c(lm(duration~negemo_pomp_10, data = new)$coefficients[2]*60,
                                   lm(sleepeff~negemo_pomp_10, data = new)$coefficients[2],
                                   lm(latency~negemo_pomp_10, data = new)$coefficients[2],
                                   lm(Number.of.Awakenings~negemo_pomp_10, data = new)$coefficients[2])) %>%
    dplyr::mutate(sign = as.character(sign(coef)),
           label = dplyr::case_when(sign == -1 & type == 'Sleep duration:\nMinutes of actual sleep'~ paste('Decreased by\n', abs(round(coef, 1)), ' minutes', sep=""),
                             sign == 1 & type == 'Sleep duration:\nMinutes of actual sleep'~ paste('Increased by\n', abs(round(coef, 1)), ' minutes', sep=""),
                             sign == -1 & type == 'Sleep efficiency: Percent of\ntime sleeping while in bed'~ paste('Decreased by\n', abs(round(coef, 2)), '%', sep=""),
                             sign == 1 & type == 'Sleep efficiency: Percent of\ntime sleeping while in bed'~ paste('Increased by\n', abs(round(coef, 2)), '%', sep=""),
                             sign == -1 & type == 'Number of minutes\nit took to fall asleep'~ paste('Decreased by\n', abs(round(coef, 1)), ' minutes', sep=""),
                             sign == 1 & type == 'Number of minutes\nit took to fall asleep'~ paste('Increased by\n', abs(round(coef, 1)), ' minutes', sep=""),
                             sign == -1 & type == 'Number of awakenings'~ paste('Decreased by\n', abs(round(coef, 1)), ' times', sep=""),
                             sign == 1 & type == 'Number of awakenings'~ paste('Increased by\n', abs(round(coef, 1)), ' times', sep=""))) %>%
    dplyr::mutate(label_pos = dplyr::case_when(sign == 1~ label),
                  label_neg = dplyr::case_when(sign == -1~ label))
  
  
  
  ggplot2::ggplot(sleepcorr, ggplot2::aes(x=type, y=coef, fill = sign)) + 
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("") +
    ggplot2::ggtitle("For every 10% increase in unpleasant emotions...") +
    ggplot2::ylab("") +
    ggplot2::ylim(min(sleepcorr$coef, na.rm=T)-40, max(sleepcorr$coef, na.rm=T)+40) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values=c("#e8a093", 
                               "#637A9F")) +
    ggplot2::theme(legend.position = "none",
          axis.text.x=ggplot2::element_blank(), 
          axis.ticks.x=ggplot2::element_blank(),
          text = ggplot2::element_text(size = 14),
          plot.title = ggplot2::element_text(hjust = .5, face="bold")) +
    ggplot2::geom_label(
      ggplot2::aes(label = label_neg), 
      hjust = 1,
      size = 4, fontface = "bold",
      fill = "white", label.size = 0) +
    ggplot2::geom_label(
      ggplot2::aes(label = label_pos), 
      hjust = -.1,
      size = 4, fontface = "bold",
      fill = "white", label.size = 0) -> corr
  
  pdf(paste(path, "/", pid, "/fig6.pdf", sep=""),
      width = 9.75, height = 5.32)
  print(corr)
  dev.off()
  
  
  sleepcorr2 <- data.frame(type = c('Sleep duration:\nMinutes of actual sleep', 
                                   'Sleep efficiency: Percent of\ntime sleeping while in bed', 
                                   'Number of minutes\nit took to fall asleep', 
                                   'Number of awakenings'),
                          coef = c(lm(duration~stressor_dc, data = new)$coefficients[2]*60,
                                   lm(sleepeff~stressor_dc, data = new)$coefficients[2],
                                   lm(latency~stressor_dc, data = new)$coefficients[2],
                                   lm(Number.of.Awakenings~stressor_dc, data = new)$coefficients[2])) %>%
    dplyr::mutate(sign = as.character(sign(coef)),
                  label = dplyr::case_when(sign == -1 & type == 'Sleep duration:\nMinutes of actual sleep'~ paste('Decreased by\n', abs(round(coef, 1)), ' minutes', sep=""),
                                           sign == 1 & type == 'Sleep duration:\nMinutes of actual sleep'~ paste('Increased by\n', abs(round(coef, 1)), ' minutes', sep=""),
                                           sign == -1 & type == 'Sleep efficiency: Percent of\ntime sleeping while in bed'~ paste('Decreased by\n', abs(round(coef, 2)), '%', sep=""),
                                           sign == 1 & type == 'Sleep efficiency: Percent of\ntime sleeping while in bed'~ paste('Increased by\n', abs(round(coef, 2)), '%', sep=""),
                                           sign == -1 & type == 'Number of minutes\nit took to fall asleep'~ paste('Decreased by\n', abs(round(coef, 1)), ' minutes', sep=""),
                                           sign == 1 & type == 'Number of minutes\nit took to fall asleep'~ paste('Increased by\n', abs(round(coef, 1)), ' minutes', sep=""),
                                           sign == -1 & type == 'Number of awakenings'~ paste('Decreased by\n', abs(round(coef, 1)), ' times', sep=""),
                                           sign == 1 & type == 'Number of awakenings'~ paste('Increased by\n', abs(round(coef, 1)), ' times', sep=""))) %>%
    dplyr::mutate(label_pos = dplyr::case_when(sign == 1~ label),
                  label_neg = dplyr::case_when(sign == -1~ label))
  
  
  
  ggplot2::ggplot(sleepcorr2, ggplot2::aes(x=type, y=coef, fill = sign)) + 
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("") +
    ggplot2::ggtitle("Compared to non-stressor days, on stressor days...") +
    ggplot2::ylab("") +
    ggplot2::ylim(min(sleepcorr2$coef, na.rm=T)-40, max(sleepcorr2$coef, na.rm=T)+40) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values=c("#e8a093", 
                                        "#637A9F")) +
    ggplot2::theme(legend.position = "none",
                   axis.text.x=ggplot2::element_blank(), 
                   axis.ticks.x=ggplot2::element_blank(),
                   text = ggplot2::element_text(size = 14),
                   plot.title = ggplot2::element_text(hjust = .5, face="bold")) +
    ggplot2::geom_label(
      ggplot2::aes(label = label_neg), 
      hjust = 1,
      size = 4, fontface = "bold",
      fill = "white", label.size = 0) +
    ggplot2::geom_label(
      ggplot2::aes(label = label_pos), 
      hjust = -.1,
      size = 4, fontface = "bold",
      fill = "white", label.size = 0) -> corr2
  
  
  pdf(paste(path, "/", pid, "/fig5.pdf", sep=""),
      width = 9.75, height = 5.32)
  print(corr2)
  dev.off()
  
  return('Done exporting figures, please check folder')
  
  
}

# library(formattable)
# library(htmltools)
# library(webshot)
# rth_feedback(path = '/Users/phoebelam/Desktop/rth',
#               pid = 571422,
#               first = '2024-05-06')



