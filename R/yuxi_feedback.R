#'@importFrom magrittr "%>%"
#'@export
yuxi_feedback <- function (path, pid, first) {
  
  filenames = intersect(list.files(path = path, pattern = "scored sleep", full.names = T, recursive = F), 
                        list.files(path = path, pattern = ".csv", full.names = T, recursive = F))
  
  dat <- read.csv(filenames, header = T)
  
  #dat <- read.csv('/Users/phoebelam/Desktop/yuxi/Yuxi_0205coded_Yuxi.csv', header = T)
  #dat <- read.csv('/Users/yuxixie/Downloads/yuxi_feedback form/Yuxi_0205coded_Yuxi.csv', header = T)
  
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
    ggplot2::ggplot( aes(x=startdate_adj, y=duration)) +
    ggplot2::geom_line(color="#A9A9A9", aes(group=1), linewidth = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.8) +
    ggplot2::geom_text(aes(label = duration), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Sleep duration: Hours of actual sleep") +
    ggplot2::ylab('') +
    ggplot2::ylim(min(dat$duration)-1, max(dat$duration)+1) +
    ggplot2::scale_x_date("", date_breaks = "days" , date_labels = "%b-%d")+
    theme(plot.title=element_text(face="bold", size = 15)) -> a
  
  # sleep efficiency
  dat %>% 
    ggplot2::ggplot( aes(x=startdate_adj, y=sleepeff)) +
    ggplot2::geom_line(color="#A9A9A9", aes(group=1), linewidth = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.8) +
    ggplot2::geom_text(aes(label = sleepeff), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Sleep efficiency: Percent of time sleeping while in bed") +
    ggplot2::ylab('') +
    ggplot2::ylim(min(dat$sleepeff)-3, 105) +
    ggplot2::scale_x_date("", date_breaks = "days" , date_labels = "%b-%d")+
    ggplot2::theme(plot.title=element_text(face="bold", size = 15)) -> b
  
  #latency
  dat %>% 
    ggplot2::ggplot( aes(x=startdate_adj, y=latency)) +
    ggplot2::geom_line(color="#A9A9A9", aes(group=1), size = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(aes(label = latency), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Number of minutes it took to fall asleep") +
    ggplot2::ylab('') +
    ggplot2::ylim(min(dat$latency)-3, max(dat$latency)+3)+
    ggplot2::scale_x_date("", date_breaks = "days" , date_labels = "%b-%d")+
    ggplot2::theme(plot.title=element_text(face="bold", size = 15)) -> c
  
  #awakening minutes total
  # dat %>% 
  #   ggplot2::ggplot( aes(x=startdate_adj, y=awakening)) +
  #   ggplot2::geom_line(color="#A9A9A9", aes(group=1), size = 1) +
  #   ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
  #   ggplot2::geom_text(aes(label = awakening), hjust=0.5, vjust=-1, size = 4) +
  #   ggplot2::theme_minimal() + 
  #   ggplot2::ggtitle("Awakenings (in minutes)") +
  #   ggplot2::ylab('') +
  #   ggplot2::ylim(min(dat$awakening)-3, max(dat$awakening)+3) +
  #   ggplot2::scale_x_date("In-Bed Date", date_breaks = "days" , date_labels = "%b-%d")
  
  #awakening number
  dat %>% 
    ggplot2::ggplot( aes(x=startdate_adj, y=Number.of.Awakenings)) +
    ggplot2::geom_line(color="#A9A9A9", aes(group=1), size = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(aes(label = Number.of.Awakenings), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Number of awakenings") +
    ggplot2::xlab('') +
    ggplot2::ylab('') +
    ggplot2::ylim(min(dat$Number.of.Awakenings)-3, max(dat$Number.of.Awakenings)+3) +
    ggplot2::scale_x_date("\nIn-Bed Date", date_breaks = "days" , date_labels = "%b-%d") +
    ggplot2::theme(plot.title=element_text(face="bold", size = 15))-> d
  
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
    ggplot2::geom_line(aes(group = startdate_adj), size = 1, color = '#A9A9A9') +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(aes(label = format(time, format = "%H:%M")), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Sleep Schedule: Actual asleep and awake times") +
    ggplot2::scale_y_time(labels = labelling_fn) +
    ggplot2::scale_x_date("\nIn-Bed Date", date_breaks = "days" , date_labels = "%b-%d")+
    ggplot2::theme(plot.title=element_text(face="bold", size = 15),
                   axis.text.y=element_blank(), 
                   axis.ticks.y=element_blank()) +
    ggplot2::labs(y = "") + 
    ggplot2::scale_y_reverse() +
    ggplot2::ylim(min(long$time_r2)-4000, max(long$time_r2)+4000)-> e
  
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
  log <- data.frame(pid = 999,
                    qualtrics_day = 999, 
                    reportdate = as.Date('2024-01-01', '%Y-%m-%d'),
                    pss_mean = 999)
  
  saveRDS(log, paste(path, "night.rds", sep="/"))
  
  filenames = intersect(list.files(path = path, pattern = "Dissertation-Night Day", full.names = T, recursive = F), 
                        list.files(path = path, pattern = ".csv", full.names = T, recursive = F))
  
  # file <- read.csv("/Users/phoebelam/Desktop/yuxi/Dissertation-Night Day 1_March 2, 2024_22.27.csv" )
  
  # file <- read.csv('/Users/phoebelam/Desktop/yuxi/Dissertation-Night Day 1-Updated_April 3, 2024_12.20.csv')
  # file <- read.csv('/Users/phoebelam/Desktop/yuxi/Dissertation-Night Day 2-Updated_April 3, 2024_13.08.csv')
  
  
  # loop
  for (f in filenames) {
    
    print (f)
    
    file<-read.csv(f)
    
    # remind yuxi to not change the name of the survey ever
    basename(f) %>%
      gsub ("Dissertation-Night Day |_", " ", .) %>%
      substr(., 1, 3) -> daynum
    
    # basename('/Users/phoebelam/Desktop/yuxi/Dissertation-Night Day 2-Updated_April 3, 2024_13.08.csv') %>%
    #   gsub ("Dissertation-Night Day |_", " ", .) %>%
    #   substr(., 1, 3) -> daynum
    
    # grab id - need to tell yuxi about external reference + do not change the name of the ID column after this
    file %>% 
      dplyr::mutate(qualtrics_day = daynum) %>%
      dplyr::select (., PID, qualtrics_day, PSS_1:PSS_5, EndDate) %>%
      dplyr::filter (PID == pid) %>%
      dplyr::rename(pid = PID)-> file
    
    # score mean
    file %>%
      dplyr::mutate_at(vars(PSS_1, PSS_2),
                       list(r = ~(as.numeric(.)-6)*-1)) %>%
      dplyr::mutate_at(vars(PSS_3, PSS_4, PSS_5),
                       list(~as.numeric(.))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(pss_mean = hablar::mean_(c(PSS_1_r, PSS_2_r, PSS_3, PSS_4, PSS_5))) %>%
      dplyr::ungroup() -> file
    
    file %>% tidyr::separate (EndDate, c("eDate", "eTime"), " ", fill = "right", remove= FALSE) %>% 
      dplyr::rename(reportdate = eDate) -> file
    
    # trim
    file %>% dplyr::select(., pid, qualtrics_day, reportdate, pss_mean) -> file
    
    # consolidate
    log <- readRDS (paste(path, "/night.rds", sep=""))
    log <- rbind(log, file)
    saveRDS (log, paste(path, "/night.rds", sep=""))
    
  }
  
  
  log <- readRDS (paste(path, "/night.rds", sep="")) [-1,-1] %>%
    dplyr::mutate(qualtrics_day = as.numeric( gsub('-', '', qualtrics_day, ignore.case = T))) %>% arrange(qualtrics_day)
  
  # use of "should" date instead of actual report date in case of late entries
  should <- c(as.Date(first), as.Date(first)+1, as.Date(first)+2, as.Date(first)+3, as.Date(first)+4, as.Date(first)+5, 
              as.Date(first)+6, as.Date(first)+7, as.Date(first)+8, as.Date(first)+9)
  
  data.frame(qualtrics_day = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
             shoulddate = should) -> should
  
  merge(log, should, by = 'qualtrics_day', all=T) -> log
  
  # create pomp scores for easier interpretation 
  log %>%
    dplyr::mutate(pss_pomp = 100*((pss_mean-1)/(5-1))) -> log
  
  log %>% 
    ggplot2::ggplot( aes(x=shoulddate, y=pss_pomp)) +
    ggplot2::geom_line(color="#A9A9A9", aes(group=1), size = 1) +
    ggplot2::geom_point(shape=21, color="#637A9F", fill="#637A9F", size=3, alpha=.9) +
    ggplot2::geom_text(aes(label = pss_pomp), hjust=0.5, vjust=-1, size = 4) +
    ggplot2::theme_minimal() + 
    ggplot2::ggtitle("Perceived stress level (0-100%)") +
    ggplot2::xlab('') +
    ggplot2::ylab('') +
    ggplot2::ylim(0, 110) +
    ggplot2::scale_x_date("\nIn-Bed Date", date_breaks = "days" , date_labels = "%b-%d") +
    ggplot2::geom_hline(yintercept=mean(log$pss_pomp), linetype="dashed", color = "#637A9F", size = .8) +
    ggplot2::xlim(min(log$shoulddate), max(log$shoulddate)+.1) +
    ggplot2::annotate("text", x= log$shoulddate[nrow(log)]-0.3, y = mean(log$pss_pomp)-3,
             label = paste("Your average"), color = '#637A9F', size = 4, fontface = 'bold') +
    ggplot2::theme(plot.title=element_text(face="bold", size = 15)) ->g
  
  
  # pdf("/Users/phoebelam/Library/CloudStorage/GoogleDrive-phoebela@andrew.cmu.edu/My Drive/3_obs/students/committee/yuxi/figures/fig3.pdf",
  #     width = 9.75, height = 5.32)
  
  pdf(paste(path, "/", pid, "/fig3.pdf", sep=""),
       width = 9.75, height = 5.32)
  print(g)
  dev.off()
  
  #stress and sleep merge
  #startdate_adj from sleep
  #should date from stress
  merge(dplyr::select(dat, startdate_adj, sleepeff, latency, awakening, duration, Number.of.Awakenings), dplyr::select(log, shoulddate, everything()), 
        by.x = 'startdate_adj', by.y= 'shoulddate', all=T) -> new
  
  new %>%
    dplyr::mutate(pss_pomp_10 = pss_pomp/10) -> new

  
  
  sleepcorr <- data.frame(type = c('Sleep duration: \nMinutes of actual sleep', 
                                   'Sleep efficiency: Percent of \ntime sleeping while in bed', 
                                   'Number of minutes \nit took to fall asleep', 
                                   'Number of awakenings'),
                          coef = c(lm(duration~pss_pomp_10, data = new)$coefficients[2]*60,
                                   lm(sleepeff~pss_pomp_10, data = new)$coefficients[2],
                                   lm(latency~pss_pomp_10, data = new)$coefficients[2],
                                   lm(Number.of.Awakenings~pss_pomp_10, data = new)$coefficients[2])) %>%
    dplyr::mutate(sign = as.character(sign(coef)),
           label = dplyr::case_when(sign == -1 & type == 'Sleep duration: \nMinutes of actual sleep'~ paste('Decreases by\n', abs(round(coef, 1)), ' minutes', sep=""),
                             sign == 1 & type == 'Sleep duration: \nMinutes of actual sleep'~ paste('Increases by\n', abs(round(coef, 1)), ' minutes', sep=""),
                             sign == -1 & type == 'Sleep efficiency: Percent of \ntime sleeping while in bed'~ paste('Decreases by\n', abs(round(coef, 2)), '%', sep=""),
                             sign == 1 & type == 'Sleep efficiency: Percent of \ntime sleeping while in bed'~ paste('Increases by\n', abs(round(coef, 2)), '%', sep=""),
                             sign == -1 & type == 'Number of minutes \nit took to fall asleep'~ paste('Decreases by\n', abs(round(coef, 1)), ' minutes', sep=""),
                             sign == 1 & type == 'Number of minutes \nit took to fall asleep'~ paste('Increases by\n', abs(round(coef, 1)), ' minutes', sep=""),
                             sign == -1 & type == 'Number of awakenings'~ paste('Decreases by\n', abs(round(coef, 1)), ' times', sep=""),
                             sign == 1 & type == 'Number of awakenings'~ paste('Increases by\n', abs(round(coef, 1)), ' times', sep=""))) %>%
    dplyr::mutate(label_pos = dplyr::case_when(sign == 1~ label),
           label_neg = dplyr::case_when(sign == -1~ label))
  
  
  
  ggplot2::ggplot(sleepcorr, aes(x=type, y=coef, fill = sign)) + 
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::xlab("") +
    ggplot2::ggtitle("For every 10% increase in perceived stress...") +
    ggplot2::ylab("") +
    ggplot2::ylim(min(sleepcorr$coef, na.rm=T)-40, max(sleepcorr$coef, na.rm=T)+40) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +    ggplot2::scale_fill_manual(values=c("#e8a093", 
                               "#637A9F")) +
    ggplot2::theme(legend.position = "none",
          axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(),
          text = element_text(size = 14),
          plot.title = element_text(hjust = .5, face="bold")) +
    ggplot2::geom_label(
      aes(label = label_neg), 
      hjust = 1,
      size = 4, fontface = "bold",
      fill = "white", label.size = 0) +
    ggplot2::geom_label(
      aes(label = label_pos), 
      hjust = -.1,
      size = 4, fontface = "bold",
      fill = "white", label.size = 0) -> corr
  
  # pdf("/Users/phoebelam/Library/CloudStorage/GoogleDrive-phoebela@andrew.cmu.edu/My Drive/3_obs/students/committee/yuxi/figures/fig4.pdf",
  #     width = 9.75, height = 5.32)
  
  pdf(paste(path, "/", pid, "/fig4.pdf", sep=""),
       width = 9.75, height = 5.32)
  print(corr)
  dev.off()
  
  return('Done exporting figures, please check folder')
  
  
}

# library(formattable)
# library(htmltools)
# library(webshot)
# yuxi_feedback(path = '/Users/phoebelam/Desktop/yuxi',
#               id = 101,
#               first = '2024-02-20')



