#'@importFrom magrittr "%>%"
#'@export
yuxi_sleeplog <- function(path, id, first) {
  
  
  log <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(log, paste(path, "sleeplog.rds", sep="/"))
  
  # remind yuxi that it's not recursive
  filenames = intersect(list.files(path = path, full.names = T, recursive = F), 
                        list.files(path = path, pattern = ".csv", full.names = T, recursive = F))
  

  # file <- read.csv("/Users/phoebelam/Desktop/yuxi/Dissertation-Morning Day 1-Updated_June 28, 2024_06.20.csv")
  
  
  # loop
  for (f in filenames) {
    
    print (f)
    
    file<-read.csv(f)
    
    # remind yuxi to not change the name of the survey ever
    
    # basename("/Users/phoebelam/Desktop/yuxi/Dissertation-Morning Day 1-Updated_June 28, 2024_06.20.csv") %>%
    #   gsub ("Dissertation-Morning Day |-", " ", .) %>%
    #   substr(., 1, 3) -> daynum
    
    basename(f) %>%
      gsub ("Dissertation-Morning Day |-", " ", .) %>%
      substr(., 1, 3) -> daynum
    
    # grab id - need to tell yuxi about external reference + do not change the name of the ID column after this
    file %>% 
      dplyr::rename (pid = PID) %>%
      dplyr::mutate(qualtrics_day = daynum) %>%
      dplyr::select (., qualtrics_day, StartDate, EndDate, pid, BedTime.1_1:WakeTime.3_1) %>%
      dplyr::filter (pid == id)-> file
  
    # grab bed time and wake time
    file %>%
      dplyr::mutate(bedtime = paste(paste(BedTime.1_1, BedTime.2_1, sep=":"), BedTime.3_1, sep=" "),
             waketime = paste(paste(WakeTime.1_1, WakeTime.2_1, sep=":"), WakeTime.3_1, sep=" ")) -> file
  

    # remind yuxi do not open the .csv from the zip
    file %>% tidyr::separate (EndDate, c("eDate", "eTime"), " ", fill = "right", remove= FALSE) %>%
      dplyr::mutate(date = as.Date(eDate, "%Y-%m-%d")-1) %>%
      dplyr::rename(reportdate = eDate) -> file
    
    # trim
    file %>% dplyr::select(., pid, qualtrics_day, date, bedtime, waketime, reportdate, EndDate) -> file
    
   
    # consolidate
    log <- readRDS (paste(path, "/sleeplog.rds", sep=""))
    log <- suppressWarnings(gtools::smartbind (log, file))
    saveRDS (log, paste(path, "/sleeplog.rds", sep=""))

  }
  
  log <- readRDS (paste(path, "/sleeplog.rds", sep="")) [-1,-1]
  
  log %>%
    mutate(qualtrics_day = as.numeric(qualtrics_day)) %>%
    arrange(qualtrics_day) -> log
  
  if (nrow (log) > 0) {
    
    
    #compliance alert
    # first meaning the first date that are supposed to do MORNING diary
    should <- c(as.Date(first), as.Date(first)+1, as.Date(first)+2, as.Date(first)+3, as.Date(first)+4, as.Date(first)+5, 
    as.Date(first)+6, as.Date(first)+7, as.Date(first)+8, as.Date(first)+9)
    
    data.frame(qualtrics_day = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
               shoulddate = should) -> should
    
    log %>% dplyr::mutate(qualtrics_day = as.numeric(qualtrics_day)) -> log
    merge(log, should, by = 'qualtrics_day', all=T) -> log
    
    log %>%
      dplyr::mutate_at(dplyr::vars(shoulddate, reportdate),
                       list(~as.Date(.))) %>% 
      dplyr::mutate(compliance = dplyr::case_when(shoulddate-reportdate==0~ 'ok',
                                                  reportdate-shoulddate == 1 & grepl('am', bedtime)==T~ 'ok',
                                                  reportdate-shoulddate == 1 & grepl('pm', bedtime)==T~ 'maybe late',
                                                  reportdate-shoulddate > 1 ~ 'definitely late',
                                                  is.na(reportdate)==T~ 'missing')) -> log
    
    #binging alert
    # binge defined by two entries done within 2 hours
    log %>%
      dplyr::mutate(EndDate = as.POSIXct(log$EndDate),
                    reportdate_diff = abs(as.numeric(difftime(EndDate, dplyr::lag(EndDate), units = 'mins'))),
                    reportdate_diff2 = abs(as.numeric(difftime(EndDate, dplyr::lead(EndDate), units = 'mins')))) %>% 
      dplyr::mutate(binge = dplyr::case_when(reportdate_diff < 120 | reportdate_diff2 < 120~ 'likely binge',
                                             is.na(reportdate_diff)==T & is.na(reportdate_diff2)==T~ NA_character_,
                                             TRUE~ 'ok')) -> log
    
    # rename for output purpose
    log %>%
      dplyr::rename(pid = pid,
             `qualtrics diary day` = qualtrics_day,
             `alleged sleep date` = date,
             `date reported sleep` = reportdate,
             `date should have reported sleep`= shoulddate) %>%
      dplyr::select(-c(reportdate_diff, reportdate_diff2, EndDate))-> log
    
    # output
    openxlsx::write.xlsx(log, paste(path, "/sleeplog_", id, ".xlsx", sep=""))
    
    return('done, please check folder')
    
  }else {
    return ("no sleep log generated: do not have at least one entry")
 
  }
  
}
  
  
# library(dplyr)
# yuxi_sleeplog(path = '/Users/phoebelam/Desktop/yuxi',
#               id = '30624114831',
#               first = '2024-03-21')

