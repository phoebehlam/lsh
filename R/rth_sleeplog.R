#'@importFrom magrittr "%>%"
#'@export
rth_sleeplog <- function(path, pid, first) {
  
  
  log <- data.frame(matrix(ncol = 1, nrow = 1))
  saveRDS(log, paste(path, "sleeplog.rds", sep="/"))
  
  filenames = intersect(list.files(path = path, full.names = T, recursive = F), 
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
      dplyr::select(., ExternalReference, qualtrics_day, StartDate, EndDate, bedtime.1_1:outbed.3_1) %>%
      dplyr::rename(id = ExternalReference) -> file

    # set am/pm
    file %>%
      dplyr::mutate(bedtime.ampm = case_when(bedtime.2_1 == 1~ 'am',
                                             bedtime.2_1 == 2~ 'pm'),
                    waketime.ampm = case_when(outbed.2_1 == 1~ 'am',
                                              outbed.2_1 == 2~ 'pm')) -> file
  
    # grab bed time and wake time
    file %>%
      dplyr::mutate(bedtime = paste(paste(bedtime.1_1, bedtime.3_1, sep=":"), bedtime.ampm, sep=" "),
             waketime = paste(paste(outbed.1_1, outbed.3_1, sep=":"), waketime.ampm, sep=" ")) -> file
  
    # grab reference date
    file %>% tidyr::separate (EndDate, c("eDate", "eTime"), " ", fill = "right", remove= FALSE) %>%
      dplyr::mutate(date = as.Date(eDate, "%Y-%m-%d")-1) %>%
      dplyr::rename(reportdate = eDate) -> file
    
    # trim
    file %>% dplyr::select(., id, qualtrics_day, date, bedtime, waketime, reportdate, EndDate) -> file
    
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
    as.Date(first)+6, as.Date(first)+7, as.Date(first)+8, as.Date(first)+9, as.Date(first)+10, as.Date(first)+11, 
    as.Date(first)+12, as.Date(first)+13, as.Date(first)+14)
    
    data.frame(qualtrics_day = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
               shoulddate = should) -> should
    
    log %>% dplyr::mutate(qualtrics_day = as.numeric(qualtrics_day)) -> log
    merge(log, should, by = 'qualtrics_day', all=T) -> log
    
    log %>%
      dplyr::mutate_at(dplyr::vars(shoulddate, reportdate),
                       list(~as.Date(.))) %>% 
      dplyr::mutate(compliance = dplyr::case_when(shoulddate-reportdate==0~ 'ok',
                                                  reportdate-shoulddate == 1 & grepl('am', bedtime)==T~ 'ok',
                                                  reportdate-shoulddate == 1 & grepl('pm', bedtime)==T~ 'maybe late',
                                                  is.na(reportdate)==T~ 'missing')) -> log
    
    # binging alert
    # binge defined by two entries done within 2 hours
    log %>%
      dplyr::mutate(EndDate = as.POSIXct(log$EndDate),
                    reportdate_diff = abs(as.numeric(difftime(EndDate, dplyr::lag(EndDate), units = 'mins'))),
                    reportdate_diff2 = abs(as.numeric(difftime(EndDate, dplyr::lead(EndDate), units = 'mins')))) %>% 
      dplyr::mutate(binge = dplyr::case_when(reportdate_diff < 120 | reportdate_diff2 < 120~ 'maybe binge',
                                             is.na(reportdate_diff)==T & is.na(reportdate_diff2)==T~ NA_character_,
                                             TRUE~ 'ok')) -> log
      
    
    # rename for output purpose
    log %>%
      dplyr::rename(id = id,
             `qualtrics diary day` = qualtrics_day,
             `alleged sleep date` = date,
             `date reported sleep` = reportdate,
             `date should have reported sleep`= shoulddate) %>%
      dplyr::select(-c(reportdate_diff, reportdate_diff2, EndDate)) -> log
    
    # output
    openxlsx::write.xlsx(log, paste(path, "/sleeplog_", pid, ".xlsx", sep=""))
    
    return('done, please check folder')
    
  }else {
    return ("no sleep log generated: do not have at least one entry")
 
  }
  
}
  
  
# library(dplyr)
# rth_sleeplog(path = '/Users/phoebelam/Desktop/rth',
#               pid = 571422,
#               first = '2024-05-05')

