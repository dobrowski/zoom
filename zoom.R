

library(MCOE)
library(tidyverse)
library(vroom)
library(janitor)
library(lubridate)
library(here)
library(googlesheets4)


### Read all files in the data directory  -----

setwd(here::here("data")) # Updated for particular school year

#files <- fs::dir_ls(glob = "meeting*")
files <- fs::dir_ls(glob = "meetinglistdetails_202*")

print(files)

output <- map_df(files, ~read_csv(.x)) %>% clean_names(case = "upper_camel")

setwd(here())


### Refine dataset  ------

ed.srv <- output %>%
    filter(str_detect(Department,"Educational")) %>%
    mutate(Day = mdy_hms(StartTime) %>% as_date() ,
           Topic = str_to_title(Topic),
           NameOriginalName = str_replace(NameOriginalName, "[[:space:]]\\(Host\\)", "")) %>%
    mutate(NameOriginalName2 = 
               str_extract(NameOriginalName, "[[:space:]]\\([[:space:]](.*)[[:space:]]\\)")
           %>% str_sub(4,-2) ,
           NameOriginalName = if_else(is.na(NameOriginalName2), NameOriginalName, NameOriginalName2) %>%
               str_replace(  "\\(.*","") %>% 
               str_replace(  "\\#.*","") %>%
               str_trim()
    ) %>%
    select(-NameOriginalName2) %>%
    group_by(Topic, UserName,MeetingId,Participants, Day, DurationMinutes = DurationMinutes11, NameOriginalName) %>%
    summarise(ParticipationMinutes = sum(DurationMinutes17)) %>%
    ungroup() 


write_rds(ed.srv, "edsrv.rds")

