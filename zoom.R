

library(MCOE)
library(tidyverse)
library(vroom)
library(janitor)
library(lubridate)
library(here)
library(googlesheets4)


### Read all files in the data directory  -----

setwd(here::here("data"))

files <- fs::dir_ls(glob = "meeting*")

print(files)

output <- map_df(files, ~read_csv(.x)) %>% clean_names(case = "upper_camel")

setwd(here())


### Refine dataset  ------

ed.srv <- output %>%
    filter(str_detect(Department,"Educational")) %>%
    mutate(Day = mdy_hms(StartTime) %>% as_date() ,
           Topic = str_to_title(Topic),
           NameOriginalName = str_replace(NameOriginalName, "[[:space:]]\\(Host\\)", "")) %>%
    group_by(Topic, UserName,MeetingId,Participants, Day, DurationMinutes, NameOriginalName) %>%
    summarise(ParticipationMinutes = sum(DurationMinutes1)) %>%
    ungroup() 


write_rds(ed.srv, "edsrv.rds")

