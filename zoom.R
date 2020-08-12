

library(MCOE)
library(tidyverse)
library(here)
library(vroom)
library(janitor)


# setup_dir()


# 
# import_files <- function(dir,globy, delimy){
#     setwd(dir)
# 
#     files <- fs::dir_ls(glob = globy)
# 
#     print(files)
# 
#     output <- map_df(files, ~vroom(.x, delim = delimy , .name_repair = ~ janitor::make_clean_names(., case = "upper_camel")))
# 
#     setwd(here())
# 
#     output
# }
# 
# 
# zoom_vroom <- import_files(here("data"),"meeting*csv",",")
# 
# 
# 
# 
# setwd(here("data"))
# 
# files <- fs::dir_ls()
# 
# zoom_vroom <- vroom(files, delim = ",", .name_repair = ~ janitor::make_clean_names(., case = "upper_camel"))
# 
# setwd(here())
# 
# 
# zoom_vroom1 <- vroom(here("data", "meetinglistdetails_20200101_20200201.csv"), delim = ",", .name_repair = ~ janitor::make_clean_names(., case = "upper_camel"))
# 
# zoom_spec <- spec(zoom_vroom1)
# 
# 
# 
# zoom6 <- read_csv(here("data", "meetinglistdetails_20200601_20200701.csv"))




setwd(here("data"))

files <- fs::dir_ls()

print(files)

output <- map_df(files, ~read_csv(.x)) %>% clean_names(case = "upper_camel")

setwd(here())




ed.srv <- output %>%
    filter(str_detect(Department,"Educational")) %>%
    group_by(Topic, UserName,MeetingId,Participants, StartTime, EndTime, DurationMinutes, NameOriginalName) %>%
    summarise(ParticipationMinutes = sum(DurationMinutes1)) %>%
    ungroup()


EScount <- ed.srv %>%
    group_by(Topic, UserName,MeetingId,Participants) %>%
    count() %>% 
    ungroup() %>% 
    na.omit() %>% 
    mutate(TrueParticipants = pmin(Participants,n))


ESstaffcount <- EScount  %>%
    group_by(UserName) %>%
    summarise(TotalParticipants = sum(TrueParticipants),
              events = n()) %>%
    ungroup()




### Graphs


ESstaffcount %>% 
    ggplot(aes(y = reorder(UserName,TotalParticipants), x = TotalParticipants )) + 
    geom_col() + 
    mcoe_theme



ESstaffcount %>% 
    ggplot(aes(y = reorder(UserName,events), x = events )) + 
    geom_col() + 
    mcoe_theme
