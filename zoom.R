

library(MCOE)
library(tidyverse)

library(vroom)
library(janitor)
library(lubridate)
library(here)
library(googlesheets4)


## Read all files in the data directory

setwd(here::here("data"))

files <- fs::dir_ls()

print(files)

output <- map_df(files, ~read_csv(.x)) %>% clean_names(case = "upper_camel")

setwd(here())


### Refine dataset

ed.srv <- output %>%
    filter(str_detect(Department,"Educational")) %>%
    mutate(Day = mdy_hms(StartTime) %>% as_date() ) %>%
    group_by(Topic, UserName,MeetingId,Participants, Day, DurationMinutes, NameOriginalName) %>%
    summarise(ParticipationMinutes = sum(DurationMinutes1)) %>%
    ungroup() 



EScount <- ed.srv %>%
    group_by(Topic, UserName,MeetingId, Participants) %>%
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






###  ILN 



ILN <- ed.srv %>%
    filter(str_detect(Topic, "ILN")) %>%
    group_by(Topic, UserName,MeetingId,Day ) %>%
    summarise(MinutesCollective = sum(ParticipationMinutes),
              Attendees = n()) %>%
    ungroup() %>% 
    na.omit() 


ggplot(ILN, aes(x= Day, y= Attendees)) +
    geom_line(color = "blue") +
    geom_point() +
    mcoe_theme +
    labs(title = "ILN Zoom meeting attendees",
         y = "Total number of unique names \nper meeting",
         caption = "Note: Only includes Meetings with 'ILN' in the Meeting Name\n
         Data pulled on July 21")


ggsave("Attendees.png", width = 8, height = 5)


ggplot(ILN, aes(x= Day, y= MinutesCollective)) +
    geom_line(color = "blue") +
    geom_point() +
    mcoe_theme +
    labs(title = "Total Minutes of Participation in ILN Zoom meetings",
         y = "Sum of minutes of all \nparticipants per meeting",
         caption = "Note: Only includes Meetings with 'ILN' in the Meeting Name\n
         Data pulled on July 21")


ggsave("Minutes.png", width = 8, height = 5)




ILNpeople <- ed.srv %>%
    filter(str_detect(Topic, "ILN")) %>%
    group_by(NameOriginalName ) %>%
    summarise(TotalMinutes = sum(ParticipationMinutes),
              TotalJoins = n()) %>%
    ungroup() %>% 
    na.omit() 

ILNpeople %>%
    arrange(desc(TotalJoins)) %>%
    top_n(20) %>%
    ggplot(aes(x = reorder(NameOriginalName,TotalJoins), y = TotalJoins)) +
    geom_col(fill = "blue") +
    lims(y = c(0,15)) +
    coord_flip() +
    mcoe_theme +
    labs(title = "Most Frequent Attendees at ILN meetings",
         y = "Number of meetings")


ggsave("Frequent.png", width = 6, height = 6)

ILNpeople %>%
    arrange(desc(TotalMinutes)) %>%
    top_n(20) %>%
    ggplot(aes(x = reorder(NameOriginalName,TotalMinutes), y = TotalMinutes)) +
    geom_col(fill = "blue") +
    coord_flip() +
    mcoe_theme +
    labs(title = "Longest Participation at ILN meetings",
         y = "Total number of minutes")


ggsave("Longest.png", width = 6, height = 6)


# Note for this to work you need to run the staff-pattern file first

ILNpeople %>%
    arrange(desc(TotalMinutes)) %>%
    top_n(20) %>%
    mutate(is.staff =  str_detect(NameOriginalName,staff.pattern)) %>%
    ggplot(aes(x = reorder(NameOriginalName,TotalMinutes), y = TotalMinutes, fill = is.staff)) +
    geom_col() +
    coord_flip() +
    mcoe_theme +
    theme(legend.position = "none") +
    labs(title = "Longest Participation at ILN meetings",
         subtitle = "Ed Services staff are in orange",
         y = "Total number of minutes")


ggsave("Longest-staff.png", width = 6, height = 6)



######  Feedback form --------


feedback <- read_sheet("https://docs.google.com/spreadsheets/d/1yFoOrQUl_L6FMZeAmX2xQ0x8lFFOH6pKv1sT_1OIRTk/edit#gid=102281525",
                       .name_repair = janitor::make_clean_names)


feedback_clean <- feedback %>% 
    mutate(meetingID = str_extract( what_was_the_link_for_your_zoom_meeting, "[[:digit:]]{11}"  ) ) %>%
    mutate(MeetingId = paste(str_sub(meetingID,1,3),str_sub(meetingID,4,7),str_sub(meetingID,8,11)  )  ) %>%
    mutate(Day = ymd_hms(timestamp) %>% as_date() ) %>%
    select(Day,
           MeetingId,
           rate_apply = please_rate_the_following_statements_i_can_apply_todays_event_to_my_work,
           rate_commun = please_rate_the_following_statements_the_presenter_s_communicated_the_material_well,
           rate_rec = please_rate_the_following_statements_i_would_recommend_this_to_my_colleagues,
           comments = additional_comments_please_note_that_we_cannot_respond_personally_to_any_statements_made_here)
    
