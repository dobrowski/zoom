
# This script is to help identify educators from DA districts that participated in Zooms during the pandemic  

### Libraries ------


library(MCOE)
library(tidyverse)
library(vroom)
library(janitor)
library(lubridate)
library(here)
library(googlesheets4)

# ed.srv <- readRDS("edsrv.rds")

### Read all files in the data directory  -----

setwd(here::here("data")) # Updated for particular school year

#files <- fs::dir_ls(glob = "meeting*")
files <- fs::dir_ls(glob = "meetinglistdetails_202*")

print(files)

output <- map_df(files, ~read_csv(.x)) %>% clean_names(case = "upper_camel")

setwd(here())



ed.srv.super <- output %>%
    filter(str_detect(Department,"Educational|Super")) %>%
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





staff <- c(
    "Alicia Diaz"
    ,"Alicia Gregory"
    ,"Caryn Lewis"
    ,"Cathy Cranson"
    ,"David Dobrowski"
    ,"Denise Green"
    ,"Dora Salazar"
    ,"Dora Ann Salazar"
    ,"Esther Rubio"
    ,"jrmendoza"
    ,"Laurie Ramirez"
    ,"Michelle Ramirez"
    ,"Megan Matteoni"
    ,"Philip Davis"
    ,"Roberto Nunez"
    ,"Roberto Núñez"
    ,"Will Franzell"
    ,"Michelle Rios"
    ,"Gail Kuehl"
    ,"Adriana Chavarin"
    ,"Maria Ramirez"
    ,"Juanita Savinon"
    ,"Rod Garcia"
    ,"Mara Wold"
    ,"Norma Esparza"
    ,"Lety Gomez-Gong"
    ,"Jennifer Elemen"
    ,"Edi Porter"
    ,"Denise Green"
    ,"Eliza Gomez"
    ,"Gelacio Gonzalez"
    ,"Irma Lopez"
    ,"Lety Gomez"
    ,"Alicia Gregory"
    ,"Monica Cano"
    ,"Eric Painter"
    ,"Debra Brau"  #  Note, not in Ed services
)


staff.pattern <- paste(staff, collapse = "|")



### Denise -------

# santa.denise <- ed.srv %>% 
#     filter(str_detect(UserName, "Denise"))
# 
# 
# denise.topics <- santa.denise %>% 
#     select(Topic) %>%
#     distinct()
# # None of the meetings have Santa Rita in title 
# 
# 
# rita.topics <- ed.srv %>% 
#     filter(str_detect(Topic,"Rita") &  # Sr
#                !str_detect(Topic,"Burk")) %>%
#     select(Topic:DurationMinutes) %>%
#     distinct()
# #  only four meetings with Santa Rita in title
# 
# 
# rita.email <- output %>%
#     filter(str_detect( UserEmail14, "santarita" ),
#            str_detect(Department,"Educational")) %>%
#     mutate(Day = mdy_hms(StartTime) %>% as_date() ,
#            Topic = str_to_title(Topic),
#            NameOriginalName = str_replace(NameOriginalName, "[[:space:]]\\(Host\\)", "")) %>%
#     mutate(NameOriginalName2 =
#                str_extract(NameOriginalName, "[[:space:]]\\([[:space:]](.*)[[:space:]]\\)")
#            %>% str_sub(4,-2) ,
#            NameOriginalName = if_else(is.na(NameOriginalName2), NameOriginalName, NameOriginalName2) %>%
#                str_replace(  "\\(.*","") %>%
#                str_replace(  "\\#.*","") %>%
#                str_trim()
#     ) %>%
#     select(-NameOriginalName2) %>%
#     group_by(Topic, UserName,MeetingId,Participants, Day, DurationMinutes = DurationMinutes11, NameOriginalName, UserEmail14) %>%
#     summarise(ParticipationMinutes = sum(DurationMinutes17)) %>%
#     ungroup()
#            

### Functions ------


email.list <- function(district) {
    
    output %>%
        filter(str_detect( UserEmail14, district ),          # Only people with emails with the 'right' domain
               str_detect(Department,"Educational|Super")) %>%     # Onl;y get Ed Services meetings 
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
        group_by(Topic, UserName,MeetingId,Participants, Day, DurationMinutes = DurationMinutes11, NameOriginalName, UserEmail14) %>%
        summarise(ParticipationMinutes = sum(DurationMinutes17)) %>%
        ungroup() 
    
    
}

email.list("santarita")


email.list("soledad")


email.list("mpusd")


email.list("smcj")


email.list("salinasuh")



meeting.name.list <- function(district) {
    
    district.list <- paste(district, collapse = "|") %>%
        str_to_lower()
    
    
    ed.srv.super %>% 
    filter(str_detect(str_to_lower(Topic),district.list) ) %>%  # Sr
    select(Topic:DurationMinutes) %>%
    distinct()
    
}


rita <- meeting.name.list("rita")


srusd <-meeting.name.list("srusd")

meeting.name.list("Mpusd")


meeting.name.list("Soledad")


meeting.name.list("Suhsd")

meeting.name.list("Smcjuhsd")

meeting.name.list("Somoco")

meeting.name.list("South")


### Find all attendees with a given email, in this case santarita ---------

# email.santa <- email.list("santarita")
# 
# # Take the list of people with the given email, and find their Zoom name in a list
# email.santa.list <- email.santa %>%
#     select(NameOriginalName) %>%
#     distinct() %>%
#     unlist()
# 
# # Find all the meetings that those Zoom names attended
# all.meetings.from.list  <- ed.srv %>%
#     filter(NameOriginalName %in% email.santa.list)


### Combine both lists ------


combo <- function(meeting.words, email.words) {
    
    
    # Take the list of people with the given email, and find their Zoom name in a list
    email.santa.list <- email.list(email.words) %>% 
        select(NameOriginalName) %>%
        distinct() %>%
        unlist()
    
    # Find all the meetings that those Zoom names attended 
    all.meetings.from.list  <- ed.srv.super %>%
        filter(NameOriginalName %in% email.santa.list)
    
    
    ##
    
     meeting.santa.list <- meeting.name.list(meeting.words) %>% 
        left_join(ed.srv.super) %>%
        filter(!str_detect(str_trim(NameOriginalName), staff.pattern))%>% 
        select(NameOriginalName) %>%
        distinct() %>%
        unlist()
    
    # Find all the meetings all those attendees also attended.
    all.meetings.from.list2  <- ed.srv.super %>%
        filter(NameOriginalName %in% meeting.santa.list)
    
    
    
    all.meetings.from.list %>%
        bind_rows(all.meetings.from.list2) %>%
        distinct()
    
}


rita <- combo(meeting.words = c("rita","srusd"), email.words = c("santarita")) %>%
    arrange((UserName))


mpusd <- combo(meeting.words = c("Mpusd"), email.words = c("mpusd"))


zoom.sheet <- "https://docs.google.com/spreadsheets/d/1bMF800Z4_yfLaGbHcXLXari4LpQObk30hheIEqUjOGU/edit#gid=642421465"

#  Need to look at the code changing output to ed.srv.super and make sure things aren't lost. 
write_sheet(rita ,
            sheet = 1,
            ss = zoom.sheet)

write_sheet(mpusd ,
            sheet = "mpusd",
            ss = zoom.sheet)



combo(meeting.words = c("soledad"), email.words = c("soledad")) %>%
    write_sheet(sheet = "soledad",
                ss = zoom.sheet)



combo(meeting.words = c("smcj","somoco","south monterey"), email.words = c("smcj")) %>%
    write_sheet(sheet = "SoMoCo",
                ss = zoom.sheet)



combo(meeting.words = c("suhsd","salinas union"), email.words = c("salinasuh")) %>%
    write_sheet(sheet = "suhsd",
                ss = zoom.sheet)



1# Four categories? 
# distance learning support /prof dev
# emergency services
# SEL work
# Level2 supports



edservice <- output %>%
    filter(str_detect(Department,"Education")) %>%
    mutate(has.email = is.na(UserEmail14))


edservice %>% janitor::tabyl(has.email)


### End --------
