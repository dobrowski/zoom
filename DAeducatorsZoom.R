
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





output.2021.22 <- output %>%
    mutate(Day = mdy_hms(StartTime) %>% as_date() ,
           Topic = str_to_title(Topic),
           NameOriginalName = str_replace(NameOriginalName, "[[:space:]]\\(Host\\)", "")
           ) %>%
           filter(Day >= mdy("7/1/2020") & Day<=mdy("6/30/2022"))




ed.srv.df <- function(df) {
    
    df %>%
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
    #   filter(Day >= mdy("7/1/2020") & Day<=mdy("6/30/2021")) %>%
    select(-NameOriginalName2) %>%
    group_by(Topic, UserName,MeetingId,Participants, Day, DurationMinutes = DurationMinutes11, NameOriginalName) %>%
    summarise(ParticipationMinutes = sum(DurationMinutes17)) %>%
    ungroup()

}


ed.srv.super <- ed.srv.df(output.2021.22)


### Staff ----

staff <- c(
    "Adilene Cabrera"
    ,"Adriana Chavarin"
    ,"Alicia Diaz"
    ,"Alicia Gregory"
    ,"Caryn Lewis"
    ,"Cathy Cranson"
    ,"David Dobrowski"
    ,"Denise Green"
    ,"Dora Salazar"
    ,"Dora Ann Salazar"
    ,"Dylan Holmes"
    ,"Edi Porter"
    ,"Eliza Gomez"
    ,"Emiliano Valdez"
    ,"Eric Painter"
    ,"Esther Rubio"
    ,"Gail Kuehl"
    ,"Gelacio Gonzalez"
    ,"Greg Fry"
    ,"Irma Lopez"
    ,"Jack Peterson"
    ,"Jeanette Vera"
    ,"Jennifer Elemen"
    ,"jrmendoza"
    ,"Juanita Savinon"
    ,"Lauren Patron-Castro"
 ,"lpatroncastro@montereycoe.org"
    ,"Laurie Ramirez"
    ,"Laurie M. Ramirez"
    ,'Laurie "M. Ramirez'
    ,"Lety Gomez-Gong"
    ,"Lety Gomez"
    ,"Lora Carey"
    ,"Mara Wold"
    ,"Maralina Milazzo"
    ,"Matt Turkie"
 ,"mturkie@montereycoe.org"
    ,"Maria Ramirez"
    ,"Megan Matteoni"
    ,"Michelle Ramirez"
    ,"Michelle Rios"
    ,"Monica Cano"
    ,"Norma Esparza"
    ,"Philip Davis"
    ,"Roberto Nunez"
    ,"Dr. Roberto"
    ,"Roberto Núñez"
    ,"Rod Garcia"
    ,"Roy Phillips"
    ,"Tara Crampton"
    ,"Will Franzell"

    ,"Debra Brau"  #  Note, not in Ed services
    ,"Allison Gribben" #  Note, not in Ed services
    ,"Deneen Guss"
 ,"Colleen Stanley" 
 ,"Nick Zafiratos"
 ,"Timothy Ryan"
 
    ,"Teri James"
    ,"Carla Strobridge Stewart"
    ,"Jessica Hull"
 ,"iPad"
 ,"iPhone"
)


staff.pattern <- paste(staff, collapse = "|")


da.staff <- c("Deneen Guss"
    ,"Teri James"
    ,"Maralina"
    ,"Michelle Ramirez"
    ,"Caryn"
    ,"Dobrowski"
    ,"Adriana Chavarin"
    
    # ,"Greg Fry"
    # ,"Lora Carey"
)


da.staff.pattern <- paste(da.staff, collapse = "|")


### Denise Testing -------

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


email.list <- function(df, district) {
    
    df %>%
        filter(str_detect( UserEmail14, district ),          # Only people with emails with the 'right' domain
               str_detect(Department,"Educational|Super")) %>%     # Onl;y get Ed Services meetings 
        # mutate(Day = mdy_hms(StartTime) %>% as_date() ,
        #        Topic = str_to_title(Topic),
        #        NameOriginalName = str_replace(NameOriginalName, "[[:space:]]\\(Host\\)", "")) %>%
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

meeting.name.list <- function(district) {
    
    district.list <- paste(district, collapse = "|") %>%
        str_to_lower()
    
    
    ed.srv.super %>% 
    filter(str_detect(str_to_lower(Topic),district.list) ) %>%  # Sr
    select(Topic:DurationMinutes) %>%
    distinct()
    
}


combo <- function(df, meeting.words, email.words) {
    
    
    # Take the list of people with the given email, and find their Zoom name in a list
    email.santa.list <- email.list(df, email.words) %>% 
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
        distinct() %>% 
        filter(!str_detect(str_trim(NameOriginalName), staff.pattern))
    
    
}


# Function to identify which Category supports should be grouped in
support.categories <- function(df) {
    
    df %>% 
        mutate(SupportCategory = 
                   case_when(str_detect(UserName,"Denise|Will|Rod|Jennifer|Dora|Edi|Norma|Painter|Megan|Gel|Laurie|Lora|Eliza|Monica|Alicia Gregory|Irma" ) ~ "Distance Learning Support",
                             str_detect(UserName,"David|Maralina|Turk" ) ~ "Level 2",
                             str_detect(UserName,"Mara |Lety|Hull") ~ "Emergency Services",
                             str_detect(UserName,"Laurie") & str_detect(Topic,"Response") ~ "Emergency Pandemic Support",
                             str_detect(Topic,"Covid|Iln|Instructional Leader|Superin") ~ "Emergency Services",
                             str_detect(UserName,"Cathy|Esther|Greg|Tara" ) ~ "SEL Work",
                             str_detect(Topic,"Trauma|Mindful|Sel|Health|Couns" ) ~ "SEL Work",
                             str_detect(Topic,"Nep|National Equity|Lcap|Account|Significant|Learning Continuity" ) ~ "Level 2",
                             str_detect(Topic,"All|Induct|Retire" ) ~ "Exclude",
                             str_detect(UserName,"Deneen|Teri|Caryn" ) ~ "Misc",
                             TRUE ~ "Misc"
                             #  str_detect(UserName,"Caryn|Michelle Ram|Infante" ) ~ "ILN, LCAP and Equity",                        
                   )
        )#  Remove Jennifer,  Remove Emilano from attendee lists
    
}

#  Function to say how many people (duplicated attendances) and how many meetings occurred 
support.category.summary <-  function(df) {
    
    df %>%
        group_by(SupportCategory) %>%
        summarise(people = n(),
                  meetings = n_distinct(Topic, MeetingId, Day)
        )
}


# Function to apply categories and write sheets

category.write <- function(df, sheetr, sheetname) {
    
 sorted <- df %>%
        support.categories()
 
 sorted %>%
     arrange(desc(SupportCategory)) %>%
     write_sheet(sheet = sheetname,
                 ss = sheetr)
 
 sorted %>%
     support.category.summary() %>%
     arrange(desc(SupportCategory)) %>%
     write_sheet(sheet = paste0(sheetname,".sum"),
                 ss = sheetr)
 
 sorted %>%
     select(SupportCategory, Day) %>%
     distinct() %>%
     arrange(desc(SupportCategory),Day) %>%
     write_sheet(sheet = paste0(sheetname,".days"),
                 ss = sheetr)
 
}

### Run functions for districts -------

email.list(output, "santarita")
email.list(output,"soledad")
email.list(output,"mpusd")
email.list(output,"smcj")
email.list(output,"salinasuh")



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


rita <- combo(df = output.2021.22,
              meeting.words = c("rita","srusd"),
              email.words = c("santarita")) %>%
    arrange((UserName))


mpusd <- combo(df = output,
               meeting.words = c("Mpusd"),
               email.words = c("mpusd"))


#  Need to look at the code changing output to ed.srv.super and make sure things aren't lost. 



### Final Main function comands ----------


# Older version for archives
# zoom.sheet <- "https://docs.google.com/spreadsheets/d/1bMF800Z4_yfLaGbHcXLXari4LpQObk30hheIEqUjOGU/edit#gid=642421465"
# zoom.sheet <- "https://docs.google.com/spreadsheets/d/1BnLOBptXEv5pL7ZxL5Q-MErA7v7Ly5BcnQyEaZBzcu4/edit#gid=0"
zoom.sheet <- "https://docs.google.com/spreadsheets/d/1BnLOBptXEv5pL7ZxL5Q-MErA7v7Ly5BcnQyEaZBzcu4/edit#gid=1115566529"


combo(df = output.2021.22,
      meeting.words = c("rita","srusd"),
      email.words = c("santarita")) %>%
    category.write(zoom.sheet, "rita")


combo(df = output.2021.22,
      meeting.words = c("Mpusd", "ord terrace", "mlk"),
      email.words = c("mpusd")) %>%
 #   filter(str_detect(str_trim(UserName), da.staff.pattern)) %>%
    category.write(zoom.sheet, "mpusd")

combo(df = output.2021.22,
      meeting.words = c("soledad", "susd"), 
      email.words = c("soledad")) %>%
#    filter(str_detect(str_trim(UserName), da.staff.pattern)) %>%
    category.write(zoom.sheet, "soledad")


combo(df = output.2021.22,
      meeting.words = c("smcj","somoco","south monterey"), 
      email.words = c("smcj")) %>%
#    filter(str_detect(str_trim(UserName), da.staff.pattern)) %>%
    category.write(zoom.sheet, "somoco")

combo(df = output.2021.22,
      meeting.words = c("suhsd","salinas union"), 
      email.words = c("salinasuh")) %>%
 #   filter(str_detect(str_trim(UserName), da.staff.pattern)) %>%
    category.write(zoom.sheet, "suhsd")



###  Experimenting ----- 




edservice <- output %>%
    filter(str_detect(Department,"Education")) %>%
    mutate(has.email = is.na(UserEmail14))


edservice %>% janitor::tabyl(has.email)


#  Filter July 1 2019 to June 30  2020 


rita.cats <- rita %>% support.categories()




rita.cats %>%
    support.category.summary()


### End --------
