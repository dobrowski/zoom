
# This script is to help identify educators from DA districts that participated in Zooms during the pandemic  



ed.srv <- readRDS("edsrv.rds")


santa.denise <- ed.srv %>% 
    filter(str_detect(UserName, "Denise"))


denise.topics <- santa.denise %>% 
    select(Topic) %>%
    distinct()
# None of the meetings have Santa Rita in title 


rita.topics <- ed.srv %>% 
    filter(str_detect(Topic,"Rita") &  # Sr
               !str_detect(Topic,"Burk")) %>%
    select(Topic:DurationMinutes) %>%
    distinct()
#  only four meetings with Santa Rita in title


rita.email <- output %>%
    filter(str_detect( UserEmail14, "santarita" ),
           str_detect(Department,"Educational")) %>%
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
           




email.list <- function(district) {
    
    output %>%
        filter(str_detect( UserEmail14, district ),          # Only people with emails with the 'right' domain
               str_detect(Department,"Educational")) %>%     # Onl;y get Ed Services meetings 
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
    
    ed.srv %>% 
    filter(str_detect(Topic,district) ) %>%  # Sr
    select(Topic:DurationMinutes) %>%
    distinct()
    
}


meeting.name.list("Rita")


meeting.name.list("Srusd")

meeting.name.list("Mpusd")


meeting.name.list("Soledad")


meeting.name.list("Suhsd")

meeting.name.list("Smcjuhsd")

meeting.name.list("Somoco")

meeting.name.list("South")









### Find all attendees with a given email, in this case santarita ---------

email.santa <- email.list("santarita")

# Take the list of people with the given email, and find their Zoom name in a list
email.santa.list <- email.santa %>% 
    select(NameOriginalName) %>%
    distinct() %>%
    unlist()

# Find all the meetings that those Zoom names attended 
all.meetings.from.list  <- ed.srv %>%
    filter(NameOriginalName %in% email.santa.list)



### Use meeting name list to get bigger list of attendance at meetings-----

#  Come up with all Meetings with common names in it (Rita or SRUSD)
meeting.santa <- meeting.name.list("Rita")

meeting.santa2 <- meeting.name.list("Srusd")

# Join them together
meeting.santa <- meeting.santa %>%
    bind_rows(meeting.santa2)


meeting.name.list("Rita|Srusd")

# Find everyone that was at that meeting, remove MCOE Ed Services staff, get a list of all the other attendees (and assume they are from the district)
meeting.santa.list <- meeting.santa %>% 
    left_join(ed.srv) %>%
    filter(!str_detect(str_trim(NameOriginalName), staff.pattern))%>% 
    select(NameOriginalName) %>%
    distinct() %>%
    unlist()

# Find all the meetings all those attendees also attended.
all.meetings.from.list2  <- ed.srv %>%
    filter(NameOriginalName %in% meeting.santa.list)



### Combine both lists ------

big.list <- all.meetings.from.list %>%
    bind_rows(all.meetings.from.list2) %>%
    distinct()


combo <- function(meeting.words, email.words) {
    
    
    # Take the list of people with the given email, and find their Zoom name in a list
    email.santa.list <- email.list(email.words) %>% 
        select(NameOriginalName) %>%
        distinct() %>%
        unlist()
    
    # Find all the meetings that those Zoom names attended 
    all.meetings.from.list  <- ed.srv %>%
        filter(NameOriginalName %in% email.santa.list)
    
    
    ##
    
     meeting.santa.list <- meeting.name.list(meeting.words) %>% 
        left_join(ed.srv) %>%
        filter(!str_detect(str_trim(NameOriginalName), staff.pattern))%>% 
        select(NameOriginalName) %>%
        distinct() %>%
        unlist()
    
    # Find all the meetings all those attendees also attended.
    all.meetings.from.list2  <- ed.srv %>%
        filter(NameOriginalName %in% meeting.santa.list)
    
    
    
    all.meetings.from.list %>%
        bind_rows(all.meetings.from.list2) %>%
        distinct()
    
}


combo(meeting.words = c("Rita","Srusd"), email.words = c("santarita"))



mpusd <- combo(meeting.words = c("Mpusd"), email.words = c("mpusd"))

