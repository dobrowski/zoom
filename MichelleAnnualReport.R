
library(googlesheets4)
library(tidyverse)

ed.srv <- read_rds("edsrv.rds")

ed.srvCOVID <- read_rds("edsrvCOVID.rds")


termlist <- c("coach",
  "webinar",
  "early learning",
  "sel ",
  "pd ",
  "distance",
  "abriendo",
  "all in",
  "book study",
  "laurie",
  "clic",
  "culturally",
  "diocese",
  "el ",
  "equitable",
  "gel",
  "gonzales",
  "greenfield",
  "iln",
  "instructional leaders' network",
  "lcp",
  "little",
  "virtual",
  "induction",
  "academy",
  "childcare",
  "national",
  "pbis",
  "quality matters",
 "civic",
    "buddy",
    "response & recovery",
    "climate",
    "lever",
    "soledad",
    "udl"
  ) %>%
    paste0(collapse = "|")

ed.srv.TOTAL <- ed.srv %>%
    bind_rows(ed.srvCOVID)

events <- ed.srv.TOTAL %>% 
    select(Day, Topic, UserName, MeetingId) %>%
    distinct() %>%
    mutate(include = str_to_lower(Topic) %>%
               str_detect( termlist   ))


write_sheet(ss = "https://docs.google.com/spreadsheets/d/14aZmSPg7WJwO7SrbNpM-seoLaFThdldX6MZYnfxvVpo/edit#gid=1807342369" , data = events, sheet = "events")





list.events <- ed.srv.TOTAL %>%
    filter(str_to_lower(Topic) %>%
               str_detect( termlist   )
           ) %>%
    group_by(Topic, UserName,MeetingId,Day ) %>%
    summarise(MinutesCollective = sum(ParticipationMinutes),
              Attendees = n()) %>%
    ungroup() %>% 
    na.omit() 





list.people <-     ed.srv.TOTAL %>%
    filter(str_to_lower(Topic) %>%
               str_detect( termlist   ) ) %>%
        group_by(NameOriginalName ) %>%
        summarise(TotalMinutes = sum(ParticipationMinutes),
                  TotalJoins = n()) %>%
        ungroup() %>% 
        na.omit() 
