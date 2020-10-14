#
# This is a Shiny web application. 
#

library(shiny)
# library(MCOE)
library(tidyverse)
library(ggthemes)

# library(vroom)
# library(janitor)
library(lubridate)
library(here)
library(usethis)
library(scales)
# library(googlesheets4)


mcoe_theme <- list(ggthemes::theme_hc(),
                   ggthemes::scale_fill_few() ,
                   #           geom_text(size = 2, position = position_dodge(width = 1)),
                   ggplot2::theme(plot.title.position = "plot"),
                   ggplot2::labs(x = "",
                                 y = "",
                                 fill ="") )


ed.srv <- readRDS("edsrv.rds")

pulldate <- max(ed.srv$Day)

zoom.events <- function(keyword){
    ed.srv %>%
        filter(str_detect(Topic, str_to_title(keyword)),
               !str_detect(Topic,"lanning")) %>%
        group_by(Topic, UserName,MeetingId,Day ) %>%
        summarise(MinutesCollective = sum(ParticipationMinutes),
                  Attendees = n()) %>%
        ungroup() %>% 
        na.omit() 
    
}

zoom.people <- function(keyword){
    ed.srv %>%
        filter(str_detect(Topic, str_to_title(keyword)),
               !str_detect(Topic,"lanning")) %>%
        group_by(NameOriginalName ) %>%
        summarise(TotalMinutes = sum(ParticipationMinutes),
                  TotalJoins = n()) %>%
        ungroup() %>% 
        na.omit() 
}

zoom.event.plot <- function(keyword, pulldate) {
    
    zoom.events(keyword) %>%
        ggplot(aes(x= Day, y= Attendees)) +
        geom_line(color = "blue") +
        geom_point() +
        mcoe_theme +
        labs(title = paste0(keyword," Zoom meeting attendees"),
             y = "Total number of unique names \nper meeting",
             caption = paste0("Note: Only includes Meetings with '",keyword,"' in the Meeting Name\n
         Data pulled through ",pulldate) )
}



zoom.event.minutes.plot <- function(keyword, pulldate) {
    
    zoom.events(keyword) %>%
        ggplot(aes(x= Day, y= MinutesCollective)) +
        geom_line(color = "blue") +
        geom_point() +
        mcoe_theme +
        labs(title = paste0("Total Minutes of Participation in ",keyword," Zoom meetings"),
             y = "Sum of minutes of all \nparticipants per meeting",
             caption = paste0("Note: Only includes Meetings with '",keyword,"' in the Meeting Name\n
         Data pulled on ",pulldate) )
}



zoom.people.plot <- function(keyword, pulldate) {
    
 zoom.people(keyword) %>%
        arrange(desc(TotalMinutes)) %>%
        slice_head(n = 30) %>%
        mutate(is.staff =  str_detect(NameOriginalName,staff.pattern)) %>%
        ggplot(aes(x = reorder(NameOriginalName,TotalMinutes), y = TotalMinutes, fill = is.staff)) +
        geom_col() +
        coord_flip() +
        mcoe_theme +
        theme(legend.position = "none") +
        labs(title = paste0("Longest Cumulative Participation at ",keyword," Zoom meetings"),
             subtitle = "Ed Services staff are in orange",
             y = "Total number of minutes",
             caption = paste0("Note: Only includes Meetings with '",keyword,"' in the Meeting Name\n
         Data pulled on ",pulldate) )
    
}



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
)

keywordtable <- tribble(~"Staff", ~"Keyword",
                       "Denise","",
                       "Will","",
                       "Rod","",
                       "Edi","",
                       "Cathy","")


staff.pattern <- paste(staff, collapse = "|")


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Zoom Meeting Analysis"),

    # Text input field
    sidebarLayout(
        textInput("keyword"," Please enter phrase from Zoom Meeting Title","ILN"),
        verbatimTextOutput("value")
        ),

        # Show plots of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Results", 
                        
            
            htmlOutput("text"),
           plotOutput("EventPlot", height = "400px", width = "600px"),
           plotOutput("EventMinutesPlot", height = "400px", width = "600px"),
           plotOutput("PeoplePlot", height = "600px", width = "600px")),

                   tabPanel("Instructions", 
                            htmlOutput("instruc"),
                            tableOutput('table'))
            )
            
            )
    )


# Define server logic required to draw graphs
server <- function(input, output) {

    
    
    output$text <- renderText({ 
        
        
        df <-  zoom.people(input$keyword)
        totalpeople <- df %>% count()
        totaljoinsss <- sum(df$TotalJoins)
        totalminutesss <- sum(df$TotalMinutes)
        
        paste0("<p style='font-size:20px'> There were <b>",totalpeople,
        "</b> cumulative names who joined <b>",  totaljoinsss,  "</b> total times.<br>
               They joined for <b>", number(totalminutesss, big.mark = "," ) ,"</b> cumulative minutes.</p>")
        })
    
    output$EventPlot <- renderPlot({
        zoom.event.plot(input$keyword, pulldate)
    })
    
    output$EventMinutesPlot <- renderPlot({
        zoom.event.minutes.plot(input$keyword, pulldate)
    })
    
    output$PeoplePlot <- renderPlot({
        zoom.people.plot(input$keyword, pulldate)
    })
    
    #<p style='font-size:20px'> 
    
    output$instruc <- renderText({ 
        paste0("The graphs and data on the Results tab automatically update
               as you enter information into the text box.  A few points to keep in mind: <br>
               <ul>
               <li> Only meetings hosted by Ed Services are included in the analysis. </li>
               <li> Events with 'Planning' in the name are excluded. </li>
                <li> The input field is not case sensitive </li>
              <li> When creating zoom events, put intentional names i.e. not 'David's Zoom'. </li>
               <li> For a series that you want to track, use the same and unique phrasing in title. </li>
               <li> Generate a new meeting id to the degree feasible i.e. do not use your default room id. </li>
               <li> A person logging in under multiple names will be counted as different people. </li>
              </ul>
              If you have suggestions on how to improve the display or analysis, please let David know. <br>
              <br>
              The following table displays agreed upon terms that will be reviewed by Caryn. TBD
                </p>")
    })
    
    output$table <- renderTable(keywordtable)
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
