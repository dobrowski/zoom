#
# This is a Shiny web application. 
#


### Libraries --------


library(shiny)
library(tidyverse)
library(ggthemes)
library(lubridate)
library(here)
library(usethis)
library(scales)


### Basic Inputs --------

mcoe_theme <- list(ggthemes::theme_hc(),
                   ggthemes::scale_fill_few() ,
                   #           geom_text(size = 2, position = position_dodge(width = 1)),
                   ggplot2::theme(plot.title.position = "plot"),
                   ggplot2::labs(x = "",
                                 y = "",
                                 fill ="") )


ed.srv <- readRDS("edsrv.rds")

pulldate <- max(ed.srv$Day)

#### Functions --------

zoom.events <- function(df, keyword){
    df %>%
        filter(str_detect(Topic, str_to_title(keyword)),
               !str_detect(Topic,"lanning")) %>%
        group_by(Topic, UserName,MeetingId,Day ) %>%
        summarise(MinutesCollective = sum(ParticipationMinutes),
                  Attendees = n()) %>%
        ungroup() %>% 
        na.omit() 
    
}

zoom.people <- function(df, keyword){
    df %>%
        filter(str_detect(Topic, str_to_title(keyword)),
               !str_detect(Topic,"lanning")) %>%
        group_by(NameOriginalName ) %>%
        summarise(TotalMinutes = sum(ParticipationMinutes),
                  TotalJoins = n()) %>%
        ungroup() %>% 
        na.omit() 
}

zoom.event.plot <- function(df , keyword, pulldate) {
    
    zoom.events(df, keyword) %>%
        ggplot(aes(x= Day, y= Attendees)) +
        geom_line(color = "blue") +
        geom_point() +
        mcoe_theme +
        labs(title = paste0(keyword," Zoom meeting attendees"),
             y = "Total number of unique names \nper meeting",
             caption = paste0("Note: Only includes Meetings with '",keyword,"' in the Meeting Name\n
         Data pulled through ",pulldate) )
}



zoom.event.minutes.plot <- function(df, keyword, pulldate) {
    
    zoom.events(df, keyword) %>%
        ggplot(aes(x= Day, y= MinutesCollective)) +
        geom_line(color = "blue") +
        geom_point() +
        mcoe_theme +
        labs(title = paste0("Total Minutes of Participation in ",keyword," Zoom meetings"),
             y = "Sum of minutes of all \nparticipants per meeting",
             caption = paste0("Note: Only includes Meetings with '",keyword,"' in the Meeting Name\n
         Data pulled on ",pulldate) )
}



zoom.people.plot <- function(df, keyword, pulldate) {
    
 zoom.people(df, keyword) %>%
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

### Datavariables --------

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
)

keywordtable <- tribble(~"Staff", ~"Keyword",
                       "Denise","",
                       "Will","",
                       "Rod","",
                       "Edi","",
                       "Cathy","")


staff.pattern <- paste(staff, collapse = "|")


###  User Interface ---------

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Zoom Meeting Analysis"),

    # Text input field
    sidebarLayout(
        sidebarPanel(
        textInput("keyword"," Please enter phrase from Zoom Meeting Title","ILN"),
        verbatimTextOutput("value")
        ,
        dateRangeInput('dateRange',
                       label = 'Date range input: yyyy-mm-dd',
                       start = "2020-07-01",
                       end = "2021-06-30"
        )
        
        )
        ,

        # Show plots of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Results", 
                        
            
            htmlOutput("text"),
           plotOutput("EventPlot", height = "400px", width = "600px"),
           plotOutput("EventMinutesPlot", height = "400px", width = "600px"),
           plotOutput("PeoplePlot", height = "600px", width = "600px"),
           verbatimTextOutput("dateRangeText"),
           verbatimTextOutput("numRowsText"),
           
           ),

                   tabPanel("Instructions", 
                            htmlOutput("instruc"),
                            tableOutput('table'))
            )
            
            )
    )
    
)


### Server Logic ----------

# Define server logic required to draw graphs
server <- function(input, output) {

    
    output$dateRangeText  <- renderText({
        paste("input$dateRange is",
              paste(as.character(input$dateRange), collapse = " to ")
        )
    })
    
    
    ed.srv2 <- reactive( {ed.srv %>%
        filter( Day  >= input$dateRange[1]  & Day <= input$dateRange[2] ) 
        }
  )
    
    
    output$numRowsText  <- renderText({
        paste("Number of rows in EdSrvs is",
              as.character( count(ed.srv2())),
              " between ",
              paste(as.character(input$dateRange), collapse = " to ")
        )
    })
    
    
    
    
    output$text <- renderText({ 
        
        
        df <-  zoom.people(ed.srv2(), input$keyword)
        totalpeople <- df %>% count()
        totaljoinsss <- sum(df$TotalJoins)
        totalminutesss <- sum(df$TotalMinutes)
        
        paste0("<p style='font-size:20px'> There were <b>",totalpeople,
        "</b> cumulative names who joined <b>",  totaljoinsss,  "</b> total times.<br>
               They joined for <b>", number(totalminutesss, big.mark = "," ) ,"</b> cumulative minutes.</p>")
        })
    
    output$EventPlot <- renderPlot({
        zoom.event.plot(ed.srv2(), input$keyword, pulldate)
    })
    
    output$EventMinutesPlot <- renderPlot({
        zoom.event.minutes.plot(ed.srv2(), input$keyword, pulldate)
    })
    
    output$PeoplePlot <- renderPlot({
        zoom.people.plot(ed.srv2(), input$keyword, pulldate)
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

#### Run the application -----


shinyApp(ui = ui, server = server)



### End -------

