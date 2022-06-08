library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(shinyalert)
library(DBI)

con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "hondademo", 
                 host = "XX.XXXX.XXX.XXX", 
                 port = 3306,
                 user = "USERNAME",
                 password = "XXXXXXXXXXXXXXXXX")

start_time <- now()
cash = 10
prices <- dbGetQuery(con, "SELECT * FROM Prices") %>%
    tail(1) %>%
    select(starts_with("p")) %>%
    as.list()
jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = 'http://34.68.222.177:3838/pred-markets/thank-you/';});"


ui <- fluidPage(
    
    tags$head(
        tags$script(jscode),
        tags$style("#cash_text")),
    useShinyalert(),
    h1(htmlOutput("cash_text")),
    # Add instructions
    h3("Which of the following features, available on premium Honda trim levels, do buyers of 2021 Honda vehicles with the base trim MOST regret not getting in their new purchase?"),
    hr(),
    h4("Power Tailgate"),
    fluidRow(column(1,
                    p(paste0(dollar(prices$p1), "/share"))),
             column(1,
                    numericInput("q1", NULL, 0, min = 0, step = 1))
             ),
    h4("One-touch Moonroof"),
    fluidRow(column(1,
                    p(paste0(dollar(prices$p2), "/share"))),
             column(1,
                    numericInput("q2", NULL, 0, min = 0, step = 1))
    ),
    h4("Leather-trimmed interior"),
    fluidRow(column(1,
                    p(paste0(dollar(prices$p3), "/share"))),
             column(1,
                    numericInput("q3", NULL, 0, min = 0, step = 1))
    ),
    h4("Android Auto and Apple CarPlay integration"),
    fluidRow(column(1,
                    p(paste0(dollar(prices$p4), "/share"))),
             column(1,
                    numericInput("q4", NULL, 0, min = 0, step = 1))
    ),
    actionButton("submit","Submit!", class = "btn-primary")
    
    
    
)

server <- function(input, output, session) {
    
    

    final_cash <- reactive({
        cash -
            (prices$p1 * input$q1) -
            (prices$p2 * input$q2) - 
            (prices$p3 * input$q3) - 
            (prices$p4 * input$q4) 
    })
    
    output$cash_text <- renderText({
        
        if(is.na(final_cash())) {color <- 'red'} else {
            if(final_cash() >= 0) color <- 'green'
            if(final_cash() < 0) color <- 'red'
        }
        
        paste0('Your Available Cash Balance = ','<span style=\"color:',color,'\">',dollar(final_cash()),"</span>")
        
        })
    

    
    observeEvent(input$submit, {
        
        if(is.na(final_cash())){
            shinyalert("Oops!", "Please ensure you have a value in each box for the number of bets you wish to place on that option.", type = "warning")
            
        } else {
            if(final_cash() > cash*.3 ){
                shinyalert("Oops!", paste("Please purchase enough bets to spend at least ", dollar(cash*.7)), type = "warning")
                
            }
            
            if(final_cash() >= 0 & final_cash() <=3){
                rid = parseQueryString(session$clientData$url_search)[["RID"]]
                if(is.null(rid)) rid <- "MISSING RID"
                
                submission_time <- now()
                
                submission <- tibble(
                    rid = paste0('"',rid,'"'),
                    time_started = paste0('"',start_time,'"'),
                    time_submitted = paste0('"',submission_time,'"'),
                    q1 = input$q1,
                    q2 = input$q2,
                    q3 = input$q3,
                    q4 = input$q4,
                    p1 = prices$p1,
                    p2 = prices$p2,
                    p3 = prices$p3,
                    p4 = prices$p4,
                    m1 = input$q1 * prices$p1,
                    m2 = input$q2 * prices$p2,
                    m3 = input$q3 * prices$p3,
                    m4 = input$q4 * prices$p4)
                
                query <- paste0("INSERT INTO Bets (",paste0(t(names(submission)), collapse = ","),") VALUES (", paste0(t(submission), collapse = ","), ")")
  
                dbGetQuery(con, query) 
                
                bets <- dbGetQuery(con, "SELECT * FROM Bets") %>%
                    select(starts_with("m")) %>%
                    summarise_all(sum) %>%
                    mutate(time = paste0('"',submission_time,'"'),
                           total = m1 + m2 + m3 + m4,
                           p1 = m1 / total,
                           p2 = m2 / total,
                           p3 = m3 / total,
                           p4 = m4 / total) %>%
                    select(time, everything(), -total)
                query <- paste0("INSERT INTO Prices (",paste0(c("time","cs1","cs2","cs3","cs4","p1","p2","p3","p4"), collapse = ","),") VALUES (",paste0(t(bets), collapse = ","), ")")

                dbGetQuery(con, query) 
                
                session$sendCustomMessage("mymessage", "mymessage")
            }
            
            if(final_cash() < 0){
                shinyalert("Oops!", paste("You can't spend more than ", dollar(cash)), type = "error")
            }
        }
        
        

            
    })
}

shinyApp(ui = ui, server = server)
