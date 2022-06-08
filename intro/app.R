library(shiny)
library(tippy)
library(scales)
p1=0.1447507968
p2=0.3599723762
p3=0.3505260302
p4=0.1447507968
cash=10
jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = 'http://34.68.222.177:3838/pred-markets/demo/';});"

ui <- fluidPage(
    
    tags$head(
        tags$script(jscode)
        ),
    h1("Welcome to Betting Markets", align = "center"),
    h4("What is this?"),
    p("This is a style of survey that rewards respondents, like yourself, according to how well they bet"),
    h4("How does this work?"),
    p("Like other surveys, you will be presented with questions with multiple choice answer options - but each option has a cost. You start with $10. If the price of the options you bet on increase, then you get more than $10 - potentially doubling or tripling your $10. If the price of the options you bet on decrease, then you not get the full $10."),
    h4("What causes the prices of the options to change?"),
    p("The price of the options increase as more people place bets on that option compared to other options. Collectively, the prices of all the options will sum to $1."),
    h4("How do I increase my reward?"),
    p("By betting wisely. Think of this as a stock market and you want to pick stocks that will go up. The stocks will go up as more people purchase them. Consider which options are overvalued and which are undervalued. You may choose to buy many cheap stocks, and if their price increases so will your reward. You may choose to buy a few expensive stocks if you feel their price has not hit it's peak yet."),
    h4("What if I don't bet the full $10?"),
    p("Your remaining balance will be added to your reward, but you need to spend at least $7 of your $10. You can't spend over $10 of course."),
    h4("How about an example?"),
    h3(htmlOutput("cash_text")),
    h3("Which of the following brands will sell the most refrigerators in 2021?"),
    hr(),
    h4("Samsung"),
    fluidRow(column(1,
                    tippy(text = paste0(dollar(p1), "/share"), tooltip = "$0.90/share")),
             column(1,
                    numericInput("q1", NULL, 0, min = 0, step = 1))
    ),
    h4("LG"),
    fluidRow(column(1,
                    tippy(text = paste0(dollar(p2), "/share"), tooltip = "$0.08/share")),
             column(1,
                    numericInput("q2", NULL, 0, min = 0, step = 1))
    ),
    h4("Whirlpool"),
    fluidRow(column(1,
                    tippy(text = paste0(dollar(p3), "/share"), tooltip = "$0.01/share")),
             column(1,
                    numericInput("q3", NULL, 0, min = 0, step = 1))
    ),
    h4("GE"),
    fluidRow(column(1,
                    tippy(text = paste0(dollar(p4), "/share"), tooltip = "$0.01/share")),
             column(1,
                    numericInput("q4", NULL, 0, min = 0, step = 1))
    ),
    h4("What's my reward?"),
    p("Hover over each price to see where the price ultimate ends ups."),
    htmlOutput('payout_text'),
    h4("Ready to begin?"),
    actionButton('begin', "Begin!", class = "btn-primary")
    
)


server <- function(input, output, session) {
    final_cash <- reactive({
        cash -
            (p1 * input$q1) -
            (p2 * input$q2) - 
            (p3 * input$q3) - 
            (p4 * input$q4) 
    })
    
    output$cash_text <- renderText({
        
        if(is.na(final_cash())) {color <- 'red'} else {
            if(final_cash() >= 0) color <- 'green'
            if(final_cash() < 0) color <- 'red'
        }
        
        paste0('Your Available Cash Balance = ','<span style=\"color:',color,'\">',dollar(final_cash()),"</span>")
        
    })
    
    payout <- reactive({
        
        final_cash() +
            (input$q1 * .9) +
            (input$q2 * .08) +
            (input$q3 * .01) +
            (input$q4 * .01)
    })
    
    output$payout_text <- renderText({
        
        if(is.na(final_cash())){
            retrun("Opps! Please ensure you have a value in each box for the number of bets you wish to place on that option.")
        }
        
        if(final_cash() < 0) {
            return(paste("Opps! You can't spend more than ", dollar(cash), "and get a payout."))
        }
        
        if(final_cash() > cash*.3){
            return(paste("Please place enough bets to spend at least ", dollar(cash*.7), "to see your payout."))
            
        } else {
            return(paste0('After the market closes and the prices have changed, your reward is <strong>', dollar(payout()), "</strong>."))
        }
        
    })
    
    observeEvent(input$begin, {
        session$sendCustomMessage("mymessage", "mymessage")
    })
}

shinyApp(ui = ui, server = server)
