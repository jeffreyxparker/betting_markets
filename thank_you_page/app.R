library(shiny)

ui <- fluidPage(
    h1("Thank you! Your bets were recorded.", align = "center"),
    h3("When a sufficient volume of bets has been received, we will close the market and pay you according to your bets and their final price, plus any balance. Hopefully, you chose wisely and the price of what you bet on increased and therefore you increased your reward. If you have any questions, please reach out to the market administrator at jeffrey.x.parker@gmail.com.", align = "center")
)

server <- function(input, output) {}
shinyApp(ui = ui, server = server)
