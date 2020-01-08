library(shiny)
library(DT)

#initilise empty teams
roster <- tibble()
test_team <- tibble(c("opener","opener","batsman","batsman","batsman","batsman","wicketkeeper","bowler","bowler","bowler","bowler"))
odi_team <- test_team
t20i_team <- test_team
# colnames(roster) <- "Squad"
colnames(test_team) <- "Test XI"
colnames(odi_team) <- "ODI XI"
colnames(t20i_team) <- "T20I XI"

# bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- navbarPage(
  
  title = 'Cricket',
      tabPanel("Test", 
               
               sidebarLayout(
                 sidebarPanel(
                    dataTableOutput("testable")
                  ),
                  mainPanel(
                    tableOutput("playman"),
                    actionButton("select", "Select player"),
                    fluidRow(
                      column(4,dataTableOutput("test")),
                    column(4,dataTableOutput("odi")),
                    column(4,dataTableOutput("t20i"))
                  ),
                  ),
               )
    )
)

server <- function(input, output) {
  options(DT.options = list(pageLength = 50))
  
  
  output$testable <- renderDataTable(datatable(table[c(1,20)],selection = "single", extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE
  )))
  output$playman <- eventReactive(input$testable_rows_selected,{ })
  renderTable( table[input$testable_rows_selected,])
  output$test <- renderDataTable(datatable(test_team,selection = "single",options = list(dom = 't')))
  output$odi <- renderDataTable(datatable(odi_team,selection = "single",options = list(dom = 't')))
  output$t20i <- renderDataTable(datatable(t20i_team,selection = "single",options = list(dom = 't')))
}

shinyApp(ui = ui, server = server)