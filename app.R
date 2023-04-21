library(tidyverse)
library(shiny)

portion_in_state = read_csv(file = "data/portion_in_state.csv")
Status_order = c(
  "Undergrad",
  "Grad",
  "Professional"
)

ui <- navbarPage(
  title = "Proprotion",
  tabPanel(
    title = "Input/Visualizaion",
    titlePanel(title = "UIUC student_by_stat1975 to 2017"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "st",
          label = "State",
          choices = sort(unique(portion_in_state$State))),
        selectInput(
          inputId = "yr",
          label = "Year",
          choices = sort(unique(portion_in_state$Year)),
        ),
        checkboxInput(inputId = "y",
                      label = "Filter Table to year",
                      value = FALSE)
      ),
      mainPanel(plotOutput("plot"))
    )
  ),
  tabPanel(title = "Table",dataTableOutput("table")),
  tabPanel(title = "About",includeMarkdown("about.Rmd"))
)


server <- function(input, output) {
    portion_st = reactive({
      portion_in_state|>
        filter(State == input$st)
    })
    
    observeEvent(
      eventExpr = input$st,
      handlerExpr = {
        updateSelectInput(inputId = "yr",
                          choices = sort(unique(portion_st()$Year)))
      }
    )
    
    
    
    output$plot = renderPlot({

      portion_in_state|>
        filter(State == input$st)|>
        filter(Year == input$yr)|>
        pivot_longer(Undergrad:Grad, names_to = "Status", values_to = "Count")|>
        group_by(Status)|>
        summarise(Count = sum(Count))|>
        mutate(Status = factor(Status, levels = Status_order))|>
        ggplot() + 
        aes(x = Status, y = Count, fill = Status)|>
        geom_bar(stat = "identity") + theme_bw()
    })
    
    
    output$table = renderDataTable({
      tab = portion_st()|>
        calc_undergrad_proportion()
      
      if (input$y) {
        tab = tab|>
          filter(Year == input$yr)
      }
      
      tab
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
