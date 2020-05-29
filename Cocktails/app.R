library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinythemes)

data <- readr::read_csv('cocktails_long_recipes.csv')
bc2 <- readr::read_csv('cleaned_cocktails.csv')
cocktail_name = "The Oaxaca Old-Fashioned"

# Define UI ----
ui <- fluidPage(theme = shinytheme("cosmo"),
                tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Pacifico" rel="stylesheet">')),
                tags$head(HTML('<style>* {font-size: 100%; font-family: Pacifico; color:#3B0F70FF;}</style>')),
                tags$body(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
                tags$body(HTML('<style>* {font-size: 100%; font-family: Roboto Mono;color:#3B0F70FF;}</style>')),
                titlePanel(div("Choose a Cocktail #TidyTuesday Style", style = "font-family:Pacifico; color:#3B0F70FF")),
                fluidRow(
                    column(6,div(DT::dataTableOutput("table"), style = "font-size:80%; font-family:Roboto Mono"), height=500),
                    column(6,p(strong("To choose a cocktail:"),"search for a name, category or ingredient in the table, and then click on a row to see the recipe *as a graph*!"),
                           p("This app was built with ", tags$a(href="https://shiny.rstudio.com", "Shiny"), "for ", tags$a(href="https://github.com/rfordatascience/tidytuesday", "#TidyTuesday"), 
                             " and code can be found on my ",  tags$a(href="https://github.com/aluby/TidyTuesday", "github")),
                                  plotOutput("cocktailPlot")
                           )
                )
)


server <- function(input, output) {
    output$table <- DT::renderDataTable(
        data, options = list(rownames = FALSE), selection = 'single')
    dataset = reactive({
        req(input$table_rows_selected)
        
        bc2 %>% filter(name == data$name[input$table_rows_selected])})
    
    
    output$cocktailPlot = renderPlot(ggplot(dataset(), aes(x= name, y = amount, fill = ingredient_number, label = ingredient)) +
        geom_bar(position = "stack", stat = "identity") +
        geom_text(position = position_stack(vjust=.5), col = "white", family = "Pacifico") +
        theme_minimal(base_family = "Pacifico", base_size = 20)  +
        theme(legend.position = "none",
              axis.text.x = element_blank()) +
        labs(
            title = data$name[input$table_rows_selected],
            x = " ",
            y = "Ounces"
        ) +
        scale_fill_viridis_c(end =.8, option = "magma"))
}

shinyApp(ui, server)
