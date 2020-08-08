library(shiny)
library(howdoi)

ui <- fluidPage(
    titlePanel("ggeasy::howdoi()"),
    sidebarLayout(
        sidebarPanel(
            textInput("text", "How do I ..."),
            actionButton("submit", "Still not sure...", icon("life-ring"))
        ),
        mainPanel(
            conditionalPanel(
                condition = "input.submit > 0",
                uiOutput("url")
            ),
            column(6, DT::dataTableOutput("easy_res")),
            column(6, DT::dataTableOutput("ggplot_res"))
        )
    )
)

server <- function(input, output) {
    observeEvent(input$submit, {
        browseURL(paste0("https://github.com/jonocarroll/howdoi/issues/new?title=[auto] howdoi suggestion&body=I was searching for '",
                         input$text,
                         "' but didn't find what I was looking for.\n\nI think I need a shortcut that <...>"))
    })
    output$easy_res <- DT::renderDataTable({
        req(input$text)
        DT::datatable(
            data.frame(ggeasy = howdoi:::find_call(input$text)[["ggeasy"]]),
            rownames = FALSE,
            colnames = NULL,
            extensions = c("Buttons", "Select"),
            options =
                list(
                    select = TRUE,
                    dom = "Brtip",
                    buttons = list(
                        list(
                            extend = "copy",
                            text = 'Copy {ggeasy} call',
                            exportOptions = list(modifier = list(selected = TRUE)),
                            title = NULL
                        )
                    )
                )
        )
    })
    output$ggplot_res <- DT::renderDataTable({
        req(input$text)
        DT::datatable(
            data.frame(ggplot2 = howdoi:::find_call(input$text)[["ggplot2"]]),
            rownames = FALSE,
            colnames = NULL,
            extensions = c("Buttons", "Select"),
            options =
                list(
                    select = TRUE,
                    dom = "Brtip",
                    buttons = list(
                        list(
                            extend = "copy",
                            text = 'Copy {ggplot2} call',
                            exportOptions = list(modifier = list(selected = TRUE)),
                            title = NULL
                        )
                    )
                )
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
