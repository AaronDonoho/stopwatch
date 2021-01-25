
library(shiny)

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
    ),
    uiOutput("outputcounter"),
    div(
        id = "controls",
        tags$fieldset(
            tags$legend("Setup"),
            actionButton("start", "start"),
            actionButton("pause","pause"),
            actionButton("reset", "reset")
        ),
        tags$fieldset(
            tags$legend("Count"),
            actionButton("countup", "count up"),
            actionButton("countdown", "count down")
        ),
        tags$fieldset(
            tags$legend("Set to"),
            textInput("value", label = NULL, value = "0"),
            br(),
            actionButton("setto", "set value")
        ),
        tags$fieldset(
            tags$legend("Speed"),
            textInput("speed", label = NULL, value = "1000"),
            br(),
            actionButton("setspeed", "set speed"),
        ),
        tags$fieldset(
            tags$legend("Increase"),
            textInput("increase", label = NULL, value = "1"),
            br(),
            actionButton("setincrease", "set increase")
        )
    )
)


server <- function(input, output) {
    
    value = reactiveVal(0)
    speed = reactiveVal(1000)
    countup = reactiveVal(TRUE)
    delta = reactiveVal(1)
    active = reactiveVal(FALSE)
    
    observe({
        invalidateLater(speed())
        isolate({
            req(active())
            value(value() + ifelse(countup(), delta(), -delta()))
        })
    })
    
    observeEvent(input$start, {
        active(TRUE)
    })
    
    observeEvent(input$pause, {
        active(FALSE)
    })
    
    observeEvent(input$reset, {
        value(0)
    })
    
    observeEvent(input$countup, {
        countup(TRUE)
    })
    
    observeEvent(input$countdown, {
        countup(FALSE)
    })
    
    observeEvent(input$setto, {
        value(as.numeric(input$value))
    })
    
    observeEvent(input$setincrease, {
        delta(as.numeric(input$increase))
    })
    
    observeEvent(input$setspeed, {
        speed(as.numeric(input$speed))
    })
    
    output$outputcounter = renderUI({
        div(
            id = "counter",
            value()
        )
    })
    
}

shinyApp(ui = ui, server = server)
