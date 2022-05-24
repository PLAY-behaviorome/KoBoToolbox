#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

email_regex <- "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}"

library(shiny)

ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    textInput("db_login",
              "Databrary login:"),
    # passwordInput("password",
    #               "Databrary password:"),
    actionButton("login", "Log in", class = "btn-success"),
    actionButton("logout", "Log out", disabled = TRUE, class = "btn-warning"),
    uiOutput("button"),
    actionButton("RunFullModel", "Run Full Model")
)

server <- function(input, output) {
    #---------------------------------------------------------------------------
    # From https://stackoverflow.com/questions/43641103/change-color-actionbutton-shiny-r?rq=1
    global <- reactiveValues(clicked = FALSE)

    observe({
        if (length(input$RunFullModel)) {
            if (input$RunFullModel)
                global$clicked <- TRUE
        }
    })
    
    output$button <-  renderUI({
        if (!is.null(input$RunFullModel) & global$clicked) {
            actionButton(
                inputId = "button",
                label = icon("tree-deciduous", lib = "glyphicon"),
                disabled = TRUE,
                style = "color: white;
                     background-color: #35e51d;
                     position: relative;
                     left: 3%;
                     height: 35px;
                     width: 35px;
                     text-align:center;
                     text-indent: -2px;
                     border-radius: 6px;
                     border-width: 2px"
            )
        } else{
            actionButton(
                inputId = "RunFullModel",
                label = icon("tree-deciduous", lib = "glyphicon")
            )
        }
    })
    #---------------------------------------------------------------------------
    
    observeEvent(input$login, {
        valid_email <-
            stringr::str_detect(toupper(input$db_login), email_regex)
        shinyFeedback::feedbackWarning("db_login", !valid_email , "Please enter valid email.")
        req(valid_email)
        message("Logging in")
        login_status <- databraryapi::login_db(input$db_login)
        if (login_status) {
            showNotification("Logged in to Databrary", type = "message")
            #updateActionButton(session, "logout", disabled = FALSE)
        }
    })
    observeEvent(input$logout, {
        message("Logging out")
        databraryapi::logout_db()
        showNotification("Logged out of Databrary")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
