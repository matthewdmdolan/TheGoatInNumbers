library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Messi: The Goat?"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Machine Learning Predictions", tabName = "mlp", icon = icon("star"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      /* Change the background color */
      .skin-olive .main-sidebar, .skin-olive .left-side { background-color: black; }
      /* Change the box color */
      .box.box-solid.box-primary>.box-header { background-color: olive; color: white;}
      /* Change the dashboard body color */
      .skin-blue .content-wrapper, .skin-olive .main-footer { background-color: black; color: white; }
      /* Change the text color */
      .skin-blue .sidebar a { color: grey; }
      /* Change the header color */
      .skin-blue .main-header .logo, .skin-blue .main-header .navbar { background-color: grey; color: white; }
      /* Change the box border color */
      .box.box-solid.box-primary { border-color: grey; }
      /* Change the font to Arial */
      body { font-family: Arial !important; }
      "))),
    tabItems(
      tabItem(tabName = "Home",
              h2("Welcome to the Football Analytics Dashboard!"),
              tags$p("Using an extensive dataset, we have conducted an in-depth analysis covering the 2011/2012 to 2016/2017 seasons of key European football leagues."),
              tags$p("Our Shiny dashboard provides an evaluation of top European footballers, comparing them to arguably one of the greatest players of all time, Lionel Messi, at his prime."),
              tags$p("Although some argue that Messi only hit his peak with his recent World Cup victory, our analysis primarily focuses on club football due to the greater availability and comparability of this data over time.")
      ),
      tabItem(tabName = "dashboard",
              fluidRow(
                column(4, 
                       box(width = NULL,
                           plotOutput("p1", height = "350px", width = "100%"), 
                           title = "Games per Season", status = "primary", solidHeader = TRUE, collapsible = FALSE, badgeColor = "grey"
                       )
                ),
                column(4, 
                       box(width = NULL, 
                           plotOutput("p2", height = "350px", width = "100%"), 
                           title = "Goals Per Season", status = "primary", solidHeader = TRUE, collapsible = FALSE, badgeColor = "grey"
                       )
                ),
                column(4, 
                       box(width = NULL, 
                           plotOutput("p3", height = "350px", width = "100%"), 
                           title = "Goals Per Game Trend Analysis", status = "primary", solidHeader = TRUE, collapsible = FALSE, badgeColor = "grey"
                       )
                )
              ),
              fluidRow(
                column(6, 
                       box(width = NULL, 
                           plotOutput("p4", height = "350px", width = "100%"), 
                           title = "Messi Goals by Location", status = "primary", solidHeader = TRUE, collapsible = FALSE, badgeColor = "grey"
                       )
                ),
                column(6, 
                       box(width = NULL, 
                           plotOutput("p5", height = "350px", width = "100%"), 
                           title = "Goals Against Opposition", status = "primary", solidHeader = TRUE, collapsible = FALSE, badgeColor = "grey"
                       )
                )
              ),
              fluidRow(
                column(6, 
                       box(width = NULL,
                           plotOutput("p7", height = "350px", width = "100%"), 
                           title = "Messi's Most Frequent Assists by Teammate ", status = "primary", solidHeader = TRUE, collapsible = FALSE, badgeColor = "grey"
                       )
                )
              )
      ),
      tabItem(tabName = "mlp",
              fluidRow(
                column(4, 
                       box(width = NULL,
                           plotOutput("p8", height = "350px", width = "100%"), 
                           title = "Games per Season", status = "primary", solidHeader = TRUE, collapsible = FALSE, badgeColor = "grey"
                       )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$p1 <- renderPlot({ p1 }, bg = "grey", res = 80)
  output$p2 <- renderPlot({ p2 }, bg = "grey", res = 80)
  output$p3 <- renderPlot({ p3 }, bg = "grey", res = 80)
  output$p4 <- renderPlot({ p4 }, bg = "grey", res = 80)
  output$p5 <- renderPlot({ p5 }, bg = "grey", res = 80)
  output$p7 <- renderPlot({ p7 }, bg = "grey", res = 80)
  output$p8 <- renderPlot({ p8 }, bg = "grey", res = 80)
}

# Launch


# Launch the Shiny app
shinyApp(ui = ui, server = server)
