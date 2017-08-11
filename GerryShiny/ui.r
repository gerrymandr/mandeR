library(shiny)

fluidPage("Gerrymandering Metrics",
                sidebarPanel(
                  selectInput(inputId="state",label="Select State",choices=c("North Carolina","Massachusetts")),
                  radioButtons(inputId="metrics",choices=c("PolsbyPopp","ConvexHull","Reock"),label="Specify Metrics to Calculate")),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Table",DT::dataTableOutput("table")),
                    tabPanel("Global",plotOutput("globalMap")),
                    tabPanel("Selection",fluidRow(plotOutput("map"))),fluidRow(dataTableOutput("table2")))
                )
)