#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
devtools::install_github('Vessy/Rmaze')
library(Rmaze)
library(shiny)
runApp( list(ui = fluidPage(
    verbatimTextOutput("result"),
    tags$script('
                $(document).on("keydown", function (e) {
                Shiny.onInputChange("mydata", e.which);
                });
                '),
    
    plotOutput('plot')
    
    )
    , server = function(input, output, session) {
        
        output$result = renderPrint({
            input$mydata
        })
        
        output$plot <- renderPlot({
            n <- 10
            maze <- makeGraph(n, n)
            set.seed(runif(1,0,100))
            maze <- makeMaze_dfs(maze)
            plotMaze(maze,n,n,TRUE)
            
            
        })
    }
))

