
library(shiny)
library(rhandsontable)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage("Constructing the Conditional Probability Tables", theme = shinytheme("flatly"),

    tabPanel("Setup",
  
      fluidRow(
        # column(12, h3("Setup"), style='padding:20px;'),
        column(3,
               uiOutput("userUI")
        , offset=1),
      
        column(3,
               uiOutput("contextUI")
        ) 
        
      ),
    
      fluidRow( 
        column(6,
               h5(strong("Network Structure:")),
               plotOutput("fullgraph", width="800px", height="300px")
        , offset=1)
      )
    ),
  
    tabPanel("Select Node",
  
  fluidRow(
    column(3,
           uiOutput("nodeUI")
           ,offset = 1),
    column(3,
           # uiOutput("nodeCheckUI"),
           tableOutput("fileList"),
           strong(textOutput("fileText")),
           tags$head(tags$style("#fileText{color: red;
                                 font-size: 12px;
                                font-style: italic;
                                }"
                         ))
    ),
    column(3,
           h5(strong("Node Preview:")),
           plotOutput("previewgraph", width="400px", height="300px")
    )
  ),
  fluidRow(
    column(6,
           
           selectInput("expertise", label="Select Overall Expertise:", c("Some Subject Knowledge"="Some", "Expert in Subject"="Expert"), selected = "Expert", multiple = FALSE, selectize = TRUE),
           helpText("Select a level of expertise you feel is representative of your knowledge about the relationships depicted in above figure. As a guide, we would suggest the following definitions:"),
           helpText("Expert: Someone who has in depth knowledge of the processes underlying these relationships and is aware of the literature surrounding this."),
           helpText("Some Subject Knowledge: Someone who is aware of the relationships depicted in the figure above but does not have in depth knowledge of the underlying processes and/or the literature surrounding these.")
           
  #   selectInput("expertise", label="Select Overall Expertise:", c("No Subject Knowledge"="None", "Some Subject Knowledge"="Some", "Expert in Subject"="Expert"), selected = NULL, multiple = FALSE, selectize = TRUE),
  #   helpText("Select a level of expertise you feel is representative of your knowledge about the relationships depicted in above figure. As a guide, we would suggest the following definitions:"),
  #   helpText("Expert: Someone who has direct knowledge of the processes underlying these relationships and is aware of the literature surrounding this."),
  #   helpText("Some Subject Knowledge: Someone who is aware of the relationships depicted in the figure above but does not have detailed knowledge of the underlying processes and/or literature surrounding these."),
  #   helpText("No Subject Knowledge: Someone who is unaware of the relationships depicted in the figure above."
  # )
  ,offset=1)
  )),
    
 tabPanel("Complete Table",
  
  fluidRow(
    column(12,
      # h3("Complete Table"),
      
      h4("Filling in the conditional probability table for ",strong(textOutput("CPTtextuser", inline=TRUE)),"for the ", strong(textOutput("CPTtextcontext", inline=TRUE)), " context"),
      tags$head(tags$style("#CPTtextuser{color: red;
                                 font-size: 20px;
                           font-style: italic;
                           }"
                         )
      ),
      tags$head(tags$style("#CPTtextcontext{color: blue;
                                 font-size: 20px;
                           font-style: italic;
                           }"
      )
      ),
      textOutput("CPTtext")
      , style='padding:20px;'),
    
      h5(""),

      column(6,
            wellPanel(
              selectInput("empty", label="Load Table", c("Automated Table"="a", "Previous table"="p", "Empty Table"="e"), selected = "a", multiple = FALSE, selectize = TRUE),
              helpText("Selecting 'Automated', will generate an initial belief table that can be edited using the options below."),
              helpText("Selecting 'Previous', will load any previously saved belief tables."),
              helpText("Selecting 'Empty', will generate a blank belief table that can be populated by entering values in the table below.")
            )
      ),
    column(6,
           wellPanel(
             selectInput("confidence", label="Select Overall Confidence:", c("Low"="Low", "Medium"="Medium", "High"="High"), selected = "Medium", multiple = FALSE, selectize = TRUE),
             helpText("Select a level of confidence you feel is representative of your beliefs about the relationships depicted in the figure below. As a guide, we would suggest the following definitions:"),
             helpText("Low: Low confidence in the final beliefs and would consider it likely the values to vary."),
             helpText("Medium: Reasonably confident/certain in the final beliefs, although you believe the final values could vary."),
             helpText("High: Reasonably confident in the final beliefs, and wouldn't consider these to vary much."
             )
           ))
  ),
  
  
   fluidRow(
     column(3,
            wellPanel(
              textOutput("scoreText"),
              h4(""),
              uiOutput("scoreParentsUI_1"),
              uiOutput("scoreParentsUI_2"),
              uiOutput("scoreParentsUI_3"),
              uiOutput("scoreParentsUI_4"),
              uiOutput("scoreParentsUI_5")
            )
    ),
    column(3,
            wellPanel(
             textOutput("directionText"),
             h4(""),
             uiOutput("directionParentsUI_1"),
             uiOutput("directionParentsUI_2"),
             uiOutput("directionParentsUI_3"),
             uiOutput("directionParentsUI_4"),
             uiOutput("directionParentsUI_5"),
             textOutput("directionHelp")
            )
    ),
    column(5,
           wellPanel(
             tabsetPanel(
               tabPanel(textOutput("dirOtherText_1"),
                        textOutput("directionHelpText_1"),
                        h5(""),
                 lapply(1:10, function(k){
                   uiOutput(paste0("dirOtherParentsUI_1.", k))
                 })
               ),
               tabPanel(textOutput("dirOtherText_2"),
                        textOutput("directionHelpText_2"),
                        h5(""),
                lapply(1:10, function(k){
                  uiOutput(paste0("dirOtherParentsUI_2.", k))
                })
               ),
               tabPanel(textOutput("dirOtherText_3"),
                        textOutput("directionHelpText_3"),
                        h5(""),
                lapply(1:10, function(k){
                  uiOutput(paste0("dirOtherParentsUI_3.", k))
                })
               ),
               tabPanel(textOutput("dirOtherText_4"),
                        textOutput("directionHelpText_4"),
                        h5(""),
                lapply(1:10, function(k){
                  uiOutput(paste0("dirOtherParentsUI_4.", k))
                })
               ),
               tabPanel(textOutput("dirOtherText_5"),
                        textOutput("directionHelpText_5"),
                        h5(""),
                lapply(1:10, function(k){
                  uiOutput(paste0("dirOtherParentsUI_5.", k))
                })
               ),
               tabPanel(textOutput("dirOtherText_6"),
                        textOutput("directionHelpText_6"),
                        h5(""),
                lapply(1:10, function(k){
                  uiOutput(paste0("dirOtherParentsUI_6.", k))
                })
               )
           ))
   )
   ),

  fluidRow(
    column(3,
           actionButton("showPlot", "Update Visualisations")#, style="color: #C49D48; width:100%;white-space:normal;")
    ),
    column(3,
           actionButton('saveData', 'Save CPT', icon=icon("save"))#, style="color: #48922E; width:100%;white-space:normal;")
    )
  ),
  
  fluidRow(
    column(6,
           helpText("Once happy with your conditional probability table (CPT), press Save CPT...")
           )
  ),
  
  # h5(""),

  fluidRow(
    column(7,
           wellPanel(
             tabsetPanel(
               tabPanel("Default",
                        plotOutput("visual2CPT")
                        ),
               tabPanel("Ranked",
                        plotOutput("visualrankedCPT")
               )
             )
           
           )
    ),
    column(5,
           h5("How many (out of 100) times would you expect ", strong(textOutput("CPTtextchild", inline=TRUE)), "to be ", textOutput("CPTtextchildlevels", inline=TRUE), textOutput("CPTtextparents", inline=TRUE), "?"),
           helpText("Individual values can be changed directly in the table below."),
           helpText("Note: Each row of frequencies must add up to 100, the Total column will turn grey, when this is satisfied."),
           h5(""),
           rHandsontableOutput('table')
    , style='padding:20px;')
   ),

   h5(""),

   fluidRow(column(12, h3(" ")))

    
  )
))
