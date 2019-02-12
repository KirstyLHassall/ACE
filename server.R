

library(shiny)
library(shinyjs)
library(rhandsontable)
library(plotrix)
library(grid)
library(RColorBrewer)
library(lattice)
library(igraph)
library(shape)
library(latticeExtra)


# options(warn=-1) # supresses warings, so only errors show in the logs

# Define server logic 
shinyServer(function(input, output, session) {
   
  
  
  # **********************************************************************
  # Preliminaries ----
  
  fileLoc <- getwd()
  pal <- brewer.pal(9, "Set1")

  print(fileLoc)
  
  set.seed(97)
  
  users <- read.csv("nodeTables//Usernames.csv")
  userElicitee <- users$User
  
  users$User <- gsub(" ", "", users$User)
  users$User <- gsub(":", "", users$User)
  users$User <- gsub("-", "", users$User)
  
  allcontexts <- read.csv("nodeTables//NetworkNames.csv")

  edges <- reactive({
    print(input$context)
    context <- gsub(" ", "", input$context)
    context <- gsub(":", "", context)
    read.csv(paste("nodeTables//Edge_", context, ".csv", sep=""))
    })
  
  nodes <- reactive({
    context <- gsub(" ", "", input$context)
    context <- gsub(":", "", context)
    read.csv(paste("nodeTables//Node_netOrder_", gsub(" ", "", context), ".csv", sep=""))
    })
  
  net <- reactive(graph_from_data_frame(d=edges(), directed=TRUE))
  
  nNodes <- reactive(length(nodes()$Node))

  nodecolours <- reactive(pal[as.numeric(as.factor(nodes()$Type))])

  source("createTable.R")
  source("plotTable.R")
 
  
  #***********************************************************************
  # Preview tables -----
 
  output$contextUI <- renderUI({
    selectInput("context", label="Choose Network:",
                allcontexts$Networks, selected=character(0), multiple = FALSE, selectize = TRUE
    )
  })
  
   output$userUI <- renderUI({
    selectInput("id", "User ID:",
                userElicitee, selected=character(0)
    )
    
  })
  
  output$nodeUI <- renderUI({
    radioButtons("child", "Select Node:",
                 # nodes()$Node[nodes()$Type=="Elicited"]
                 nodes()$Node[nodes()$nodeOrder], selected=character(0)
    )
    
  })
  
 
  
  getFileList <- eventReactive(input$child, {
    savedTables <- list.files("savedCPTs")
    
    
    if (!is.null(input$context) & !is.null(input$id)){
      
      allFiles <- rep(NA, nNodes())
      
      for (i in 1:nNodes()){
        
        # How many Parents
        selectedNode <- as.character(nodes()$Node[nodes()$nodeOrder[i]])
        
        if (length(adjacent_vertices(net(), selectedNode, mode = c("in"))) > 0){
          parents <- names(adjacent_vertices(net(), selectedNode, mode = c("in"))[[1]])
        } else {
          parents <- NULL
        }
        
        selectedNode <- gsub(" ", "-", selectedNode)
        parents <- gsub(" ", "-", parents)
        context <- gsub(" ", "",input$context)
        context <- gsub(":", "", context)
        id <- gsub(" ", "", input$id)
        
        allFiles[i] <- paste(context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
        
      }
      
      x <- nodes()$Node[nodes()$nodeOrder[allFiles %in% savedTables]]  
      
      if (is.null(x))
        x <- character(0)    
      
      listData <- data.frame('Completed Nodes'=x)
      names(listData) <- "Completed Nodes"
      return(listData)
    } else{
      return(NULL)
    }
  })
  
  getFilesDone <- eventReactive(input$child, {
    savedTables <- list.files("savedCPTs")
    
    
    if (!is.null(input$context) & !is.null(input$id)){
      
      allFiles <- rep(NA, nNodes())
      
      for (i in 1:nNodes()){
        
        # How many Parents
        selectedNode <- as.character(nodes()$Node[nodes()$nodeOrder[i]])
        
        if (length(adjacent_vertices(net(), selectedNode, mode = c("in"))) > 0){
          parents <- names(adjacent_vertices(net(), selectedNode, mode = c("in"))[[1]])
        } else {
          parents <- NULL
        }
        
        selectedNode <- gsub(" ", "-", selectedNode)
        parents <- gsub(" ", "-", parents)
        context <- gsub(" ", "",input$context)
        context <- gsub(":", "", context)
        id <- gsub(" ", "", input$id)
        
        allFiles[i] <- paste(context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
        
      }
      
      x <- nodes()$Node[nodes()$nodeOrder[allFiles %in% savedTables]]  
      
      if (is.null(x))
        x <- character(0)    
      
      return(paste(length(x),"out of", nNodes(), "completed"))
    } else{
      return(NULL)
    }
  })
  
  output$fileList <- renderTable({
    
    getFileList()
    
  }, spacing="xs", bordered=FALSE)
  
  output$fileText <- renderText({
    
    getFilesDone()
    
  })
  
  #***********************************************************************
  # Preview Network Structure -----
  
  output$fullgraph <- renderPlot({
    
    if (!is.null(input$context)){
      nodeNames <- V(net())$name
      nodeNames <- gsub(" ", "\n", nodeNames)
      # V(net())$name <- nodeNames
      
      net_layout <- layout_with_fr(net())
      net_layout <- norm_coords(net_layout, ymin=-1, ymax=1, xmin=-2, xmax=2)
  
      par(mar=c(1,1,1,1))
      plot(net(),
           vertex.color=nodecolours(), vertex.size=20, vertex.frame.color=NA, vertex.label.family="sans", vertex.label.font=2, vertex.label=nodeNames,
           edge.arrow.size=.8, rescale=FALSE,
           layout=net_layout)
    } else{
      return(NULL)
    }
  })
  
  
  output$previewgraph<- renderPlot({
    
    selectedNode <- input$child
    
    if (is.null(selectedNode)){
      par(mar=c(0,0,0,0))
      plot(c(-6, 6), c(-3,3), asp=1, type = "n", xlab="", ylab="", axes=FALSE)
      text(0,0, "Please select a child node")
      box()
    } else if(sum(selectedNode == nodes()$Node) == 0){
      
      par(mar=c(0,0,0,0))
      plot(c(-6, 6), c(-3,3), asp=1, type = "n", xlab="", ylab="", axes=FALSE)
      text(0,0, "Please select a child node")
      box()
      
    } else if (sum(selectedNode == nodes()$Node) == 1){
      if (length(adjacent_vertices(net(), selectedNode, mode = c("in"))) > 0){
        parents <- names(adjacent_vertices(net(), selectedNode, mode = c("in"))[[1]])
      } else {
        parents <- NULL
      }
      
      selectedNode <- gsub(" ", "\n", selectedNode)
      parents <- gsub(" ", "\n", parents)
      
      # # get vertex ids
      # keepID <- which(V(net)$name%in%c(selectedNode, parentID))
      # 
      # net2 <- subgraph(net, keepID)
      # 
      # 
      # V(net2)$name <- gsub(" ", "\n", V(net2)$name)
      # 
      # par(mar=c(1,1,1,1))
      # plot(net2,
      # vertex.color="orange", vertex.size=20, vertex.frame.color=NA, vertex.label.family="sans", vertex.label.font=2,
      # edge.arrow.size=.8, rescale=FALSE,
      # layout=layout_on_grid)
      
      
      par(mar=c(0,0,0,0))
      plot(c(-6, 6), c(-3,3), asp=1, type = "n", xlab="", ylab="", axes=FALSE)
      
      if (length(parents) == 0){
        
        # rect( -1, -1, 1, 1) 
        draw.circle( 0, 0, 5/4, col="orange" )
        text(0,0, selectedNode)
        
      } else {
        
        nParents <- length(parents)
        
        
        centrex <- seq(from=-6 + .1, to=6 - .1, length=nParents + 1)
        r <- min((centrex[2] - centrex[1]) * 0.4, 5/4)
        centrex <- centrex - (centrex[2] - centrex[1]) * 0.5
        
        # rect( -1, -1, 1, 1) 
        draw.circle( 0, -3 + r*1.1, r, col="orange" )
        text(0,-3 + r*1.1, selectedNode)
        
        
        
        for (i in 1:nParents){
          draw.circle( centrex[i + 1], 4 - r*1.1, r )
          text(centrex[i + 1], 4 - r*1.1, paste(strsplit(parents[i], ".", fixed=TRUE)[[1]], collapse="\n"))
          Arrows(centrex[i + 1], 4 - r*2.1, 0, -3 + r*2.1, lwd=1, arr.type = "triangle")
          
        }
      }
      box()
      
    } else{
    
      par(mar=c(0,0,0,0))
      plot(c(-6, 6), c(-3,3), asp=1, type = "n", xlab="", ylab="", axes=FALSE)
      text(0,0, "Something isn't quite right,\n please contact a facilitator", col="red")
      box()
      
    }
  })
  
  
  
  # ************************************************************************
  # Tables -----

  
  
  output$CPTtext <- renderText({
    
    
    if(!is.null(input$child)){
      selectedNode <- input$child
      
      if (sum(selectedNode == nodes()$Node) == 0){
        
        print("Something isn't quite right, please reselect a child node")
        
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        
        # parents <- gsub("-", " ", parents)
        
        if (length(parents) > 0){
        
          paste("Quantifying beliefs about the node '", selectedNode, "' given all possible states of the input nodes, '", paste0(parents, collapse="' and '"), "'.", sep="")
        } else{
          paste("Quantifying beliefs about the distribution of the node '", selectedNode, "'.", sep="")
        }
      }
    }
  })
  
  output$CPTtextuser <- renderText({
    
    if(!is.null(input$id)){
      paste(input$id)
    }
  })
  
  output$CPTtextchild <- renderText({
    
    if(!is.null(input$child)){
      paste(input$child)
    }
  })
  
  output$CPTtextchildlevels <- renderText({
    
    if(!is.null(input$child)){
      if (sum(input$child == nodes()$Node) == 0){
        paste("")
      } else{
        childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
        childStates <- childStates[!is.na(childStates)]
        childStates <- childStates[childStates != ""]
        childStates <- childStates[childStates != " "]
        
        paste(childStates, collapse=", ")
      }
      
    }
  })
  
 
  
  output$CPTtextparents <- renderText({
    
    if(!is.null(input$child)){
      selectedNode <- input$child
      
      if (sum(selectedNode == nodes()$Node) == 0){
        
        print("")
        
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        
        # parents <- gsub("-", " ", parents)
        
        if (length(parents) > 0){
          
          paste(" for different values of ", paste0(parents, collapse=" and "))
        } else{
          print("")
        }
      }
    }
  })
  
  output$CPTtextcontext <- renderText({
    
    if(!is.null(input$context)){
      paste(input$context)
    }
  })
  
  expertise <- eventReactive(input$expertise, return(input$expertise))
  confidence <- eventReactive(input$confidence, return(input$confidence))
  
  output$scoreText <- renderText({
    
    if(!is.null(input$child)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else{
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
      
        if (input$empty == "a" && nParents > 0){
          paste("Weight the parents by relative importance/influence on the child node:", input$child)
        } else{
          return(NULL)
        }
      }
      
      
    } else{
      return(NULL)
    }
    })
  
  output$scoreParentsUI_1 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        parents <- paste("Parent:", parents)
        if (nParents == 0){
          return(NULL)
        } else if (nParents == 1){
          # sliderInput("p1", parents[1], min=0, max=1, value=0.5, step=.1)
          numericInput("p1", parents[1], value=1, min=1, step=1)
        } else if (nParents > 1){
          # sliderInput("p1", parents[1], min=0, max=1, value=0.5, step=.1)
          numericInput("p1", parents[1], value=1, min=1, step=1)
        }
      }
      
    } else{
      return(NULL)
    }
  })
  
  output$scoreParentsUI_2 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        parents <- paste("Parent:", parents)
        if (nParents > 1){
          # sliderInput("p2", parents[2], min=0, max=1, value=0.5, step=.1)
          numericInput("p2", parents[2], value=1, min=1, step=1)
        } else {
          # radioButtons("p2",choices = list("Absent" = 0))
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$scoreParentsUI_3 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        parents <- paste("Parent:", parents)
        if (nParents > 2){
          # sliderInput("p3", parents[3], min=0, max=1, value=0.5, step=.1)
          numericInput("p3", parents[3], value=1, min=1, step=1)
        } else {
          return(NULL)
        }
      }
    } else {
      return(NULL)
    }
  })
  
  output$scoreParentsUI_4 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        parents <- paste("Parent:", parents)
        if (nParents > 3){
          # sliderInput("p4", parents[4], min=0, max=1, value=0.5, step=.1)
          numericInput("p4", parents[4], value=1, min=1, step=1)
        } else {
          # radioButtons("p4",choices = list("Absent" = 0))
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$scoreParentsUI_5 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        parents <- paste("Parent:", parents)
        if (nParents > 4){
          # sliderInput("p5", parents[5], min=0, max=1, value=0.5, step=.1)
          numericInput("p5", parents[5], value=1, min=1, step=1)
        } else {
          # radioButtons("p5",choices = list("Absent" = 0))
          return(NULL)
        }
      }
    } else {
      return(NULL)
    }
  })
  
  output$scoreParentsUI_6 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        parents <- paste("Parent:", parents)
        if (nParents > 5){
          # sliderInput("p6", parents[6], min=0, max=1, value=0.5, step=.1)
          numericInput("p6", parents[6], value=1, min=1, step=1)
        } else {
          # radioButtons("p6",choices = list("Absent" = 0))
          return(NULL)
        }
      }
    } else {
      return(NULL)
    }
  })
  
  output$directionText <- renderText({
    
    if(!is.null(input$child)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && nParents > 0){
          childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
          childStates <- childStates[!is.na(childStates)]
          childStates <- childStates[childStates != ""]
          childStates <- childStates[childStates != " "]
          
          paste("Define the direction of the relationship each parent has on the child node:", input$child, "which has levels ", paste(childStates, collapse=", "))
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$directionHelp <- renderText({
    if(!is.null(input$child)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && nParents > 0){
          paste("Selecting 'Other', will generate further options (right) to specify bespoke relationships")
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$directionParentsUI_1 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        if (nParents == 0){
          return(NULL)
        } else 
          
          p <- parents[1]
          parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
          parentStates <- parentStates[!is.na(parentStates)]
          parentStates <- parentStates[parentStates != ""]
          parentStates <- parentStates[parentStates != " "]
          
          selectInput("d1", paste(p, ": ", paste(parentStates, collapse=", "), sep=""), c("Positive"="pos", "Negative"="neg", "Other"="other"), selected="pos")
      }
    } else{
      return(NULL)
    }
  })
  
  output$directionParentsUI_2 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        if (nParents > 1){
          
          p <- parents[2]
          parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
          parentStates <- parentStates[!is.na(parentStates)]
          parentStates <- parentStates[parentStates != ""]
          parentStates <- parentStates[parentStates != " "]
          
          selectInput("d2", paste(p, ": ", paste(parentStates, collapse=", "), sep=""), c("Positive"="pos", "Negative"="neg", "Other"="other"), selected="pos")
        } else {
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$directionParentsUI_3 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        if (nParents > 2){
          
          p <- parents[3]
          parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
          parentStates <- parentStates[!is.na(parentStates)]
          parentStates <- parentStates[parentStates != ""]
          parentStates <- parentStates[parentStates != " "]
          
          selectInput("d3", paste(p, ": ", paste(parentStates, collapse=", "), sep=""), c("Positive"="pos", "Negative"="neg", "Other"="other"), selected="pos")
        } else {
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$directionParentsUI_4 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        if (nParents > 3){
          
          p <- parents[4]
          parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
          parentStates <- parentStates[!is.na(parentStates)]
          parentStates <- parentStates[parentStates != ""]
          parentStates <- parentStates[parentStates != " "]
          
          selectInput("d4", paste(p, ": ", paste(parentStates, collapse=", "), sep=""), c("Positive"="pos", "Negative"="neg", "Other"="other"), selected="pos")
        } else {
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$directionParentsUI_5 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        if (nParents > 4){
          
          p <- parents[5]
          parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
          parentStates <- parentStates[!is.na(parentStates)]
          parentStates <- parentStates[parentStates != ""]
          parentStates <- parentStates[parentStates != " "]
          
          selectInput("d5", paste(p, ": ", paste(parentStates, collapse=", "), sep=""), c("Positive"="pos", "Negative"="neg", "Other"="other"), selected="pos")
        } else {
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$directionParentsUI_6 <- renderUI({
    if(!is.null(input$child) & input$empty == "a"){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        if (nParents > 5){
          
          p <- parents[6]
          parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
          parentStates <- parentStates[!is.na(parentStates)]
          parentStates <- parentStates[parentStates != ""]
          parentStates <- parentStates[parentStates != " "]
          
          selectInput("d6", paste(p, ": ", paste(parentStates, collapse=", "), sep=""), c("Positive"="pos", "Negative"="neg", "Other"="other"), selected="pos")
        } else {
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$directionHelpText_1 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d1)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d1 == "other" && nParents > 0){
          
          childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
          childStates <- childStates[!is.na(childStates)]
          childStates <- childStates[childStates != ""]
          childStates <- childStates[childStates != " "]
          
          paste("For each state of ", parents[1], ", select a relative order associated with the", input$child, "order", paste(childStates, collapse=", "))
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
    
  })
  
  output$directionHelpText_2 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d2)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d2 == "other" && nParents > 0){
          
          childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
          childStates <- childStates[!is.na(childStates)]
          childStates <- childStates[childStates != ""]
          childStates <- childStates[childStates != " "]
          
          paste("For each possible state of ", parents[2], ", select a relative order associated with the", input$child, "order", paste(childStates, collapse=", "))
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
    
  })
  
  output$directionHelpText_3 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d3)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d3 == "other" && nParents > 0){
          
          childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
          childStates <- childStates[!is.na(childStates)]
          childStates <- childStates[childStates != ""]
          childStates <- childStates[childStates != " "]
          
          paste("For each possible state of ", parents[3], ", select a relative order associated with the", input$child, "order", paste(childStates, collapse=", "))
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
    
  })
  
  output$directionHelpText_4 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d4)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d4 == "other" && nParents > 0){
          
          childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
          childStates <- childStates[!is.na(childStates)]
          childStates <- childStates[childStates != ""]
          childStates <- childStates[childStates != " "]
          
          paste("For each possible state of ", parents[4], ", select a relative order associated with the", input$child, "order", paste(childStates, collapse=", "))
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
    
  })
  
  output$directionHelpText_5 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d5)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d5 == "other" && nParents > 0){
          
          childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
          childStates <- childStates[!is.na(childStates)]
          childStates <- childStates[childStates != ""]
          childStates <- childStates[childStates != " "]
          
          paste("For each possible state of ", parents[5], ", select a relative order associated with the", input$child, "order", paste(childStates, collapse=", "))
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
    
  })
  
  output$directionHelpText_6 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d6)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d6 == "other" & nParents > 0){
          
          childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
          childStates <- childStates[!is.na(childStates)]
          childStates <- childStates[childStates != ""]
          childStates <- childStates[childStates != " "]
          
          paste("For each possible state of ", parents[6], ", select a relative order associated with the", input$child, "order", paste(childStates, collapse=", "))
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
    
  })
  
  output$dirOtherText_1 <- renderText({
    # print(input$d1)
    if(!is.null(input$child) & !is.null(input$d1)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d1 == "other" ){
          paste(parents[1])
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$dirOtherText_2 <- renderText({
    # print(input$d2)
    if(!is.null(input$child)& !is.null(input$d2)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d2 == "other" ){
          paste(parents[2])
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$dirOtherText_3 <- renderText({
    
    if(!is.null(input$child)& !is.null(input$d3)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d3 == "other" ){
          paste(parents[3])
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$dirOtherText_4 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d4)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d4 == "other" ){
          paste(parents[4])
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$dirOtherText_5 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d5)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d5 == "other" ){
          paste(parents[5])
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  output$dirOtherText_6 <- renderText({
    
    if(!is.null(input$child) & !is.null(input$d6)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(NULL)
      } else {
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        if (input$empty == "a" && input$d6 == "other" ){
          paste(parents[6])
        } else{
          return(NULL)
        }
      }
    } else{
      return(NULL)
    }
  })
  
  lapply(1:10, function(k){
    output[[paste0("dirOtherParentsUI_1.", k)]] <- renderUI({
      if(!is.null(input$child) & input$empty == "a" & !is.null(input$d1)){
        if (input$d1 == "other"){
          
          if (sum(input$child == nodes()$Node) == 0){
            return(NULL)
          } else {
            parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
            nParents <- length(parents)
            p <- parents[1]
            parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
            parentStates <- parentStates[!is.na(parentStates)]
            parentStates <- parentStates[parentStates != ""]
            parentStates <- parentStates[parentStates != " "]
  
            pStates <- length(parentStates)
            if (k <= pStates){
        
              numericInput(paste("d1", k, sep="_"), parentStates[k], value=k, min=1, max=pStates, step=1)
            } else {
              return(NULL)
            }
          }
        } else {
          return(NULL)
        }
      } else{
        return(NULL)
      }
    })
  })
  
  lapply(1:10, function(k){
    output[[paste0("dirOtherParentsUI_2.", k)]] <- renderUI({
      if(!is.null(input$child) & input$empty == "a" & !is.null(input$d2)){
        if (input$d2 == "other"){
          if (sum(input$child == nodes()$Node) == 0){
            return(NULL)
          } else {
            parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
            nParents <- length(parents)
            p <- parents[2]
            parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
            parentStates <- parentStates[!is.na(parentStates)]
            parentStates <- parentStates[parentStates != ""]
            parentStates <- parentStates[parentStates != " "]
            
            pStates <- length(parentStates)
            if (k <= pStates){
              numericInput(paste("d2", k, sep="_"), parentStates[k], value=k, min=1, max=pStates, step=1)
            } else {
              return(NULL)
            }
          }
        } else {
          return(NULL)
        }
      } else{
        return(NULL)
      }
    })
  })
  
  lapply(1:10, function(k){
    output[[paste0("dirOtherParentsUI_3.", k)]] <- renderUI({
      if(!is.null(input$child) & input$empty == "a" & !is.null(input$d3)){
        if (input$d3 == "other"){
          
          if (sum(input$child == nodes()$Node) == 0){
            return(NULL)
          } else {
            parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
            nParents <- length(parents)
            p <- parents[3]
            parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
            parentStates <- parentStates[!is.na(parentStates)]
            parentStates <- parentStates[parentStates != ""]
            parentStates <- parentStates[parentStates != " "]
            
            pStates <- length(parentStates)
            if (k <= pStates){
              numericInput(paste("d3", k, sep="_"), parentStates[k], value=k, min=1, max=pStates, step=1)
            } else {
              return(NULL)
            }
          }
        } else {
          return(NULL)
        }
      } else{
        return(NULL)
      }
    })
  })
  
  lapply(1:10, function(k){
    output[[paste0("dirOtherParentsUI_4.", k)]] <- renderUI({
      if(!is.null(input$child) & input$empty == "a" & !is.null(input$d4)){
        if (input$d4 == "other"){
          
          if (sum(input$child == nodes()$Node) == 0){
            return(NULL)
          } else {
            parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
            nParents <- length(parents)
            p <- parents[4]
            parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
            parentStates <- parentStates[!is.na(parentStates)]
            parentStates <- parentStates[parentStates != ""]
            parentStates <- parentStates[parentStates != " "]
            
            pStates <- length(parentStates)
            if (k <= pStates){
              numericInput(paste("d4", k, sep="_"), parentStates[k], value=k, min=1, max=pStates, step=1)
            } else {
              return(NULL)
            }
          }
        } else {
          return(NULL)
        }
      } else{
        return(NULL)
      }
    })
  })
  
  lapply(1:10, function(k){
    output[[paste0("dirOtherParentsUI_5.", k)]] <- renderUI({
      if(!is.null(input$child) & input$empty == "a" & !is.null(input$d5)){
        if (input$d5 == "other"){
          
          if (sum(input$child == nodes()$Node) == 0){
            return(NULL)
          } else {
            parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
            nParents <- length(parents)
            p <- parents[5]
            parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
            parentStates <- parentStates[!is.na(parentStates)]
            parentStates <- parentStates[parentStates != ""]
            parentStates <- parentStates[parentStates != " "]
            
            pStates <- length(parentStates)
            if (k <= pStates){
              numericInput(paste("d5", k, sep="_"), parentStates[k], value=k, min=1, max=pStates, step=1)
            } else {
              return(NULL)
            }
          }
        } else {
          return(NULL)
        }
      } else{
        return(NULL)
      }
    })
  })
  
  lapply(1:10, function(k){
    output[[paste0("dirOtherParentsUI_6.", k)]] <- renderUI({
      if(!is.null(input$child) & input$empty == "a" & !is.null(input$d6)){
        if (input$d6 == "other"){
          
          if (sum(input$child == nodes()$Node) == 0){
            return(NULL)
          } else {
            parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
            nParents <- length(parents)
            p <- parents[6]
            parentStates <- c(t(nodes()[which(p == nodes()$Node),-c(1,2,3)]))
            parentStates <- parentStates[!is.na(parentStates)]
            parentStates <- parentStates[parentStates != ""]
            parentStates <- parentStates[parentStates != " "]
            
            pStates <- length(parentStates)
            if (k <= pStates){
              numericInput(paste("d6", k, sep="_"), parentStates[k], value=k, min=1, max=pStates, step=1)
            } else {
              return(NULL)
            }
          }
        } else {
          return(NULL)
        }
      } else{
        return(NULL)
      }
    })
  })
  
  
  
  
  # Set Initial Empty Table - almost defunct
  initTable <- reactive({
    
      parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
      selectedNode <- gsub(" ", "-", input$child)
      parents <- gsub(" ", "-", parents)
      context <- gsub(" ","",input$context)
      context <- gsub(":", "", context)
      id <- gsub(" ", "", input$id)
      
      filename = paste("initialCPTs//", context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
      
      tbl <- read.csv(filename)
      nrows <- dim(tbl)[1]
      
      tbl$Expertise <- factor(rep(expertise(), nrows), levels=c("Some", "Expert", "N/A"))
      tbl$Confidence <- factor(rep(confidence(), nrows), levels=c("Low", "Medium", "High"))
      
      return(tbl)
    
    })
  
  weightedRankings <- reactive({
    allW <- c(input$p1, input$p2, input$p3, input$p4, input$p5, input$p6)
    
    
    
    if(!is.null(input$child) & input$empty == "a" ){
      
      parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
      nParents <- length(parents)
      
      allW[(nParents + 1):6] <- rep("", 6 - nParents)
      allW[is.na(allW)] <- 1 
      return(allW)
      
    } else{
      return(rep("", 6))
    }
    
  })
  
  direct <- reactive({
    allD <- c(input$d1, input$d2, input$d3, input$d4, input$d5, input$d6)
    
    if (is.null(allD)){
      return(rep("", 6))
    } else{
      if(!is.null(input$child) & input$empty == "a" ){
      
      parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
      nParents <- length(parents)
      
      allD[is.na(allD)] <- "pos"
      allD[(nParents + 1):6] <- rep("", 6 - nParents)
      
      return(allD)
      
    } else{
      return(rep("", 6))
    }
    }
    
    
    
  })
  
  parentdir <- reactive({
    
    allPdirs <- list(c(input$d1_1, input$d1_2,input$d1_3,input$d1_4,input$d1_5,input$d1_6),
                     c(input$d2_1, input$d2_2,input$d2_3,input$d2_4,input$d2_5,input$d2_6),
                     c(input$d3_1, input$d3_2,input$d3_3,input$d3_4,input$d3_5,input$d3_6),
                     c(input$d4_1, input$d4_2,input$d4_3,input$d4_4,input$d4_5,input$d4_6),
                     c(input$d5_1, input$d5_2,input$d5_3,input$d5_4,input$d5_5,input$d5_6),
                     c(input$d6_1, input$d6_2,input$d6_3,input$d6_4,input$d6_5,input$d6_6))
    
    # if(is.null(input$d1)){
    #   allPdirs[[1]] <- rep("", 6)
    # }
    # if(is.null(input$d2)){
    #   allPdirs[[2]] <- rep("", 6)
    # }
    # if(is.null(input$d3)){
    #   allPdirs[[3]] <- rep("", 6)
    # }
    # if(is.null(input$d4)){
    #   allPdirs[[4]] <- rep("", 6)
    # }
    # if(is.null(input$d5)){
    #   allPdirs[[5]] <- rep("", 6)
    # }
    # if(is.null(input$d6)){
    #   allPdirs[[6]] <- rep("", 6)
    # }
    
    
    if(!is.null(input$child) & input$empty == "a" ){
      
      parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
      nParents <- length(parents)
      
      if(is.null(input$d1) | nParents < 0){
        allPdirs[[1]] <- ""
      } else {
        if(input$d1 != "other"){
        allPdirs[[1]] <- ""
      }
      }
      
      if(is.null(input$d2)| nParents < 1){
        allPdirs[[2]] <- ""
      } else {
        if(input$d2 != "other"){
          allPdirs[[2]] <- ""
        }
      }
      
      if(is.null(input$d3) | nParents < 2){
        allPdirs[[3]] <- ""
      } else {
        if(input$d3 != "other"){
          allPdirs[[3]] <- ""
        }
      }
      
      if(is.null(input$d4) | nParents < 3){
        allPdirs[[4]] <- ""
      } else {
        if(input$d4 != "other"){
          allPdirs[[4]] <- ""
        }
      }
      
      if(is.null(input$d5) | nParents < 4){
        allPdirs[[5]] <- ""
      } else {
        if(input$d5 != "other"){
          allPdirs[[5]] <- ""
        }
      }
      
      if(is.null(input$d6) | nParents < 5){
        allPdirs[[6]] <- ""
      } else {
        if(input$d6 != "other"){
          allPdirs[[6]] <- ""
        }
      }
      
      return(allPdirs)
      
    } else{
      return(list("", "", "", "", "", ""))
    }

  })
  
  # Create initial table
  prevTable <- reactive({
   
    childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
    childStates <- childStates[!is.na(childStates)]
    childStates <- childStates[childStates != ""]
    childStates <- childStates[childStates != " "]
    nChildStates <- length(childStates)
    
    context <- gsub(" ","", input$context)
    context <- gsub(":", "", context)
    
    new_pdir <- paste(paste(parentdir()[[1]], collapse="*"), paste(parentdir()[[2]], collapse="*"), paste(parentdir()[[3]], collapse="*"), paste(parentdir()[[4]], collapse="*"), paste(parentdir()[[5]], collapse="*"), paste(parentdir()[[6]], collapse="*"), sep="bbb")
    
    
    # print(new_pdir)

    id <- gsub(" ", "", input$id)
    
    createTable(net(), input$child, context, id, expertise(), confidence(), input$empty, weights=weightedRankings(), directions = direct(), nChildStates, pdir=parentdir())
    
    
    
  })
  
  # Detect changes in overall confidence
  updatedTable_confidence <- eventReactive(input$confidence,{
    
    newTable <- as.data.frame(hot_to_r(input$table))
    nrows <- dim(newTable)[1]
    newTable$Confidence <- isolate(factor(rep(confidence(), nrows), levels=c("Low", "Medium", "High")))
    return(newTable)
  })
  
  # Detect changes in overall expertise
  updatedTable_expertise <- eventReactive(input$expertise,{
    
    newTable <- as.data.frame(hot_to_r(input$table))
    nrows <- dim(newTable)[1]
    newTable$Expertise <- isolate(factor(rep(expertise(), nrows), levels=c("Some", "Expert", "N/A")))
    return(newTable)
  })
  
  # Detect changes in choice of initial table
  updatedTable_empty <- eventReactive(input$empty, {
    
    newTable <- prevTable()
    return(newTable)
  
  }) 

  
  updatedTable_weights <- reactive({
    
    # not quite right yet - need to add in feature to adjust new values, rather than always starting from scratch...
    newTable <- prevTable()
    return(newTable)
    
  })
  
  updatedTable_dir <- reactive({
    
    # not quite right yet - need to add in feature to adjust new values, rather than always starting from scratch...
    newTable <- prevTable()
    return(newTable)
    
  })
  
  updatedTable_dir2 <- reactive({
    
    # not quite right yet - need to add in feature to adjust new values, rather than always starting from scratch...
    newTable <- prevTable()
    return(newTable)
    
  })
  
  # Detect changes in table entries
  updatedTable_entry <- reactive({
    if (!is.null(input$table$changes$changes)){
      
      newTable <- as.data.frame(hot_to_r(input$table))
      print(newTable)
      
      parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
      
      childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
      childStates <- childStates[!is.na(childStates)]
      childStates <- childStates[childStates != ""]
      childStates <- childStates[childStates != " "]
      nChildStates <- length(childStates)
      
      if(is.null(parents)){
        # print(input$table$changes$changes)
        newTable$Total <- rowSums(newTable[,2:(nChildStates + 1)])
        # print(input$table$changes$changes)
      } else if (length(parents) == 0){
        # print(newTable[,2:(nChildStates + 1)])
        newTable$Total <- sum(newTable[,2:(nChildStates + 1)])
      } else {
        nParents <- length(parents)
        newTable$Total <- rowSums(newTable[,(1:nChildStates) + nParents])
        
      }
      
      nrows <- dim(newTable)[1]
      
      return(newTable)
    }
  })
  

  

  
  sepNames <- function(x){
    paste(strsplit(x, ".", fixed=TRUE)[[1]], collapse=" ")
  }
  
  
  output$table <- renderRHandsontable({
    
    if (!is.null(input$child)){
      
      if (sum(input$child == nodes()$Node) == 0){
        return(rhandsontable(data.frame(1), rowHeaders=NULL, colHeaders=NULL))
      } else {
      
        if (length(adjacent_vertices(net(), input$child, mode = c("in")))== 0){
          parents <- NULL
        } else{
          parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        }
      
      
        nParents <- length(parents)
        
        
        childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
        childStates <- childStates[!is.na(childStates)]
        childStates <- childStates[childStates != ""]
        childStates <- childStates[childStates != " "]
        nChildStates <- length(childStates)
        
        
     
        # h <- (3^nParents) * 50
        h <- (3^2) * 50 - 50
    
        if (is.null(parents) && is.null(input$child)){
          
          nullTable <- matrix(NA, nrow=1, ncol=5)
          
          outTable <- rhandsontable(nullTable, rowHeaders=NULL, colHeaders=NULL) %>%
            hot_cols(manualColumnResize = TRUE, colWidths=c(rep(60,5))) 
          
          return(outTable)
          
        } else if(nParents > 0){
          
          parentHeaders <- sapply(parents, sepNames)
          allHeaders <- c(parents, childStates,"Total", "Expertise", "Confidence")
          
          allHeaders[1:nParents] <- parentHeaders
          allHeaders[(nParents + 1):(nParents + length(childStates))] <- paste(input$child, childStates, sep=": ")
  
          
         if (is.null(input$table$data)){
           updatedTable <- prevTable()
           print(1)
           
         } else if (length(unique(unlist(input$table$data))) == 1 ){
           
           updatedTable <- prevTable()
           print(2)
         } else if (sum(abs(dim(hot_to_r(input$table)) - dim(initTable()))) != 0){
           # initialised table
           updatedTable <- prevTable()
           print(3)
         } else {
           curr_table <- as.data.frame(hot_to_r(input$table))
           
           # tmpTable <- updatedTable_empty()
           # diffEmpty <- sum(tmpTable[, (nParents + 1):((nParents + nChildStates))] - curr_table[, (nParents + 1):((nParents + nChildStates))], na.rm=TRUE)
           
           curr_empty <- strsplit(row.names(curr_table)[1], "bbb", fixed=TRUE)[[1]][1]
           curr_w <- strsplit(row.names(curr_table)[1], "bbb", fixed=TRUE)[[1]][2]
           curr_d <- strsplit(row.names(curr_table)[1], "bbb", fixed=TRUE)[[1]][3]
           curr_pdir <- row.names(curr_table)[2]
           new_pdir <- paste(paste(parentdir()[[1]], collapse="*"), paste(parentdir()[[2]], collapse="*"), paste(parentdir()[[3]], collapse="*"), paste(parentdir()[[4]], collapse="*"), paste(parentdir()[[5]], collapse="*"), paste(parentdir()[[6]], collapse="*"), sep="bbb")
           
           # print(curr_pdir)
           # print(new_pdir)
           
           curr_weights <- as.numeric(strsplit(curr_w, "*", fixed=TRUE)[[1]])
           # print(curr_weights)
           curr_dir <- as.numeric(strsplit(curr_d, "%", fixed=TRUE)[[1]])
           if (length(curr_dir < 6)){
             curr_dir <- c(curr_dir, rep(NA, 6 - length(curr_dir)))
           }
           
           new_direcs <- direct()
           new_dir <- c(rep(1, nParents), rep(NA, 6 - nParents))
           new_dir[new_direcs == "neg"] <- -1
           new_dir[new_direcs == "other"] <- 0
           
           new_dir[is.na(new_dir)] <- -99
           curr_dir[is.na(curr_dir)] <- -99
           
           new_weights <- as.numeric(weightedRankings())
           new_weights <- new_weights[!is.na(new_weights)]
           
           # print(new_weights)
           # print(curr_weights)
           # 
           # print(curr_dir)
           # print(new_dir)
           # print(new_direcs)
           # print(row.names(curr_table))
  
           
           if (curr_empty != input$empty){
             
             updatedTable <- prevTable()
             print("6a")
             
            } else if (!is.null(input$table$changes$changes)){
             updatedTable <- updatedTable_entry()
             print(4)
  
           } else if (length(unique(curr_table$Expertise)) == 1 && curr_table$Expertise[1] != input$expertise && input$empty != "p"){
             updatedTable <- updatedTable_expertise()
             print(5)
  
           } else if (length(unique(curr_table$Confidence)) == 1 && curr_table$Confidence[1] != input$confidence && input$empty != "p"){
             updatedTable <- updatedTable_confidence()
             print(6)
  
           } else if (input$empty == "a"){
             
             if (length(curr_weights) != length(new_weights)){
               updatedTable <- prevTable()
               
             } else if (sum(curr_weights - new_weights) != 0 ){
               updatedTable <- updatedTable_weights()
               print("6b")
               
             } else if (sum(curr_dir - new_dir) != 0){
               updatedTable <- updatedTable_dir()
               print("6c")
  
             } else if (curr_pdir != new_pdir){
               updatedTable <- updatedTable_dir()
               print("6d")
               
             } else {
               updatedTable <- curr_table
               print(7)
             }
             
           }else {
             updatedTable <- curr_table
             print(7)
           }
           
         }
     
    
          # updatedTable <- updatedTable_confidence()
          
          outTable <- rhandsontable(updatedTable, height=h, rowHeaders=NULL, colHeaders=allHeaders) %>%
            hot_cols(manualColumnResize = TRUE, colWidths=c(rep(90,nParents), rep(60,nChildStates), 50, 70, 80)) %>%
            hot_col((nParents + 1):(nParents + nChildStates + 1), format="Numeric") %>%
            hot_col("Total",format="Numeric",renderer = "
                    function (instance, td, row, col, prop, value, cellProperties) {
                    Handsontable.renderers.NumericRenderer.apply(this, arguments);
                    if (value!=100) {
                    td.style.background = 'red';
                    } else {
                    td.style.background = 'lightgrey';
                    }
                    }") %>%
            hot_col(parentHeaders, readOnly=TRUE, type="text")  %>%
            hot_col(col = "Expertise", type = "dropdown", c("Some", "Expert", "N/A")) %>%
            hot_col(col = "Confidence", type = "dropdown", c("Low", "Medium", "High"))
    
          return(outTable)
          
        } else {
        
    
          allHeaders <- c(" ",childStates,"Total", "Expertise", "Confidence")
          allHeaders[(2):(1 + length(childStates))] <- paste(input$child, childStates, sep=": ")
          
          
          if (is.null(input$table$data)){
            updatedTable <- prevTable()
            print(1)
            
          } else if (length(unique(unlist(input$table$data))) == 1 ){
            
            updatedTable <- prevTable() 
            
          } else if (sum(abs(dim(hot_to_r(input$table)) - dim(initTable()))) != 0){
            # initialised table
            updatedTable <- prevTable()
          } else {
            curr_table <- as.data.frame(hot_to_r(input$table))
            
            if (!is.null(input$table$changes$changes)){
              updatedTable <- updatedTable_entry()
              print(4)
              
            } else if (length(unique(curr_table$Expertise)) == 1 && curr_table$Expertise[1] != input$expertise && input$empty != "p"){ 
              updatedTable <- updatedTable_expertise()
              print(5)
              
            } else if (length(unique(curr_table$Confidence)) == 1 && curr_table$Confidence[1] != input$confidence && input$empty != "p"){ 
              updatedTable <- updatedTable_confidence()
              print(6)
            } else if (row.names(curr_table)[1] != input$empty){
              
              updatedTable <- prevTable()
              print("6a")
              
              # } else if (diffEmpty != 0){
              #   updatedTable <- prevTable()
              #   
            } else {
              updatedTable <- curr_table
              print(7)
            }
            
          }
          
      
          outTable <- rhandsontable(updatedTable, height=h, rowHeaders=NULL, colHeaders=allHeaders) %>% 
            hot_cols(manualColumnResize = TRUE, colWidths=c(90, rep(60, nChildStates),50,70,80)) %>%
            hot_col(2:(nChildStates + 1), format="Numeric") %>%
            hot_col("Total",format="Numeric" ,renderer = "
               function (instance, td, row, col, prop, value, cellProperties) {
                                                Handsontable.renderers.NumericRenderer.apply(this, arguments);
                                                if (value!=100) {
                                                td.style.background = 'red';
                                                } else {
                                                td.style.background = 'lightgrey';
                                                } 
      }")  %>%
            hot_col(1, readOnly=TRUE, type="text") %>%
            hot_col(col = "Expertise", type = "dropdown", c("Some", "Expert", "N/A")) %>%
            hot_col(col = "Confidence", type = "dropdown", c("Low", "Medium", "High"))
          
          return(outTable)
        }
      }
    } else {
      return(rhandsontable(data.frame(1), rowHeaders=NULL, colHeaders=NULL))
    }
    
    })



  
  # ********************************************************************
  # Visualisation ----
  
  
  cpt <- eventReactive(input$showPlot, {
    
    if (is.null(input$child)){
      cpt_l <- NULL
    } else {
      if (sum(input$child == nodes()$Node) == 0){
        
        cpt_l <- NULL
        
      } else {
        
      
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        nParents <- length(parents)
        
        childStates <- c(t(nodes()[which(input$child == nodes()$Node),-c(1,2,3)]))
        childStates <- childStates[!is.na(childStates)]
        childStates <- childStates[childStates != ""]
        childStates <- childStates[childStates != " "]
        nChildStates <- length(childStates)
        
        cptable <- as.data.frame(hot_to_r(input$table))
        e <- dim(cptable)[2] - 3
        s <- e - nChildStates + 1
        levelNames <- names(cptable)[s:e]
        
        cpt_l <- reshape(cptable, varying=levelNames, direction="long", v.names="Prob", times=levelNames, drop=c("Total", "Confidence", "Expertise"))
        
        row.names(cpt_l) <- NULL
        names(cpt_l)[names(cpt_l) == "time"] <- input$child
        
        if (dim(cptable)[2] == 1){
          cpt_l <- NULL
        } else {
          for (j in 1:s){
          cpt_l[,j] <- factor(cpt_l[,j], levels=unique(as.vector(cpt_l[,j])))
          }
        }

        
        # cpt_l
      }
    }
    return(list(cpt_l = cpt_l))
  })
  

  
  
  
  output$visual2CPT <- renderPlot({
    
    if (is.null(input$child)){
      par(mar=c(0,0,0,0))
      plot(c(-6, 6), c(-3,3), asp=1, type = "n", xlab="", ylab="", axes=FALSE)
      text(0,0, "Something went awry,\n please reselect a child node")
      box()
    } else {
      if (sum(input$child == nodes()$Node) == 0 | is.null(cpt()$cpt_l)){
        par(mar=c(0,0,0,0))
        plot(c(-6, 6), c(-3,3), asp=1, type = "n", xlab="", ylab="", axes=FALSE)
        text(0,0, "Something went awry,\n please reselect a child node")
        box()
      } else {
        cpt_tab <- cpt()$cpt_l
        
        plotTable(cpt_tab, net(), nodes(), input$child, weights=NULL, ranked=FALSE)
      }
    }
    
   
  })
  
  # Plot the CPT in order of ranked parents
  output$visualrankedCPT <- renderPlot({
    if (is.null(input$child)){
      par(mar=c(0,0,0,0))
      plot(c(-6, 6), c(-3,3), asp=1, type = "n", xlab="", ylab="", axes=FALSE)
      text(0,0, "Something went awry,\n please reselect a child node")
      box()
    } else {
      if (sum(input$child == nodes()$Node) == 0){
        par(mar=c(0,0,0,0))
        plot(c(-6, 6), c(-3,3), asp=1, type = "n", xlab="", ylab="", axes=FALSE)
        text(0,0, "Something went awry,\n please reselect a child node")
        box()
      } else {
      
        if (input$empty == "a"){
          cpt_tab <- cpt()$cpt_l
        
        plotTable(cpt_tab, net(), nodes(), input$child, weights=weightedRankings(), ranked=TRUE)
        } else {
          return(NULL)
        }
      }
    }

  })
  
     
  
  
  
  
  # ***********************************************
  # Saving ----
  observeEvent(input$saveData,{
    
    if (is.null(input$child)){
      showNotification(paste("Cannot save CPT: No child node has been selected"), duration = 5)
    } else {
      
      if (sum(input$child == nodes()$Node) == 0){
        
        showNotification(paste("Cannot save CPT: Selected node is incompatible with the selected network"), duration = 5)
        
      } else{
        
      
        parents <- names(adjacent_vertices(net(), input$child, mode = c("in"))[[1]])
        selectedNode <- gsub(" ", "-", input$child)
        parents <- gsub(" ", "-", parents)
        context <- gsub(" ", "",input$context)
        context <- gsub(":", "", context)
        id <- gsub(" ", "", input$id)
    
        filename = paste("savedCPTs//", context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
    
        outTable <- as.data.frame(hot_to_r(input$table))
        # print(filename)
        print(outTable)
        write.csv(outTable, file = filename, row.names=TRUE)
    
        showNotification(paste("Saved CPT: ", "Scenario_", selectedNode, ".", paste(parents, collapse=".")), duration = 5)
        
        # show a notification if table looks suspicious
        
        nparents <- length(parents)
        
        if (nparents > 0){
          tableNumbers <- outTable[, (nparents+1):(which(names(outTable) == "Total") - 1)]
          
          # print(tableNumbers)
          colUniques <- apply(tableNumbers, 2, function(x){length(unique(x))})
          rowRanges <- apply(tableNumbers, 1, function(x){diff(range(x))})
          
          if (sum(colUniques == 1) > 0){
            showNotification("Some columns of the table always have the same value - please check", duration = NULL, type="error")
          } 
          if(sum(tableNumbers < 0) > 0){
            showNotification("There are negative values in the table - please check", duration = NULL, type="error")
          } 
          if (sum(outTable$Total != 100) > 0){
            showNotification("Some rows of the table do not sum to 100 - please check", duration = NULL, type="error")
          }
          if (max(rowRanges) < 50){
            showNotification(paste("The maximum range in any row of the table is", max(rowRanges), "- please check"), duration = NULL, type="message")
          }
        } 
      }
    
    }
})



  
  session$onSessionEnded(stopApp)
  
})
