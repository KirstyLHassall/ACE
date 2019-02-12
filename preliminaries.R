

# Create all auxilliary files
# Creates initial Network diagram and saves the coordinates
# Creates initial CPT files



library(igraph)
library(shape)
library(RColorBrewer)

appName <- "ACE"


allcontexts <- read.csv(paste(appName, "//nodeTables//NetworkNames.csv", sep=""))
contexts <- as.character(allcontexts$Networks)
contexts <- gsub(" ", "", contexts)
contexts <- gsub(":", "", contexts)

users <- read.csv(paste(appName, "//nodeTables//Usernames.csv", sep=""))
elicitees <- users$Users
elicitees <- gsub(" ", "", elicitees)
elicitees <- gsub(":", "", elicitees)
elicitees <- gsub("-", "", elicitees)

# choose 9 distinct colours
pal <- brewer.pal(9, "Set1")

for (k in contexts){
  
  print(k)
  set.seed(97)
  
  # draw network
  
  edges <- read.csv(paste(appName, "//nodeTables//Edge_", k, ".csv", sep=""))
  nodes <- read.csv(paste(appName, "//nodeTables//Node_", k, ".csv", sep=""), colClasses="character")
  
  ue <- unique(c(as.character(edges[,1]), as.character(edges[,2])))
  un <- as.character(nodes[,1])
  
  test <- cbind(sort(ue), sort(un))
  sum(test[,1] != test[,2])
  print(test)
  
  nNodes <- length(nodes$Node)
  
  net <- graph_from_data_frame(d=edges, directed=TRUE)
  
  # order nodes in nodesFile by the Network ordering
  index <- NULL
  for (j in 1:nNodes){
    index <- c(index, which(nodes$Node == V(net)$name[j]))
  }
  nodes <- data.frame(nodeOrder = c(1:nNodes), nodes)
  nodes <- nodes[index, ]
  
  # resave Node files
  write.csv(nodes, paste(appName, "//nodeTables//Node_netOrder_", k, ".csv", sep=""), row.names=FALSE)
  
  nodecolours <- pal[as.numeric(as.factor(nodes$Type))]
  
  
  
  
  nodeNames <- V(net)$name
  nodeNames <- gsub(" ", "\n", nodeNames)
  # V(net())$name <- nodeNames
  
  net_layout <- layout_with_fr(net)
  net_layout <- norm_coords(net_layout, ymin=-1, ymax=1, xmin=-2, xmax=2)
  
  pdf(width=15, height=10, file=paste(appName, "//nodeTables//fullNetwork_", k, ".pdf", sep=""))
  
  par(mar=c(1,1,1,1))
  plot(net,
       vertex.color=nodecolours, vertex.size=20, vertex.frame.color=NA, vertex.label.family="sans", vertex.label.font=2, vertex.label=nodeNames,
       edge.arrow.size=.8, rescale=FALSE,
       layout=net_layout*.7)
  
  dev.off()
  
  write.csv(data.frame(nodes=V(net)$name, net_layout), file=paste(appName, "//nodeTables//NetworkCoords_", k, ".csv", sep=""), row.names=FALSE)
  
  
  for (i in 1:nNodes){
    
    selectedNode <- as.character(nodes$Node[i])
    
    # How many Parents
    parents <- names(adjacent_vertices(net, selectedNode, mode = c("in"))[[1]])
    
    
    # How many Child States
    childStates <- c(t(nodes[i,-c(1,2,3)]))
    childStates <- childStates[!is.na(childStates)]
    childStates <- childStates[childStates != ""]
    childStates <- childStates[childStates != " "]
    nChildStates <- length(childStates)
    
    if(length(parents) < 1){
      
      # defaultFreq <- rep(round(100/nChildStates), nChildStates)
      # defaultFreq[nChildStates] <- 100 - sum(defaultFreq[nChildStates - 1])
      defaultFreq <- rep(0, nChildStates)
      
      tbl <- data.frame(cbind(c("Frequency"), matrix(defaultFreq, 1, nChildStates)))
      for (j in 2:(nChildStates + 1)){
        tbl[,j] <- as.numeric(as.vector(tbl[,j]))
      }
      names(tbl) <- c(" ", as.character(childStates))
      
      tbl$Total <- rowSums(tbl[,(2:(nChildStates + 1))])
      
      
    } else {
      
      # to ensure columns of the CPT are in the right order, get the parent Nodes one by one
      nParents <- length(parents)
      colID <- NULL
      for (parent in 1:nParents){
        colID <- c(colID, which(nodes$Node == parents[parent]))
      }
      
      parentNodes <- nodes[colID , -c(1:3)]
      
      npStates <- sapply(1:nParents, function(x) sum(parentNodes[x,] != "", na.rm=TRUE))
      pStates <- lapply(1:nParents, function(x){
          out <- parentNodes[x, ]
          out <- out[!is.na(out)]
          out <- out[out != ""]
          })
      
      nrows <- prod(npStates)
      ncols <- nChildStates
      if (nParents > 1){
        tempInd <- 1:nParents
        tbl <- data.frame(cbind(expand.grid(rev(pStates))[,rev(tempInd)], matrix(0.00, nrows, ncols)))
      } else{
        tbl <- data.frame(cbind(expand.grid(pStates), matrix(0.00, nrows, ncols)))
      }
      
      
      names(tbl) <- c(parents, as.character(childStates))
      tbl$Total <- rowSums(tbl[,(1:nChildStates) + nParents])
      
    }
    
    nrows <- dim(tbl)[1]
    tbl$Expertise <- factor(rep(NA, nrows), levels=c("None", "Some", "Expert"))
    tbl$Confidence <- factor(rep(NA, nrows), levels=c("Low", "Medium", "High"))
    

    for (e in elicitees){
      
      
      
      # childStates <- c(t(nodes()[which(selectedNode == nodes()$Node),-c(1,2,3)]))
      # childStates <- childStates[!is.na(childStates)]
      # childStates <- childStates[childStates != ""]
      # childStates <- childStates[childStates != " "]
      childHeaders <- childStates
      childHeaders <- sub("<", ".lt.", childHeaders, fixed=TRUE)
      childHeaders <- sub(">", ".gt.", childHeaders, fixed=TRUE)
      childHeaders <- sub("-", "..", childHeaders, fixed=TRUE)
      childHeaders <- sub(" ", ".", childHeaders, fixed=TRUE)
      if (length(parents) == 0){
        allHeaders <- c("X.",childHeaders,"Total", "Expertise", "Confidence")
      } else {
        allHeaders <- c(parents, childHeaders,"Total", "Expertise", "Confidence")
      }
      
      
      names(tbl) <- allHeaders
      
      selectedNode <- gsub(" ", "-", selectedNode)
      parents <- gsub(" ", "-", parents)
      
      filename = paste(appName, "//initialCPTs//", k, "_", selectedNode, "_", paste(parents, collapse="."), "_", e, ".csv", sep="")
      
      write.csv(tbl, file=filename, row.names=FALSE)
    }
    
  }

}






