


###########
#
# Function to plot CPT
#
#############


plotTable <- function(cpt_tab, net, nodes, child, weights, ranked=FALSE,...){
  
  pal <- brewer.pal(8,"Dark2")
  
  trellis.par.set(box.rectangle = modifyList(trellis.par.get("box.rectangle"), list (col = "black", lty=1)), 
                  box.dot = modifyList(trellis.par.get("box.dot"), list(pch="|", col="black")),
                  box.umbrella = modifyList(trellis.par.get("box.umbrella"), list(lty=1, col=1)),
                  plot.symbol=modifyList(trellis.par.get("plot.symbol"), list(pch=19, col=pal)),
                  superpose.symbol=modifyList(trellis.par.get("superpose.symbol"), list(pch=19, col=pal)),
                  superpose.polygon=modifyList(trellis.par.get("superpose.polygon"), list(col=pal)),
                  strip.background=modifyList(trellis.par.get("strip.background"), list(col="black")))
  
  lw <- list(left.padding = list(x = 0, units = "inches"))
  lw$top.padding <- list(y = 10, units = "inches")
  
  lattice.options(layout.heights = lw)
  
  
  
  parents <- names(adjacent_vertices(net, child, mode = c("in"))[[1]])
  nParents <- length(parents)
  
  childStates <- c(t(nodes[which(child == nodes$Node),-c(1,2,3)]))
  childStates <- childStates[!is.na(childStates)]
  childStates <- childStates[childStates != ""]
  childStates <- childStates[childStates != " "]
  nChildStates <- length(childStates)
  
  nP <- length(parents)
 
  if (nP > 0){
    parents <- names(cpt_tab)[1:nP]
    
    # reorder parents
    if (ranked == TRUE){
      weights <- as.numeric(weights)
      weights <- weights[!is.na(weights)]
      
      parents <- parents[rev(order(weights))]
    }
    
    parents <- gsub("-", ".", parents)
    
    # a catch for when parents and/or have changed
    nameMatch <- c(parents, child)%in%names(cpt_tab)
    if ( sum(nameMatch == FALSE) > 0 ){
      return(NULL)
    } else{
      
      Parentlabel <- paste(parents, collapse=":")
    
      fplot <- as.formula(paste(paste(parents, collapse=":"), " ~ Prob"))
      
      fcollapse <- as.formula(paste("Prob ~", paste(rev(parents), collapse=":")))
    
      collapsedTable <- aggregate(fcollapse, data=cpt_tab, FUN=mean, na.rm=TRUE)
      collapsedTable <- collapsedTable[, c(nP:1, nP+1)]
      # print(collapsedTable)
      nRows <- dim(collapsedTable)[1]
      
    
      
      ind <- 0
      ylabels <- NULL
      while(ind < nP){
        ylabels <- paste(ylabels, collapsedTable[,ind + 1], sep=":")
        ind <- ind + 1
      }
      # print(Parentlabel)
      # print(max(1, ceiling(nChildStates/3)))
      
      outPlot <- barchart(fplot, groups=cpt_tab[,nP + 1], data=cpt_tab, xlim=c(0,100), ylim=rev(c(0.5, nRows + 0.5)), auto.key=list(corner=c(1,1 + 0.1 *3), columns=max(1, ceiling(nChildStates/3)), title=paste(child), reverse.rows=FALSE, cex=0.8), par.strip.text=list(col="white"), cex=1.5, strip=strip.custom(style=1, strip.names=TRUE), scales=list(y=list(at=nRows:1, labels=rev(ylabels))), stack=TRUE, xlab="Frequency", ylab="", horiz=TRUE, par.settings=list(clip=list(panel=FALSE)))  + latticeExtra::layer(panel.text(x=0, y=0, label=Parentlabel), data=data.frame(Parentlabel))
    
      
      return(outPlot)
    }
    
  } else{
    # a catch for when parents and/or have changed
    nameMatch <- c(child)%in%names(cpt_tab)
    if ( sum(nameMatch == FALSE) > 0 ){
      return(NULL)
    } else{
      
      fplot <- as.formula(paste(" ~ Prob"))
      # print(cpt_tab)
      nRows <- 1
  
      ind <- 0
      ylabels <- NULL
   
      return(barchart(fplot, groups=cpt_tab[,2], data=cpt_tab, xlim=c(0,100), ylim=rev(c(0.5, nRows + 0.5)), auto.key=list(corner=c(1,1 + 0.1 *3), title=paste(child), columns=max(1, ceiling(nChildStates/3)), reverse.rows=FALSE, cex=0.8), par.strip.text=list(col="white"), cex=1.5, strip=strip.custom(style=1, strip.names=TRUE), scales=list(y=list(at = 1, labels="")), stack=TRUE, xlab="Frequency", ylab="", horiz=TRUE))
    }
  } 
  
  
}

