
###########
#
# Function to initialise CPT
#
#############



createTable <- function(net, child, context, id, expertise, confidence, empty, weights, directions, nChildStates, pdir){

  # print(pdir)
  
  parents <- names(adjacent_vertices(net, child, mode = c("in"))[[1]])
  selectedNode <- gsub(" ", "-", child)
  parents <- gsub(" ", "-", parents)
  id <- gsub(" ", "", id)
  nParents <- length(parents)
  
  prevFile <- paste(context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
  
  
  if (empty == "e"){
    filename = paste("initialCPTs//", context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
    
    tbl <- read.csv(filename)
    nrows <- dim(tbl)[1]
    
    tbl$Expertise <- factor(rep(expertise, nrows), levels=c("Some", "Expert", "N/A"))
    tbl$Confidence <- factor(rep(confidence, nrows), levels=c("Low", "Medium", "High"))
    
    direc <- rep(1, nParents)
    weights <- rep(1, nParents)
    pdir <- NULL
    
  } else if(empty == "p"){
    if (sum(list.files("savedCPTs") == prevFile) == 1 ){
      
      filename = paste("savedCPTs//", context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
      
      tbl <- read.csv(filename, row.names=1)

      curr_d <- strsplit(row.names(tbl)[1], "bbb", fixed=TRUE)[[1]][3]
      curr_pdir <- row.names(tbl)[2]
      direc <- as.numeric(strsplit(curr_d, "%", fixed=TRUE)[[1]])

      
    } else{
      filename = paste("initialCPTs//", context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
      
      tbl <- read.csv(filename)
      nrows <- dim(tbl)[1]
      
      tbl$Expertise <- factor(rep(expertise, nrows), levels=c("Some", "Expert", "N/A"))
      tbl$Confidence <- factor(rep(confidence, nrows), levels=c("Low", "Medium", "High"))
      
      direc <- rep(1, nParents)
      weights <- rep(1, nParents)
      pdir <- NULL
    }
  } else if (empty == "a"){
    
    # create a pre-populated table
    filename = paste("initialCPTs//", context, "_", selectedNode, "_", paste(parents, collapse="."), "_", id, ".csv", sep="")
    
    tbl <- read.csv(filename)
    nrows <- dim(tbl)[1]
    
    tbl$Expertise <- factor(rep(expertise, nrows), levels=c("Some", "Expert", "N/A"))
    tbl$Confidence <- factor(rep(confidence, nrows), levels=c("Low", "Medium", "High"))
    
    if (nParents > 0){
      scores <- matrix(NA, ncol=nParents, nrow=nrows)
      
      direc <- rep(1, nParents)
      direc[directions == "pos"] <- 1
      direc[directions == "neg"] <- -1
      direc[directions == "other"] <- 0
      # print(direc)
      
      # 1. get basic score of states for each parent
      for (p in 1:nParents){
        nst <- length(unique(tbl[,p]))
        if (direc[p] == 0){
          
          sc_temp <- seq(from=1, to=0, length=nst)
          sc <-  sc_temp[pdir[[p]]]
          # sc[pdir[[p]] == "pos"] <- 0
          # sc[pdir[[p]] == "neg"] <- 1
          # sc[pdir[[p]] == "neut"] <- 0.5
          
        } else {
          sc <- seq(from=1, to=0, length=nst)
          if (direc[p] == -1){
            sc <- rev(sc)
          }
        }
        # print(pdir[[p]])
        # print(sc)
        
        for (k in 1:nst){
          scores[tbl[,p] == unique(tbl[,p])[k],p] <- sc[k]
        }
        
      }
      
      # 2. weight the basic scores by importance/influence
      if (is.null(weights)){
        weights <- rep(1, nParents)
      } else{
        weights <- as.numeric(weights)
        weights <- weights[!is.na(weights)]
        
        if(length(weights) == 0){
          weights <- rep(1, nParents)
        }
      }
      
      # print("Earlier")
      # print(weights)
      # print(scores)
      # print(matrix(rep(weights, nrows), nrow=nrows, ncol=nParents, byrow=TRUE))
      
      wscores <- scores * matrix(rep(weights, nrows), nrow=nrows, ncol=nParents, byrow=TRUE)
      
      print(scores)
      print(wscores)
      
      # 3. calculate combined score for each parent set
      combscore <- rowSums(wscores) / sum(weights)
      
      print(combscore)
      
      # 4. calculate individual probabilities
      if (nChildStates == 2){
        
        tbl[, nParents + 1] <- round(combscore*100)
        tbl[, nParents + 2] <- 100 - round(combscore*100)
        
      } else if (nChildStates == 3){
        
        cuts <- matrix(rep(seq(from=0, to=1, length=(nChildStates + 1)), nrows), nrow=nrows, byrow=TRUE)
        
        grad <- matrix(rep(1 - 2*combscore, nChildStates), nrow=nrows)
        intercept <- matrix(rep(combscore, nChildStates), nrow=nrows)
        ah <- intercept + cuts[ , 1:(nChildStates)] * grad
        bh <- intercept + cuts[ , 2:(nChildStates + 1)] * grad
        h <- cuts[ , 2:(nChildStates + 1)] - cuts[ , 1:(nChildStates)]
        probs <- (0.5*(ah + bh) * h) * 2
        tbl[, (nParents + 1):((nParents + nChildStates - 1))] <- round(probs[,1:(nChildStates-1)] * 100)
        tbl[, nParents + nChildStates] <- 100 - rowSums(tbl[, (nParents + 1):((nParents + nChildStates - 1))])
        
        
        # pos <- t(sapply(combscore, function(x) seq(from = x, to = (1 - x), length = nChildStates)))
        # tbl[, nParents + 1] <- abs(round(pos[,1] * 100))
        # tbl[, nParents + 2] <- abs(round(pos[,2] * 100) - round(pos[,1] * 100))
        # tbl[, nParents + nChildStates] <- 100 - rowSums(tbl[, (nParents + 1):((nParents + nChildStates - 1))])
  
      } else if (nChildStates){
        
        cuts <- matrix(rep(seq(from=0, to=1, length=(nChildStates + 1)), nrows), nrow=nrows, byrow=TRUE)
        
        grad <- matrix(rep(1 - 2*combscore, nChildStates), nrow=nrows)
        intercept <- matrix(rep(combscore, nChildStates), nrow=nrows)
        ah <- intercept + cuts[ , 1:(nChildStates)] * grad
        bh <- intercept + cuts[ , 2:(nChildStates + 1)] * grad
        h <- cuts[ , 2:(nChildStates + 1)] - cuts[ , 1:(nChildStates)]
        probs <- (0.5*(ah + bh) * h) * 2
        tbl[, (nParents + 1):((nParents + nChildStates - 1))] <- round(probs[,1:(nChildStates-1)] * 100)
        tbl[, nParents + nChildStates] <- 100 - rowSums(tbl[, (nParents + 1):((nParents + nChildStates - 1))])
        
        # pos <- t(sapply(combscore, function(x) seq(from = x, to = (1 - x), length = nChildStates)))
        # tbl[, nParents + 1] <- round(pos[,1] * 100)
        # tbl[, nParents + 2] <- abs(round(pos[,2] * 100) - round(pos[,1] * 100))
        # for (ch in 3:(nChildStates - 1)){
        #   tbl[, nParents + ch] <- abs(round(pos[,ch] * 100) - rowSums(round(pos[,1:(ch-1)] * 100)))
        # }
        # tbl[, nParents + nChildStates] <- 100 - rowSums(tbl[, (nParents + 1):((nParents + nChildStates - 1))])
        
      }
      
      tbl$Total <-  rowSums(tbl[, (nParents + 1):((nParents + nChildStates))])
    } else {
      # print(tbl)
      tbl[, 2:(nChildStates)] <- round(100 / nChildStates)
      tbl[, (nChildStates + 1)] <- 100 - sum(tbl[, 2:(nChildStates)])
      tbl$Total <-  rowSums(tbl[, 2:((1 + nChildStates))])
    }
    
  }
  

  
  # # Associate some attributes to tbl, to aid in tracking of changes through app
  # attributes(tbl)$empty <- empty
  # attributes(tbl)$weights <- weights
  
  # hmm, may need attributes that are always present for dataframes
  # very hacky code, but seems to work
  row.names(tbl)[1] <- empty
  if (nParents > 0){

    row.names(tbl)[1] <- paste(empty, paste(weights[!is.na(weights)], collapse="*"), paste(direc[!is.na(direc)], collapse="%"), sep="bbb")
    
    if(!is.null(pdir[[1]])){
      pdir1 <- pdir[[1]][!is.na(pdir[[1]])]
    } else{
      pdir1 <- pdir[[1]]
    }
    if(!is.null(pdir[[2]])){
      pdir2 <- pdir[[2]][!is.na(pdir[[2]])]
    } else {
      pdir2 <- pdir[[2]]
    }
    if(!is.null(pdir[[3]])){
      pdir3 <- pdir[[3]][!is.na(pdir[[3]])]
    } else {
      pdir3 <- pdir[[3]]
    }
    if(!is.null(pdir[[4]])){
      pdir4 <- pdir[[4]][!is.na(pdir[[4]])]
    } else {
      pdir4 <- pdir[[4]]
    }
    if(!is.null(pdir[[5]])){
      pdir5 <- pdir[[5]][!is.na(pdir[[5]])]
    } else {
      pdir5 <- pdir[[5]]
    }
    if(!is.null(pdir[[6]])){
      pdir6 <- pdir[[6]][!is.na(pdir[[6]])]
    } else {
      pdir6 <- pdir[[6]]
    }
    
    row.names(tbl)[2] <- paste(paste(pdir1, collapse="*"), paste(pdir2, collapse="*"), paste(pdir3, collapse="*"), paste(pdir4, collapse="*"), paste(pdir5, collapse="*"), paste(pdir6, collapse="*"), sep="bbb")
    # print(direc)
  }
  
  # print("createTable")
  # print(weights)
  return(tbl)

}

