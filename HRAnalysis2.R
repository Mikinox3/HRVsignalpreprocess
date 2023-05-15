library(ggplot2)
library(zoo)


# initialisation
currentPeakTimes = allPeaktime$V1

rawBPM = 60/diff(currentPeakTimes)
rolledMean = zoo::rollmean(rawBPM, k = rollOffset, fill=NA,align = "left")

allHRs <- data.frame(Type = character(6),Total=double(6),TotalStd=double(6), Prev=double(6),
                     PrevStd=double(6),StimOnly=double(6), StimOnlyStd=double(6), StimAndAfter=double(6),
                     StimAndAfterStd=double(6), After=double(6), AfterStd=double(6))

# nomme les differentes categories
allHRs$Type[1] <- "Images Positives"
allHRs$Type[2] <- "Images Neutres"
allHRs$Type[3] <- "Images Negatives"
allHRs$Type[4] <- "Film Positif"
allHRs$Type[5] <- "Film Neutre"
allHRs$Type[6] <- "Film Negatif"


# fait des calculs pour tout ces differents segment
for(i in 1:6){
  
  if(i == 1){relevantTimes = Image}
  else if (i == 2){relevantTimes = Image2}
  else if (i == 3){relevantTimes = Image3}
  else if (i == 4){relevantTimes = Filmo}
  else if (i == 5){relevantTimes = Filmo2}
  else if (i == 6){relevantTimes = Filmo3}
  
  
  if (relevantTimes != 0){
    
    # etablit les temps maximums et minimums de l'interval
    BeforeStart = relevantTimes$Pstart - (5*2000)
    AfterEnd = relevantTimes$Pend + (15*2000)
    
    stuff <-data.frame(Type=double(8), Prev=double(8), StimOnly=double(8), StimAndAfter=double(8), After=double(8))
    
    # Si image
    if(i<4){
      # passe a travers les 8 images pour chaque
      for(j in 1:8){
        
        # trouve les pics R correspondant a chaque sous fenetre
        
        # fenetre entiere : -5 secondes -> 21 secondes
        positionX = (currentPeakTimes > (BeforeStart[j]/2000)) * (currentPeakTimes < (AfterEnd[j]/2000))
        position = which(positionX == 1)
        stuff$Total[j] <- mean(na.omit(rolledMean[position]))
        
        # fenetre avant : -5 secondes -> 0 secondes
        positionX = (currentPeakTimes > (BeforeStart[j]/2000)) * (currentPeakTimes < (relevantTimes$Pstart[j]/2000))
        position = which(positionX == 1)
        stuff$Prev[j] <- mean(na.omit(rolledMean[position]))
        
        # fenetre pendant : 0 secondes -> 6 secondes
        positionX = (currentPeakTimes > (relevantTimes$Pstart[j]/2000)) * (currentPeakTimes < (relevantTimes$Pend[j]/2000))
        position = which(positionX == 1)
        stuff$StimOnly[j] <- mean(na.omit(rolledMean[position]))
        
        # fenetre pendant et apres : 0 secondes -> 21 secondes
        positionX = (currentPeakTimes > (relevantTimes$Pstart[j]/2000)) * (currentPeakTimes < (AfterEnd[j]/2000))
        position = which(positionX == 1)
        stuff$StimAndAfter[j] <- mean(na.omit(rolledMean[position]))
        
        # fenetre apres : 6 secondes -> 21 secondes
        positionX = (currentPeakTimes > (relevantTimes$Pend[j]/2000)) * (currentPeakTimes < (AfterEnd[j]/2000))
        position = which(positionX == 1)
        stuff$After[j] <- mean(na.omit(rolledMean[position]))
      }
      
      # calcul moyenne et ecart type pour chaque type de fenetre sur les 8 images
      allHRs$Total[i] = mean(na.omit(stuff$Total))
      allHRs$TotalStd[i] = sd(na.omit(stuff$Total))
      allHRs$Prev[i] = mean(na.omit(stuff$Prev))
      allHRs$PrevStd[i] = sd(na.omit(stuff$Prev))
      allHRs$StimOnly[i] = mean(na.omit(stuff$StimOnly))
      allHRs$StimOnlyStd[i] = sd(na.omit(stuff$StimOnly))
      allHRs$StimAndAfter[i] = mean(na.omit(stuff$StimAndAfter))
      allHRs$StimAndAfterStd[i] = sd(na.omit(stuff$StimAndAfter))
      allHRs$After[i] = mean(na.omit(stuff$After))
      allHRs$AfterStd[i] = sd(na.omit(stuff$After))
    }
    else{ # idem pour les films mais sans moyennage entre film (et donc sans ecart-type)
      
      positionX = (currentPeakTimes > (BeforeStart/2000)) * (currentPeakTimes < (AfterEnd/2000))
      position = which(positionX == 1)
      allHRs$Total[i] <- mean(na.omit(rolledMean[position]))
      
      positionX = (currentPeakTimes > (BeforeStart/2000)) * (currentPeakTimes < (relevantTimes$Pstart/2000))
      position = which(positionX == 1)
      allHRs$Prev[i] <- mean(na.omit(rolledMean[position]))
      
      positionX = (currentPeakTimes > (relevantTimes$Pstart/2000)) * (currentPeakTimes < (relevantTimes$Pend/2000))
      position = which(positionX == 1)
      allHRs$StimOnly[i] <- mean(na.omit(rolledMean[position]))
      
      positionX = (currentPeakTimes > (relevantTimes$Pstart/2000)) * (currentPeakTimes < (AfterEnd/2000))
      position = which(positionX == 1)
      allHRs$StimAndAfter[i] <- mean(na.omit(rolledMean[position]))
      
      positionX = (currentPeakTimes > (relevantTimes$Pend/2000)) * (currentPeakTimes < (AfterEnd/2000))
      position = which(positionX == 1)
      allHRs$After[i] <- mean(na.omit(rolledMean[position]))
      
    }
  }
}

if (doPlotEpochs == 1){
  
  # Graphique pour les films
  
  if (Filmo != 0 & Filmo2 != 0 & Filmo3 != 0 ){
    relevantTimes = Filmo
    BeforeStart = relevantTimes$Pstart - (5*2000)
    AfterEnd = relevantTimes$Pend + (15*2000)
    positionX = (currentPeakTimes > (BeforeStart/2000)) * (currentPeakTimes < (AfterEnd/2000))
    position = which(positionX == 1)
    
    dfFilm1 <- data.frame(data = rolledMean[position],times = currentPeakTimes[position] - (relevantTimes$Pstart/2000), name= "Film Positif")
    
    relevantTimes = Filmo2
    BeforeStart = relevantTimes$Pstart - (5*2000)
    AfterEnd = relevantTimes$Pend + (15*2000)
    positionX = (currentPeakTimes > (BeforeStart/2000)) * (currentPeakTimes < (AfterEnd/2000))
    position = which(positionX == 1)
    
    dfFilm2 <- data.frame(data = rolledMean[position],times = currentPeakTimes[position] - (relevantTimes$Pstart/2000), name= "Film Positif")
    
    relevantTimes = Filmo3
    BeforeStart = relevantTimes$Pstart - (5*2000)
    AfterEnd = relevantTimes$Pend + (15*2000)
    positionX = (currentPeakTimes > (BeforeStart/2000)) * (currentPeakTimes < (AfterEnd/2000))
    position = which(positionX == 1)
    
    dfFilm3 <- data.frame(data = rolledMean[position],times = currentPeakTimes[position] - (relevantTimes$Pstart/2000), name= "Film Positif")
    
    plotFilms <- ggplot() + geom_line(data=dfFilm1, aes(x=times, y = data), color= "green",group="positif",size=2) + 
      geom_line(data=dfFilm2, aes(x=times, y = data), color= "blue",group="neutre",size=2) + 
      geom_line(data=dfFilm3, aes(x=times, y = data), color= "red",group="negatif",size=2) + 
      labs(title="Rythme Cardiaque pendant les Films",x ="Temps (en secondes)", y = "BPM") + 
      geom_vline(xintercept = 0,linetype="dotted") + geom_vline(xintercept = 120,linetype="dotted")
    #plotFilms
  }
  
  # Graphique pour les images
  
  epochLength = 52015
  
  for(i in 1:3){
    mat = matrix(0,epochLength,8)
    
    if(i == 1){relevantTimes = Image}
    else if (i == 2){relevantTimes = Image2}
    else if (i == 3){relevantTimes = Image3}
    
    BeforeStart = relevantTimes$Pstart - (5*2000)
    AfterEnd = relevantTimes$Pend + (15*2000)
    
    for(k in 1:8){
      
      positionX = (currentPeakTimes > (BeforeStart[k]/2000)) * (currentPeakTimes < (AfterEnd[k]/2000))
      position = which(positionX == 1)
      sets = rolledMean[position]
      
      positionY = 2000*(currentPeakTimes[position] - (BeforeStart[k]/2000))
      
      prevElement = rolledMean[position[1]-1]
      
      for(j in 1:epochLength){
        currentId = sum(positionY <= j)
        if (currentId > 0){mat[j,k] = sets[currentId]}
        else{mat[j,k] = prevElement}
      }
    }
    
    dftmp = data.frame(data=double(epochLength),stds=double(epochLength),times=((c(1:52015)-10000)/2000))
    for(j in 1:epochLength){
      dftmp$data[j] = mean(na.omit(mat[j,]))
      dftmp$stds[j] = sd(na.omit(mat[j,]))
    }
    
    if(i == 1){dfImg1 = dftmp}
    else if (i == 2){dfImg2 = dftmp}
    else if (i == 3){dfImg3 = dftmp}
  }
  
  plotImgs <- ggplot()  +
    geom_line(data=dfImg1, aes(x=times, y = data), color= "green",group="positif",size=1) +
    geom_ribbon(data=dfImg1, aes(x=times, y = data,ymin = data-stds, ymax = data+stds),fill = "green",alpha=0.3)+
    geom_line(data=dfImg2, aes(x=times, y = data), color= "blue",group="neutre",size=1) +
    geom_ribbon(data=dfImg2, aes(x=times, y = data,ymin = data-stds, ymax = data+stds),fill = "blue",alpha=0.3)+
    geom_line(data=dfImg3, aes(x=times, y = data), color= "red",group="negatif",size=1) +
    geom_ribbon(data=dfImg3, aes(x=times, y = data,ymin = data-stds, ymax = data+stds),fill = "red",alpha=0.3)+
    labs(title="Rythme Cardiaque pendant les Images",x ="Temps (en secondes)", y = "BPM") + 
    geom_vline(xintercept = 0,linetype="dotted") + geom_vline(xintercept = 6,linetype="dotted")
  #plotImgs
  
}

