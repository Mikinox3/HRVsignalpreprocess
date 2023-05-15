rm(list=ls()) # suppr TOUTES LES DATAS
#**************************************

chemin1="D:/Stage Inserm RemBAU/EDA M1 CNP 2021-2022/R data & script" #for now, needs to contain all the R scripts called in this one and all the files with row data from the experiment
setwd(chemin1)

library(RHRV)
library(tkrplot)
library(ggplot2)
library(wavelets)
library(waveslim)
library(lomb)
library(wvtool)
library(signal)
library(zoo)


#lists containing the final data after analysis
dataPosImg = list()
dataNeuImg = list()
dataNegImg = list()
dataPosFilm = list()
dataNeuFilm = list()
dataNegFilm = list()
dataHR = list()
#create lists with the names of all the files to analyse in the loop
data_files_csv = list()
data_files_tsv = list()
data_files_tsv <- list.files("D:/Stage Inserm RemBAU/EDA M1 CNP 2021-2022/R data & script/data_files_tsv")  
data_files_csv <- list.files("D:/Stage Inserm RemBAU/EDA M1 CNP 2021-2022/R data & script/data_files_csv")


#--------------------LOOP IMG------------------
k=1

  for (k in (1:length(data_files_tsv))) {

    
#------------------1.data extraction/convertion--------
    
dataX<- read.table(data_files_tsv[k],header= FALSE,skip = 1)    
dataX$bin=paste(dataX$V12,dataX$V11,dataX$V10,dataX$V9,dataX$V8,dataX$V7,dataX$V6,sep="")
dataX$V1 <- dataX$V1/60 
dataX$V2 <- dataX$V3 
columDec <- 13

# Meme chose pour les deux
dataX$bin=as.numeric(dataX$bin)
dataX$bin=dataX$bin/5

dataX$dec=apply(dataX[columDec],1,bin2dec) #conversion
#dataX$dec=strtoi(dataX$bin, base = 2 ) conversion 2 bin2dec
dataX$idx <- seq.int(nrow(dataX))

windows(12,5) #plot signal
plot(dataX$V2, t="l",col="black", xlab="Time (min)", ylab="HB (volt)")
windows(12,5) #plot triggers
plot(dataX$idx,dataX$dec, t="l",col="dark red", xlab="Time (min)", ylab="Triggers")


#----------- 2. Cut the data (images) ---------
#choix=1 #Repos 1
choix=2 #Image
#choix=3 #Repos 2
#choix=4 #Film
source("ImgConversion_Rembau_HRV2.R")

#allPeaktime <- read.csv("A_002_BL_peaks.csv",header=FALSE)

allPeaktime <- read.csv(data_files_csv[k],header=FALSE)
peaktime = subset(allPeaktime, V1 > bnd1*60 & V1<=bnd2*60)$V1


#----------- 3. Artifact removal function (directly on the signal)---------
# Attention, cela affecte directement le signal. Pas conseillé.
#source("Artifact_removal_Rembau_HRV.R")
#----------- 4. Peak detection---------
#SampleFreq <-2000 # set the sample frequency of the signal
#thr <-0.0015      # threshold for the R-R detection
# df=dfTrue # seulement si le 3 a été réalisé
# dfzz=read.table("A_002_BL.txt_Artifact_removal_function_.txt",sep="\t",header=T) # seulement si le 3 a été réalisé
#source("Peakdetection_Rembau_HRV.R")



#----------- 5. RR Analysis (images) ---------

choix_edit_NHR=0 # 1 = manual edition NHR, 0 = pas de manual edition NHR
tag_valence= "Positive" #"Positive","Neutral","Negative"
source("RRanalysis_Rembau_HRV2.R")


# add to dataPos le results2.df
dataPosImg[1+2*(k-1)]= results2.df[1] 
dataPosImg[2+2*(k-1)]= mean(splitting.data_PB$InEpisodes$HF)



tag_valence= "Negative"
source("RRanalysis_Rembau_HRV2.R")

dataNegImg[1+2*(k-1)]= results2.df[1]
dataNegImg[2+2*(k-1)]= mean(splitting.data_PB$InEpisodes$HF)


tag_valence= "Neutral"
source("RRanalysis_Rembau_HRV2.R")


dataNeuImg[1+2*(k-1)]= results2.df[1] 
dataNeuImg[2+2*(k-1)]=mean(splitting.data_PB$InEpisodes$HF)

#----------- 6. General HR Analysis ---------
m=k

rollOffset = 10
doPlotEpochs = 1
source("HRAnalysis2.R") 
allHRs 
dataHR[1+4*(m-1)]= data_files_csv[m]
dataHR[2+4*(m-1)]= allHRs[1] # specific data stored but can be modified depending on what u want to keep
dataHR[3+4*(m-1)]= allHRs[6]
dataHR[4+4*(m-1)]= allHRs[7]
k=m

#windows(12,5)
#plotFilms
#windows(12,5)
#plotImgs


#-----------end-------------
k=k+1
rm(list=ls()[! ls() %in% c("data_files_csv","data_files_tsv","dataPosImg","dataNegImg","dataNeuImg","dataPosFilm","dataNeuFilm","dataNegFilm","dataHR","k")])


}



#--------------------LOOP FILM------------------
k=1


for (k in (1:length(data_files_tsv))) {
  
  #------------------1.data extraction/convertion--------
  
  dataX<- read.table(data_files_tsv[k],header= FALSE,skip = 1)    
  dataX$bin=paste(dataX$V12,dataX$V11,dataX$V10,dataX$V9,dataX$V8,dataX$V7,dataX$V6,sep="")
  dataX$V1 <- dataX$V1/60 
  dataX$V2 <- dataX$V3 
  columDec <- 13
  
  # Meme chose pour les deux
  dataX$bin=as.numeric(dataX$bin)
  dataX$bin=dataX$bin/5
  
  dataX$dec=apply(dataX[columDec],1,bin2dec) #conversion
  #dataX$dec=strtoi(dataX$bin, base = 2 ) conversion 2 bin2dec
  dataX$idx <- seq.int(nrow(dataX))
  
  windows(12,5) #plot signal
  plot(dataX$V2, t="l",col="black", xlab="Time (min)", ylab="HB (volt)")
  windows(12,5) #plot triggers
  plot(dataX$idx,dataX$dec, t="l",col="dark red", xlab="Time (min)", ylab="Triggers")
  
  
  #----------- 2. Cut the data (film) ---------
  #choix=1 #Repos 1
  #choix=2 #Image
  #choix=3 #Repos 2
  choix=4 #Film
  source("ImgConversion_Rembau_HRV2.R")
  #allPeaktime <- read.csv("A_002_BL_peaks.csv",header=FALSE)
  
  allPeaktime <- read.csv(data_files_csv[k],header=FALSE)
  peaktime = subset(allPeaktime, V1 > bnd1*60 & V1<=bnd2*60)$V1
  
  if(bnd1+bnd2!=1){
  
  #----------- 3. Artifact removal function (directly on the signal)---------
  # Attention, cela affecte directement le signal. Pas conseillé.
  #source("Artifact_removal_Rembau_HRV.R")
  #----------- 4. Peak detection---------
  #SampleFreq <-2000 # set the sample frequency of the signal
  #thr <-0.0015      # threshold for the R-R detection
  # df=dfTrue # seulement si le 3 a été réalisé
  # dfzz=read.table("A_002_BL.txt_Artifact_removal_function_.txt",sep="\t",header=T) # seulement si le 3 a été réalisé
  #source("Peakdetection_Rembau_HRV.R")
  
  
  
  #----------- 5. RR Analysis (film)---------
  
    choix_edit_NHR=0 # 1 = manual edition NHR, 0 = pas de manual edition NHR
    tag_valence= "Positive" #"Positive","Neutral","Negative"
    source("RRanalysis_Rembau_HRV2.R")
    
    
    
    dataPosFilm[1+2*(k-1)]=results2.df[1]
    dataPosFilm[2+2*(k-1)]= mean(splitting.data_PB$InEpisodes$HF) 
    
    
    tag_valence= "Negative"
    source("RRanalysis_Rembau_HRV2.R")
    
    dataNegFilm[1+2*(k-1)]= results2.df[1] 
    dataNegFilm[2+2*(k-1)]= mean(splitting.data_PB$InEpisodes$HF) 
    
    
    tag_valence= "Neutral"
    source("RRanalysis_Rembau_HRV2.R")
    
    dataNeuFilm[1+2*(k-1)]= results2.df[1]
    dataNeuFilm[2+2*(k-1)]= mean(splitting.data_PB$InEpisodes$HF)  
  

  }
  else {
    dataPosFilm[1+2*(k-1)]= NA
    dataPosFilm[2+2*(k-1)]= NA 
    dataNegFilm[1+2*(k-1)]= NA 
    dataNegFilm[2+2*(k-1)]= NA
    dataNeuFilm[1+2*(k-1)]= NA
    dataNeuFilm[2+2*(k-1)]= NA 
  }
  
  #-----------end-------------
  k=k+1
  rm(list=ls()[! ls() %in% c("data_files_csv","data_files_tsv","dataPosImg","dataNegImg","dataNeuImg","dataPosFilm","dataNeuFilm","dataNegFilm","dataHR","k")])
  
 
  
  
}


#-------------------Convert data lists in txt/csv files-----------

source("dataConverter2.R")

#convert in .csv
capture.output(Newlist,file = "data.csv")


#convert in .txt
#sink("data.txt")
#print(Newlist)
#sink()





