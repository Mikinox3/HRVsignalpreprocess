#-----------A. R-HRV ANALYSIS---------------
#http://rhrv.r-forge.r-project.org/
#1. create the data structure
hrv.data = CreateHRVData()
hrv.data = SetVerbose(hrv.data, TRUE)
#2. Load the heart beats and the Tag
hrv.data = LoadBeatVector(HRVData = hrv.data, beatPositions  = peaktime,  scale = 1)
if(choix==2) {
  hrv.data = AddEpisodes(hrv.data,InitTimes = c(Image$Pstart*0.0005,Image2$Pstart*0.0005,Image3$Pstart*0.0005),
                         Tags=c(rep("Positive",8),rep("Neutral",8),rep("Negative",8)),
                         Durations = c(Image$diff*0.0005,Image2$diff*0.0005,Image3$diff*0.0005),
                         Values=rep(0,8*3))
}
if(choix==4) {
  hrv.data = AddEpisodes(hrv.data,InitTimes = c(Filmo$Pstart*0.0005,Filmo2$Pstart*0.0005,Filmo3$Pstart*0.0005),
                         Tags=c("Positive","Neutral","Negative"),
                         Durations = c(Filmo$diff*0.0005,Filmo2$diff*0.0005,Filmo3$diff*0.0005),
                         Values=c(0,0,0))
}
#3. Estimate the non-interpolated heart rate frequency
hrv.data = BuildNIHR(hrv.data)
#x11()
#PlotNIHR(hrv.data, Tags = "all")# non-interpolated heart rate or Tags = c("Positive","Neutral","Negative")
#faire spectrogram (in progess)
# dfi <- data.frame(temps=as.numeric(hrv.data$Beat$Time), frequence=as.numeric(hrv.data$Beat$niHR*0,01666667), amplitude=as.numeric(hrv.data$Beat$niHR))
# spectrogramme<- ggplot(dfi)+
#   geom_raster(aes(temps, frequence, fill = amplitude), interpolate = TRUE)+
#   scale_y_continuous(breaks = seq(from=0, to=frequence.max, by=500))+
#   labs(x="Temps (s)", y="Fréquence (Hz)", title="")+
#   coord_cartesian(expand=FALSE)
# x11()
# spectrogramme
#4. Reject artifacts with automatic filter
hrv.data=FilterNIHR(hrv.data)
#x11()
#PlotNIHR(hrv.data, Tags = "all",main="Non-Interpolated HR after filter")# non-interpolated heart rate #after filter
#4.bis. Reject artifacts with manual editing
if(choix_edit_NHR==1) {
  EditNHR <-
    function(HRVData,scale=1.0, verbose=NULL) {
      editFunction <- function(HRVData) {
        HRVDataOld <- HRVData
        Myhscale <- 2*scale    # Horizontal scaling
        Myvscale <- 1.5*scale    # Vertical scaling
        plt <- c()
        usr <- c()
        coords <- c()
        pointsInArea <- c()
        numPointsInArea <- 0
        numCoords <- 0
        numRemovedPoints <- 0
        vectorx <- c()
        vectory <- c()
        plotFunction <- function()
        {
          vectorx <<- HRVData$Beat$Time
          vectory <<- HRVData$Beat$niHR
          plot(vectorx,vectory, type = "l", xlab = "time (sec.)",
               ylab = "HR (beats/min.)",
               ylim = c(min(vectory),max(vectory)*1.1))
          title(main = "Non-interpolated instantaneous heart rate")
          
          if (numCoords == 1) {
            points(coords[1],coords[2], pch = "+", col = "deeppink3")
          }
          if (numCoords == 2) {
            rect(min(coords[1],coords[3]),min(coords[2],coords[4]),
                 max(coords[1],coords[3]),max(coords[2],coords[4]),border="deeppink3")
            areaString=paste("No. of selected points: ",numPointsInArea)
            text((coords[1]+coords[3])/2,max(coords[2],coords[4]),areaString,pos=3,col="deeppink3")
            points(vectorx[pointsInArea==TRUE],vectory[pointsInArea==TRUE],pch=20,col="deeppink3")
            
          }
          usr <<- par('usr')
          plt <<- par('plt')		
        }
        tt <- tcltk::tktoplevel()
        tcltk::tkwm.deiconify(tt)
        tcltk::tkgrab.set(tt)
        tcltk::tkfocus(tt)
        tcltk::tkwm.title(tt,"Outliers removal")
        img <- tkrplot::tkrplot(tt,fun=plotFunction,hscale=Myhscale,vscale=Myvscale)
        Remove <- function()
        {
          numCoords <<-0
          coords <<- c()
          HRVData$Beat <<- subset(HRVData$Beat, pointsInArea==FALSE)
          numRemovedPoints <<- numRemovedPoints + numPointsInArea
          pointsInArea <<- c()
          numPointsInArea <<- 0
          tkrplot::tkrreplot(img)
        }
        Clear <- function() {
          numCoords <<- 0
          coords <<- c()
          pointsInArea <<- c()
          numPointsInArea <<- 0
          tkrplot::tkrreplot(img)
        }
        Quit <- function() {
          if (numRemovedPoints > 0) {
            msg <- paste(numRemovedPoints,"outliers to be removed\nProceed?")
            mbval <- tcltk::tkmessageBox(title = "Confirmation", message = msg,
                                         type = "yesnocancel", icon = "question")
            if (tcltk::tclvalue(mbval) == "no") {
              tcltk::tkgrab.release(tt)
              tcltk::tkdestroy(tt)
              HRVData <<- HRVDataOld
            }
            if (tcltk::tclvalue(mbval) == "yes") {
              tcltk::tkgrab.release(tt)
              tcltk::tkdestroy(tt)
            }		
          } else {
            tcltk::tkdestroy(tt)
          }
        }
        OnLeftClick <- function(x,y) {
          xClick <- as.numeric(x)
          yClick <- as.numeric(y)
          width  <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqwidth",img)))
          height <- as.numeric(tcltk::tclvalue(tcltk::tkwinfo("reqheight",img)))
          xMin <- plt[1] * width
          xMax <- plt[2] * width
          yMin <- plt[3] * height
          yMax <- plt[4] * height
          rangeX <- usr[2] - usr[1]
          rangeY <- usr[4] - usr[3]
          xCoord <- usr[1] + (xClick - xMin) * rangeX / (xMax - xMin)
          yCoord <- usr[3] + ((height - yClick) - yMin) * rangeY / (yMax - yMin)
          if ((xClick > xMin * 0.95) && (xClick < xMax * 1.1) && 
              (yClick > yMin * 0.95) && (yClick < yMax * 1.1)) {
            if (numCoords == 0) {
              numCoords <<- 1
              #message("Primer punto\n")
              coords <<- c(xCoord,yCoord)
            } else if (numCoords == 1) {
              coords <<- c(coords,xCoord,yCoord)
              numCoords <<- 2
              pointsInArea <<- (
                (vectorx > min(coords[1], coords[3])) &
                  (vectorx < max(coords[1], coords[3])) &
                  (vectory > min(coords[2], coords[4])) &
                  (vectory < max(coords[2], coords[4]))
              )
              numPointsInArea <<- length(pointsInArea[pointsInArea==TRUE])
            }
            tkrplot::tkrreplot(img)
          }
        }
        buttonremove <- tcltk::tkbutton(tt, text = "Remove outliers", command = Remove)
        buttonclear <- tcltk::tkbutton(tt, text = "Clear", command = Clear)
        buttonremove2 <- tcltk::tkbutton(tt, text = "End", command = Quit)
        tcltk::tkgrid(img, columnspan = 3)
        tcltk::tkgrid(buttonremove,buttonclear,buttonremove2)
        tcltk::tkbind(img, "<Button-1>",OnLeftClick)
        tcltk::tkwait.window(tt)
        return(HRVData)
      }
      HRVDataNew = editFunction(HRVData)
      return(HRVDataNew)
    }
  hrv.data= EditNHR(hrv.data) 
  x11()
  ggplot(hrv.data$Beat, aes(x = Time)) + 
    geom_line(aes(y = niHR), colour="dark red")
}
if(choix_edit_NHR==0) {
  EditNHR <- NULL
}
#5. Interpolate the heart rate signal
hrv.data = InterpolateNIHR(hrv.data, freqhr = 4) #Freq 4 hz =permet de calculer estimation spectrale fiable entre 0 et 1 Hz, soit la bande de freq pour laquelle le SNA à une réponse significative (Singh et al, 2014)
#x11()
#PlotHR(hrv.data,Tags = "all")  # interpolated heart rate

#6. Perform time Analysis
hrv.data=CreateTimeAnalysis(hrv.data,size = 10)#size (10) = fenêtre de temps du calcul (plus grand que le temps de stimulation) / valeurs défault size = 300 

source("CreateTimeAnalysisByEpisodes.R")
results2=CreateTimeAnalysisByEpisodes(hrv.data, Tag = tag_valence, size = 10 , numofbins = NULL, 
                                      interval = 7.8125, verbose =T ) #Interval, concerne le design du plot, pas de différence sur les valeurs
#Result
print (tag_valence)
results2.df = data.frame(cbind(In=results2$resultIn,Out=results2$resultOut))
print(results2.df)

# print (paste('SDNN',hrv.data2$resultIn$SDNN))
# print (paste('rMSSD',hrv.data2$resultIn$rMSSD))
# print (paste('IRRR',hrv.data2$resultIn$IRRR))
# print (paste('MADRR',hrv.data2$resultIn$MADRR))

#7. Perform frequency Analysis
hrv.data=CreateFreqAnalysis(hrv.data)
hrv.data=CalculatePowerBand(hrv.data,indexFreqAnalysis = 1,size = 10, shift = 5, type = "wavelet")
#x11()
#PlotPowerBand(hrv.data,indexFreqAnalysis = 1, Tags = "all")
#7.bis Divide by Tags
#HR
splitting.data_HR=SplitHRbyEpisodes(hrv.data, Tag = tag_valence)
#Powerband
splitting.data_PB=SplitPowerBandByEpisodes(hrv.data,indexFreqAnalysis = 1,Tag =tag_valence)
#Resul
#cat("Positive mean HR: ", mean(splitting.data_HR$InEpisodes),"\n")
#cat("Positive mean HR: ", mean(splitting.data_HR$OutEpisodes),"\n")
cat("Positive mean HF in: ", mean(splitting.data_PB$InEpisodes$HF),"\n") #ULF,VLF,LF, HF
cat("Positive mean HF out: ", mean(splitting.data_PB$OutEpisodes$HF),"\n") #ULF,VLF,LF, HF

#8. Perform non-linear Analysis (not for the image)
#hrv.data=CreateNonLinearAnalysis(hrv.data)
#hrv.data=NonlinearityTests(hrv.data)
#hrv.data=SurrogateTest(hrv.data,significance = 0.05,useFunction = timeAsymmetry2,tau=4, doPlot = TRUE)
#timeLag = CalculateTimeLag(hrv.data,technique = "acf",method = "first.e.decay",lagMax = 100)
#EmbeddingDim = CalculateEmbeddingDim(hrv.data,numberPoints = 10000, timeLag = timeLag,maxEmbeddingDim = 15)

