CreateTimeAnalysisByEpisodes <-
  function(HRVData, Tag="", size = 60, numofbins = NULL,
           interval = 7.8125, verbose=NULL){
    if (!is.null(verbose)) {
      cat("  --- Warning: deprecated argument, using SetVerbose() instead ---\n    --- See help for more information!! ---\n")
      SetVerbose(HRVData,verbose)
    }
    
   if (is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present\n    --- Quitting now!! ---\n")
    }
    
    if (is.null(HRVData$Beat$RR)) { 
      stop("  --- RR time intervals not present\n    --- Quitting now!! ---\n")
    }
    
    if (HRVData$Verbose) {
      if (Tag=="") {
        cat("   No tag was specified\n")
      } else {
        cat("   Using episodes with tag:",Tag,"\n")
      }
    }    
    vectors=SplitNIRRbyEpisodes(HRVData,Tag=Tag)
    
    resultIn = timeAnalysisStatistics(vectors$InEpisodesTime,vectors$InEpisodes,
                                      size = size, numofbins = numofbins, 
                                      interval = interval)
        
    resultOut = timeAnalysisStatistics(vectors$OutEpisodesTime,vectors$OutEpisodes,
                                      size = size, numofbins = numofbins, 
                                      interval = interval)
    
    result=list(resultIn=resultIn, resultOut=resultOut)
    
    return(result)
 }


################################ Private part of the code #####################
#### Private function
SplitNIRRbyEpisodes <-
  function(HRVData, Tag="", verbose=NULL) {
    # -------------------------------------------------
    # Splits NI RR Data using Episodes information
    # -------------------------------------------------
    #  Tag -> specifies tag of episodes
    #  Returns a list with 4 vectors: InEpisodes and OutEpisodes; anmd
    #  InEpisodesTime and OutEpisodesTime
    
    if (!is.null(verbose)) {
      cat("  --- Warning: deprecated argument, 
          using SetVerbose() instead ---\n    
          --- See help for more information!! ---\n")
      SetVerbose(HRVData,verbose)
    }
    
    if (HRVData$Verbose) {
      cat("** Splitting heart rate signal using episodes **\n");
    }
    
    if (is.null(HRVData$Episodes)) {
      stop("  --- Episodes not present\n    --- Quitting now!! ---\n")
    }
    
    if (is.null(HRVData$Beat$RR)) { 
      stop("  --- RR intervals not present\n    --- Quitting now!! ---\n")
    }
    
    if (HRVData$Verbose) {
      if (Tag=="") {
        cat("   No tag was specified\n");
      } else {
        cat("   Using episodes with tag:",Tag,"\n");
      }
    }
    
    # Select episodes to split signal
    if (Tag=="") {
      ActiveEpisodes=HRVData$Episodes
    } else {
      ActiveEpisodes=subset(HRVData$Episodes,HRVData$Episodes$Type==Tag)
    }
    
    if (HRVData$Verbose) {
      cat("   Number of episodes:",length(ActiveEpisodes$InitTime),"\n")
    }
    
    Beg=ActiveEpisodes$InitTime
    End=ActiveEpisodes$InitTime+ActiveEpisodes$Duration
    
    npoints = length(HRVData$Beat$RR)
    x = HRVData$Beat$Time
    
    # Auxiliary signal used to mark points inside episodes
    Aux=rep(0,times=npoints)
    for (i in 1:length(Beg)) {
      Aux[x>=Beg[i] & x<=End[i]] = 1
    }
    
    l=list(InEpisodes=HRVData$Beat$RR[Aux==1], InEpisodesTime = HRVData$Beat$Time[Aux==1],
           OutEpisodes=HRVData$Beat$RR[Aux==0], OutEpisodesTime = HRVData$Beat$Time[Aux==0])
    
    return(l)
  }

#### Private function
timeAnalysisStatistics <- 
  function(time,rr, size = 300, numofbins = NULL, interval = 7.8125){

    results = list()
    results$size = size
    minRR = min(rr)
    maxRR = max(rr)
    if (!is.null(numofbins)) {
      interval = (maxRR - minRR)/(numofbins - 2)
      vecthist = seq(minRR - interval/2, maxRR + interval/2, 
                     len = numofbins)
    }
    else {
      medRR = (min(rr) + max(rr))/2
      lowhist = medRR - interval * ceiling((medRR - minRR)/interval)
      longhist = ceiling((maxRR - lowhist)/interval) + 1
      vecthist = seq(from = lowhist, by = interval, length.out = longhist)
    }
    results$SDNN = sd(rr)
    WindowMin = head(time, n = 1)
    WindowMax = WindowMin + size
    WindowIndex = 1
    RRWindowMean = c(0)
    RRWindowSD = c(0)
    while (WindowMax < tail(time, 1)) {
      RRWindow = rr[time >= WindowMin & 
                                   time < WindowMax]
      if (length(RRWindow) == 0) {
        RRWindowMean[WindowIndex] = NA
        RRWindowSD[WindowIndex] = NA
      }else{
        RRWindowMean[WindowIndex] = mean(RRWindow)
        RRWindowSD[WindowIndex] = sd(RRWindow)  
      }
      WindowMin = WindowMin + size
      WindowMax = WindowMax + size
      WindowIndex = WindowIndex + 1
    }
    numberOfWindows = WindowIndex - 1
    if (numberOfWindows <= 1) {
      warning("There is no window or just one window. Cannot compute the standard deviation!! Returning NA in SDANN\n")
    }
   # results$SDANN = sd(RRWindowMean,na.rm=TRUE)
    #results$SDNNIDX = mean(RRWindowSD,na.rm=TRUE)
    NRRs = length(rr)
    RRDiffs = diff(rr)
    RRDiffs50 = RRDiffs[abs(RRDiffs) > 50]
    #results$pNN50 = 100 * length(RRDiffs50)/length(RRDiffs)
    #results$SDSD = sd(RRDiffs)
    results$rMSSD = sqrt(mean(RRDiffs^2))
    RRQuant = quantile(RRDiffs)
    results$IRRR = RRQuant[[4]] - RRQuant[[2]]
    results$MADRR = median(abs(RRDiffs))
    h = hist(rr, breaks = vecthist, plot = FALSE)
    area = length(rr) * interval
    maxhist = max(h$counts)
    #results$TINN = area/maxhist
    #results$HRVi = length(rr)/maxhist

    return(results)
}
