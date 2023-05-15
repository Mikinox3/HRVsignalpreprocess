
Newlist= list()
g=1

Newlist$name = data_files_csv

for (g in 1:length(data_files_csv)) {
  
  
  Newlist$SDNNip = append(Newlist$SDNNip,dataPosImg[[1+2*(g-1)]][[2]])
  Newlist$rMSSDip = append(Newlist$rMSSDip,dataPosImg[[1+2*(g-1)]][[3]])
  Newlist$IRRRip = append(Newlist$IRRRip,dataPosImg[[1+2*(g-1)]][[4]])
  Newlist$MADRRip = append(Newlist$MADRRip,dataPosImg[[1+2*(g-1)]][[5]])
  Newlist$HFip = append(Newlist$HFip,dataPosImg[[2+2*(g-1)]][[1]])
  
  Newlist$SDNNiNeu = append(Newlist$SDNNiNeu,dataNeuImg[[1+2*(g-1)]][[2]])
  Newlist$rMSSDiNeu = append(Newlist$rMSSDiNeu,dataNeuImg[[1+2*(g-1)]][[3]])
  Newlist$IRRRiNeu = append(Newlist$IRRRiNeu,dataNeuImg[[1+2*(g-1)]][[4]])
  Newlist$MADRRiNeu = append(Newlist$MADRRiNeu,dataNeuImg[[1+2*(g-1)]][[5]])
  Newlist$HFiNeu = append(Newlist$HFiNeu,dataNeuImg[[2+2*(g-1)]][[1]])
  
  Newlist$SDNNiNeg = append(Newlist$SDNNiNeg,dataNegImg[[1+2*(g-1)]][[2]])
  Newlist$rMSSDiNeg = append(Newlist$rMSSDiNeg,dataNegImg[[1+2*(g-1)]][[3]])
  Newlist$IRRRiNeg = append(Newlist$IRRRiNeg,dataNegImg[[1+2*(g-1)]][[4]])
  Newlist$MADRRiNeg = append(Newlist$MADRRiNeg,dataNegImg[[1+2*(g-1)]][[5]])
  Newlist$HFiNeg = append(Newlist$HFiNeg,dataNegImg[[2+2*(g-1)]][[1]])
  
  if(length(dataPosFilm)!=0){
    
    
    if( (is.na(dataPosFilm[1+2*(g-1)])==FALSE&&is.na(dataPosFilm[2+2*(g-1)])==FALSE))
    {
      Newlist$SDNNfp = append(Newlist$SDNNfp,dataPosFilm[[1+2*(g-1)]][[2]])
      Newlist$rMSSDfp = append(Newlist$rMSSDfp,dataPosFilm[[1+2*(g-1)]][[3]])
      Newlist$IRRRfp = append(Newlist$IRRRfp,dataPosFilm[[1+2*(g-1)]][[4]])
      Newlist$MADRRfp = append(Newlist$MADRRfp,dataPosFilm[[1+2*(g-1)]][[5]])
      Newlist$HFfp = append(Newlist$HFfp,dataPosFilm[[2+2*(g-1)]][[1]])
      
      Newlist$SDNNfNeu = append(Newlist$SDNNfNeu,dataNeuFilm[[1+2*(g-1)]][[2]])
      Newlist$rMSSDfNeu = append(Newlist$rMSSDfNeu,dataNeuFilm[[1+2*(g-1)]][[3]])
      Newlist$IRRRfNeu = append(Newlist$IRRRfNeu,dataNeuFilm[[1+2*(g-1)]][[4]])
      Newlist$MADRRfNeu = append(Newlist$MADRRfNeu,dataNeuFilm[[1+2*(g-1)]][[5]])
      Newlist$HFfNeu = append(Newlist$HFfNeu,dataNeuFilm[[2+2*(g-1)]][[1]])
      
      Newlist$SDNNfNeg = append(Newlist$SDNNfNeg,dataNegFilm[[1+2*(g-1)]][[2]])
      Newlist$rMSSDfNeg = append(Newlist$rMSSDfNeg,dataNegFilm[[1+2*(g-1)]][[3]])
      Newlist$IRRRfNeg = append(Newlist$IRRRfNeg,dataNegFilm[[1+2*(g-1)]][[4]])
      Newlist$MADRRfNeg = append(Newlist$MADRRfNeg,dataNegFilm[[1+2*(g-1)]][[5]])
      Newlist$HFfNeg = append(Newlist$HFfNeg,dataNegFilm[[2+2*(g-1)]][[1]])
    }
    
    else{
      
      Newlist$SDNNfp = append(Newlist$SDNNfp,0)
      Newlist$rMSSDfp = append(Newlist$rMSSDfp,0)
      Newlist$IRRRfp = append(Newlist$IRRRfp,0)
      Newlist$MADRRfp = append(Newlist$MADRRfp,0)
      Newlist$HFfp = append(Newlist$HFfp,0)
      
      Newlist$SDNNfNeu = append(Newlist$SDNNfNeu,0)
      Newlist$rMSSDfNeu = append(Newlist$rMSSDfNeu,0)
      Newlist$IRRRfNeu = append(Newlist$IRRRfNeu,0)
      Newlist$MADRRfNeu = append(Newlist$MADRRfNeu,0)
      Newlist$HFfNeu = append(Newlist$HFfNeu,0)
      
      Newlist$SDNNfNeg = append(Newlist$SDNNfNeg,0)
      Newlist$rMSSDfNeg = append(Newlist$rMSSDfNeg,0)
      Newlist$IRRRfNeg = append(Newlist$IRRRfNeg,0)
      Newlist$MADRRfNeg = append(Newlist$MADRRfNeg,0)
      Newlist$HFfNeg = append(Newlist$HFfNeg,0)
    }
    
  }
  
  Newlist$Stimonlyip= append(Newlist$Stimonlyip, dataHR[[3+4*(g-1)]][[1]])
  Newlist$StimonlyiNeu= append(Newlist$StimonlyiNeu, dataHR[[3+4*(g-1)]][[2]])
  Newlist$StimonlyiNeg= append(Newlist$StimonlyiNeg, dataHR[[3+4*(g-1)]][[3]])
  Newlist$Stimonlyfp= append(Newlist$Stimonlyfp, dataHR[[3+4*(g-1)]][[4]])
  Newlist$StimonlyfNeu= append(Newlist$StimonlyfNeu, dataHR[[3+4*(g-1)]][[5]])
  Newlist$StimonlyfNeg= append(Newlist$StimonlyfNeg, dataHR[[3+4*(g-1)]][[6]])
  
  Newlist$StimonlyStdip= append(Newlist$StimonlyStdip, dataHR[[4+4*(g-1)]][[1]])
  Newlist$StimonlyStdiNeu= append(Newlist$StimonlyStdiNeu, dataHR[[4+4*(g-1)]][[2]])
  Newlist$StimonlyStdiNeg= append(Newlist$StimonlyStdiNeg, dataHR[[4+4*(g-1)]][[3]])
  Newlist$StimonlyStdfp= append(Newlist$StimonlyStdfp, dataHR[[4+4*(g-1)]][[4]])
  Newlist$StimonlyStdfNeu= append(Newlist$StimonlyStdfNeu, dataHR[[4+4*(g-1)]][[5]])
  Newlist$StimonlyStdfNeg= append(Newlist$StimonlyStdfNeg, dataHR[[4+4*(g-1)]][[6]])
  
  g=g+1
}
