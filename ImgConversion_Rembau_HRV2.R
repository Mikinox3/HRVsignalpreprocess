#---------- A. Legend Cut the RembAU file--------------
# CUT THE FILE IN 4 PARTS : rest 1 / img /rest 2 /film

############ BIOPAC
#-- CODE IMAGE
#IMGTYPE:-IMGScrambled--IMG--IMGScrambled--COTation
##NEG--71--72--73--74 ## MID--81--82--83--84 ## POS--91--92--93--94
#-- CODE FILM
#FILMTYPE:-AV--FILM--AP--COT
##NEG--101--102--103--104 ## MID--111--112--113--114 ## POS--121--122--123--124
#-- CODE REPOS
##REPOS1--65 ## REPOS2--66/65 ## CALIBRATION--67 ## VALIDATION--68

############ SMI
#-- CODE IMAGE
#IMGTYPE:-IMGScrambled--IMG--IMGScrambled--COTation
##NEG--142--144--146--148 ## MID--162--164--166--168 ## POS--182--184--186--188
#-- CODE FILM
#FILMTYPE:-AV--FILM--AP--COT
##NEG--202--204--206--208 ## MID--222--224--226--228 ## POS--242--244--246--248
#-- CODE REPOS
##REPOS1--130 ## REPOS2--132 ## CALIBRATION--134 ## VALIDATION--136

############ In this script:
#Repos[1] = idx of the first 65 Peak in rest1 seq
#Lastidx = idx of the last 65 peak in rest1 seq
#Peak1 = value of the first peak detected in the img sequence
#Peakidx = idx of the first peak detected in the img seq
#PeakError = data frame containing the values and indexes of unexpected peaks between rest 1 and img seq
#LastPeakval = value of the last peak detected in the img seq
#LastPeakidx = idx of the last peak detected in the img seq
#PeakImg = data frame containing the values and indexes of unexpected peaks in img seq
#Firstidx2 = idx of the first 65 Peak in rest2 seq
#Lastidx2 = idx of the last 65 Peak in rest2 seq
#PeakFilm1= value of the first Peak detected in the film seq
#PeakFilmidx = idx of the first Peak detected in the film seq
#PeakError2 = df containing the values and indexes of unexpected peaks between rest 2 and film seq
#LastPeakFilmval =  value of the last peak detected in the film seq
#LastPeakFilmidx =  idx of the last peak detected in the film seq
#PeakFilm = data frame containing the values and indexes of unexpected peaks in film seq

#-----------B. Peak REPOS 1 first & last value detection---------------
Repos<-which(dataX$dec==65 | dataX$dec==66) #all the idx where you can find 65 in the data frame
Repos[1]#idx of the first position of 65
Lastidx=Repos[200] #Lastidx, each trigger 200 lines, 1 line= 0.0005s, 120000 lines = 1 min
abline(v=Repos[1],col="darkred")
abline(v=Lastidx,col="darkred")
#-----------C. First Img peak detection (either = 71 /81 or 91)-----------------------
Fst_img=which(dataX$dec==71|dataX$dec==81|dataX$dec==91)
Peakidx=min(Fst_img) #idx = position of the first peak of img seq
Peak1<- dataX$dec[Peakidx] # Peak1 = value of the first peak in img seq
abline(v=Peakidx,col="darkorchid4")
#-----------C. 1. Control : presence of unexpected peaks between Repos1 & img1---------------
i=Lastidx
PeakError <- data.frame(Val=numeric(Peakidx-Lastidx),Idx=numeric(Peakidx-Lastidx),Log=logical(Peakidx-Lastidx)) #Peakerror = df containing unexpected peaks between rest 1 and img sequences
while(i< Peakidx ){
  if(dataX$dec[i]!=0){
    PeakError$Val[i-Lastidx]<- dataX$dec[i]
    PeakError$Idx[i-Lastidx]<- i
    PeakError$Log[i-Lastidx] = TRUE
  }
  i<-i+1
}
Errors<-which(PeakError$Log==TRUE)
ifelse(is.integer(Errors),"Good","Bad")
#-----------D. Last img peak detection (either = 74 /84 or 94)---------------------
Lst_img=which(dataX$dec==74 | dataX$dec==84 | dataX$dec==94)
LastPeakidx=max(Lst_img) #idx = position of the first peak of img seq
LastPeakval<- dataX$dec[LastPeakidx] # Peak1 = value of the first peak in img seq
abline(v=LastPeakidx,col="darkorchid4")
#-----------D. 1. Control : presence of unexpected peak values in img sequence-----------------------
i=Peakidx
PeakImg <- data.frame(Val=numeric(LastPeakidx-Peakidx),Idx=numeric(LastPeakidx-Peakidx),Log=logical(LastPeakidx-Peakidx))
while(i< LastPeakidx ){
  if(dataX$dec[i]!=0&&dataX$dec[i]!=71&&dataX$dec[i]!=72&&dataX$dec[i]!=73&&dataX$dec[i]!=74&&dataX$dec[i]!=81&&dataX$dec[i]!=82&&dataX$dec[i]!=83&&dataX$dec[i]!=84&&dataX$dec[i]!=91&&dataX$dec[i]!=92&&dataX$dec[i]!=93&&dataX$dec[i]!=94){
    PeakImg$Val[i-Peakidx]<- dataX$dec[i]
    PeakImg$Idx[i-Peakidx]<- i
    PeakImg$Log[i-Peakidx] = TRUE
  }
  i<-i+1
}
seqImgError<-which(PeakImg$Val!=0)
ifelse(is.integer(seqImgError),"Good","Bad")


#**********
#*#**********
#*#**********
#*#**********
#*#**********
#-----------E. Peak REPOS 2 first & last value detection--------------------------
Firstidx2=min(Repos[LastPeakidx<Repos])
if (Firstidx2!=Inf){
Firstval2<- dataX$dec[Firstidx2]
Lastidx2=max(Repos[LastPeakidx<Repos]) 
Lastval2<- dataX$dec[Lastidx2]
abline(v=Firstidx2,col="darkorange3")
abline(v=Lastidx2,col="darkorange3")

#-----------F. First Film peak detection (either = 101 /111 or 121)---------------------
Fst_film=which(dataX$dec==101 | dataX$dec==111 | dataX$dec==121)
PeakFilmidx=min(Fst_film) #idx = position of the first peak of img seq
PeakFilm1<- dataX$dec[PeakFilmidx] # Peak1 = value of the first peak in img seq
abline(v=PeakFilmidx,col="red")
#-----------F. 1. Control : presence of unexpected peaks between Repos2 & film1--------------------
i=Lastidx2
PeakError2 <- data.frame(Val=numeric(PeakFilmidx-Lastidx2),Idx=numeric(PeakFilmidx-Lastidx2),Log=logical(PeakFilmidx-Lastidx2))
while(i< PeakFilmidx ){
  if(dataX$dec[i]!=0){
    PeakError2$Val[i-Lastidx2]<- dataX$dec[i]
    PeakError2$Idx[i-Lastidx2]<- i
    PeakError2$Log[i-Lastidx2] = TRUE
  }
  i<-i+1
}
Errors2<-which(PeakError2$Log==TRUE)
ifelse(is.integer(Errors2),"Good","Bad")
#-----------G. Last film peak detection (either = 104 /114 or 124)------------------
Lst_film=which(dataX$dec==104 | dataX$dec==114 | dataX$dec==124)
LastPeakFilmidx=max(Lst_film) #idx = position of the first peak of img seq
LastPeakFilmval<- dataX$dec[LastPeakFilmidx] # Peak1 = value of the first peak in img seq
abline(v=LastPeakFilmidx,col="red")
#-----------G. 1. Control : presence of unexpected peak values in film sequence----------
i=PeakFilmidx
PeakFilm <- data.frame(Val=numeric(LastPeakFilmidx-PeakFilmidx),Idx=numeric(LastPeakFilmidx-PeakFilmidx),Log=logical(LastPeakFilmidx-PeakFilmidx))
while(i< LastPeakFilmidx ){
  if(dataX$dec[i]!=0&&dataX$dec[i]!=101&&dataX$dec[i]!=102&&dataX$dec[i]!=103&&dataX$dec[i]!=104&&dataX$dec[i]!=111&&dataX$dec[i]!=112&&dataX$dec[i]!=113&&dataX$dec[i]!=114&&dataX$dec[i]!=121&&dataX$dec[i]!=122&&dataX$dec[i]!=123&&dataX$dec[i]!=124){
    PeakFilm$Val[i-PeakFilmidx]<- dataX$dec[i]
    PeakFilm$Idx[i-PeakFilmidx]<- i
    PeakFilm$Log[i-PeakFilmidx] = TRUE
  }
  i<-i+1
}
seqFilmError<-which(PeakFilm$Val!=0)
ifelse(is.integer(seqFilmError),"Good","Bad")

}

#---------- A1. Legend SAVE ECG BY IMG TYPE---------------------------
#Pos = dataX's idx corresponding to peaks 92 (one 92 peak lasts during about 200 lines --> one peak = 200 datax'S idx)
#ImgPos = df with the first peak line only (8 idx for the 8 peaks we are looking for)
#True92Img = df with the 8 idx only (ImgPos contains also NA)
#endpos = dataX's idx corresponding to peaks 93
#ImgEndPos = df with the first peak line only 
#True93Img = df with the 8 idx only 
#Image = df containing 2 col for the 2 x 8 peaks (92/93)
#ImageEcg = df with 8 col --> 1 col = Ecg for one positive img (from 92 to 93 peak)
#The same structure is used for negative img (72/73) and neutral img (82/83)

#---------- A2. ECG FOR POSITIVE IMG (92 peaks in a dataframe) ---------------------------
pos<-which(dataX$dec==92)
datalist_pos = list()
for (i in (1:(length(pos)-1))){
  if((pos[i+1]-pos[i])>1)
  {datalist_pos[[i+1]]=pos[i+1]
  }
}
big_data_pos = do.call(rbind, datalist_pos)
True92Img=rbind(big_data_pos,pos[1])
True92Img=sort(True92Img)
True92Img=data.frame(trigger92=True92Img, idx=seq(length(True92Img)))
points(True92Img$trigger92, y =c(rep(100,times=length(True92Img$trigger92))), pch=3 ,col = "darkmagenta", cex = 0.8)
#--------------------save all the 93 peaks in a dataframe
endpos<-which(dataX$dec==93)
datalist_end = list()
for (i in (1:(length(endpos)-1))){
  if((endpos[i+1]-endpos[i])>1)
  {datalist_end[[i+1]]=endpos[i+1]
  }
}
big_data_end_pos = do.call(rbind, datalist_end)
True93Img=rbind(big_data_end_pos,endpos[1])
True93Img=sort(True93Img)
True93Img=data.frame(trigger93=True93Img, idx=seq(length(True93Img)))
#-----------------------combine dataframes
Image=merge(True92Img,True93Img,by=c("idx"))
names(Image)[2] <- "Pstart"
names(Image)[3] <- "Pend"
Image$diff=Image$Pend-Image$Pstart
#----------------SAVE ECG between my 92 and 93 peaks(ECG for positive img)
for(m in 1:8){
  x=Image$Pstart[m]; x1=Image$Pend[m]
  sig= dataX$V2[dataX$idx[x]:dataX$idx[x1]]
  nam <- paste("Ecg", m, sep = "")
  assign(nam, sig)
}
ImgEcg=list(Ecg1,Ecg2,Ecg3,Ecg4,Ecg5,Ecg6,Ecg7,Ecg8)
# cols <- rainbow(8)
# x11()
# plot(NA, ylim = c(-1, 1), xlim = c(0, 12050))
# for(i in 1:8) lines(ImgEcg[[i]], col = cols[i], lwd=2)
# legend("bottomleft", legend=paste("number",1:8), lwd=2, col=cols)

#---------- A3. ECG FOR NEUTRAL IMG (82 peaks in a dataframe) ---------------------------
neu<-which(dataX$dec==82)
datalist_neu = list()
for (i in (1:(length(neu)-1))){
  if((neu[i+1]-neu[i])>1)
  {datalist_neu[[i+1]]=neu[i+1]
  }
}
big_data_neu = do.call(rbind, datalist_neu)
True82Img=rbind(big_data_neu,neu[1])
True82Img=sort(True82Img)
True82Img=data.frame(trigger82=True82Img, idx=seq(length(True82Img)))
points(True82Img$trigger82, y =c(rep(100,times=length(True82Img$trigger82))), pch="=" ,col = "darkmagenta", cex = 0.8)
#--------------------save all the 83 peaks in a dataframe
endneu<-which(dataX$dec==83)
datalist_end_neu = list()
for (i in (1:(length(endneu)-1))){
  if((endneu[i+1]-endneu[i])>1)
  {datalist_end_neu[[i+1]]=endneu[i+1]
  }
}
big_data_end_neu = do.call(rbind, datalist_end_neu)
True83Img=rbind(big_data_end_neu,endneu[1])
True83Img=sort(True83Img)
True83Img=data.frame(trigger83=True83Img, idx=seq(length(True83Img)))
#-----------------------combine dataframes
Image2=merge(True82Img,True83Img,by=c("idx"))
# name the columns
names(Image2)[2] <- "Pstart"
names(Image2)[3] <- "Pend"
Image2$diff=Image2$Pend-Image2$Pstart
#----------------SAVE ECG between my 82 and 83 peaks(ECG for neutral img)
for(m in 1:8){
  x=Image2$Pstart[m]; x1=Image2$Pend[m]
  sig= dataX$V2[dataX$idx[x]:dataX$idx[x1]]
  nam <- paste("EcgN", m, sep = "")
  assign(nam, sig)
}
ImgEcg2=list(EcgN1,EcgN2,EcgN3,EcgN4,EcgN5,EcgN6,EcgN7,EcgN8)
# cols <- rainbow(8)
# x11()
# plot(NA, ylim = c(-1, 1), xlim = c(0, 12050))
# for(i in 1:8) lines(ImgEcg2[[i]], col = cols[i], lwd=2)
# legend("bottomleft", legend=paste("number",1:8), lwd=2, col=cols)

#---------- A4. ECG FOR NEGATIVE IMG (72 peaks in a dataframe) ---------------------------
neg<-which(dataX$dec==72)
datalist_neg = list()
for (i in (1:(length(neg)-1))){
  if((neg[i+1]-neg[i])>1)
  {datalist_neg[[i+1]]=neg[i+1]
  }
}
big_data_neg = do.call(rbind, datalist_neg)
True72Img=rbind(big_data_neg,neg[1])
True72Img=sort(True72Img)
True72Img=data.frame(trigger72=True72Img, idx=seq(length(True72Img)))
points(True72Img$trigger72, y =c(rep(100,times=length(True72Img$trigger72))), pch="-" ,col = "darkmagenta", cex = 0.8)
#--------------------save all the 73 peaks in a dataframe
endneg<-which(dataX$dec==73)
datalist_end_neg = list()
for (i in (1:(length(endneg)-1))){
  if((endneg[i+1]-endneg[i])>1)
  {datalist_end_neg[[i+1]]=endneg[i+1]
  }
}
big_data_end_neg = do.call(rbind, datalist_end_neg)
True73Img=rbind(big_data_end_neg,endneg[1])
True73Img=sort(True73Img)
True73Img=data.frame(trigger73=True73Img, idx=seq(length(True73Img)))
#-----------------------combine dataframes
Image3=merge(True72Img,True73Img,by=c("idx"))
# name the columns
names(Image3)[2] <- "Pstart"
names(Image3)[3] <- "Pend"
Image3$diff=Image3$Pend-Image3$Pstart
#----------------SAVE ECG between my 72 and 73 peaks(ECG for negative img)
for(m in 1:8){
  x=Image3$Pstart[m]; x1=Image3$Pend[m]
  sig= dataX$V2[dataX$idx[x]:dataX$idx[x1]]
  nam <- paste("EcgNeg", m, sep = "")
  assign(nam, sig)
}
ImgEcg3=list(EcgNeg1,EcgNeg2,EcgNeg3,EcgNeg4,EcgNeg5,EcgNeg6,EcgNeg7,EcgNeg8)
# cols <- rainbow(8)
# x11()
# plot(NA, ylim = c(-1, 1), xlim = c(0, 12050))
# for(i in 1:8) lines(ImgEcg3[[i]], col = cols[i], lwd=2)
# legend("bottomleft", legend=paste("number",1:8), lwd=2, col=cols)



#*********
#*#*********
#*#*********
#*#*********
#*
#---------- AA1. Legend SAVE ECG BY FILM TYPE---------------------------
# Same than IMG but with film
#---------- AA2. ECG FOR POSITIVE FILM (122 peaks in a dataframe) ---------------------------
posF<-which(dataX$dec==122)
if(length(posF)!=0){
datalist_posF = list()
for (i in (1:(length(posF)-1))){
  if((posF[i+1]-posF[i])>1)
  {datalist_posF[[i+1]]=posF[i+1]
  }
}
big_data_posF = do.call(rbind, datalist_posF)
True122film=rbind(big_data_posF,posF[1])
True122film=sort(True122film)
True122film=data.frame(trigger122=True122film, idx=seq(length(True122film)))
points(True122film$trigger122, y =c(rep(126,times=length(True122film$trigger122))), pch=3 ,col = "chocolate4", cex = 0.8)
#--------------------save all the 123 peaks in a dataframe
endposF<-which(dataX$dec==123)
#-----------------------combine dataframes
Filmo=data.frame(idx=seq(1),Pstart=posF[1], Pend=endposF[1])
Filmo$diff=Filmo$Pend-Filmo$Pstart

#----------------SAVE ECG between my 122 and 123 peaks(ECG for positive film)
for(m in 1:1){
  x=Filmo$Pstart[m]; x1=Filmo$Pend[m]
  sig= dataX$V2[dataX$idx[x]:dataX$idx[x1]]
  nam <- paste("EcgF", m, sep = "")
  assign(nam, sig)
}
filmEcgF=list(EcgF1)
# cols <- rainbow(8)
# x11()
# plot(NA, ylim = c(-1, 1), xlim = c(0, 12050))
# for(i in 1:8) lines(filmEcgF[[i]], col = cols[i], lwd=2)
# legend("bottomleft", legend=paste("number",1:8), lwd=2, col=cols)

#---------- AA3. ECG FOR NEUTRAL FILM (112 peaks in a dataframe) ---------------------------
neuF<-which(dataX$dec==112)
datalist_neuF = list()
for (i in (1:(length(neuF)-1))){
  if((neuF[i+1]-neuF[i])>1)
  {datalist_neuF[[i+1]]=neuF[i+1]
  }
}
big_data_neuF = do.call(rbind, datalist_neuF)
True112film=rbind(big_data_neuF,neuF[1])
True112film=sort(True112film)
True112film=data.frame(trigger112=True112film, idx=seq(length(True112film)))
points(True112film$trigger112, y =c(rep(126,times=length(True112film$trigger112))), pch="=" ,col = "chocolate4", cex = 0.8)
#--------------------save all the 113 peaks in a dataframe
endneuF<-which(dataX$dec==113)
#-----------------------combine dataframes
Filmo2=data.frame(idx=seq(1),Pstart=neuF[1], Pend=endneuF[1])
Filmo2$diff=Filmo2$Pend-Filmo2$Pstart
#----------------SAVE ECG between my 112 and 113 peaks(ECG for neutral film)
for(m in 1:1){
  x=Filmo2$Pstart[m]; x1=Filmo2$Pend[m]
  sig= dataX$V2[dataX$idx[x]:dataX$idx[x1]]
  nam <- paste("EcgFN", m, sep = "")
  assign(nam, sig)
}
filmEcgF2=list(EcgFN1)
# cols <- rainbow(8)
# x11()
# plot(NA, ylim = c(-1, 1), xlim = c(0, 12050))
# for(i in 1:8) lines(filmEcgF2[[i]], col = cols[i], lwd=2)
# legend("bottomleft", legend=paste("number",1:8), lwd=2, col=cols)

#---------- AA4. ECG FOR NEGATIVE FILM (102 peaks in a dataframe) ---------------------------
negF<-which(dataX$dec==102)
datalist_negF = list()
for (i in (1:(length(negF)-1))){
  if((negF[i+1]-negF[i])>1)
  {datalist_negF[[i+1]]=negF[i+1]
  }
}
big_data_negF = do.call(rbind, datalist_negF)
True102film=rbind(big_data_negF,negF[1])
True102film=sort(True102film)
True102film=data.frame(trigger102=True102film, idx=seq(length(True102film)))
points(True102film$trigger102, y =c(rep(126,times=length(True102film$trigger102))), pch="-" ,col = "chocolate4", cex = 0.8)
#--------------------save all the 103 peaks in a dataframe
endnegF<-which(dataX$dec==103)
#-----------------------combine dataframes
Filmo3=data.frame(idx=seq(1),Pstart=negF[1], Pend=endnegF[1])
Filmo3$diff=Filmo3$Pend-Filmo3$Pstart
#----------------SAVE ECG between my 102 and 103 peaks(ECG for negative film)
for(m in 1:1){
  x=Filmo3$Pstart[m]; x1=Filmo3$Pend[m]
  sig= dataX$V2[dataX$idx[x]:dataX$idx[x1]]
  nam <- paste("EcgFNeg", m, sep = "")
  assign(nam, sig)
}
filmEcgF3=list(EcgFNeg1)
# cols <- rainbow(8)
# x11()
# plot(NA, ylim = c(-1, 1), xlim = c(0, 12050))
# for(i in 1:8) lines(filmEcgF3[[i]], col = cols[i], lwd=2)
# legend("bottomleft", legend=paste("number",1:8), lwd=2, col=cols)
abline(v=endnegF[1]+200,col="chocolate4",lty=2)


}else {
  Filmo= 0
  Filmo2= 0
  Filmo3= 0
}

#---------- B1. choix du cut ---------------------------
# define the bounds for signal analysis
#Repos 1


                
if(choix==1) {
  ((bnd1 = dataX$V1[Repos[1]]) & (bnd2 = dataX$V1[Lastidx]))
  text(Repos[1],120, "Analyse Repos", cex=0.65, pos=4,col="deeppink") 
  print("Choix 1, repos")
}
#Image
if(choix==2) {
  ((bnd1 = dataX$V1[Peakidx]) & (bnd2 = dataX$V1[LastPeakidx]))
  text(Peakidx,120, "Analyse Image", cex=0.65, pos=4,col="darkorchid4") 
  print("Choix 2, Image")

}
#Repos 2
if(choix==3) {((bnd1 = dataX$V1[Firstidx2]) & (bnd2 = dataX$V1[Lastidx2]))
  text(Firstidx2,120, "Analyse Image", cex=0.65, pos=4,col="darkorange") 
  print("Choix 3, Repos 2")
}
#Repos Film to do
if(choix==4) {
  #*********
  #*#*********
  #*#*********
  if(length(posF)!=0){
  ((bnd1 = dataX$V1[PeakFilmidx]) & (bnd2 = dataX$V1[endnegF[1]]))
  text(PeakFilmidx,120, "Analyse Image", cex=0.65, pos=4,col="chocolate4") 
  print("Choix 4, Film")
  }
  
  else{
    ((bnd1 = 0 ) & (bnd2 = 1 ))
  }
}

# name the columns
names(dataX)[1] <- "min"
names(dataX)[2] <- "hb"
# create subset df
df <- subset(dataX, select=c(min,hb,idx), subset=(min > bnd1 & min<=bnd2 ))
df$sec<-df$min*60
df$idx <- seq.int(nrow(df))