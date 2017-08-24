studygroup_f<-factor(study_group)

#Change to num
testInteger <- function(x){
  test <- is.integer(x)
  if(test == "TRUE"){ as.numeric(x) }
  else {
    if(test=="FALSE"){x}
  }
}  
updated_petdata<-sapply(petdata_small,testInteger)

#Mn_exposure vs dBP
    tmp_w<-lm(mn_cumulative[studygroup_f=="w"]~R_Striatum_dBP[studygroup_f=="w"])
    tmp_c<-lm(mn_cumulative[studygroup_f=="c"]~R_Striatum_dBP[studygroup_f=="c"])
    plot(R_Striatum_dBP,mn_cumulative,col=studygroup_f)
    abline(tmp_w,col="red")
    abline(tmp_c,col="black")
    cor.test(mn_cumulative[studygroup_f=="c"]~R_Striatum_dBP[studygroup_f=="c"])
    cor.test(mn_cumulative[studygroup_f=="w"],R_Striatum_dBP[studygroup_f=="w"])
    dev.copy(png,"Mn_RStriatum_dBP.png")
    dev.off()

    tmp_w<-lm(mn_cumulative[studygroup_f=="w"]~L_Striatum_dBP[studygroup_f=="w"])
    tmp_c<-lm(mn_cumulative[studygroup_f=="c"]~L_Striatum_dBP[studygroup_f=="c"])
    plot(R_Striatum_dBP,mn_cumulative,col=studygroup_f)
    abline(tmp_w,col="red")
    abline(tmp_c,col="black")
    cor.test(mn_cumulative[studygroup_f=="c"]~L_Striatum_dBP[studygroup_f=="c"])
    cor.test(mn_cumulative[studygroup_f=="w"],L_Striatum_dBP[studygroup_f=="w"])
    dev.copy(png,"Mn_LStriatum_dBP.png")
    dev.off()
    
#Mn_exposure vs iBP
#UPDRS_Score vs dBP
#UPDRS_Score vs iBP
#UPDRS_Bradykinesia vs dBP
#UPDRS_Bradykinesia vs iBP
#NP_Fingertapping vs dBP
#NP_Fingertapping vs iBP
#NP_pegs vs dBP
#NP_pegs vs iBP


c"])
