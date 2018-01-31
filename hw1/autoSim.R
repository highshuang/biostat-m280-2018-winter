# autoSim.R

nVals = seq(100, 500, by=100)
distTypes = c("t1","t5","gaussian")

for (n in nVals) {
  for(dist in distTypes) {
    oFile <- paste("n", n,"dist",dist,".txt", sep="")
    arg <- paste(paste("seed=280",sep=""),paste("n=", n,sep=""),paste("\'dist=",'\"',dist,'\"',"\'",sep=""),
                 paste("rep=50",sep = ""),sep=" ")
    sysCall = paste("nohup Rscript runSim.R ", arg, " > ", oFile,sep="")
    system(sysCall)
    print(paste("sysCall=", sysCall, sep=""))
  }
}







