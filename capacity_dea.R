#DEA analysis
library(Benchmarking)
#select positive columns only
summer <- function(x){
  if(is.numeric(x)){
    sum(x) > 0
  } else {
    TRUE
  }
}
save.seed = 27091322

#### defining the components of the DEA model
res = data.frame(NULL) 
n=1000
#Country numbers
for (j in c(584,188,352,266,862,736,8,12,24,28,32,36,44,50,52,56,76,84,90,96,100,104,111,116,120,124,132,144,152,156,157,170,174,
            178,180,184,192,196,204,208,212,214,218,222,226,242,246,250,262,266,270,276,288,296,300,304,
            308,320,324,328,332,340,356,360,364,372,376,380,384,388,392,400,404,410,414,422,430,434,450,
            458,462,470,478,480,484,504,508,512,520,528,548,554,558,566,570,578,583,585,586,591,598,604,
            608,620,624,634,642,643,659,662,670,678,682,686,690,694,702,704,706,710,724,736,740,752,760,764,
            768,776,780,784,788,792,798,818,826,834,840,858,882,887,234,48,616,516,233,428,440,804,268,191,
            368,705,626,892,388)){
  for (i in c("A","I")){
    eurc = subset(eur2, cnumber %in% c(j) & sector %in% c(i) & year > 1949 & year <2015)
    eurd = subset(eur3,cnumber %in% c(j) & sector %in% c(i) & year > 1949 & year <2015)
    
    #here if the data contains no rows then skip to next country, usually because artisanal is not in every fleet
    if(nrow(eurc)<30) {
      print("Skipping")
      next
    }
    print(j)
    
    
    
    fix     = as.matrix(eurc[,c(5,7)]) ## fixed inputs
    all     = as.matrix(eurc[,c(5:7)]) ## all inputs, effort (days) is variable 
    outputs  = as.matrix(eurc[,c(8:16)][, sapply(eurc[,c(8:16)],  summer)]) ##outputs
    prices   = as.matrix(eurd[,c(8:16)][, sapply(eurd[,c(8:16)],  summer)])
    

    allinvrs          = dea(as.matrix(all), as.matrix(outputs),RTS="vrs",ORIENTATION="out")
    
    #summary(allinvrs) ## produces summary output 
    eurc$TEvrs        = round(1/(allinvrs$eff),2)
    
    set.seed(save.seed)

      allinboot         = dea.boot(as.matrix(all), as.matrix(outputs),n,RTS="vrs",ORIENTATION="out")
      eurc$TEvrsbc      = 1/(allinboot$eff.bc) 
      eurc$TEvrsbcu     = 1/(allinboot$conf.int)[,1]
      eurc$TEvrsbcl     = 1/(allinboot$conf.int)[,2]

    
    #allocative efficiency
    
    ropt              = revenue.opt(all, outputs, prices,RTS="vrs")
    eurc$revobs       = rowSums(outputs * prices) 
    obsrev            = outputs * prices
    optrev            = ropt$yopt * prices
    
    colnames(optrev)  = paste( colnames(eurc[,c(8:16)])[sapply(eurc[,c(8:16)],summer)],"_opt", sep="")
    colnames(obsrev)  = paste( colnames(eurc[,c(8:16)])[sapply(eurc[,c(8:16)],summer)],"_obs", sep="")
    
    allrev            = cbind(obsrev, optrev)
    
    # observed revenue 
    eurc$revopt       = rowSums(ropt$yopt * prices)
    
    #optimal revenue 
    eurc$RE           = eurc$revobs/eurc$revopt #revenue efficiency observed maximised to potential.
    eurc$allocative   = eurc$RE/eurc$TEvrsbc #(AE) represents how well the fishers both catch fish and catch the most valuable combination of fish
   
    eurc              = cbind(eurc,allrev)
    
    nms=c("year","cnumber","rname","sector",
          "vessno","days","kw","TEvrs",	
          "revobs","revopt",	"TEvrsbc",	"TEvrsbcu","TEvrsbcl",
          "RE","allocative","crustacean_obs",	"elasmobranchs_obs","crustacean_fw_obs",			
          "large_pelagic_obs",	"marine_obs",	"mixed_pelagic_obs",	"mollusc_obs","mollusc_fw_obs","small_pelagic_obs","other_obs","other_opt",
          "crustacean_opt",	"elasmobranchs_opt", "crustacean_fw_ops",	"mollusc_fw_opt",		
          "large_pelagic_opt",	"marine_opt",	"mixed_pelagic_opt",	"mollusc_opt","small_pelagic_opt")	
    missing = setdiff(nms, names(eurc))  # Find names of missing columns
    eurc[missing] = 0                    # Add them, filled with '0's
    eurc = eurc[nms]    
    
    res               = as.data.frame(rbind(res,eurc))
  }  
}