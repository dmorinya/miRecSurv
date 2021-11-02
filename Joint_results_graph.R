All <- rbind(Gaceta250, Gaceta500, Gaceta1000, SJWEH250, SJWEH500, SJWEH1000)
#All.Model <- " "
All$Model[All$c0=="ComP (CP) F"] <- "SHFMI.CP" 
All$Model[All$c0=="ComP (GT) F"] <- "SHFMI.GT"  
All$Model[All$c0=="AG F"] <- "CHFM.strata" 
WriteXLS("All", "all_results.xls")
All2 <- All[!is.na(All$Model),]
table(All$Model)
table(All2$Model)
WriteXLS("All2", "all_results_graph.xls")
