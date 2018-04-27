findGeneExpressionLocationsViaUniprot = function(GeneNames = c("CD180"), SleepTime = 0, Organism = c("Human"))
{
  TISSUELocation = list()
  for(cGeneName in GeneNames)
  {
    url = paste0("https://www.uniprot.org/uniprot/?query=gene%3A", cGeneName,"+organism%3A", Organism, "&format=tab")
    QueryResult = read.table(url, header = T, fill = NA, sep = "\t", stringsAsFactors = F)
    EntryIDs = QueryResult[, "Entry"]
    if(length(EntryIDs) == 0 )
      next
    TISSUELocation[[cGeneName]] = list()
    for(cEntryID in EntryIDs)
    {
      print(paste0(cGeneName," ", cEntryID, " ", which(EntryIDs == cEntryID), "/", length(EntryIDs)))
      url = paste0("https://www.uniprot.org/uniprot/", cEntryID,".txt") 
      txt = readLines(url)
      txt = txt[grep("TISSUE=", txt)]
      if(length(txt) == 0)
        next
      TISSUELocation[[cGeneName]] = rbind(TISSUELocation[[cGeneName]], 
                                            cbind(cEntryID, Location = gsub(".*TISSUE=|;$| \\{.*\\}|\\{.*,", "", txt)))
      Sys.sleep(ifelse(SleepTime == 0, SleepTime, max(0, SleepTime + rnorm(1, 2, 1))))
    }
    #TISSUELocation[[cGeneName]] = unique(as.character(TISSUELocation[[cGeneName]][, "Location"]))
  }
  TISSUELocation
}
