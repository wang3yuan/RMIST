findGeneExpressionLocationsViaUniprot = function(GeneNames = c("CD180"), SleepTime = 0)
{
  TISSUELocation = list()
  for(cGeneName in GeneNames)
  {
    url = paste0("https://www.uniprot.org/uniprot/?query=", cGeneName,"&format=tab")
    QueryResult = read.table(url, header = T, fill = NA, sep = "\t", stringsAsFactors = F)
    EntryIDs = QueryResult[, "Entry"]
    TISSUELocation[[cGeneName]] = list()
    for(cEntryID in EntryIDs)
    {
      print(paste0(cGeneName," ", cEntryID, " ", which(EntryID == cEntryID), "/", length(EntryIDs)))
      url = paste0("https://www.uniprot.org/uniprot/", cEntryID,".txt") 
      txt = readLines(url)
      txt = txt[grep("TISSUE=", txt)]
      if(length(txt) > 0)
      {
        TISSUELocation[[cGeneName]] = rbind(TISSUELocation[[cGeneName]], 
                                            cbind(cEntryID, Location = gsub(".*TISSUE=|;$| \\{.*\\}|\\{.*,", "", txt)))
      }
      Sys.sleep(SleepTime)
    }
    #TISSUELocation[[cGeneName]] = unique(as.character(TISSUELocation[[cGeneName]][, "Location"]))
  }
  TISSUELocation
}
