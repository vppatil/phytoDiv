#functions to add nico functionalgroup classifications to a phytoplankton dataframe
#will need to load his crossmap as a data file associated with the package.

#for now, read it in manually from a csv file
nico.phyto.convert<-function(genus,species)
{
  
  species.funcgroup<-read.csv('~/gleon/Geisha/phyto_package/MFGs_archive.csv',stringsAsFactors = F)
  species.funcgroup<-species.funcgroup[!duplicated(species.funcgroup),]
  
  genus=gsub('Unknown ','',genus)
  if(species %in% species.funcgroup$species==F){species=''}
  mfg=species.funcgroup$MFG[species.funcgroup$genus==genus]
  mfg=ifelse(length(unique(mfg))==1,unique(mfg),species.funcgroup$MFG[species.funcgroup$genus==genus & species.funcgroup$species==species])
  mfg<-ifelse(length(mfg)==0,NA,mfg)
  return(mfg)
}

phyto.convert.df=function(phyto.df)
{
  mfgs<-vector(length=dim(phyto.df)[1])
  for(i in 1:dim(phyto.df)[1])
  {
    mfgs[i]=nico.phyto.convert(phyto.df$genus[i],phyto.df$species[i])
  }
  phyto.df$MFG=mfgs
  return(phyto.df)
}

#agg.df.mfg<-phyto.convert.df(agg.df)

# jason.gen<-read.csv('Grigel_UnderIceGenera.csv',header=F)
# names(jason.gen)[1]='genus'
# jason.gen$species=''
# jason.gen.mfg<-phyto.convert.df(jason.gen)
# 
# write.csv(jason.gen.mfg,'Grigel_UnderIceGenera_MFG.csv')

