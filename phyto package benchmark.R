#benchmark script for comparing different name checks 
#using a random sample of taxonomy names from Vortsjarv as a test case

setwd('~/gleon/Geisha/phyto_package/')
source('nico_mfg_convert.r')

#annecy has 269 unique species names
#can check a random sample of 100 against the rest of the list
#would be cool to see if there are a few errors that are more common than others- see if you get two species names that are very close....

library(Reol)
library(taxize)

#grab random list of 10 species names
annecy<-read.csv('Annecy_phyto_names.csv',stringsAsFactors = F)
names(annecy)[1:2]=c('genus','species')

annecy.unique=annecy[!duplicated(annecy),]
test.names=annecy[,c(1,2)]
test.names$species=gsub('sp','',test.names$species)
test.names=paste(test.names[,1],test.names[,2])
test.names=unique(test.names)

#taxize check
tnrs(test.names,getpost = "POST",source_="iPlant_TNRS")
annecy.eol=gnr_resolve(names = test.names)[,-2]
gnr_resolve(names='Sphaerospermum')

annecy.eol=annecy.eol[annecy.eol$score>.6,]
annecy.eol=annecy.eol[!duplicated(annecy.eol$user_supplied_name),]
head(annecy.eol)

taxize function- clean up based on year, higher taxonomy, frequency of name return across multiple databases.

#have cross map with reol, try taxize, - check for names, and synonyms
#also try entering species lists into the online tools that are available.
#finally, try on a SMALL subset - could even enter them in manually- do they have synonyms? are they accepted?

#nico package test with annecy

annecy=annecy[!duplicated(annecy),]

annecy.mfg=phyto.convert.df(annecy)
head(annecy.mfg[,-c(5,6,7)])




#checking on performance
table(is.na(annecy.mfg$MFG))

223+46
table(is.na(annecy.mfg$MFG))/dim(annecy.mfg)[1]
annecy.mfg[is.na(annecy.mfg$MFG),]

write.csv(head(annecy.mfg[,c(1,2,3,4,8)]),'annecymfgtest.csv')


#17% errors
