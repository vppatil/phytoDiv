#functions for querying a genus + species in algaebase and scraping the resulting web
#page for a match

#website is www.algaebase.org
#for any resulting publications or presentations, please cite as: 
#Guiry, M.D. & Guiry, G.M. 2017. AlgaeBase. World-wide electronic publication, 
#National University of Ireland, Galway. http://www.algaebase.org;

library(RCurl)
library(httr)
library(XML)
library(rvest)
library(plyr)


24       Sphaerocystis       schroeteri                                                       
1126          Closterium       striolatum                                                       
1163           Teilingia         excavata

genus='Teilingia'
species='excavata'

algae.search=function(genus,species='',b=F,long=F)
{
	#if only a genus is entered, or species is blank, it will execute a genus search.
	#simply returns the binomial name if confirmed. else returns na
	status=0
	family=0
	
	if(grepl('acea',genus))#truncating family searches
	{
		family.name=genus
		genus=substr(genus,1,4)
		family=1
		URL=paste('www.algaebase.org/search/?genus=',genus,sep='')
	}else
	{
		URL=paste('www.algaebase.org/search/?species=',
        genus,'%20',
        species,sep='')
	}
	
	groups = c('Empire','Kingdom','Phylum','Class','Order','Family')
	
    if(is.na(genus)|genus=='NA'|genus=='NaN'|genus=='na'|genus=='')
    {
      res.df=data.frame(genus=NA,species=NA,checked=0,synonyms=NA,orig.spelling=0)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)
    }

	url.get=GET(URL)
	
	parsed=htmlParse(url.get)
	plain.text <- xpathSApply(parsed, "//p", xmlValue)
	status=plain.text[grep('Status',plain.text)]
	status=ifelse(status=="Status of nameThis name is of an entity that is currently accepted taxonomically.",1,0)
	status=ifelse(length(status)==0,0,status)
	
	tabs=readHTMLTable(parsed)
	if(length(tabs)==0){return(zero.table(genus=genus,species=species,status=status,b=b,long=long))};#if no hits, try again with truncated genus and species names. else break
	
	results.tab=data.frame(tabs[[1]],stringsAsFactors = F)
	
	genus.match=grepl(genus,results.tab[[1]])
	species.match=grepl(species,sapply(as.character(results.tab[[1]]),function(x) substring(strsplit(x,split=' ')[[1]][2],1,nchar(species))),fixed=T)
	results.tab<-results.tab[genus.match & species.match,]
	
	if(dim(results.tab)[1]==0){
		return(zero.table(genus=genus,species=species,status=status,b=b,long=long))};

	if(family==1) #drilling into genus search results
	{
		res.df=data.frame(genus=NA,species=NA,checked=0,synonyms=NA,orig.spell=0)
		if(long)
		{
		  res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
		}
		
		return(res.df)
	}
	
	  colnames(results.tab)[2]='Current name if different'
	  results.tab$Name<-sapply(as.character(results.tab$Name),function(x) return(strsplit(x,split=' var.',fixed=T)[[1]][1]))
	  res.names=results.tab$Name
	  #check if there are any verified names.
	  rms<- -unique(c(grep('var.',res.names,fixed=T),grep('subsp',res.names),grep('Unchecked',res.names)))#pros and cons to this vs. dropping the .var. names several lines below.
	 
  
	  good.index=1:length(res.names)
	  if(length(rms)>0)
	  {
	    good.index=good.index[-unique(c(grep('var.',res.names,fixed=T),grep('subsp',res.names),grep('Unchecked',res.names)))]
	  }

	  good.names=as.character(res.names[good.index])[1] #arbitrarily returning first hit.
	  
	  if(length(good.names)==1) 
	  {
	    good.names=strsplit(good.names,split=' ')[[1]][1:2]
	    res.genus=good.names[1]
	    res.species=good.names[2]
	    res.checked=1
	    res.synonyms=unique(results.tab[good.index,2])
	    res.synonyms=paste(res.synonyms,collapse=',')
	    spell=1
	  }else #do the spell check here.
	  {
	    good.names=sapply(good.names,function(x) paste(strsplit(x,split=' ')[[1]][1:2],sep=' '))
	    good.names=unique(good.names)[1]
	    res.genus=good.names[1]
	    res.species=good.names[2]
	    res.checked=0
	    res.synonyms=unique(results.tab[,2]) #grabbing all possible synonyms from search
	    res.synonyms=res.synonyms[res.synonyms!='']
	    res.synonyms=paste(res.synonyms,collapse=',')
	    spell=1
	  }
	  
	 res.df=data.frame(genus=res.genus,species=res.species,checked=res.checked,synonyms=res.synonyms,orig.spell=spell)
	
	if(long)
	{
	  if(status==1)
	  {
	    details.parsed=read_html(url.get)
	  }else
	  {
	    links.parsed=xpathSApply(parsed,"//a/@href")
	    
	    links.parsed<-links.parsed[grep('results',links.parsed)]
	    links.parsed<-links.parsed[seq(1,length(links.parsed),by=2)]
		links.parsed=links.parsed[genus.match & species.match]
		good.link=links.parsed[good.index[1]]
	    good.link=paste('www.algaebase.org',good.link,sep='')
	    
	    details=GET(good.link)
	    details.parsed=read_html(details)
	  }
  #this bit doesn't work if the code DOES find a match for a truncated genus
	  #debug with Bico
	  classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
	  taxa.levels = html_text(html_nodes(classification.node,"i"))
	  if(length(taxa.levels)==0)
	  {
	    df=data.frame(Empire=NA,Kingdom=NA,Phylum=NA,Class=NA,Order=NA,Family=NA)
	  }else
	  {
	    taxa=	html_text(html_nodes(classification.node,"a"))		
	    df=data.frame(rbind(taxa))
	    names(df)=taxa.levels
	    df=df[,match(groups,names(df))] #make sure there is a consistent set of names
	  }
	  res.df<-cbind(res.df,df)
	}
	
	if(is.na(species) | is.null(species) | species %in% c('','sp','sp.','spp.'))
	{
	  res.df$synonyms=res.df$species=''
	}
	  
	return(res.df)
}	

#now to create a wrapper for this.
agg.list=list()
spp.list.algaebase=function(phyto.df,lakename,long=F)
{
  spp.list<-as.character(phyto.df$PHYTO_NAME)
  spp.list<-unique(spp.list)
  gen.spp=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]])
  gen.spp<-t(sapply(gen.spp,function(x) x=x[1:2]))
  gen.spp[gen.spp %in% c('sp','sp.','spp','spp.')]=''
  gen.spp[grepl(glob2rx('sp *'),gen.spp)]=''
  gen.spp<-gsub('?','',gen.spp,fixed=T)
  
  # gen.spp=tail(gen.spp)
  # spp.list=tail(spp.list)
  
  #agg.list=vector("list",length=dim(gen.spp)[1])
  
  for(i in 268:length(gen.spp))#doing it as a loop with a pause in the middle to avoid overloading servers.
  {
    agg.list[[i]]=algae.search(gen.spp[i,1],gen.spp[i,2],long=long)
    agg.list[[i]]$Orig_Name=spp.list[i]
    print(i)
    Sys.sleep(2)
  }
  
  save.image(paste(lakename,'sppcheck.RData',sep=''))
  agg.df<-ldply(agg.list)

  return(agg.df)
}

zero.table<-function(status,genus,species,b,long)
{
  #if you get a good hit
  if(status==1)
  {
    res.genus=genus
    res.species=species
    checked=1
    spell=1
    
    plain.text=get('plain.text',envir=parent.frame())
    url.get=get('url.get',envir=parent.frame())
    
    groups = c('Empire','Kingdom','Phylum','Class','Order','Family')
    
    res.synonyms=plain.text[grep('Synonym',plain.text)]
    res.synonyms=ifelse(grepl('Homotypic',res.synonyms,fixed=T),
                        gsub('Homotypic Synonym(s)','',res.synonyms,fixed=T),
                        gsub('Synonym(s)','',res.synonyms,fixed=T))
    res.synonyms[grepl('No synonym',res.synonyms,fixed=T)]=''
    res.df=data.frame(genus=res.genus,species=res.species,checked=1,synonyms=res.synonyms,orig.spell=spell)
    if(long)
    {
      details.parsed=read_html(url.get)
      classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
      taxa.levels = html_text(html_nodes(classification.node,"i"))	
      taxa=	html_text(html_nodes(classification.node,"a"))		
      df=data.frame(rbind(taxa))
      names(df)=taxa.levels
      df=df[,match(groups,names(df))] #make sure there is a consistent set of names
      
      res.df<-cbind(res.df,df)
    }
    return(res.df)
    
  }else if(b)
  {
    res.df=data.frame(genus=NA,species=NA,checked=0,synonyms=NA,orig.spell=0)
    if(long)
    {
      res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
    }
    return(res.df)
  }else
  {
    genus=substr(genus,1,4)
    species=substr(species,1,4)
    res=algae.search(genus,species,b=T,long=long)
    res$orig.spell=0
    return(res)
  }
}