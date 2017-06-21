zero.table<-function(status)
{
	  #if you get a good hit

	  if(status==1)
	  {
	    res.genus=genus
	    res.species=species
	    checked=1
	    spell=1
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