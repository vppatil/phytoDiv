###test code to correct spelling errors

###example data file with small errors in both genus and species
nametest<-read.csv('~/gleon/Geisha/phyto_package/phytonametest.csv')

####function for spelling matches. maxErr refers to maximum acceptable number of insertions/deletions/etc.
####first argument is the name to check, and the second is a vector of correctly spelled names
####second argument will eventually come from returns from an algaebase search.

####if two accepted names are equally different from the one you are checking, the function will not try to differentiate between them
bestmatch=function(enteredName,possibleNames,maxErr=3)
{
  for(i in 0:maxErr)
  {
    match=agrep(enteredName,possibleNames,max.distance=i,value=T)
    if(length(match)==1) {return(match)}
    if(length(match)>1) {return('multiplePartialMatch')}
  }
  
}

######creating columns for the best matches found
nametest$genusmatches=''
nametest$sppmatches=''


#####calling the function for all four names- matching each 'wrongname' against all of the 'rightnames'
for(i in 1:dim(nametest)[1])
{
  nametest$genusmatches[i]=bestmatch(nametest$WrongGen[i],nametest$GoodGen)
  nametest$sppmatches[i]=bestmatch(nametest$WrongSpp[i],nametest$GoodSpp)
}