
eco.monthly <- function(fred.keys,key.rename, shape){

  shape <- ifelse(missing(shape),'w',shape)

  if(shape == 'l'){
    fred.keys <-append('USRECM', fred.keys)
    df <-data.frame(map_dfr(fred.keys,fredr))
    df[df$series_id=='USRECM'] <-'recession'
    df <-df[order(series_id,as.Date(df[,1], format="%d/%m/%Y")),]
    df$quarter <-paste(quarters(as.Date(df[,1])), format(df[,1], '%y'), sep=" ")
    return(df)
  }else{
    fred.keys <-append('USRECM', fred.keys)
    df <-data.frame(map_dfr(fred.keys,fredr))
    df <-stats::reshape(df, idvar='date', timevar='series_id', direction='wide')
    key.rename <-append('recession', key.rename)
    key.rename <-append('date', key.rename)
    colnames(df) <-key.rename
    df <-df[order(as.Date(df[,1], format="%d/%m/%Y")),]
    rownames(df) <-df$date
    df$quarter <-paste(quarters(as.Date(df[,1])), format(df[,1], '%y'), sep=" ")
    return(df)
  }


}






