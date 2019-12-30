eco.download <- function(keys, key.rename, shape, periodicity, show.recession, time.series){

  periodicity <-ifelse(missing(periodicity),'monthly',periodicity)
  shape <- ifelse(missing(shape),'w',shape)
  show.recession <-ifelse(missing(show.recession),'y',show.recession)
  time.series <-ifelse(missing(time.series),'n',time.series)

  # Check periodicity
  if((periodicity =='monthly')&(show.recession == 'y')){
    keys <-append('USRECM', keys)
    key.rename <- append('recession',key.rename)

  }else if((periodicity == 'quarterly')&(show.recession == 'y')){
    keys <-append('USRECQ', keys)
    key.rename <- append('recession',key.rename)

  }else if(show.recession == 'n'){
    keys <- keys
    key.rename <- key.rename
  } else{
    warning("Improper Inputs")
  }

if((time.series == 'n') & (shape == 'l')){
  # Shape dataframe
    df <-data.frame(map_dfr(keys,fredr))
    df$series_id <- factor(x= df, levels = keys, labels = key.rename)
    df <-df[order(series_id,as.Date(df[ , 1], format="%d/%m/%Y")),]
    df$quarter <-paste(quarters(as.Date(df[ , 1])), format(df[ , 1], '%y'), sep=" ")
    #df$year <-lubridate::year(df$date)
    return(df)

  }else if((time.series == 'n') & (shape == 'w')){

    df <-data.frame(map_dfr(keys,fredr))
    df <-stats::reshape(df, idvar='date', timevar='series_id', direction='wide')
    key.rename <-append('date', key.rename)
    colnames(df) <-key.rename
    df <-df[order(as.Date(df[ , 1], format="%d/%m/%Y")),]
    rownames(df) <-df$date
    df$quarter <-paste(quarters(as.Date(df[ , 1])), format(df[ , 1], '%y'), sep=" ")
    #df$year <-lubridate::year(df$date)
    return(df)

  } else if((time.series == 'y') & (shape == 'w')){

    df <-data.frame(map_dfr(keys,fredr))
    df <-stats::reshape(df, idvar='date', timevar='series_id', direction='wide')
    key.rename <-append('date', key.rename)
    colnames(df) <-key.rename
    df <-df[order(as.Date(df[ , 1], format="%d/%m/%Y")),]
    rownames(df) <-df$date
    df <-xts::xts(x = df, order.by = df$date)
    df$date <-NULL
    return(df)

  }else{
    warning("Improper Inputs Entered")
  }
}
