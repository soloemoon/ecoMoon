eco.download <- function(keys, key.rename, shape, periodicity, show.recession, time.series){

# Set default values
periodicity <-ifelse(missing(periodicity),'monthly',periodicity)
shape <- ifelse(missing(shape),'w',shape)
show.recession <-ifelse(missing(show.recession),'y',show.recession)
time.series <-ifelse(missing(time.series),'n',time.series)

# Ensure function inputs have proper classes
if(class(keys) != 'character'){
  warning('keys entered in improper class')
  return(print('please enter keys as character'))
}

if(class(key.rename) != 'character'){
    warning('key rename entered in improper class')
    return(print('please enter key.rename as character'))
  }

if(class(shape) != 'character'){
    warning('Shape entered in improper class')
    return(print('please enter shape as character'))
  }

if(class(show.recession) != 'character'){
    warning('show.recession entered in improper class')
    return(print('please enter show.recession as character'))
  }

if(class(time.series) != 'character'){
    warning('time.series entered in improper class')
    return(print('please enter time.series as character'))
  }

# Check periodicity
if(class(periodicity) != 'character'){

  warning('Periodicty entered with improper class')
  return(print('Please enter periodicity as a character'))

}else{

    if((periodicity =='monthly')&(show.recession == 'y')){
      keys <-append('USRECM', keys)
      key.rename <- append('recession',key.rename)

}else if((periodicity == 'quarterly')&(show.recession == 'y')){

      keys <-append('USRECQ', keys)
      key.rename <- append('recession',key.rename)

}else if((periodicity == 'daily')&(show.recession == 'y')){

      keys <-append('USRECD', keys)
      key.rename <- append('recession',key.rename)

}else if(show.recession == 'n'){
      keys <- keys
      key.rename <- key.rename
  }
}

# Download format
if((time.series == 'n') & (shape == 'l')){
  # Shape dataframe
    df <-data.frame(map_dfr(keys,fredr))
    df <-df[order(df$series_id,as.Date(df[ , 1], format="%d/%m/%Y")),]
    df$quarter <-paste(quarters(as.Date(df[ , 1])), format(df[ , 1], '%y'), sep=" ")
    df$year <-lubridate::year(df$date)
    return(df)

}else if((time.series == 'n') & (shape == 'w')){

    df <-data.frame(map_dfr(keys,fredr))
    df <-stats::reshape(df, idvar='date', timevar='series_id', direction='wide')
    key.rename <-append('date', key.rename)
    colnames(df) <-key.rename
    df <-df[order(as.Date(df[ , 1], format="%d/%m/%Y")),]
    rownames(df) <-df$date
    df$quarter <-paste(quarters(as.Date(df[ , 1])), format(df[ , 1], '%y'), sep=" ")
    df$year <-lubridate::year(df$date)
    rownames(df) <- df$date
    #df <-within(df, rm('date'))
    return(df)

} else if((time.series == 'y')){

    df <-data.frame(map_dfr(keys,fredr))
    df <-stats::reshape(df, idvar='date', timevar='series_id', direction='wide')
    key.rename <-append('date', key.rename)
    colnames(df) <-key.rename
    df <-df[order(as.Date(df[ , 1], format="%d/%m/%Y")),]
    rownames(df) <-df$date
    df$series_id <-NULL
    df <-xts::xts(x = df, order.by = df$date)
    df$date <-NULL
    storage.mode(df) <-'numeric'
    return(df)

}else{
    warning("Improper Inputs Entered")
  }
}
