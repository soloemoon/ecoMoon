eco.transform <-function(df, transformation, lags, index.date){

'%notin%' <-Negate('%in%')

if(transformation %notin% c('% change', 'difference', 'annualize monthly',' annualize quarterly','log', 'log difference')){
  warning('Invalid transformation option selected')
  return(print('Invalid Transformation selected'))
}

# Set defaults
lags <-ifelse(missing(lags),1,lags)
index <-ifelse(missing(index.date),0,index.date)

# Create transformation set
df.trans <-df

# Store column names. Used to rename dataframe columns post transformation
name.store <- names(df.trans)

# Check for year column and exclude from transformation dataset
if('year' %in% colnames(df.trans)){
  year <-df.trans$year
  df.trans <-within(df.trans, rm('year'))

} else if('Year' %in% colnames(df.trans)){
  year <-df.trans$Year
  df.trans <-within(df.trans, rm('Year'))
}

# Check for recession indicator and exclude from transformation dataset
if('recession' %in% colnames(df)){
  recession <-df$recession
  df.trans <-within(df.trans, rm('recession'))

} else if('Recession' %in% colnames(df)){
  Recession <-df$Recession
  df.trans <-within(df.trans,rm('Recession'))
}

# Check for Quarter and exclude from transformation dataset
if('quarter' %in% colnames(df)){
  quarter <-df$quarter
  df.trans <-within(df.trans, rm('quarter'))

} else if('Quarter' %in% colnames(df)){
  quarter <-df$Quarter
  df.trans <-within(df.trans,rm('Quarter'))
}

# Set date as Index
if('date' %in% colnames(df)){
  rownames(df) <- df$date
  df <-within(df, rm('date'))

} else if('Date' %in% colnames(df)){
  rownames(df) <- df$Date
  df <-within(df, rm('Date'))
}

# Determine class of object - dataframe or xts.
if(is.data.frame(df.trans) == TRUE){

# Non Annualized Growth Rates
  if(transformation == '% change'){

    # Apply function only to numeric columns
    df.trans <-plyr::colwise(function(x){
    if(is.numeric(x)){x / lag(x,lags) - 1
    }else{x}})(df.trans)

    if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

    if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

    if(exists('quarter') == TRUE){df.trans <-cbind(df.trans, quarter)}

    return(df.trans)

# Change
}else if(transformation == 'difference'){

    df.trans <-plyr::colwise(function(x){
    if(is.numeric(x)){x - lag(x,lags)
    }else{x}})(df.trans)

    if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

    if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

    if(exists('quarter') == TRUE){df.trans <-cbind(df.trans, quarter)}

    return(df.trans)

# Log Transformation
}else if(transformation == 'log'){

  df.trans <-data.frame(log(df.trans[ , unlist(lapply(df.trans, is.numeric))]))

  if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

  if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

  if(exists('quarter') == TRUE){df.trans <-cbind(df.trans, quarter)}

  names(df.trans) <-name.store

  return(df.trans)

# Monthly Annualized % Change
} else if(transformation == 'log difference'){

  df.trans <-data.frame(log(df.trans[ , unlist(lapply(df.trans, is.numeric))]))

  df.trans <-plyr::colwise(function(x){
    if(is.numeric(x)){x - lag(x,lags)
    }else{x}})(df.trans)

  if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

  if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

  if(exists('quarter') == TRUE){df.trans <-cbind(df.trans, quarter)}

  names(df.trans) <-name.store

  return(df.trans)

}else if(transformation == 'annualize monthly'){

  df.trans <-plyr::colwise(function(x){
  if(is.numeric(x)){((x/lag(x,lags))^12 - 1)
  }else{x}})(df.trans)

  if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

  if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

  if(exists('quarter') == TRUE){df.trans <-cbind(df.trans, quarter)}

  return(df.trans)

# Quarterly Annualized % Change
}else if(transformation == 'annualize quarterly'){

  df.trans <-plyr::colwise(function(x){
  if(is.numeric(x)){((x/lag(x,lags))^4 - 1)
  }else{x}})(df.trans)

  if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

  if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

  if(exists('quarter') == TRUE){df.trans <-cbind(df.trans, quarter)}

  return(df.trans)}

}else if(is.xts(df.trans) == TRUE){

  xts.dates <-index(df.trans)

  # Non Annualized Growth Rates
  if(transformation == '% change'){

  df.trans <-xts(vapply(df.trans, function(x){
  if(is.numeric(x)){x / lag(x, lags) - 1
  }else{x}}, FUN.VALUE = numeric(nrow(df.trans))), order.by = xts.dates )

  return(df.trans)

# Change
}else if(transformation == 'difference'){

  df.trans <-xts(vapply(df.trans, function(x){
  if(is.numeric(x)){x - lag(x,lags)
  }else{x}}, FUN.VALUE = numeric(nrow(df.trans))), order.by = xts.dates )

  return(df.trans)

# Log Transformation
}else if(transformation == 'log'){

  df.trans <-log(df.trans[ , unlist(lapply(df.trans, is.numeric))])

  df.trans <-xts(vapply(df.trans, function(x){
  if(is.numeric(x)){x - lag(x,lags)
  }else{x}}, FUN.VALUE = numeric(nrow(df.trans))), order.by = xts.dates )

  return(df.trans)

# Monthly Annualized % Change
} else if(transformation == 'annualize monthly'){

  df.trans <-xts(vapply(df.trans, function(x){
  if(is.numeric(x)){((x/lag(x,lags))^12 - 1)
  }else{x}}, FUN.VALUE = numeric(nrow(df.trans))), order.by = xts.dates )

  return(df.trans)

    # Quarterly Annualized % Change
}else if(transformation == 'annualize quarterly'){

    df.trans <-xts(vapply(df.trans, function(x){
    if(is.numeric(x)){((x/lag(x,lags))^4 - 1)
    }else{x}}, FUN.VALUE = numeric(nrow(df.trans))), order.by = xts.dates )

    return(df.trans)
    }
  }
}
