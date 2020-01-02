eco.transform <-function(df, transformation, lags, index.date){

'%notin%' <-Negate('%in%')

if(transformation %notin% c('% change', 'difference', 'annualize monthly',' annualize quarterly','log')){
  warning('Invalid transformation option selected')
  return(print('Invalid Transformation selected'))
}

# Set defaults
lags <-ifelse(missing(lags),1,lags)
index <-ifelse(missing(index.date),0,index.date)

# Create transformation set
df.trans <-df

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

    return(df.trans)

# Change
}else if(transformation == 'difference'){

    df.trans <-plyr::colwise(function(x){
    if(is.numeric(x)){x - lag(x,lags)
    }else{x}})(df.trans)

    if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

    if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

    return(df.trans)

# Log Transformation
}else if(transformation == 'log'){

  df.trans <-log(df.trans[ , unlist(lapply(df.trans, is.numeric))])

  df.trans <-plyr::colwise(function(x){
  if(is.numeric(x)){x - lag(x,lags)
  }else{x}})(df.trans)

  if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

  if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

  return(df.trans)

# Monthly Annualized % Change
} else if(transformation == 'annualize monthly'){

  df.trans <-plyr::colwise(function(x){
  if(is.numeric(x)){((x/lag(x,lags))^12 - 1)
  }else{x}})(df.trans)

  if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

  if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

  return(df.trans)

# Quarterly Annualized % Change
}else if(transformation == 'annualize quarterly'){

  df.trans <-plyr::colwise(function(x){
  if(is.numeric(x)){((x/lag(x,lags))^4 - 1)
  }else{x}})(df.trans)

  if(exists('year') == TRUE){df.trans <-cbind(df.trans, year)}

  if(exists('recession') == TRUE){df.trans <-cbind(df.trans, recession)}

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
