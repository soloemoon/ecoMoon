eco.transform <-function(df, transformation, lags, index.date){

  lags <-ifelse(missing(lags),1,lags)
  index <-ifelse(missing(index.date),0,index.date)

  df.trans <-df

  # Remove recession indicator
  if('recession' %in% colnames(df)){
  df.trans <-within(df, rm('recession'))
  }

# Determine class of object - dataframe or xts.
if(is.data.frame(df.trans) == 'TRUE'){

# Non Annualized Growth Rates
if(transformation == '% change'){

  # Apply function only to numeric columns
  df.trans <-plyr::colwise(function(x){
    if(is.numeric(x)){x / lag(x,lags) - 1
    }else{x}})(df.trans)

  return(df.trans)

# Change
}else if(transformation == 'difference'){

  df.trans <-plyr::colwise(function(x){
    if(is.numeric(x)){x - lag(x,lags)
    }else{x}})(df.trans)

  return(df.trans)

# Log Transformation
  }else if(transformation == 'log'){

      df.trans <-log(df.trans[ , unlist(lapply(df.trans, is.numeric))])

      df.trans <-plyr::colwise(function(x){
      if(is.numeric(x)){x - lag(x,lags)
      }else{x}})(df.trans)

    return(df.trans)

# Monthly Annualized % Change
  } else if(transformation == 'annualize monthly'){

  df.trans <-plyr::colwise(function(x){
    if(is.numeric(x)){((x/lag(x,lags))^12 - 1)
    }else{x}})(df.trans)

  return(df.trans)

# Quarterly Annualized % Change
  }else if(transformation == 'annualize quarterly'){

    df.trans <-plyr::colwise(function(x){
      if(is.numeric(x)){((x/lag(x,lags))^4 - 1)
      }else{x}})(df.trans)

    return(df.trans)

  }
}else if(is.xts(df.trans) == 'TRUE'){

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

