recession.probability <-function(start.date,show.plot){
  
  plot <-ifelse(missing(show.plot),'n','y')
  start.date <-ifelse(missing(start.date),'2000-01-01',start.date)
  
  if(show.plot == 'y'){
    
    RP <-eco.download('T10Y3MM','Prob') %>% na.omit() 
    RP$date <-as.Date(index(RP))
    RP <-subset(RP$date >= as.Date(start.date))
    RP$Prob <-pnorm(-0.5333 - 0.6330 * RP$Prob) * 100
    RP$Prob <- round(RP$Prob,2)
    
    if(sum(RP[['recession']]) !=0){
      
      start <- if(head(RP[['recession']],1)== 1){
        RP$date[head(RP[['recession']],1)]
      }else{
        RP$date[which(diff(RP[['recession']])==1)]
      }
      
      end <-RP$date[which(diff(RP[['recession']])==-1)]
      
      if (length(end)>length(start)){
        end <-end[-1]
        recession.df <-data.frame(start = start, end = end)
        recession.df <- subset(recession.df, start>=min(RP[['recession']]))
      } else {
        recession.df <-data.frame(start = start, end = end)
        recession.df <- subset(recession.df, start>=min(RP[['recession']]))
      } 
      
      recession.plot <-ggplot()+
        geom_line(aes(x=RP$date, y=RP$Prob,colour = '#014d64'),size=.7)+
        labs(subtitle='Based on 3 Month Treasury Spread', y='%',x='',title='Recession Probability',caption='Source: Treasury Model')+
        scale_x_date(date_breaks = '1 year',labels = date_format('%y'))+ 
        scale_color_manual(values = c('#014d64'), labels = c('12M Recession Probability')) + 
        geom_rect(data=recession.df,aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf),alpha=.3,color='grey80')+
        theme_economist_white(gray_bg = FALSE)+
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title = element_text(size=15),
              plot.caption = element_text(size=13),
              legend.text = element_text(size = 15))
      
      
      return(recession.plot)
      
    }else {
      
      recession.plot <-ggplot()+
        geom_line(aes(x=RP$date, y=RP$Prob, colour = '#014d64'),size=.7)+
        labs(subtitle='Based on 3 Month Treasury Spread', y='%',x='',title='Recession Probability',caption='Source: Treasury Model')+
        scale_color_manual(values = c('#014d64'), labels = c('12M Recession Probability')) +    
        scale_x_date(date_breaks = '1 year',labels = date_format('%y'))+
        theme_economist_white(gray_bg = FALSE)+
        theme(legend.title = element_blank(),
              axis.text.x = element_text(size=15),
              axis.text.y = element_text(size=15),
              axis.title = element_text(size=15),
              plot.caption = element_text(size=13),
              legend.text = element_text(size = 15))
      return(recession.plot)
      
    }
    
  }else{
    
    RP <-eco.download('T10Y3MM','Prob') %>% na.omit() %>% subset(date >= as.Date(start.date))
    RP$Prob <-pnorm(-0.5333 - 0.6330 * RP$Prob) * 100
    RP$Prob <- round(RP$Prob,2)
    return(RP)
    
  }
}
