
recession.probability <-function(start.date,show.plot){

  plot <-ifelse(missing(show.plot),'n','y')
  start.date <-ifelse(missing(start.date),'2000-01-01',start.date)

  if(show.plot == 'y'){

    RP <-fredr('T10Y3MM', observation_start = as.Date(start.date))
    RP$Prob <-pnorm(-0.5333 - 0.6330 * RP$value) * 100
    RP$Prob <- round(RP$Prob,2)


    recession.plot <-ggplot()+
      geom_line(aes(x=RP$date, y=RP$Prob,color='#4682b4'),size=.7)+
      labs(subtitle='Recession Probability Low but Rising', y='%',x='',title='Recession Probability',caption='Source: Treasury Model')+
      scale_color_manual(labels=c(RP$Prob),values=c('#4682b4'))+
      scale_x_date(date_breaks = '1 year',labels = date_format('%y'))+
      theme_economist_white(gray_bg = FALSE)+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(size=15),
            axis.text.y = element_text(size=15),
            axis.title = element_text(size=15),
            plot.caption = element_text(size=13),
            legend.text = element_text(size=15))
    return(recession.plot)


  }else{

    RP <-fredr('T10Y3MM', observation_start = as.Date(start.date))
    RP$Prob <-pnorm(-0.5333 - 0.6330 * RP$value) * 100
    RP$Prob <- round(RP$Prob,2)
    return(RP)

  }


}

