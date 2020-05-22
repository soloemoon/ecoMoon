eco.barline.plot <-function(df,x,bar,line,bar.color,line.color,title,caption,subtitle,line.size,date.break,date.format,y.title,x.title,legend.pos, xaxis.text.size,yaxis.text.size,title.size,caption.size,legend.size, axis.title.size){
  x <- ifelse(missing(x),'date',x)
  bar.color <- ifelse(missing(bar.color),'#4682b4',bar.color)
  line.color <-ifelse(missing(line.color),'#66CD00',line.color)
  subtitle <- ifelse(missing(subtitle),'',subtitle)
  y.title <- ifelse(missing(y.title),'', y.title)
  x.title <- ifelse(missing(x.title),'', x.title)
  title <- ifelse(missing(title),'', title)
  caption <- ifelse(missing(caption),'', caption)
  legend.pos <- ifelse(missing(legend.pos),'top', legend.pos)
  date.break <- ifelse(missing(date.break),'2 year', date.break)
  date.format <- ifelse(missing(date.format),'%y', date.format)
  line.size <-ifelse(missing(line.size),.7, line.size)
  yaxis.text.size <-ifelse(missing(yaxis.text.size),15, yaxis.text.size)
  xaxis.text.size <-ifelse(missing(xaxis.text.size),15, xaxis.text.size)
  title.size <-ifelse(missing(title.size),20, title.size)
  caption.size <-ifelse(missing(caption.size),13, caption.size)
  legend.size <-ifelse(missing(legend.size),15, legend.size)
  axis.title.size <-ifelse(missing(axis.title.size), 15, axis.title.size)

  if(sum(df[['recession']]) !=0){

    start <- if(head(df[['recession']],1)== 1){
      df$date[head(df[['recession']],1)]
    }else{
      df$date[which(diff(df[['recession']])==1)]
    }

    end <-df$date[which(diff(df[['recession']])==-1)]

    if (length(end)>length(start)){
      end <-end[-1]
      recession.df <-data.frame(start = start, end = end)
      recession.df <- subset(recession.df, start>=min(df[['recession']]))
    } else {
      recession.df <-data.frame(start = start, end = end)
      recession.df <- subset(recession.df, start>=min(df[['recession']]))
    }

    ggplot()+
      geom_bar(aes(x=df[[x]], y=df[[bar]], color=bar.color),stat='identity')+
      geom_line(aes(x=df[[x]], y=df[[line]],color=line.color),size=line.size)+
      scale_color_manual(labels=c(bar, line),values = c(bar.color, line.color))+
      labs(title=title,caption=caption,subtitle = subtitle)+
      ylab(y.title)+xlab(x.title)+
      scale_x_date(date_breaks = date.break,labels = date_format(date.format))+
      geom_rect(data=recession.df,aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf),alpha=0.3,color='grey80')+
      theme_economist_white(gray_bg = FALSE)+
     theme(legend.position = legend.pos,
            legend.title = element_blank(),
            axis.text.x = element_text(size=xaxis.text.size),
            axis.text.y = element_text(size=yaxis.text.size),
            axis.title = element_text(size=axis.title.size),
            plot.caption = element_text(size=caption.size),
            legend.text = element_text(size=legend.size),
           plot.title = element_text(size= title.size))

  } else {
    ggplot()+
      geom_bar(aes(x=df[[x]], y=df[[bar]], color=bar.color),stat='identity')+
      geom_line(aes(x=df[[x]], y=df[[line]],color=line.color),size=line.size)+
      scale_color_manual(labels=c(bar, line),values = c(bar.color, line.color))+
      labs(title=title,caption=caption,subtitle = subtitle)+
      ylab(y.title)+xlab(x.title)+
      scale_x_date(date_breaks = date.break,labels = date_format(date.format))+
      theme_economist_white(gray_bg = FALSE)+
     theme(legend.position = legend.pos,
            legend.title = element_blank(),
            axis.text.x = element_text(size=xaxis.text.size),
            axis.text.y = element_text(size=yaxis.text.size),
            axis.title = element_text(size=axis.title.size),
            plot.caption = element_text(size=caption.size),
            legend.text = element_text(size=legend.size),
           plot.title = element_text(size= title.size))
  }
}
