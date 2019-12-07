eco.plot3 <-function(df,x,y1,y2,y3,ycolor1,ycolor2,ycolor3,subtitle,y.title,x.title,title,caption,legend.pos,date.break, date.format,line.size,yaxis.text.size,xaxis.text.size,title.size,caption.size,legend.size){
  # Set Default Values - only df, x, y1,y2 are required
  x <- ifelse(missing(x),'date',x)
  subtitle <- ifelse(missing(subtitle),'',subtitle)
  y.title <- ifelse(missing(y.title),'', y.title)
  x.title <- ifelse(missing(x.title),'', x.title)
  title <- ifelse(missing(title),'', title)
  caption <- ifelse(missing(caption),'', caption)
  legend.pos <- ifelse(missing(legend.pos),'top', legend.pos)
  ycolor1 <- ifelse(missing(ycolor1),'#4682b4',ycolor1)
  ycolor2 <-ifelse(missing(ycolor2),'#66CD00',ycolor2)
  ycolor3 <-ifelse(missing(ycolor3),'#8A1F03',ycolor3)
  date.break <- ifelse(missing(date.break),'1 year', date.break)
  date.format <- ifelse(missing(date.format),'%y', date.format)
  line.size <-ifelse(missing(line.size),.7, line.size)
  yaxis.text.size <-ifelse(missing(yaxis.text.size),15, yaxis.text.size)
  xaxis.text.size <-ifelse(missing(xaxis.text.size),15, xaxis.text.size)
  caption.size <-ifelse(missing(caption.size),13, caption.size)
  legend.size <-ifelse(missing(legend.size),15, legend.size)
  title.size <-ifelse(missing(title.size),20, title.size)

  if(sum(df[['recession']]) != 0){

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
      geom_line(aes(x = df[[x]], y = df[[y1]], color = ycolor1),size=line.size)+
      geom_line(aes(x = df[[x]], y=df[[y2]],color = ycolor2),size =line.size)+
      geom_line(aes(x = df[[x]], y=df[[y3]],color = ycolor3),size =line.size)+
      labs(subtitle = subtitle, y = y.title,x = x.title,title = title,caption = caption)+
      scale_color_manual(values = c(ycolor1, ycolor2,ycolor3),labels = c(y1,y2,y3))+
      scale_x_date(date_breaks = date.break,labels = date_format(date.format))+
      geom_rect(data=recession.df,aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf),alpha=0.3,color='grey80')+
      theme_economist_white(gray_bg = FALSE)+
      theme(legend.position = legend.pos,
            legend.title = element_blank(),
            axis.text.x = element_text(size=xaxis.text.size),
            axis.text.y = element_text(size=yaxis.text.size),
            axis.title = element_text(size=title.size),
            plot.caption = element_text(size=caption.size),
            legend.text = element_text(size=legend.size))

  } else {
    ggplot()+
      geom_line(aes(x = df[[x]], y = df[[y1]], color = ycolor1),size=line.size)+
      geom_line(aes(x = df[[x]], y=df[[y2]],color = ycolor2),size =line.size)+
      geom_line(aes(x = df[[x]], y=df[[y3]],color = ycolor3),size =line.size)+
      labs(subtitle = subtitle, y = y.title,x = x.title,title = title,caption = caption)+
      scale_color_manual(values = c(ycolor1, ycolor2,ycolor3),labels = c(y1,y2,y3))+
      scale_x_date(date_breaks = date.break,labels = date_format(date.format))+
      theme_economist_white(gray_bg = FALSE)+
      theme(legend.position = legend.pos,
            legend.title = element_blank(),
            axis.text.x = element_text(size=xaxis.text.size),
            axis.text.y = element_text(size=yaxis.text.size),
            axis.title = element_text(size=title.size),
            plot.caption = element_text(size=caption.size),
            legend.text = element_text(size=legend.size))
  }
}
