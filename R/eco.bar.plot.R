eco.bar.plot <-function(df,x,y,fill,y.title,x.title,title,caption,labels,subtitle, legend.pos,bar.position,xaxis.text.size,yaxis.text.size,title.size,caption.size,legend.size){
  x <- ifelse(missing(x),'date',x)
  bar.position <- ifelse(missing(bar.position),'',bar.position)
  subtitle <- ifelse(missing(subtitle),'',subtitle)
  y.title <- ifelse(missing(y.title),'', y.title)
  x.title <- ifelse(missing(x.title),'', x.title)
  title <- ifelse(missing(title),'', title)
  caption <- ifelse(missing(caption),'', caption)
  legend.pos <- ifelse(missing(legend.pos),'top', legend.pos)
  fill <-ifelse(missing(fill),'series_id',fill)
  yaxis.text.size <-ifelse(missing(yaxis.text.size),15, yaxis.text.size)
  xaxis.text.size <-ifelse(missing(xaxis.text.size),15, xaxis.text.size)
  caption.size <-ifelse(missing(caption.size),13, caption.size)
  legend.size <-ifelse(missing(legend.size),15, legend.size)
  title.size <-ifelse(missing(title.size),20, title.size)

  ggplot(df,aes(x=df[[x]],y=df[[y]],fill=df[[fill]]))+
    geom_bar(stat='identity',position=bar.position)+
    labs(y=y.title,title=title,caption=caption, subtitle = subtitle,x=x.title)+
    theme_economist_white(gray_bg = FALSE)+
    scale_color_stata()+
    theme(legend.position = legend.pos,
          legend.title = element_blank(),
          axis.text.x = element_text(size=xaxis.text.size),
          axis.text.y = element_text(size=yaxis.text.size),
          axis.title = element_text(size=title.size),
          plot.caption = element_text(size=caption.size),
          legend.text = element_text(size=legend.size))
}
