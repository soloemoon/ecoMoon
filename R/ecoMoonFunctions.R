extrafont::loadfonts(device="win")
if (!require("fredr")) {install.packages("fredr"); library("fredr")}
if (!require("purrr")) {install.packages("purrr"); library("purrr")}
if (!require("reshape2")) {install.packages("reshape2"); library("reshape2")}
if (!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if (!require("lubridate")) {install.packages("lubridate"); library("lubridate")}
if (!require("ggplot2")) {install.packages("ggplot2"); library("ggplot2")}
if (!require("scales")) {install.packages("scales"); library("scales")}
if (!require("tis")) {install.packages("tis"); library("tis")}
if (!require("gridExtra")) {install.packages("gridExtra"); library("gridExtra")}
if (!require("grid")) {install.packages("grid"); library("grid")}
if (!require("formattable")) {install.packages("formattable"); library("formattable")}
if (!require("ggtheme")) {install.packages("ggtheme"); library("ggtheme")}
if (!require("kable")) {install.packages("kable"); library("kable")}
# add bloomberg, quandl pulls
fred.api.key <-function(key){fredr_set_key(key)}

eco.monthly.w <- function(fred.keys,col.rename){
  FredrNames <-append('USRECM', fred.keys)
  df <-data.frame(map_dfr(fred.keys,fredr))
  df <-reshape(df, idvar='date', timevar='series_id', direction='wide')
  col.rename <-append('recession', col.rename)
  col.rename <-append('date', col.rename)
  colnames(df) <-col.rename
  df <-df[order(as.Date(df[,1], format="%d/%m/%Y")),]
  rownames(df) <-df$date
  df$quarter <-paste(quarters(as.Date(df[,1])), format(df[,1], '%y'), sep=" ")
  return(df)
}

eco.monthly.l <- function(fred.keys,col.rename){
  FredrNames <-append('USRECM', fred.keys)
  df <-data.frame(map_dfr(fred.keys,fredr))
  df[df$series_id=='USRECM'] <-'recession'
  df <-df[order(series_id,as.Date(df[,1], format="%d/%m/%Y")),]
  df$quarter <-paste(quarters(as.Date(df[,1])), format(df[,1], '%y'), sep=" ")
  return(df)
}

eco.quarterly.w <-function(fred.keys,col.rename){
  fred.keys <-append('USRECQM', fred.keys)
  df <-data.frame(map_dfr(fred.keys,fredr))
  df <-reshape(df, idvar='date', timevar='series_id', direction='wide')
  col.rename <-append('recession', col.rename)
  col.rename <-append('date', col.rename)
  colnames(df) <-col.rename
  df <-df[order(as.Date(df[,1], format="%d/%m/%Y")),]
  rownames(df) <-df$date
  df$Quarter <-paste(quarters(as.Date(df[,1])), format(df[,1], '%y'), sep=" ")
  return(df)
}

eco.quarterly.l <-function(fred.keys,col.rename){
  fred.keys <-append('USRECQM', fred.keys)
  df <-data.frame(map_dfr(fred.keys,fredr))
  df[df$series_id=='USRECM'] <-'recession'
  df$Quarter <-paste(quarters(as.Date(df[,1])), format(df[,1], '%y'), sep=" ")
  return(df)
}

eco.plot <-function(df,x,y,sub,y.title,x.title,title,caption,date.break,color,date.format,line.size, yaxis.text.size,xaxis.text.size,title.size,caption.size,legend.size,legend.pos){
  # Set Default Values
  x <- ifelse(is.null(x),'date',x)
  color <- ifelse(is.null(color),'#4682b4',color)
  subtitle <- ifelse(is.null(subtitle),'',subtitle)
  y.title <- ifelse(is.null(y.title),'', y.title)
  x.title <- ifelse(is.null(x.title),'', x.title)
  title <- ifelse(is.null(title),'', title)
  caption <- ifelse(is.null(caption),'', caption)
  legend.pos <- ifelse(is.null(legend.pos),'top', legend.pos)
  date.break <- ifelse(is.null(date.break),'1 year', date.break)
  date.format <- ifelse(is.null(date.format),'%y', date.format)
  line.size <-ifelse(is.null(line.size),.7, line.size)
  yaxis.text.size <-ifelse(is.null(yaxis.text.size),15, yaxis.text.size)
  xaxis.text.size <-ifelse(is.null(xaxis.text.size),15, xaxis.text.size)
  title.size <-ifelse(is.null(title.size),20, title.size)
  caption.size <-ifelse(is.null(caption.size),13, caption.size)
  legend.size <-ifelse(is.null(legend.size),15, legend.size)

  start <-df$date[which(diff(df[['recession']])==1)]
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
    geom_line(aes(x=df[[x]], y=df[[y]],color=color),size=line.size)+
    labs(subtitle=subtitle, y=y.title,x=x.title,title=title,caption=caption)+
    scale_color_manual(labels=c(y),values=c(color))+
    scale_x_date(date_breaks = date.break,labels = date_format(date.format))+
    geom_rect(data=recession.df,aes(xmin=start,xmax=end, ymin=-Inf,ymax=+Inf),alpha=.3,color='grey80')+
    theme_economist_white(gray_bg = FALSE)+
    theme(legend.position = legend.pos,
          legend.title = element_blank(),
          axis.text.x = element_text(size=xaxis.text.size),
          axis.text.y = element_text(size=yaxis.text.size),
          axis.title = element_text(size=title.size),
          plot.caption = element_text(size=caption.size),
          legend.text = element_text(size=legend.size))
}

eco.plot2 <-function(df,x,y1,y2,ycolor1,ycolor2,subtitle,y.title,x.title,title,caption,legend.pos,date.break, date.format,line.size,yaxis.text.size,xaxis.text.size,title.size,caption.size,legend.size ){
  # Set Default Values - only df, x, y1,y2 are required
  x <- ifelse(is.null(x),'date',x)
  subtitle <- ifelse(is.null(subtitle),'',subtitle)
  y.title <- ifelse(is.null(y.title),'', y.title)
  x.title <- ifelse(is.null(x.title),'', x.title)
  title <- ifelse(is.null(title),'', title)
  caption <- ifelse(is.null(caption),'', caption)
  legend.pos <- ifelse(is.null(legend.pos),'top', legend.pos)
  y1color <- ifelse(is.null(y1color),'#4682b4',y1color)
  y2color <-ifelse(is.null(y2color),'#66CD00',y2color)
  date.break <- ifelse(is.null(date.break),'1 year', date.break)
  date.format <- ifelse(is.null(date.format),'%y', date.format)
  line.size <-ifelse(is.null(line.size),.7, line.size)
  yaxis.text.size <-ifelse(is.null(yaxis.text.size),15, yaxis.text.size)
  xaxis.text.size <-ifelse(is.null(xaxis.text.size),15, xaxis.text.size)
  caption.size <-ifelse(is.null(caption.size),13, caption.size)
  legend.size <-ifelse(is.null(legend.size),15, legend.size)

  start <-df$date[which(diff(df[['recession']])==1)]
  end <-df$date[which(diff(df[['recession']])==-1)]

  if (length(end)>length(start)){
    end <-end[-1]
    recession.df <-data.frame(start = start, end = end)
    recession.df <- subset(recession.df, start>=min(df[['recession']]))
  } else {
    recession.df <-data.frame(start = start, end = end)
    recession.df <- subset(recession.df, start>=min(df[['recession']]))
  }

  ggplot()+geom_line(aes(x = df[[x]], y = df[[y1]], color = ycolor1),size=line.size)+
    geom_line(aes(x = df[[x]], y=df[[y2]],color = ycolor2),size =line.size)+
    labs(subtitle = subtitle, y = y.title,x = x.title,title = title,caption = caption)+
    scale_color_manual(values = c(ycolor1, ycolor2),labels = c(y1,y2))+
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

}

eco.plot3 <-function(df,x,y1,y2,y3,ycolor1,ycolor2,ycolor3,subtitle,y.title,x.title,title,caption,legend.pos,date.break, date.format,line.size,yaxis.text.size,xaxis.text.size,title.size,caption.size,legend.size ){
  # Set Default Values - only df, x, y1,y2 are required
  x <- ifelse(is.null(x),'date',x)
  subtitle <- ifelse(is.null(subtitle),'',subtitle)
  y.title <- ifelse(is.null(y.title),'', y.title)
  x.title <- ifelse(is.null(x.title),'', x.title)
  title <- ifelse(is.null(title),'', title)
  caption <- ifelse(is.null(caption),'', caption)
  legend.pos <- ifelse(is.null(legend.pos),'top', legend.pos)
  y1color <- ifelse(is.null(y1color),'#4682b4',y1color)
  y2color <-ifelse(is.null(y2color),'#66CD00',y2color)
  y3color <-ifelse(is.null(y3color),'#8A1F03',y3color)
  date.break <- ifelse(is.null(date.break),'1 year', date.break)
  date.format <- ifelse(is.null(date.format),'%y', date.format)
  line.size <-ifelse(is.null(line.size),.7, line.size)
  yaxis.text.size <-ifelse(is.null(yaxis.text.size),15, yaxis.text.size)
  xaxis.text.size <-ifelse(is.null(xaxis.text.size),15, xaxis.text.size)
  caption.size <-ifelse(is.null(caption.size),13, caption.size)
  legend.size <-ifelse(is.null(legend.size),15, legend.size)

  start <-df$date[which(diff(df[['recession']])==1)]
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

}

eco.barline.plot <-function(df,x,bar,line,bar.color,line.color,title,caption,subtitle,line.size,date.break,date.format,y.title,x.title,legend.pos, xaxis.text.size,yaxis.text.size,title.size,caption.size,legend.size){
  x <- ifelse(is.null(x),'date',x)
  bar.color <- ifelse(is.null(bar.color),'#4682b4',bar.color)
  line.color <-ifelse(is.null(line.color),'#66CD00',line.color)
  subtitle <- ifelse(is.null(subtitle),'',subtitle)
  y.title <- ifelse(is.null(y.title),'', y.title)
  x.title <- ifelse(is.null(x.title),'', x.title)
  title <- ifelse(is.null(title),'', title)
  caption <- ifelse(is.null(caption),'', caption)
  legend.pos <- ifelse(is.null(legend.pos),'top', legend.pos)
  date.break <- ifelse(is.null(date.break),'2 year', date.break)
  date.format <- ifelse(is.null(date.format),'%y', date.format)
  line.size <-ifelse(is.null(line.size),.7, line.size)
  yaxis.text.size <-ifelse(is.null(yaxis.text.size),15, yaxis.text.size)
  xaxis.text.size <-ifelse(is.null(xaxis.text.size),15, xaxis.text.size)
  title.size <-ifelse(is.null(title.size),20, title.size)
  caption.size <-ifelse(is.null(caption.size),13, caption.size)
  legend.size <-ifelse(is.null(legend.size),15, legend.size)

  start <-df$date[which(diff(df[['recession']])==1)]
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
    geom_line(aes(x=x=df[[x]], y=df[[line]],color=line.color),size=line.size)+
    scale_color_manual(values=c(bar, line),labels = c(bar.color, line.color))+
    labs(title=title,caption=caption,subtitle = subtitle)+
    ylab(y.title)+xlab(x.title)+
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

}

eco.bar.plot <-function(df,x,y,fill,yTitle,title,caption,labels, bar.position, subtitle, legend.pos,bar.position,xaxis.text.size,yaxis.text.size,title.size,caption.size,legend.size){
  x <- ifelse(is.null(x),'date',x)
  bar.position <- ifelse(is.null(bar.position),'dodge')
  subtitle <- ifelse(is.null(subtitle),'',subtitle)
  y.title <- ifelse(is.null(y.title),'', y.title)
  x.title <- ifelse(is.null(x.title),'', x.title)
  title <- ifelse(is.null(title),'', title)
  caption <- ifelse(is.null(caption),'', caption)
  legend.pos <- ifelse(is.null(legend.pos),'top', legend.pos)
  fill <-ifelse(is.null(fill),'series_id',fill)
  date.break <- ifelse(is.null(date.break),'1 year', date.break)
  date.format <- ifelse(is.null(date.format),'%y', date.format)
  line.size <-ifelse(is.null(line.size),.7, line.size)
  yaxis.text.size <-ifelse(is.null(yaxis.text.size),15, yaxis.text.size)
  xaxis.text.size <-ifelse(is.null(xaxis.text.size),15, xaxis.text.size)
  caption.size <-ifelse(is.null(caption.size),13, caption.size)
  legend.size <-ifelse(is.null(legend.size),15, legend.size)

  ggplot(df,aes(x=df[[x]],y=df[[y]],fill=df[[fill]]))+
    geom_bar(stat='identity',position=bar.position)+
    labs(y=y.title,title=title,caption=caption, subtitle = subtitle)+
    scale_fill_economist(name='',labels=labels)+
    theme_economist_white(gray_bg = FALSE)+
    theme(legend.position = legend.pos,
      legend.title = element_blank(),
          axis.text.x = element_text(size=xaxis.text.size),
          axis.text.y = element_text(size=yaxis.text.size),
          axis.title = element_text(size=title.size),
          plot.caption = element_text(size=caption.size),
          legend.text = element_text(size=legend.size))
  }

eco.table(df,align,header.color,header.text.color,row.label.pos,font.size){

  col.spec <-length(names(df))
  font.size <-ifelse(is.null(font.size),12,font.size)
  header.color <-ifelse(is.null(header.color),'#003D73',header.color
                        )
  align <-ifelse(is.null(align),'c',align)

  row.label.pos <-ifelse(is.null(row.label.pos),'c',row.label.pos)

  df %>% kable(escape=F, align = align)%>%
    column_spec(1,border_left = T,extra_css = "border-left:2px solid lightgrey;")%>%
    column_spec(col.spec,border_right = T,extra_css = "border-right:2px solid lightgrey;") %>%
    row_spec(0,bold = TRUE, background = '#003D73',color = '#ffffff') %>%
    kable_styling(row_label_position = row.label.pos,font_size = font.size)
}

