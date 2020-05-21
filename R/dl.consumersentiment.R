dl.consumer.sentiment <-function(save.location, start.date){

download.file('http://www.sca.isr.umich.edu/files/tbmics.xls',save.location, method = 'auto', mode = 'wb',quiet=TRUE) 

consumer.sentiment <-read.xlsx(save.location,1, startRow = 4, colIndex = 1:3) %>% na.omit()
consumer.sentiment$date <-mdy(paste(consumer.sentiment$DATE.OF.SURVEY, '01',consumer.sentiment$NA.))
consumer.sentiment <-consumer.sentiment[ ,c('INDEX.OF.CONSUMER.SENTIMENT', 'date')]
row.names(consumer.sentiment) <-consumer.sentiment$date
colnames(consumer.sentiment) <-c('Consumer Sentiment', 'date')
consumer.sentiment <- subset(consumer.sentiment,consumer.sentiment$date >= start.date)
return(consumer.sentiment)
}
