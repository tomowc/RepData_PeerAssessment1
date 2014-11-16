library(ggplot2)
library(plyr)

Sys.setlocale("LC_TIME", "English")
?unzip
unzip("./activity.zip")
df <- read.csv("./activity.csv", stringsAsFactors = FALSE)
df[,2] <- as.Date(df[,2])


hist(df$steps, breaks=100)

mean(df$steps, na.rm=T)
median(df$steps, na.rm=T)


interval_avg <- tapply(df$steps, df$interval, function(x) mean(x, na.rm=T))
plot(unique(df$interval), interval_avg, type="l")


df$interval[which(interval_avg==max(interval_avg))]


sum(!complete.cases(df))
sum(is.na(df$steps))
sum(is.na(df$intervals))

missing_case_ind <- which(!complete.cases(df))


interval_avg_df <- data.frame(interval = names(interval_avg), avg = interval_avg)

x <- merge(df[missing_case_ind,],interval_avg_df)
x <- x[order(x$date),]


df_inp <- df
df_inp$steps[missing_case_ind] <- x$avg

mean(df_inp$steps, na.rm=T)
median(df_inp$steps, na.rm=T)

sum(df$steps, na.rm=T)
sum(df_inp$steps, na.rm=T)

x <- rep("weekday", dim(df_inp)[1])
x[weekdays(df_inp$date) == "Saturday" | weekdays(df_inp$date) == "Sunday"] <- "weekend"

df_inp$day_type <- factor(x)

df_inp_avg <- ddply(df_inp, .(interval, day_type), function(x) mean(x[,1], na.rm=T))
names(df_inp_avg)[3] <- "steps"
p1 <- ggplot(df_inp_avg, aes(interval, steps))
p1 + geom_line() + facet_grid(day_type ~ .)
