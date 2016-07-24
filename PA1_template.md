Course Project - Activity Monitoring data
================

Loading and preprocessing the data
----------------------------------

Read the "activity" datapack

``` r
activity <- read.csv("activity.csv",header=TRUE)
```

Format second columns as Date

``` r
factortocharacter <- as.character(activity[,2])
activity[,2] <- as.Date(factortocharacter,format="%Y-%m-%d")
```

What is mean total number of steps taken per day?
-------------------------------------------------

Table of the total number of steps taken per day

``` r
totalsteps <- aggregate(activity$steps, by=list(Date=activity$date), FUN=sum)
```

Histogram of the total number of steps taken each day

``` r
hist(totalsteps$x, breaks = 6, main = "Frequency of number of steps per day", 
    xlab = "Number of steps per day", ylab = "Frequency", col = "red")
```

![](markdown_files/figure-markdown_github/histogram%20of%20total%20steps-1.png)

``` r
mean(totalsteps$x,na.rm=TRUE)
```

    ## [1] 10766.19

``` r
median(totalsteps$x,na.rm=TRUE)
```

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

``` r
averagestepstime <- aggregate(activity$steps,by=list(Interval=activity$interval),FUN=mean,na.rm=TRUE)
plot(averagestepstime,type="l",xlab="Time",ylab="Average Number of steps")
```

![](markdown_files/figure-markdown_github/average%20steps%20by%20interval-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
maxsteps <- max(averagestepstime$x)
averagestepstime$Interval[averagestepstime$x==maxsteps]
```

    ## [1] 835

The interval from 8:35 to 8:40

Imputing missing values
-----------------------

Number of NA value in activity datafram

``` r
sum(is.na(activity$steps))
```

    ## [1] 2304

Devise a strategy for filling in all of the missing values in the dataset

*I will fill the NA with average value for that 5-min interval*

Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
imputed <- activity

for (i in 1:nrow(imputed)) {
    if(is.na(imputed$steps[i])){
        imputed$steps[i] <- averagestepstime$x[which(imputed$interval[i] == averagestepstime$Interval)]}
}
```

Make a histogram of the total number of steps taken each day

``` r
totalstepsimputed <- aggregate(imputed$steps, by=list(Date=imputed$date), FUN=sum)
hist(totalstepsimputed$x, breaks = 6, main = "Frequency of number of steps per day", xlab = "Number of steps per day", ylab = "Frequency", col = "red")
```

![](markdown_files/figure-markdown_github/new%20histogram%20-1.png)

Calculate and report the mean

``` r
mean(totalstepsimputed$x)
```

    ## [1] 10766.19

Calculate and report the median

``` r
median(totalstepsimputed$x)
```

    ## [1] 10766.19

Do these values differ from the estimates from the first part of the assignment?

``` r
mean(totalstepsimputed$x)-mean(totalsteps$x,na.rm=TRUE)
```

    ## [1] 0

Mean does not change between the two datasets

``` r
median(totalstepsimputed$x)-median(totalsteps$x,na.rm=TRUE)
```

    ## [1] 1.188679

Median change between the two different datasets

``` r
sum(totalstepsimputed$x) - sum(totalsteps$x,na.rm=TRUE)
```

    ## [1] 86129.51

The new datasets take 86K+ more steps into account
