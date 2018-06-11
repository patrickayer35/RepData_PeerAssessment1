Loading and preprocessing the data
----------------------------------

Question 1:

    adata <- read.csv("activity.csv")
    head(adata, n = 30)

    ##    steps       date interval
    ## 1     NA 2012-10-01        0
    ## 2     NA 2012-10-01        5
    ## 3     NA 2012-10-01       10
    ## 4     NA 2012-10-01       15
    ## 5     NA 2012-10-01       20
    ## 6     NA 2012-10-01       25
    ## 7     NA 2012-10-01       30
    ## 8     NA 2012-10-01       35
    ## 9     NA 2012-10-01       40
    ## 10    NA 2012-10-01       45
    ## 11    NA 2012-10-01       50
    ## 12    NA 2012-10-01       55
    ## 13    NA 2012-10-01      100
    ## 14    NA 2012-10-01      105
    ## 15    NA 2012-10-01      110
    ## 16    NA 2012-10-01      115
    ## 17    NA 2012-10-01      120
    ## 18    NA 2012-10-01      125
    ## 19    NA 2012-10-01      130
    ## 20    NA 2012-10-01      135
    ## 21    NA 2012-10-01      140
    ## 22    NA 2012-10-01      145
    ## 23    NA 2012-10-01      150
    ## 24    NA 2012-10-01      155
    ## 25    NA 2012-10-01      200
    ## 26    NA 2012-10-01      205
    ## 27    NA 2012-10-01      210
    ## 28    NA 2012-10-01      215
    ## 29    NA 2012-10-01      220
    ## 30    NA 2012-10-01      225

Question 2: - remove the rows with missing values, as they won't be
needed - create new dataset of complete rows, stored in `ccdata` but
keep original dataset

    ccdata <- adata[complete.cases(adata), ]
    head(ccdata, n = 30)

    ##     steps       date interval
    ## 289     0 2012-10-02        0
    ## 290     0 2012-10-02        5
    ## 291     0 2012-10-02       10
    ## 292     0 2012-10-02       15
    ## 293     0 2012-10-02       20
    ## 294     0 2012-10-02       25
    ## 295     0 2012-10-02       30
    ## 296     0 2012-10-02       35
    ## 297     0 2012-10-02       40
    ## 298     0 2012-10-02       45
    ## 299     0 2012-10-02       50
    ## 300     0 2012-10-02       55
    ## 301     0 2012-10-02      100
    ## 302     0 2012-10-02      105
    ## 303     0 2012-10-02      110
    ## 304     0 2012-10-02      115
    ## 305     0 2012-10-02      120
    ## 306     0 2012-10-02      125
    ## 307     0 2012-10-02      130
    ## 308     0 2012-10-02      135
    ## 309     0 2012-10-02      140
    ## 310     0 2012-10-02      145
    ## 311     0 2012-10-02      150
    ## 312     0 2012-10-02      155
    ## 313     0 2012-10-02      200
    ## 314     0 2012-10-02      205
    ## 315     0 2012-10-02      210
    ## 316     0 2012-10-02      215
    ## 317     0 2012-10-02      220
    ## 318     0 2012-10-02      225

What is mean total number of steps taken per day?
-------------------------------------------------

Question 1:

    dates <- unique(as.factor(ccdata$date))
    l <- length(dates)
    total_steps <- vector("numeric", length = l)
    for (i in 1:l)
    {
        ddata <- ccdata[ccdata$date == as.character(dates[i]), ]
        total_steps[i] <- sum(ddata$steps)
    }
    head(total_steps, n = 30)

    ##  [1]   126 11352 12116 13294 15420 11015 12811  9900 10304 17382 12426
    ## [12] 15098 10139 15084 13452 10056 11829 10395  8821 13460  8918  8355
    ## [23]  2492  6778 10119 11458  5018  9819 15414 10600

    hist(total_steps, breaks = l, xlab = "steps", main = "Steps per day")

![](PA1_template_files/figure-markdown_strict/histogram%201-1.png)
Question 2: mean and median total steps taken per day:

    print(paste("Mean is ", mean(total_steps)), sep = "")

    ## [1] "Mean is  10766.1886792453"

    print(paste("Median is ", median(total_steps)), sep = "")

    ## [1] "Median is  10765"

What is the average daily activity pattern?
-------------------------------------------

Question 1:

    intervals <- unique(as.factor(ccdata$interval))
    averages <- vector("numeric", length = length(intervals))
    max_avg <- 0
    max_interval <- 0
    for (i in 1:length(intervals))
    {
        idata <- ccdata[ccdata$interval == as.character(intervals[i]), ]
        averages[i] <- mean(idata$steps)
        if (averages[i] > max_avg)
        {
            max_avg <- averages[i]
            max_interval <- intervals[i]
        }
    }
    head(averages, n = 20)

    ##  [1] 1.7169811 0.3396226 0.1320755 0.1509434 0.0754717 2.0943396 0.5283019
    ##  [8] 0.8679245 0.0000000 1.4716981 0.3018868 0.1320755 0.3207547 0.6792453
    ## [15] 0.1509434 0.3396226 0.0000000 1.1132075 1.8301887 0.1698113

    plot(intervals, averages, type = "l")

![](PA1_template_files/figure-markdown_strict/daily%20activity%20pattern-1.png)

    max_avg

    ## [1] 206.1698

    as.integer(as.character(max_interval))

    ## [1] 835

Question 2: the 5-minute interval with the maximum number of steps, on
average, is the interval 835, with a value of 206.1698113

Imputing missing values
-----------------------

Question 1: the total number of missing values is 2304 Question 2 & 3:
the strategy I will use in filling in the missing data is by replacing
all NA values with the means for that time interval

    for (n in 1:nrow(adata))
    {
        if (is.na(adata[n, 1]))
        {
            this_i <- adata[n, 3]
            for (i in 1:length(intervals))
            {
                if (intervals[i] == this_i)
                {
                    adata[n, 1] <- averages[i]
                }
            }
        }
    }
    head(adata, n = 30)

    ##        steps       date interval
    ## 1  1.7169811 2012-10-01        0
    ## 2  0.3396226 2012-10-01        5
    ## 3  0.1320755 2012-10-01       10
    ## 4  0.1509434 2012-10-01       15
    ## 5  0.0754717 2012-10-01       20
    ## 6  2.0943396 2012-10-01       25
    ## 7  0.5283019 2012-10-01       30
    ## 8  0.8679245 2012-10-01       35
    ## 9  0.0000000 2012-10-01       40
    ## 10 1.4716981 2012-10-01       45
    ## 11 0.3018868 2012-10-01       50
    ## 12 0.1320755 2012-10-01       55
    ## 13 0.3207547 2012-10-01      100
    ## 14 0.6792453 2012-10-01      105
    ## 15 0.1509434 2012-10-01      110
    ## 16 0.3396226 2012-10-01      115
    ## 17 0.0000000 2012-10-01      120
    ## 18 1.1132075 2012-10-01      125
    ## 19 1.8301887 2012-10-01      130
    ## 20 0.1698113 2012-10-01      135
    ## 21 0.1698113 2012-10-01      140
    ## 22 0.3773585 2012-10-01      145
    ## 23 0.2641509 2012-10-01      150
    ## 24 0.0000000 2012-10-01      155
    ## 25 0.0000000 2012-10-01      200
    ## 26 0.0000000 2012-10-01      205
    ## 27 1.1320755 2012-10-01      210
    ## 28 0.0000000 2012-10-01      215
    ## 29 0.0000000 2012-10-01      220
    ## 30 0.1320755 2012-10-01      225

Question 4: histogram of total steps per day

    dates <- unique(as.factor(adata$date))
    l <- length(dates)
    total_steps <- vector("numeric", length = l)
    head(total_steps, n = 10)

    ##  [1] 0 0 0 0 0 0 0 0 0 0

    for (i in 1:l)
    {
        ddata <- adata[adata$date == as.character(dates[i]), ]
        total_steps[i] <- sum(ddata$steps)
    }
    head(total_steps, n = 30)

    ##  [1] 10766.19   126.00 11352.00 12116.00 13294.00 15420.00 11015.00
    ##  [8] 10766.19 12811.00  9900.00 10304.00 17382.00 12426.00 15098.00
    ## [15] 10139.00 15084.00 13452.00 10056.00 11829.00 10395.00  8821.00
    ## [22] 13460.00  8918.00  8355.00  2492.00  6778.00 10119.00 11458.00
    ## [29]  5018.00  9819.00

    hist(total_steps, breaks = l, xlab = "steps", main = "Steps per day")

![](PA1_template_files/figure-markdown_strict/histogram%202-1.png) mean
and median total number of steps taken per day:

    print(paste("Mean is ", mean(total_steps)), sep = "")

    ## [1] "Mean is  10766.1886792453"

    print(paste("Median is ", median(total_steps)), sep = "")

    ## [1] "Median is  10766.1886792453"

It's strange that these values are exactly the same, but they are very
close to the original values with only the complete data set. The impact
of estimating the missing values appears to be negligible.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Question 1: below is the code chunk to create a new factor variable in
the dataset to indicate weekends and weekdays:

    wdata <- ccdata
    wdata$daytypes <- ""
    for (i in 1:nrow(wdata))
    {
        day <- weekdays(as.Date(wdata[i, 2]))
        if (day == "Saturday" | day == "Sunday")
        {
            wdata[i, 4] <- "weekend"
        }
        else
        {
            wdata[i, 4] <- "weekday"
        }
    }

    dweekdays <- wdata[wdata$daytypes == "weekday", ]
    dweekends <- wdata[wdata$daytypes == "weekend", ]
    head(dweekdays, n = 30)

    ##     steps       date interval daytypes
    ## 289     0 2012-10-02        0  weekday
    ## 290     0 2012-10-02        5  weekday
    ## 291     0 2012-10-02       10  weekday
    ## 292     0 2012-10-02       15  weekday
    ## 293     0 2012-10-02       20  weekday
    ## 294     0 2012-10-02       25  weekday
    ## 295     0 2012-10-02       30  weekday
    ## 296     0 2012-10-02       35  weekday
    ## 297     0 2012-10-02       40  weekday
    ## 298     0 2012-10-02       45  weekday
    ## 299     0 2012-10-02       50  weekday
    ## 300     0 2012-10-02       55  weekday
    ## 301     0 2012-10-02      100  weekday
    ## 302     0 2012-10-02      105  weekday
    ## 303     0 2012-10-02      110  weekday
    ## 304     0 2012-10-02      115  weekday
    ## 305     0 2012-10-02      120  weekday
    ## 306     0 2012-10-02      125  weekday
    ## 307     0 2012-10-02      130  weekday
    ## 308     0 2012-10-02      135  weekday
    ## 309     0 2012-10-02      140  weekday
    ## 310     0 2012-10-02      145  weekday
    ## 311     0 2012-10-02      150  weekday
    ## 312     0 2012-10-02      155  weekday
    ## 313     0 2012-10-02      200  weekday
    ## 314     0 2012-10-02      205  weekday
    ## 315     0 2012-10-02      210  weekday
    ## 316     0 2012-10-02      215  weekday
    ## 317     0 2012-10-02      220  weekday
    ## 318     0 2012-10-02      225  weekday

    head(dweekends, n = 30)

    ##      steps       date interval daytypes
    ## 1441     0 2012-10-06        0  weekend
    ## 1442     0 2012-10-06        5  weekend
    ## 1443     0 2012-10-06       10  weekend
    ## 1444     0 2012-10-06       15  weekend
    ## 1445     0 2012-10-06       20  weekend
    ## 1446     0 2012-10-06       25  weekend
    ## 1447     0 2012-10-06       30  weekend
    ## 1448     0 2012-10-06       35  weekend
    ## 1449     0 2012-10-06       40  weekend
    ## 1450     0 2012-10-06       45  weekend
    ## 1451     0 2012-10-06       50  weekend
    ## 1452     0 2012-10-06       55  weekend
    ## 1453     0 2012-10-06      100  weekend
    ## 1454     0 2012-10-06      105  weekend
    ## 1455     0 2012-10-06      110  weekend
    ## 1456     0 2012-10-06      115  weekend
    ## 1457     0 2012-10-06      120  weekend
    ## 1458     0 2012-10-06      125  weekend
    ## 1459     0 2012-10-06      130  weekend
    ## 1460     0 2012-10-06      135  weekend
    ## 1461     0 2012-10-06      140  weekend
    ## 1462     0 2012-10-06      145  weekend
    ## 1463     0 2012-10-06      150  weekend
    ## 1464     0 2012-10-06      155  weekend
    ## 1465     0 2012-10-06      200  weekend
    ## 1466     0 2012-10-06      205  weekend
    ## 1467     0 2012-10-06      210  weekend
    ## 1468     0 2012-10-06      215  weekend
    ## 1469     0 2012-10-06      220  weekend
    ## 1470     0 2012-10-06      225  weekend

Question 2: panel plot of time intervals and average steps taken.

    ints <- unique(dweekdays$interval)
    l <- length(ints)
    avgs <- vector("numeric", length = l)
    days <- rep("weekday", l)
    for (i in 1:l)
    {
        i_data <- dweekdays[dweekdays$interval == ints[i], ]
        avgs[i] <- mean(i_data$steps)
    }
    panel_data1 <- cbind(ints, avgs, days)

    ints <- unique(dweekends$interval)
    l <- length(ints)
    avgs <- vector("numeric", length = l)
    days <- rep("weekend", l)
    for (i in 1:l)
    {
        i_data <- dweekends[dweekends$interval == ints[i], ]
        avgs[i] <- mean(i_data$steps)
    }
    panel_data2 <- cbind(ints, avgs, days)

    panel_data <- rbind(panel_data1, panel_data2)
    panel_data <- data.frame(panel_data)
    panel_data$avgs <- as.numeric(as.character(panel_data$avgs))
    panel_data$ints <- as.integer(as.character(panel_data$ints))
    panel_data$days <- as.factor(panel_data$days)

    library(ggplot2)
    sp <- ggplot(panel_data, aes(x = ints, y = avgs)) + geom_line() + facet_grid(days ~ .)
    sp

![](PA1_template_files/figure-markdown_strict/plot%20weekdays-1.png)
