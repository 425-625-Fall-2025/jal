#' ---
#' title: "Sleep Study"
#' date: "`r Sys.Date()`"
#' author: "Statistical Case Studies"
#' output: pdf_document
#' ---
#' 
#' 
#' # Sleep Study Graphical Exploration Challenge
#'
#' - See `SleepStudy.html` for a description of the research problem.
#' - See `SleepData/` for 6 days of data, each day including two data
#'   files plus a README file.  There is a zip file of the data, too.
#'

library(ggplot2)
setwd('/Users/jaketodd/Desktop/CaseStudy2')
days <- dir("SleepData")

#
# Read in one day of data for starters...
#

thisday <- days[2]
files <- dir(file.path("SleepData", thisday),
             full.names = TRUE, pattern = "^[^R]")

x <- read.table(files[1], as.is = TRUE, skip = 1, header = FALSE)
names(x) <- "angle"

y <- read.csv(files[2], as.is = TRUE)
names(y)[2] <- "angle"

#'
#' ## Challenge 1
#' 
#' Create a list of length 6 (there are 6 days of data).
#' Each element of the list should be a list of two data frames.
#' Use suitable names for both the main list and the internal lists.
#' This should (might?) save you time down the road...
#'

data_days <- list()

for(i in 1:length(days)){
  data_devices <- list()
  thisday <- days[i]
  files <- dir(file.path("SleepData", thisday),
               full.names = TRUE, pattern = "^[^R]")
  x <- read.table(files[1], as.is = TRUE, skip = 1, header = FALSE)
  names(x) <- "angle"
  y <- read.csv(files[2], as.is = TRUE)
  names(y)[2] <- "angle"
  data_devices[[1]] <- x
  data_devices[[2]] <- y
  data_days[[i]] <- data_devices
}

length(data_days)
length(data_days[[1]])
identical(data_days[[6]][[1]],x)
identical(data_days[[6]][[2]],y)

#'
#' ## Challenge 2
#' 
#' Try to do a basic graphical exploration of the data
#' from any one of the days.  For this to make any sense at all, you'll
#' need to read (carefully) `SleepStudy.html` and perhaps look at the
#' contents of the README files (in the data directories).
#'
#' \clearpage
#'
#'

### Day 4 (Nov 19)

temp_data <- data_days[[4]]

embletta <- temp_data[[1]]
embletta <- data.frame(embletta)

start <- as.POSIXct("2011-11-19 22:00:00")
end <- as.POSIXct("2011-11-20 07:00:00")
nrow(embletta)
timestamps <- seq(start, end, by = 0.1)[1:nrow(embletta)]
length(timestamps)
embletta$Time_of_day <- timestamps

colnames(embletta) <- c('angle', 'Time_of_day')

embletta_const <- embletta[embletta$angle < 50,]
embletta_const <- embletta_const[embletta_const$angle > -50,]
const1 <- median(embletta_const$angle)
embletta$angle <- embletta$angle - const1

embletta <- embletta[embletta$angle < 180,]
embletta <- embletta[embletta$angle > -180,]

somnopose <- temp_data[[2]]

somnopose_const <- somnopose[somnopose$angle < 50,]
somnopose_const <- somnopose_const[somnopose_const$angle > -50,]
const2 <- median(somnopose_const$angle)
somnopose$angle <- somnopose$angle - const2

somnopose <- somnopose[somnopose$angle < 180,]
somnopose <- somnopose[somnopose$angle > -180,]

for(i in 1:nrow(somnopose)){
  if(grepl("^23:", somnopose$Time_of_day[i])){
    somnopose$Time_of_day[i] <- paste0('2011-11-19 ', somnopose$Time_of_day[i])
  } else {
    somnopose$Time_of_day[i] <- paste0('2011-11-20 ', somnopose$Time_of_day[i])
  }
}

somnopose$Time_of_day <- as.POSIXct(somnopose$Time_of_day)

library(dplyr)
library(tidyverse)
comb = dplyr::full_join(embletta, somnopose, by = "Time_of_day")
comb = comb %>% dplyr::select(c('angle.x', 'Time_of_day', 'angle.y'))
summary(comb)
long_comb <- comb %>%
  pivot_longer(cols = c(angle.x, angle.y), 
               names_to = "Device", 
               values_to = "Angle")
long_comb <- na.omit(data.frame(long_comb))
g1 = ggplot(long_comb, aes(x = Time_of_day, y = Angle, color = Device)) + 
  geom_line(na.rm = TRUE) + 
  labs(title = "Embletta vs. SomnoPose") + 
  scale_color_manual(
    values = c("angle.x" = "darkred", "angle.y" = "turquoise"),
    labels = c("angle.x" = "Embletta", "angle.y" = "Somnopose")
  ) + geom_abline(slope = 0, intercept = 0, linetype = "dashed", lwd = 1, color = 'black') + 
  theme_classic()
g1

### Day 1 (Nov 15)

temp_data <- data_days[[1]]

embletta <- temp_data[[1]]
embletta <- data.frame(embletta)

start <- as.POSIXct("2011-11-15 22:00:00")
end <- as.POSIXct("2011-11-16 08:00:00")
nrow(embletta)
timestamps <- seq(start, end, by = 0.1)[1:nrow(embletta)]
length(timestamps)
embletta$Time_of_day <- timestamps
embletta <- na.omit(embletta)
tail(embletta)
colnames(embletta) <- c('angle', 'Time_of_day')

embletta_const <- embletta[embletta$angle < 50,]
embletta_const <- embletta_const[embletta_const$angle > -50,]
const1 <- median(embletta_const$angle)
embletta$angle <- embletta$angle - const1

embletta <- embletta[embletta$angle < 180,]
embletta <- embletta[embletta$angle > -180,]

somnopose <- temp_data[[2]]

somnopose_const <- somnopose[somnopose$angle < 50,]
somnopose_const <- somnopose_const[somnopose_const$angle > -50,]
const2 <- median(somnopose_const$angle)
somnopose$angle <- somnopose$angle - const2

somnopose <- somnopose[somnopose$angle < 180,]
somnopose <- somnopose[somnopose$angle > -180,]

for(i in 1:nrow(somnopose)){
  if(grepl("^23:", somnopose$Time_of_day[i])){
    somnopose$Time_of_day[i] <- paste0('2011-11-15 ', somnopose$Time_of_day[i])
  } else {
    somnopose$Time_of_day[i] <- paste0('2011-11-16 ', somnopose$Time_of_day[i])
  }
}

somnopose$Time_of_day <- as.POSIXct(somnopose$Time_of_day)
somnopose <- somnopose[somnopose$Time_of_day < '2011-11-16 08:00:00', ]

library(dplyr)
library(tidyverse)
comb = dplyr::full_join(embletta, somnopose, by = "Time_of_day")
comb = comb %>% dplyr::select(c('angle.x', 'Time_of_day', 'angle.y'))
summary(comb)
long_comb <- comb %>%
  pivot_longer(cols = c(angle.x, angle.y), 
               names_to = "Device", 
               values_to = "Angle")
long_comb <- na.omit(data.frame(long_comb))
g2 = ggplot(long_comb, aes(x = Time_of_day, y = Angle, color = Device)) + 
  geom_line(na.rm = TRUE) + 
  labs(title = "Embletta vs. SomnoPose") + 
  scale_color_manual(
    values = c("angle.x" = "darkred", "angle.y" = "turquoise"),
    labels = c("angle.x" = "Embletta", "angle.y" = "Somnopose")
  ) + 
  geom_abline(slope = 0, intercept = 0, linetype = "dashed", lwd = 1, color = 'black') + 
  theme_classic()
g2

#' ## ULTIMATE GOAL
#' 
#' Ideally, produce a 6-page-plus PDF file (maybe more if it includes the
#' processed report) that includes one graphic per day
#' that could be used to help the researchers visualize the study results
#' and decide how to proceed with future research.  Be as professional as
#' possible with the graphics and the short report, where
#' you should discuss and try to answer the questions posed by the
#' researcher in `SleepStudy.html`.  Yes, this could be a very short report
#' (though plots and explorations consume quite a bit of space)!
#'
#' Guidelines:
#'
#' - You may use any graphical tools or packages that you want, ___BUT___
#'   if you use non-base R or non-`ggplot` graphics or some package to assist with the
#'   processing, your code should be particularly beautiful and well-documented
#'   to the extent that we might use it as a learning example to be shared
#'   with the whole class!  It should also answer the question, "Why do you think
#'   this is better than using base R or `ggplot`?"  
#'   The answer is hopefully more interesting than, "I don't know how to do this in base R."  
#'   So, you don't have to receive permission -- the answer is yes **as long
#'   as** you agree to the guideline above.
#'
#' - Eventually, your group should submit your script containing all your work;
#'   team members should be clearly listed at the top.  Only the group
#'   leader should submit this R script and an accompanying PDF file (of either
#'   6 plots or a processed report, whatever you want).  You should each come
#'   to the "extra sessions" with your group script
#'   and you should be comfortable with everything in the script in case
#'   I want to ask questions.  Saying, "Oh, the group leader did that and I'm
#'   not sure!" isn't ideal, needless to say.
#'   
#' - The PDF should not identify your group or your group members, so
#'   student assessments of submissions will be anonymous.
#'
#'
#' # How?
#' 
#' Starting Tuesday, September 2: You should form groups of 3 if possible. There may be one or two groups of 2. 
#'
#' This will continue on Thursday, September 4 (maybe along with some other
#' things during class time), with a final assignment submission (group) due
#' before class on Tuesday, September 9.
#'
