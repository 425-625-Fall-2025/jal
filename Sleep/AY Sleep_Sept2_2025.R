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
# create an empty list to store the data for all 6 days
daily_data_list <- list()

for (i in 1:length(days)) {
    thisday <- days[i]
    files <- dir(file.path("SleepData", thisday),
             full.names = TRUE, pattern = "^[^R]")
    # read in somno data
    embletta_df <- read.table(files[1], as.is = TRUE, skip = 1, header = FALSE)
    names(embletta_df) <- "angle"

    # read in embletta data
    somno_df <- read.csv(files[2], as.is = TRUE)
    names(somno_df)[2] <- "angle"
  
    # add in an entry for that day to the list
    day_list <- list(embletta = embletta_df, somno = somno_df)
    daily_data_list[[i]] <- day_list
}

#'
#' ## Challenge 2
#' 
#' Try to do a basic graphical exploration of the data
#' from any one of the days.  For this to make any sense at all, you'll
#' need to read (carefully) `SleepStudy.html` and perhaps look at the
#' contents of the README files (in the data directories).
#'
#' 
library(ggplot2)
library(dplyr)

day_3 <- daily_data_list[[3]]
day_3_embletta <- day_3$embletta
day_3_somno <- day_3$somno
# we don't know the timestamps for embletta, but we know it was recording 10 pm - 7 am
# create a vector that has a timestamp for 10 measurements every second
# then merge with the dataframe
start_time <- as.POSIXct("2011-11-18 22:00:00", tz = "UTC")
end_time <- as.POSIXct("2011-11-19 06:59:59.9", tz = "UTC")

time_sequence <- seq(from = start_time, to = end_time, by = .1)
time_sequence[1]

day_3_embletta_df <- data.frame(
    timestamp = time_sequence,
    angle = day_3_embletta
)

day_3_embletta_df$Time_of_day <- format(day_3_embletta_df$timestamp, "%H:%M:%S", tz = "UTC")

day_3_somno$device <- "Somno"
day_3_embletta_df$device <- "Embletta"

day_3_somno$Time_of_day <- format(as.POSIXct(day_3_somno$Time_of_day, format = "%H:%M:%OS"), format = "%H:%M:%OS")

# Convert Time_of_day to POSIXct with the correct date
day_3_somno$Datetime <- as.POSIXct(day_3_somno$Time_of_day, format="%H:%M:%S")

# Assign dates manually based on a cutoff (e.g., midnight)
times <- hms::as_hms(day_3_somno$Time_of_day)

# If time is less than some threshold (e.g., 12:00), assign next day
day_3_somno$Datetime <- as.POSIXct(
  paste(ifelse(times < hms::as_hms("12:00:00"), "2011-11-19", "2011-11-18"), 
        day_3_somno$Time_of_day),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

cutoff_start <- as.POSIXct("2011-11-18 00:41:00", tz = "UTC")
cutoff_end <- as.POSIXct("2011-11-19 06:59:59", tz = "UTC")

cutoff_start_time <- hms::as_hms(cutoff_start)
cutoff_end_time <- hms::as_hms(cutoff_end)

day_3_embletta_df <- day_3_embletta_df %>%
  filter(
    {
      t <- hms::as_hms(timestamp)
      if(cutoff_end_time < cutoff_start_time){
        # Time window goes over midnight
        t >= cutoff_start_time | t <= cutoff_end_time
      } else {
        # Time window within the same day
        t >= cutoff_start_time & t <= cutoff_end_time
      }
    }
  )
day_3_somno <- day_3_somno %>%
  filter(Datetime >= cutoff_start & Datetime <= cutoff_end)

day_3_somno$Time_of_day <- day_3_somno$Datetime
day_3_embletta_df$Time_of_day <- day_3_embletta_df$timestamp

day_3_embletta_df <- day_3_embletta_df %>%
  filter(angle >= 0 & angle <= 180)

combined_data <- bind_rows(day_3_somno, day_3_embletta_df)

ggplot(combined_data, aes(x = Time_of_day, y = angle, color = device)) +
  geom_line() +
  labs(
    title = "Orientation by Time for Day 3",
    x = "Time",
    y = "Orientation",
    color = "Device"  # Label for the legend
  ) +
  theme_minimal()

ggplot(day_3_somno, aes(x = Time_of_day, y = angle)) +
  geom_line() +
  labs(
    "Orientation by Time for Day 3",
    x = "Time",
    y = "Orientation"
  ) +
  theme_minimal()

ggplot(day_3_embletta_df, aes(x = timestamp, y = angle)) +
  geom_line() +
  labs(
    "Orientation by Time for Day 3",
    x = "Time",
    y = "Orientation"
  ) +
  theme_minimal()

ggplot(day_3_somno, aes(y = angle)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Orientation",
    x = "Day",
    y = "Orientation"
  ) +
  theme_minimal()

# day 2
day_2 <- daily_data_list[[2]]
day_2_embletta <- day_2$embletta
day_2_somno <- day_2$somno

# we don't know the timestamps for embletta, but we know it was recording 10 pm - 7 am
# create a vector that has a timestamp for 10 measurements every second
# then merge with the dataframe
start_time <- as.POSIXct("2011-11-17 22:00:00", tz = "UTC")
end_time <- as.POSIXct("2011-11-18 06:59:59.9", tz = "UTC")

time_sequence <- seq(from = start_time, to = end_time, by = .1)

day_2_embletta_df <- data.frame(
    timestamp = time_sequence,
    angle = day_2_embletta
)

day_2_embletta_df$Time_of_day <- format(day_2_embletta_df$timestamp, "%H:%M:%S", tz = "UTC")

day_2_somno$device <- "Somno"
day_2_embletta_df$device <- "Embletta"

day_2_somno$Time_of_day <- format(as.POSIXct(day_2_somno$Time_of_day, format = "%H:%M:%OS"), format = "%H:%M:%OS")

# Convert Time_of_day to POSIXct with the correct date
day_2_somno$Datetime <- as.POSIXct(day_2_somno$Time_of_day, format="%H:%M:%S")

# Assign dates manually based on a cutoff (e.g., midnight)
times <- hms::as_hms(day_2_somno$Time_of_day)

# If time is less than some threshold (e.g., 12:00), assign next day
day_2_somno$Datetime <- as.POSIXct(
  paste(ifelse(times < hms::as_hms("12:00:00"), "2011-11-18", "2011-11-17"), 
        day_2_somno$Time_of_day),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)
tail(day_2_somno$Datetime)

cutoff_start <- as.POSIXct("2011-11-17 22:28:29", tz = "UTC")
cutoff_end <- as.POSIXct("2011-11-18 02:19:10", tz = "UTC")

cutoff_start_time <- hms::as_hms(cutoff_start)
cutoff_end_time <- hms::as_hms(cutoff_end)

day_2_embletta_df <- day_2_embletta_df %>%
  filter(
    {
      t <- hms::as_hms(timestamp)
      if(cutoff_end_time < cutoff_start_time){
        # Time window goes over midnight
        t >= cutoff_start_time | t <= cutoff_end_time
      } else {
        # Time window within the same day
        t >= cutoff_start_time & t <= cutoff_end_time
      }
    }
  )
day_2_somno <- day_2_somno %>%
  filter(Datetime >= cutoff_start & Datetime <= cutoff_end)

day_2_somno$Time_of_day <- day_2_somno$Datetime
day_2_embletta_df$Time_of_day <- day_2_embletta_df$timestamp

combined_data <- bind_rows(day_2_somno, day_2_embletta_df)
ggplot(combined_data, aes(x = Time_of_day, y = angle, color = device)) +
  geom_line() +
  labs(
    title = "Orientation by Time for Day 2",
    x = "Time",
    y = "Orientation",
    color = "Device"  # Label for the legend
  ) +
  theme_minimal()


# day 6
day_6 <- daily_data_list[[6]]
day_6_embletta <- day_6$embletta
day_6_somno <- day_6$somno

# we don't know the timestamps for embletta, but we know it was recording 10 pm - 7 am
# create a vector that has a timestamp for 10 measurements every second
# then merge with the dataframe
thresh = 100
start_index <- which(day_6_embletta$angle > thresh)[1]
start_index <- start_index + 3000
# Keep all rows from that index onward
day_6_embletta <- day_6_embletta[start_index:nrow(day_6_embletta), ]
length(day_6_embletta)
start_time <- as.POSIXct("2011-11-24 00:38:19", tz = "UTC")
end_time <- as.POSIXct("2011-11-24 06:59:59", tz = "UTC")

time_sequence <- seq(from = start_time, to = end_time, by = .1)

time_sequence <- time_sequence[1:length(day_6_embletta)]
day_6_embletta_df <- data.frame(
    timestamp = time_sequence,
    angle = day_6_embletta
)

day_6_embletta_df$Time_of_day <- format(day_6_embletta_df$timestamp, "%H:%M:%S", tz = "UTC")

day_6_somno$device <- "Somno"
day_6_embletta_df$device <- "Embletta"

day_6_somno$Time_of_day <- format(as.POSIXct(day_6_somno$Time_of_day, format = "%H:%M:%OS"), format = "%H:%M:%OS")

# Convert Time_of_day to POSIXct with the correct date
day_6_somno$Datetime <- as.POSIXct(day_6_somno$Time_of_day, format="%H:%M:%S")

# Assign dates manually based on a cutoff (e.g., midnight)
times <- hms::as_hms(day_6_somno$Time_of_day)

# If time is less than some threshold (e.g., 12:00), assign next day
day_6_somno$Datetime <- as.POSIXct(
  paste(ifelse(times < hms::as_hms("12:00:00"), "2011-11-24", "2011-11-23"), 
        day_6_somno$Time_of_day),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

day_6_somno$Time_of_day <- day_6_somno$Datetime
day_6_embletta_df$Time_of_day <- day_6_embletta_df$timestamp

day_6_somno$angle <- day_6_somno$angle * -1

day_6_embletta_df <- day_6_embletta_df %>%
  filter(angle >= 0 & angle <= 180)

combined_data <- bind_rows(day_6_somno, day_6_embletta_df)
ggplot(combined_data, aes(x = Time_of_day, y = angle, color = device)) +
  geom_line() +
  labs(
    title = "Orientation by Time for Day 6",
    x = "Time",
    y = "Orientation",
    color = "Device"  # Label for the legend
  ) +
  theme_minimal()

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
