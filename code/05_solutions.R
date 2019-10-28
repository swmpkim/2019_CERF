## 1. File directory
in_path <- here::here("data", "final_challenge")

## 2. Vector of file names
my_stations <- c("apaebwq", "gndbhwq", "gtmpcwq", "rkblhwq", "wkbfrwq")
my_files <- paste0(my_stations, ".csv")

## 3. Write a function
sterr <- function(x){
  sd(x, na.rm = TRUE) / sqrt( sum(!is.na(x)) )
}

m_to_ft <- function(x){
  x * 3.28
}


for(i in seq_along(my_files)){

  # find the file
  in_file <- here::here("data", "final_challenge", my_files[i])

  # read in the file
  dat <- read.csv(in_file, stringsAsFactors = FALSE)

  dat <- dat %>%
    mutate(
      # fill in your function to turn depth in m into depth in ft
      depth_ft = m_to_ft(depth),
      # fill in the function needed to make sure date is correctly "seen"
      date = lubridate::ymd(date),
      # the lines below are "gimmes" because we haven't discussed them
      # but: they pull out the portion of the date we want, and
      # we can use that later
      year = lubridate::year(date),
      month = lubridate::month(date))

  # make AND PRINT a line graph of depth in feet,
  # where each year is a different color
  # add in an informative title (the file name is unique)
  # and axis labels, and whatever theme/colors you want
  q <- ggplot(dat) +
    geom_line(aes(x = date, y = depth_ft, col = factor(year))) +
    theme_bw() +
    labs(title = my_files[i])

  print(q)

  # now generate and print a summary table, by year,
  # to show min, median, mean, max, and sterr of depth_ft

  # hint: if you use a filter statement to get rid of
  # rows where depth_ft is NA  (in the dplyr script we learned how
  # to negate "is.na" ), you don't have to worry about
  # using "na.rm = TRUE" in each individual summary function
  dat %>%
    group_by(year) %>%
    filter(!is.na(depth_ft)) %>%
    summarize(min = min(depth_ft),
              median = median(depth_ft),
              mean = mean(depth_ft),
              max = max(depth_ft),
              sterr = sterr(depth_ft)) %>%
    print()

  # Group by year and month, calculate only the mean of depth_ft,
  # and pivot the data frame so that months are columns

  # what you want to see is something like:
  # year - month1 - month2 - .... - month12
  # 2016 - some mean depth - some mean depth 2 - .... - some mean depth 12
  # a table with one row per year, and one column per month
  # the values in each cell are the mean depth for that month/year combination
  dat %>%
    group_by(year, month) %>%
    filter(!is.na(depth_ft)) %>%
    summarize(mean = mean(depth_ft)) %>%
    pivot_wider(names_from = "month", values_from = "mean") %>%
    print()

}
