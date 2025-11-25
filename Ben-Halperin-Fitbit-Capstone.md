# 1. ASK

The goal of this project is to analyze Fitbit smart device data to
identify user activity and sleep behavior trends that can inform
Bellabeat’s marketing strategy.  
We focus on user consistency, daily patterns, and behavioral segments.

Key Questions: - How active are users across days? - How well do they
sleep? - What relationships exist between activity, sleep, and
calories? - What behavior segments can marketing target?

------------------------------------------------------------------------

# 2. PREPARE

## 2.1 Data Source

Fitbit Fitness Tracker Data (Kaggle, CC0).  
30 consenting users, March–April 2016.

## 2.2 Load Data

    daily_activity <- read_csv(
      "data/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv") %>%
      clean_names()

    sleep_minute <- read_csv(
      "data/Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv") %>%
      clean_names()

\##Inspect structure:

    glimpse(daily_activity)

    ## Rows: 457
    ## Columns: 15
    ## $ id                         <dbl> 1503960366, 1503960366, 1503960366, 1503960…
    ## $ activity_date              <chr> "3/25/2016", "3/26/2016", "3/27/2016", "3/2…
    ## $ total_steps                <dbl> 11004, 17609, 12736, 13231, 12041, 10970, 1…
    ## $ total_distance             <dbl> 7.11, 11.55, 8.53, 8.93, 7.85, 7.16, 7.86, …
    ## $ tracker_distance           <dbl> 7.11, 11.55, 8.53, 8.93, 7.85, 7.16, 7.86, …
    ## $ logged_activities_distance <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ very_active_distance       <dbl> 2.57, 6.92, 4.66, 3.19, 2.16, 2.36, 2.29, 3…
    ## $ moderately_active_distance <dbl> 0.46, 0.73, 0.16, 0.79, 1.09, 0.51, 0.49, 0…
    ## $ light_active_distance      <dbl> 4.07, 3.91, 3.71, 4.95, 4.61, 4.29, 5.04, 3…
    ## $ sedentary_active_distance  <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0…
    ## $ very_active_minutes        <dbl> 33, 89, 56, 39, 28, 30, 33, 47, 40, 15, 43,…
    ## $ fairly_active_minutes      <dbl> 12, 17, 5, 20, 28, 13, 12, 21, 11, 30, 18, …
    ## $ lightly_active_minutes     <dbl> 205, 274, 268, 224, 243, 223, 239, 200, 244…
    ## $ sedentary_minutes          <dbl> 804, 588, 605, 1080, 763, 1174, 820, 866, 6…
    ## $ calories                   <dbl> 1819, 2154, 1944, 1932, 1886, 1820, 1889, 1…

    glimpse(sleep_minute)

    ## Rows: 198,559
    ## Columns: 4
    ## $ id     <dbl> 1503960366, 1503960366, 1503960366, 1503960366, 1503960366, 150…
    ## $ date   <chr> "3/13/2016 2:39:30 AM", "3/13/2016 2:40:30 AM", "3/13/2016 2:41…
    ## $ value  <dbl> 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ log_id <dbl> 11114919637, 11114919637, 11114919637, 11114919637, 11114919637…

# 3. PROCESS

## 3.1 Clean & Transform

### Fix daily activity date

    daily_activity <- daily_activity %>%
      mutate(activity_date = mdy(activity_date))

    ### Aggregate minute-level sleep → daily sleep

    sleep_day <- sleep_minute %>%
      mutate(
        sleep_datetime = mdy_hms(date),
        sleep_date = as.Date(sleep_datetime),
        asleep_flag = if_else(value == 1, 1L, 0L),
        in_bed_flag = 1L
      ) %>%
      group_by(id, sleep_date) %>%
      summarise(
        total_minutes_asleep = sum(asleep_flag, na.rm = TRUE),
        total_time_in_bed = sum(in_bed_flag, na.rm = TRUE),
        total_sleep_records = n_distinct(log_id),
        .groups = "drop"
      )

    #Check:

    glimpse(sleep_day)

    ## Rows: 467
    ## Columns: 5
    ## $ id                   <dbl> 1503960366, 1503960366, 1503960366, 1503960366, 1…
    ## $ sleep_date           <date> 2016-03-13, 2016-03-14, 2016-03-15, 2016-03-16, …
    ## $ total_minutes_asleep <int> 411, 354, 312, 333, 402, 379, 447, 469, 390, 281,…
    ## $ total_time_in_bed    <int> 426, 386, 335, 366, 437, 411, 468, 476, 427, 297,…
    ## $ total_sleep_records  <int> 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 1, 1, 1…

## 3.2 Join Activity + Sleep

    activity_sleep <- daily_activity %>%
      inner_join(sleep_day, by = c("id", "activity_date" = "sleep_date")) %>%
      mutate(
        total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes,
        sleep_efficiency = if_else(total_time_in_bed > 0,
                                   total_minutes_asleep / total_time_in_bed,
                                   NA_real_),
        hours_asleep = total_minutes_asleep / 60,
        weekday = weekdays(activity_date),
        is_weekend = weekday %in% c("Saturday","Sunday")
      )

Basic Checks:

    summary(activity_sleep$total_steps)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       0    4340    7605    7939   11121   19658

    summary(activity_sleep$hours_asleep)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.1667  5.5667  6.9000  6.5639  7.8917 15.0167

# 4. ANALYZE

## 4.1 Steps by Weekday

    steps_by_weekday <- daily_activity %>%
      mutate(
        weekday = factor(
          weekdays(activity_date),
          levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
        )
      ) %>%
      group_by(weekday) %>%
      summarise(avg_steps = mean(total_steps, na.rm=TRUE))

steps\_by\_weekday

    ggplot(steps_by_weekday, aes(weekday, avg_steps)) +
      geom_col(fill="steelblue") +
      theme_minimal() +
      labs(title="Average Steps by Weekday", x="Weekday", y="Steps")

![](Ben-Halperin-Fitbit-Capstone_files/figure-markdown_strict/unnamed-chunk-7-1.png)
\## 4.2 Steps vs Calories

    ggplot(daily_activity, aes(total_steps, calories)) +
      geom_point(alpha=0.4, color="darkgreen") +
      geom_smooth(method="lm", se=FALSE, color="black") +
      theme_minimal() +
      labs(title="Daily Steps vs Calories Burned", x="Steps", y="Calories")

![](Ben-Halperin-Fitbit-Capstone_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    cor(daily_activity$total_steps, daily_activity$calories, use="complete.obs")

    ## [1] 0.5813802

## 4.3 Sleep Distribution

    ggplot(activity_sleep, aes(hours_asleep)) +
      geom_histogram(binwidth=0.5, fill="purple", color="white") +
      theme_minimal() +
      labs(title="Distribution of Sleep Hours", x="Hours Asleep", y="Count")

![](Ben-Halperin-Fitbit-Capstone_files/figure-markdown_strict/unnamed-chunk-9-1.png)
\## 4.4 User Segments

    user_summary <- activity_sleep %>%
      group_by(id) %>%
      summarise(
        avg_steps = mean(total_steps, na.rm=TRUE),
        avg_sleep = mean(hours_asleep, na.rm=TRUE),
        avg_sedentary = mean(sedentary_minutes, na.rm=TRUE)
      ) %>%
      mutate(
        segment = case_when(
          avg_steps >= 10000 ~ "Highly Active",
          avg_steps >= 5000 ~ "Casual",
          TRUE ~ "Low Engagement"
        )
      )

user\_summary

    segment_counts <- user_summary %>%
      count(segment)

    ggplot(segment_counts, aes(segment, n, fill=segment)) +
      geom_col() +
      theme_minimal() +
      labs(title="User Engagement Segments", x="Segment", y="Users")

![](Ben-Halperin-Fitbit-Capstone_files/figure-markdown_strict/unnamed-chunk-11-1.png)
\# 5. SHARE

Insights: - Steps drop on weekends. - Sleep often below recommended
levels. - Steps ↔ calories strongly correlated. - Three natural user
behavior segments appear.

------------------------------------------------------------------------

# 6. ACT — Recommendations

- Add consistency challenges inside the Bellabeat app.  
- Weekend wellness reminders.  
- Market Bellabeat as a holistic tracker (activity + sleep +
  mindfulness).  
- Personalize notifications to increase engagement.  
- Collect more Bellabeat-specific app data for better segmentation.

------------------------------------------------------------------------

# END OF REPORT
