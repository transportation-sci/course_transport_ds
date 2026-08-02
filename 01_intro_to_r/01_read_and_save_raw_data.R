# Data Details ------------------------------
## Download Original NGSIM data: https://datahub.transportation.gov/api/views/8ect-6jqj/rows.csv
## Description: https://datahub.transportation.gov/stories/s/Next-Generation-Simulation-NGSIM-Open-Data/i5zb-xe34/
## Dictionary: https://data.transportation.gov/Automobiles/Next-Generation-Simulation-NGSIM-Vehicle-Trajector/8ect-6jqj/about_data

## Data path
data_path <- "data/raw/Next_Generation_Simulation__NGSIM__Vehicle_Trajectories_and_Supporting_Data.csv"


# Slow read ----------------------------------
system.time(
  readr::read_csv(data_path)
) 
#  user  system elapsed 
# 65.86    9.81   10.08 

# Double click the CSV file in Positron -----
## Rows, columns, filters, summary stats, histograms, and convert to code

# Partition and save data -------------------
## Look at data without loading it in memory
data_ref <- arrow::open_dataset(data_path, format = "csv")

## Divide data by time period
### Global_Time is elapsed time in milliseconds since Jan 1, 1970
time_range <- data_ref |>
  dplyr::mutate(
    Global_Time_1000 = Global_Time / 1000
  ) |> 
  dplyr::group_by(Location) |>
  dplyr::summarize(
    min_time = min(Global_Time_1000),
    max_time = max(Global_Time_1000)
  ) |> 
  dplyr::collect() |>
  dplyr::mutate(
    n_periods = dplyr::if_else(Location %in% c("us-101", "i-80"), 3L, 2L),
    period_width = (max_time - min_time) / n_periods,
    b1 = min_time + period_width,
    b2 = min_time + 2 * period_width   # only meaningful when n_periods == 3
  ) |>
  dplyr::select(Location, n_periods, b1, b2)


## Save data by location and time period
data_ref |>
  dplyr::left_join(time_range, by = "Location") |>
  dplyr::mutate(
    Global_Time_1000 = Global_Time / 1000,
    Period = dplyr::case_when(
      Global_Time_1000 <= b1  ~ 1L,
      n_periods == 2L    ~ 2L,   # 2-period locations: anything past b1 is period 2
      Global_Time_1000 <= b2  ~ 2L,   # 3-period locations
      TRUE               ~ 3L
    )
  ) |> 
  dplyr::group_by(Location, Period) |>
  arrow::write_dataset(path = "data/ngsim", format = "parquet")

## Sizes:
tibble::tibble(
  files = list.files("data/ngsim", recursive = TRUE),
  size_MB = file.size(file.path("data/ngsim", files)) / 1024^2
)

summary_stats_speed <- arrow::open_dataset("data/ngsim") |>
  dplyr::group_by(Location, Period) |> 
  dplyr::summarize(median_vel = median(v_Vel),
                   min_vel = min(v_Vel),
                   max_vel = max(v_Vel),
                   percentile_25 = quantile(v_Vel, 0.25),
                   percentile_75 = quantile(v_Vel, 0.75)) |> 
  dplyr::collect()


ggplot2::ggplot(
  summary_stats_speed, 
  ggplot2::aes(x = Period, group = Period, color = Location)
) +
  ggplot2::geom_boxplot(
    ggplot2::aes(
      ymin   = min_vel,  # Bottom whisker
      lower  = percentile_25,  # Bottom of the box
      middle = median_vel,  # Median line
      upper  = percentile_75,  # Top of the box
      ymax   = max_vel   # Top whisker
    ),
    stat = "identity"    
  ) +
  ggplot2::facet_wrap(~ Location)

