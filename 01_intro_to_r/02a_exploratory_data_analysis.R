# This covers:
# - Data transformation
# - Data visualization

# Load libraries ----------------------------
library(tidyverse)
library(arrow)


# Work with Arrow Dataset -------------------
ngsim_arrow <- open_dataset("data/ngsim")


# Q1: How big is the data and what are the columns? ------------------
dim(ngsim_arrow)
schema(ngsim_arrow)


# Change the column names to lowercase
ngsim_arrow <- ngsim_arrow |> 
  rename_with(janitor::make_clean_names)


# Q2: Summary statistics of a variable -----------------
## Let's consider the variable `v_vel` = Speed in ft/s
summary_stats_speed <- ngsim_arrow |>
  group_by(location, period) |> 
  summarize(median_vel = median(v_vel),
            min_vel = min(v_vel),
            max_vel = max(v_vel),
            percentile_25 = quantile(v_vel, 0.25),
            percentile_75 = quantile(v_vel, 0.75)) |> 
  collect()


## Plot speeds
ggplot(
  data = summary_stats_speed
) +
  geom_boxplot(
    mapping = aes(
      x = as.factor(period), 
      ymin   = min_vel,      
      lower  = percentile_25,
      middle = median_vel,    
      upper  = percentile_75,  
      ymax   = max_vel,   
      color = location     
    ),
    stat = "identity" 
  ) +
  facet_wrap(~ location) +
  labs(
    title = "Speed on I-80 (3, 15-min Time periods)",
    subtitle = "April 13, 2005",
    x = "Time Period",
    y = "Speed (ft/s)",
    caption = "Source: NGSIM Project",
    color = "Time Period"
  ) +
  theme_light() +
  theme(strip.text = element_text(size = 14))


###################################################################
# YOUR TURN
## Modify the code above to summarize and visualize v_length
###################################################################



# Visualization with ggplot2 ----------------------
## Well, we did a lot of stuff in a few lines of code!
## Let's try to understand how ggplot2 works.

## What is a boxplot?
### https://en.wikipedia.org/wiki/Box_plot

## Start with data:
i_80_summary <- summary_stats_speed |> 
  filter(location == "i-80")

ggplot(data = i_80_summary)

## Add x and y aesthetics:
ggplot(
  data = i_80_summary
) +
  geom_boxplot(
    mapping = aes(
      x = as.factor(period), 
      ymin   = min_vel,        # Bottom whisker
      lower  = percentile_25,  # Bottom of the box
      middle = median_vel,     # Median line
      upper  = percentile_75,  # Top of the box
      ymax   = max_vel         # Top whisker
    ),
    stat = "identity" 
  )


## Add color:
ggplot(
  data = i_80_summary
) +
  geom_boxplot(
    mapping = aes(
      x = as.factor(period), 
      ymin   = min_vel,      
      lower  = percentile_25,
      middle = median_vel,    
      upper  = percentile_75,  
      ymax   = max_vel,   
      color = as.factor(period)     
    ),
    stat = "identity" 
  )

### Use your own colors:
ggplot(
  data = i_80_summary
) +
  geom_boxplot(
    mapping = aes(
      x = as.factor(period), 
      ymin   = min_vel,      
      lower  = percentile_25,
      middle = median_vel,    
      upper  = percentile_75,  
      ymax   = max_vel,   
      color = as.factor(period)     
    ),
    stat = "identity" 
  ) +
  scale_color_manual(values = c("skyblue", "orange", "purple"))


### Or use a custom single color:
ggplot(
  data = i_80_summary
) +
  geom_boxplot(
    mapping = aes(
      x = as.factor(period), 
      ymin   = min_vel,      
      lower  = percentile_25,
      middle = median_vel,    
      upper  = percentile_75,  
      ymax   = max_vel
    ),
    color = "red",
    stat = "identity" 
  )



## Add title, subtitle and axis labels
ggplot(
  data = i_80_summary
) +
  geom_boxplot(
    mapping = aes(
      x = as.factor(period), 
      ymin   = min_vel,      
      lower  = percentile_25,
      middle = median_vel,    
      upper  = percentile_75,  
      ymax   = max_vel,   
      color = as.factor(period)     
    ),
    stat = "identity" 
  ) +
  labs(
    title = "Speed on I-80 (3, 15-min Time Periods)",
    subtitle = "April 13, 2005",
    x = "Time Period",
    y = "Speed (ft/s)",
    caption = "Source: NGSIM Project",
    color = "Time Period"
  )


## Change theme
ggplot(
  data = i_80_summary
) +
  geom_boxplot(
    mapping = aes(
      x = as.factor(period), 
      ymin   = min_vel,      
      lower  = percentile_25,
      middle = median_vel,    
      upper  = percentile_75,  
      ymax   = max_vel,   
      color = as.factor(period)     
    ),
    stat = "identity" 
  ) +
  labs(
    title = "Speed on I-80 (3, 15-min Time Periods)",
    subtitle = "April 13, 2005",
    x = "Time Period",
    y = "Speed (ft/s)",
    caption = "Source: NGSIM Project",
    color = "Time Period"
  ) +
  theme_classic()


## Add facets
### Using all periods and locations
plot_speeds <- ggplot(
  data = summary_stats_speed
) +
  geom_boxplot(
    mapping = aes(
      x = as.factor(period), 
      ymin   = min_vel,      
      lower  = percentile_25,
      middle = median_vel,    
      upper  = percentile_75,  
      ymax   = max_vel,   
      color = location     
    ),
    stat = "identity" 
  ) +
  facet_wrap(~ location) +
  labs(
    title = "Speed on I-80 (3, 15-min Time Periods)",
    subtitle = "April 13, 2005",
    x = "Time Period",
    y = "Speed (ft/s)",
    caption = "Source: NGSIM Project",
    color = "Time Period"
  ) +
  theme_light()

plot_speeds


## Increase font size
plot_speeds +
  theme(strip.text = element_text(size = 14))



###################################################################
# YOUR TURN
## Explore and try different themes
###################################################################


# Remove undesired columns -------------
ngsim_arrow <- ngsim_arrow |> 
  select(-c(
    total_frames, global_x, global_y,
    o_zone, d_zone, int_id, section_id,
    direction, movement
  ))


# Change data types and convert to metric units ----------------
ngsim_arrow <- ngsim_arrow |> 
  mutate(
    across(
      .cols = c(vehicle_id, v_class, lane_id, preceding, following, period), 
      .fns = ~ as.character(.x)
    ),
    across(
      .cols = where(is.double), 
      .fns = ~ round(.x * .3048, 2)
    )
  )



# Create variables for preceding vehicle ---------
ngsim_arrow <- ngsim_arrow |> 
  left_join(
    ngsim_arrow |> 
      select(
        location, period, frame_id, 
        preceding = vehicle_id, 
        p_local_y = local_y,
        p_local_x = local_x,
        p_v_length = v_length,
        p_v_width = v_width,
        p_v_class = v_class,
        p_v_vel = v_vel,
        p_v_acc = v_acc
      ),
    by = c("location", "period", "frame_id", "preceding")
  )


# Sort by vehicle id and frame id ----------
ngsim_arrow <- ngsim_arrow |> 
  group_by(location, period) |> 
  arrange(vehicle_id, frame_id) |> 
  ungroup()




# Save -------------------------------------
ngsim_arrow |> 
  group_by(location, period) |>
  write_dataset(path = "data/ngsim_tmp", format = "parquet")

## Replace the previous folder
unlink("data/ngsim", recursive = TRUE)
file.rename("data/ngsim_tmp", "data/ngsim")




