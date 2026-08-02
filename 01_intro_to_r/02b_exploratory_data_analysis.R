#################
# I-80 only
#################

# Load libraries ----------------------------
library(tidyverse)
library(arrow)
library(plotly)
library(patchwork)
library(trelliscope)


df_i80 <- open_dataset("data/ngsim") |> 
  filter(location == "i-80") |> 
  collect()

# Convert integer time to datetime ---------
df_i80 <- df_i80 |> 
  group_by(period) |> 
  arrange(vehicle_id, frame_id) |> 
  ungroup() |> 
  mutate(date_time = as_datetime(global_time / 1000,
    origin = "1970-01-01",
    tz = "America/Los_Angeles"
  ))


# Q: Num of lanes ---------
unique(df_i80$lane_id)


# Keep only car-following vehicles -----------
## Lane 1 is farthest left lane; lane 5 is farthest right lane.
df_i80c <- df_i80 |> 
  filter(lane_id %in% c("1", "2", "3"),
                preceding != "0")  



# Create pair id, find car-following duration, filter out long time headway -----
df_i80c <- df_i80c |> 
  group_by(period, vehicle_id) |> 
  mutate(num_of_lanes = length(unique(lane_id))) |> # find if a vehicle changed lane and how many times
  ungroup() |> 
  filter(num_of_lanes == 1) |>  # remove any lane-changing vehicles
  select(-num_of_lanes) |> 
  mutate(pair_id = paste0(period, "-", vehicle_id, "-", preceding)) |>  # create a pair_id column
  group_by(pair_id) |> 
  mutate(time = (0:(n()-1))/10, # elapsed time
         duration = tail(time, 1), # car-following pair was observed for this duration
         any_time_headway_gtoet_5 = any(time_headway >= 5)) |> # check if time headway was greater than or equal to 5 sec at any instant
  ungroup() |> 
  filter(duration >= 30,
         any_time_headway_gtoet_5 == FALSE) |> # remove any vehicle that had time headway greater than or equal to 5 sec at any instant
  select(-any_time_headway_gtoet_5) 


length(unique(df_i80c$pair_id)) # 1047 pairs

## Save:
dir.create("data/i80")
write_parquet(x = df_i80c, sink = "data/i80/i80_cf.parquet")
write_parquet(x = df_i80, sink = "data/i80/i80.parquet")

## Distribution of duration:
plot_dur <- ggplot(df_i80c) +
  geom_histogram(aes(x = duration))

### Make the plot interactive:
ggplotly(plot_dur)



# Plot 1 pair -------------------
df_one <- df_i80c |> 
  filter(pair_id == "1-1003-996")


speed <- ggplot(data = df_one) +
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = p_v_vel, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  labs(title = "Vehicle 1003",
       x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()



accel <- ggplot(data = df_one) +
  geom_line(mapping = aes(x = time, y = v_acc, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = p_v_acc, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  labs(x = "Time (s)",
       y = "Acceleration (m/s^2)") +
  theme_classic()


spacing <- ggplot(data = df_one) +
  geom_line(mapping = aes(x = time, y = space_headway)) +
  labs(x = "Time (s)",
       y = "Spacing (m)") +
  theme_classic()


## Combine all plots
speed / accel / spacing

### Combine legends
plot_combined <- (speed / accel + plot_layout(guides = "collect")) / spacing

### Save the plot
ggsave(plot = plot_combined,
       filename = "01_intro_to_r/results/plot_combined.png",
       width = 11,
       height = 8,
       units = "in",
       dpi = 300)


###############################################
# YOUR TURN
# Explore the patchwork pkg and create 
# multiple layouts
###############################################



# Visualizing all data ----------------------------------------------------

length(unique(df_i80c$pair_id)) # 1047

## Speed

ggplot(data = df_i80c) + # <- changed data
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = p_v_vel, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  facet_wrap(~ pair_id) + # <- added facets
  labs(x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()



p <- ggplot(data = df_i80c) +
  geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
  geom_line(mapping = aes(x = time, y = p_v_vel, color = "Preceding Vehicle")) +
  scale_color_manual(values = c("grey50", "blue")) +
  facet_panels(vars(pair_id)) + # <- trelliscope facets
  labs(x = "Time (s)",
       y = "Speed (m/s)") +
  theme_classic()


p_df <- as_panels_df(p)
tdf <- as_trelliscope_df(p_df, name = "I-80 Speeds", path = "01_intro_to_r/results/speed_plots")
disp <- write_trelliscope(tdf)
view_trelliscope(disp)



##################################################################
# CHALLENGE
# Create a space-time diagram for I-80, period 3 (lanes: 1-3)
# Hint: Map date_time to x-axis and local_y to y-axis
##################################################################
