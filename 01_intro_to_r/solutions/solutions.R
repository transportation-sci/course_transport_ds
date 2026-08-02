# Solutions -----------
library(tidyverse)
library(arrow)

## Create a space-time diagram for I-80, period 3 (lanes: 1-3)
i80_filtered <- read_parquet("data/i80/i80.parquet") |>
  filter(
    period == "3",
    lane_id %in% c("1", "2", "3")
  ) 

time_space_diagram_i80 <- i80_filtered |> 
  ggplot() +
  geom_point(
    aes(
      x = date_time,
      y = local_y,
      color = v_vel,
      group = vehicle_id
    ),
    alpha = 0.5
  ) +
  scale_color_gradient(low = "red", high = "green") +
  facet_grid(lane_id ~ ., labeller = "label_both") +
  labs(
    x = "Time",
    y = "Location",
    color = "Speed (m/s)"
  ) +
  theme_minimal()

time_space_diagram_i80

ggsave(plot = time_space_diagram_i80,
       filename = "01_intro_to_r/results/plot_ts.png",
       width = 11,
       height = 8,
       units = "in",
       dpi = 600)
