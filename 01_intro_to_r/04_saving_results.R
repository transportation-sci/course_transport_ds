library(tidyverse)
library(arrow)
library(patchwork)
library(officer)
library(flextable)
library(rvg)

df <- read_parquet("data/i80/i80_cf.parquet")


# Save plots as images ----------------------------------------------------

## Let's use our previous function to create plots for a single pair and then store it on disk:
plot_multiple_vars_per_pair <- function(dataset){
  
  # Combine multiple plots for 1 pair 
  
  plot_speed <- ggplot(data = dataset) +
    geom_line(mapping = aes(x = time, y = v_vel, color = "Subject Vehicle")) +
    geom_line(mapping = aes(x = time, y = p_v_vel, color = "Preceding Vehicle")) +
    scale_color_manual(values = c("grey50", "blue")) +
    labs(x = "Time (s)",
         y = "Speed (m/s)") +
    theme_classic()
  
  
  
  plot_accel <- ggplot(data = dataset) +
    geom_line(mapping = aes(x = time, y = v_acc, color = "Subject Vehicle")) +
    geom_line(mapping = aes(x = time, y = p_v_acc, color = "Preceding Vehicle")) +
    scale_color_manual(values = c("grey50", "blue")) +
    labs(x = "Time (s)",
         y = "Acceleration (m/s^2)") +
    theme_classic()
  
  
  plot_spacing <- ggplot(data = dataset) +
    geom_line(mapping = aes(x = time, y = space_headway)) +
    labs(x = "Time (s)",
         y = "Spacing (m)") +
    theme_classic()
  
  
  
  ### Combine plots
  (plot_speed / plot_accel + plot_layout(guides = "collect")) / plot_spacing
  
}


df_one <- df |> filter(vehicle_id == "101")


final_plot <- plot_multiple_vars_per_pair(df_one)



# Save table to a Word document -------------------------------------------

## Create a table of results:

results_table <- df |> 
  select(v_class, v_vel, v_acc, space_headway, p_v_vel, p_v_acc) |> 
  group_by(v_class) |> 
  summarise(across(.cols = where(is.numeric), .fns = median))



## Make it pretty:
flextable(results_table) 


my_ft <- flextable(results_table) |> 
  theme_vanilla()

my_ft


my_ft <- my_ft |> 
  set_header_labels(
    v_class = "Class",
    v_vel = "Speed (m/s)",
    v_acc = "Acceleration (m/s^2)",
    space_headway = "Spacing (m)",
    p_v_vel = "Speed (m/s)",
    p_v_acc = "Acceleration (m/s^2)"
  ) |> 
  italic(j = 1) |> 
  color(~ p_v_acc < 0.00, ~ p_v_acc, color = "red") |> 
  bold(~ p_v_acc < 0.00, ~ p_v_acc, bold = TRUE) |> 
  add_header_row(
    values = c("Subject Vehicle", "Preceding Vehicle"),
    colwidths = c(4, 2)
    ) |> 
  align(i = 1, part = "header", align = "center")


### save to a Word doc:
my_ft |>  
  save_as_docx(path = "01_intro_to_r/results/veh_table.docx")






# Send table and plot to a PowerPoint presentation -------------------------------------------

final_plot

my_vec_graph <- rvg::dml(ggobj = final_plot)

doc <- officer::read_pptx()

doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")

doc <- ph_with(doc, my_vec_graph, location = ph_location_fullsize())

doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")

doc <- ph_with(doc, my_ft, location = ph_location_left())

print(doc, target = "01_intro_to_r/results/presentation_res.pptx")
