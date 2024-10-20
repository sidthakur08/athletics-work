pacman::p_load(dplyr, data.table, ggplot2, plotly, purrr, signal, gridExtra, rgl, MASS, reshape2, geometry)

# Filtering using low-pass butterworth filter ----------------------------------
# filter function
filter_channels <- function(channel, cutoff){
  
  if(is.na(channel[1])){
    channel[1] = channel[2]
  }
  
  if(is.na(tail(channel, n=1))) {
    tail(channel, n=1) = tail(channel, n=2)[1]
  }
  
  
  # filter 
  bf <- signal::butter(2, cutoff/(300/2))
  
  fake = round(length(channel) * .2)
  
  # reflect the ends of the data to prevent boundary effects from filter
  reflected_col =  c(rev(channel[1:fake]), channel, rev(tail(channel, n=fake)))
  
  # filter the column vector
  filtered_col = signal::filtfilt(bf, reflected_col)
  
  #filter force plate channel
  filtered_signal = filtered_col[-c(1:fake, (length(filtered_col)-(fake-1)):length(filtered_col))]
  
  #replace unfiltered channel for filtered
  channel = filtered_signal
  
  return(channel)
}
# END --------------------------------------------------------------------------

swings <- read.csv("Sport Science Analyst Exercise.csv")

colSums(is.na(swings))

swings <- swings %>%
  group_by(player) %>%
  mutate(
    time_index = row_number(),
    bat.speed = possibly(filter_channels, otherwise = NaN)(bat.speed, 35),
    hand.speed = possibly(filter_channels, otherwise = NaN)(hand.speed, 35),
    attack.angle = possibly(filter_channels, otherwise = NaN)(attack.angle, 35),
    vertical.bat.angle = possibly(filter_channels, otherwise = NaN)(vertical.bat.angle, 35),
    x_barrel = possibly(filter_channels, otherwise = NaN)(x_barrel, 35),
    y_barrel = possibly(filter_channels, otherwise = NaN)(y_barrel, 35),
    z_barrel = possibly(filter_channels, otherwise = NaN)(z_barrel, 35),
    flag = ifelse(row_number() >= which.max(bat.speed), 1, 0)
    ) %>% ungroup()

write.csv(swings,"filtered_swings.csv", row.names = FALSE)

batSpeed_plot <- ggplot(swings, aes(x = time_index, y=bat.speed, color=player)) +
  geom_line(data = swings[swings['flag']==0,],  linetype = 'solid') +
  geom_line(data = swings[swings['flag']==1,],  linetype = 'dashed') +
  labs(title = "Bat Speed Comparison Between Players", x = "Time Index", y = "Bat Speed") +
  scale_color_manual(values = c("Player 1" = "blue", "Player 2" = "red")) +
  theme_minimal()
batSpeed_plot

vba_plot <- ggplot(swings, aes(x = time_index, y=vertical.bat.angle, color=player)) +
  geom_line(data = swings[swings['flag']==0,],  linetype = 'solid') +
  geom_line(data = swings[swings['flag']==1,],  linetype = 'dashed') +
  labs(title = "Vertical Bat Angle Comparison Between Players", x = "Time Index", y = "Vertical Bat Angle") +
  scale_color_manual(values = c("Player 1" = "blue", "Player 2" = "red")) +
  theme_minimal()
vba_plot

attackAngle_plot <- ggplot(swings, aes(x = time_index, y=attack.angle, color=player)) +
  geom_line(data = swings[swings['flag']==0,],  linetype = 'solid') +
  geom_line(data = swings[swings['flag']==1,],  linetype = 'dashed') +
  labs(title = "Attack Angle Speed Comparison Between Players", x = "Time Index", y = "Attack Angle") +
  scale_color_manual(values = c("Player 1" = "blue", "Player 2" = "red")) +
  theme_minimal()
attackAngle_plot

handSpeed_plot <- ggplot(swings, aes(x = time_index, y=hand.speed, color=player)) +
  geom_line(data = swings[swings['flag']==0,],  linetype = 'solid') +
  geom_line(data = swings[swings['flag']==1,],  linetype = 'dashed') +
  labs(title = "Hand Speed Comparison Between Players", x = "Time Index", y = "Hand Speed") +
  scale_color_manual(values = c("Player 1" = "blue", "Player 2" = "red")) +
  theme_minimal()
handSpeed_plot

# Create a bird's eye view plot of the hitter's swing using x, y coordinates of barrel
bird_eye_view <- ggplot(swings) +
  geom_path(data = swings[swings['flag']==0,], aes(x = x_barrel, y = y_barrel, color = player), size=1, alpha=0.7) +
  geom_path(data = swings[swings['flag']==1,], aes(x = x_barrel, y = y_barrel, color = player), size=1, alpha=0.7, linetype = 'dashed') +
  labs(title = "Bird's Eye View of Hitter's Swing", x = "X Coordinate", y = "Y Coordinate") +
  scale_color_manual(values = c("Player 1" = "blue", "Player 2" = "red")) +
  xlim(-1.5, 1.5) +
  ylim(-1, 1) +
  theme_minimal()

# Create a side view plot of the hitter's swing using y, z coordinates of barrel
first_base_view <- ggplot(swings) +
  geom_path(data = swings[swings['flag']==0,], aes(x = y_barrel, y = z_barrel, color = player), size=1, alpha=0.7) +
  geom_path(data = swings[swings['flag']==1,], aes(x = y_barrel, y = z_barrel, color = player), size=1, alpha=0.7, linetype = 'dashed') +
  labs(title = "Side View of Hitter's Swing", x = "Y Coordinate", y = "Z Coordinate") +
  scale_color_manual(values = c("Player 1" = "blue", "Player 2" = "red")) +
  xlim(-1, 1) +
  ylim(-1, 2) +
  theme_minimal()

# # Create a catcher view plot of the hitter's swing using z, x coordinates of barrel
# catcher_view <- ggplot(swings) +
#   geom_path(aes(x = z_barrel, y = x_barrel, color = player), size = 1, alpha = 0.7) +
#   labs(title = "Catcher View of Hitter's Swing", x = "X Coordinate", y = "Y Coordinate") +
#   scale_color_manual(values = c("Player 1" = "blue", "Player 2" = "red")) +
#   xlim(-1.5, 2) +
#   ylim(-1.5, 1.5) +
#   theme_minimal()

grid.arrange(bird_eye_view, first_base_view, ncol = 2)

# Visualization 1: 3D Swing Path
swing1 <- swings %>% dplyr::filter(player == "Player 1")
plot3d(swing1$x_barrel, swing1$y_barrel, swing1$z_barrel, type = 'l', col = 'blue',
       xlab = 'X Position', ylab = 'Y Position', zlab = 'Z Position',
       main = '3D Swing Path')

# Create vectors that interleave barrel and knob coordinates with NA to separate segments
x_coords <- c(rbind(swing1$x_barrel, swing1$x_knob, NA))
y_coords <- c(rbind(swing1$y_barrel, swing1$y_knob, NA))
z_coords <- c(rbind(swing1$z_barrel, swing1$z_knob, NA))

# Plot the line segments connecting barrel and knob at each time stamp
p <- plot_ly(type = 'scatter3d', mode = 'lines') %>%
  add_trace(
    x = x_coords,
    y = y_coords,
    z = z_coords,
    line = list(color = 'blue', width = 2),
    showlegend = FALSE
  )

# Add markers for the barrel positions over time
p <- p %>% add_trace(
  x = swing1$x_barrel,
  y = swing1$y_barrel,
  z = swing1$z_barrel,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 3, color = 'red'),
  name = 'Barrel'
)

# Add markers for the knob positions over time
p <- p %>% add_trace(
  x = swing1$x_knob,
  y = swing1$y_knob,
  z = swing1$z_knob,
  type = 'scatter3d',
  mode = 'markers',
  marker = list(size = 3, color = 'green'),
  name = 'Knob'
)

# Display the plot
p
