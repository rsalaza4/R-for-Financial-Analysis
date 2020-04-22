### BREAK EVEN POINT ###

# Set unitary price
price <- as.numeric(readline(prompt = "Type in the unitary price: "))
while(price <= 0){
  price <- as.numeric(readline(prompt = "Error! The price must be greater than 0: "))
}

# Set unitary variable cost
individual_variable_cost <- as.numeric(readline(prompt = "Type in the individual variable cost: "))
while(individual_variable_cost <= 0){
  individual_variable_cost <- as.numeric(readline(prompt = "Error! The individual variable cost must be greater than 0: "))
}

# Create units column
units <- c(seq(0, 20000, length.out = 11))

# Create fixed cost column
fixed_cost <- as.numeric(readline(prompt = "Type in fixed costs: "))
while(fixed_cost <= 0){
  fixed_cost <- as.numeric(readline(prompt = "Error! The fixed costs must be greater than 0: "))
}
fixed_cost <- c(rep(fixed_cost, length(units)))

# Create variable cost column
variable_cost <- individual_variable_cost*units

# Create total cost column
total_cost <- fixed_cost + individual_variable_cost*units

# Create revenue column
revenue <- price*units

# Build data frame
df <- data.frame(units, fixed_cost, variable_cost, total_cost, revenue)

# Calculate contribution margin
contribution_margin <- price-individual_variable_cost

# Obtain break even units 
break_even_units <- fixed_cost[1]/contribution_margin

# Import ggplot2 package
library(ggplot2)

# Build Break Even Point Graph
ggplot(data = df, aes(x = units)) +
  geom_line(aes(y = fixed_cost,
                col = "Fixed Cost")) +
  geom_line(aes(y = variable_cost,
                col = "Variable Cost")) +
  geom_line(aes(y = total_cost,
                col = "Total Cost")) +
  geom_line(aes(y = revenue,
                col = "Revenue")) +
  geom_segment(aes(x = break_even_units, xend = break_even_units, 
                   y = 0, yend = break_even_units*price),
               linetype = "dashed") +
  geom_segment(aes(x = 0, xend = break_even_units, 
                   y = break_even_units*price, yend = break_even_units*price),
               linetype = "dashed") +
  geom_point(aes(x = break_even_units, 
                 y= break_even_units*price), 
             colour = "black", size = 4) +
  annotate("text", x = break_even_units, y = 0, label = paste("Break Even Point:", break_even_units)) +
  scale_color_manual(labels = c("Fixed Cost", "Revenue", "Total Cost", "Variable Cost"),
                     values = c("Fixed Cost" = "black", "Variable Cost" = "blue", "Total Cost" = "red", "Revenue" = "green")) +
  geom_ribbon(data = df[df$total_cost >= df$revenue, ], aes(x = units, ymin = revenue, ymax = total_cost), fill = "red", alpha = 0.15) +
  geom_ribbon(data = df[df$total_cost <= df$revenue, ], aes(x = units, ymin = total_cost, ymax = revenue), fill = "green", alpha = 0.15) +
  labs(title = "Break Even Point Graph",
       subtitle = "Company Name",
       x = "Units",
       y = "Dollars",
       caption = "Break Even Analysis",
       color = NULL)

# EXAMPLE

price <- 12
individual_variable_cost <- 2

units <- c(seq(0, 20000, length.out = 11))
fixed_cost <- c(rep(100000, length(units)))
variable_cost <- individual_variable_cost*units
total_cost <- fixed_cost + individual_variable_cost*units
revenue <- price*units

df <- data.frame(units, fixed_cost, variable_cost, total_cost, revenue)

contribution_margin <- price-individual_variable_cost

break_even_units <- fixed_cost[1]/contribution_margin

library(ggplot2)

ggplot(data = df, aes(x = units)) +
  geom_line(aes(y = fixed_cost,
                col = "Fixed Cost")) +
  geom_line(aes(y = variable_cost,
                col = "Variable Cost")) +
  geom_line(aes(y = total_cost,
                col = "Total Cost")) +
  geom_line(aes(y = revenue,
                col = "Revenue")) +
  geom_segment(aes(x = break_even_units, xend = break_even_units, 
                   y = 0, yend = break_even_units*price),
                   linetype = "dashed") +
  geom_segment(aes(x = 0, xend = break_even_units, 
                   y = break_even_units*price, yend = break_even_units*price),
               linetype = "dashed") +
  geom_point(aes(x = break_even_units, 
                 y= break_even_units*price), 
             colour = "black", size = 4) +
  annotate("text", x = break_even_units, y = 0, label = paste("Break Even Point:", break_even_units)) +
  scale_color_manual(labels = c("Fixed Cost", "Revenue", "Total Cost", "Variable Cost"),
                     values = c("Fixed Cost" = "black", "Variable Cost" = "blue", "Total Cost" = "red", "Revenue" = "green")) +
  geom_ribbon(data = df[df$total_cost >= df$revenue, ], aes(x = units, ymin = revenue, ymax = total_cost), fill = "red", alpha = 0.15) +
  geom_ribbon(data = df[df$total_cost <= df$revenue, ], aes(x = units, ymin = total_cost, ymax = revenue), fill = "green", alpha = 0.15) +
  labs(title = "Break Even Point Graph",
       subtitle = "Company Name",
       x = "Units",
       y = "Dollars",
       caption = "Break Even Analysis",
       color = NULL)
