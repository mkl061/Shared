# Packages:
if (!require("tidyverse", quietly = T)) {
  install.packages("tidyverse", quiet = T)
} # dplyr, ggplot2, and magrittr



# Function to generate a data frame:
gen_df <- function(trees_pr_plot=9, base_mean=9, SD=1, altered=1) {
  
  # Custom function to give a normal distributed value:
  rand_norm <- function(mean=base_mean) {
    return(rnorm(trees_pr_plot, mean, SD))
  }
  
  # Increased mean values after growth period:
  l <- 4 # Low N
  m <- 6 # Medium N
  h <- 8 # High N
  
  # Data frame
  df <- cbind.data.frame(
    "Nitrogen" = c(
      rep("Low", 2*trees_pr_plot), 
      rep("Medium", 2*trees_pr_plot), 
      rep("High", 2*trees_pr_plot)),
    "Density" =
      rep(c(rep("Low", trees_pr_plot), rep("High", trees_pr_plot)), 3),
    "Start" = rep(rand_norm(), 6),
    "End" = c(rand_norm(base_mean+l),
              rand_norm(base_mean+l-2),
              rand_norm(base_mean+m),
              rand_norm(base_mean+m-2),
              rand_norm(base_mean+h),
              rand_norm(base_mean+h-2)
    )
  ) %>% 
    mutate(Growth = End-Start) %>%  # Growth. Growth rate = growth / year
    select(-Start, -End)
    
  # Simulate some set differences in the data:
  df <- df %>% 
    mutate(Growth = Growth*altered) # The "altered" input argument is a percent!
  
  return(df)
}



# Function to plot the data:
plot_df <- function(df, by_block=F) {
  df %>% 
    ggplot(aes(x=reorder(Nitrogen, Growth), y=Growth, fill=Density)) + 
    {if(by_block)facet_wrap(~greenhouse)}+
    geom_boxplot()+
    xlab("Nitrogen content in soil") +
    ylab("Growth (kilo grams)")+ 
    theme(text = element_text(size = 20))  
}



# Set seed, to get the same result every time, even though the data is
# created with a random element:
set.seed(1)


# Generate data for the three greenhouses:
df1 <- gen_df() %>% mutate(greenhouse = "greenhouse 1") # 100% 
df2 <- gen_df(altered = 1.2) %>% mutate(greenhouse = "greenhouse 2") # 120% 
df3 <- gen_df(altered = 0.8) %>% mutate(greenhouse = "greenhouse 3") # 80%

# Bind them all in a single data frame:
df_fin <- bind_rows(df1, df2, df3) %>%
  group_by(greenhouse)

# Plot data:
plot_df(df_fin, by_block = F) # Not taking blocks into account
plot_df(df_fin, by_block = T) # Taking blocks into account


model <- aov(data = df_fin,
         formula = Growth~Nitrogen*Density)
summary(model)

hist(a$residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "steelblue")
