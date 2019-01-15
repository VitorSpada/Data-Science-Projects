# Check out the head of sleep_sim
head(sleep_sim)

# Store the chains in a data frame
sleep_chains <- data.frame(sleep_sim[[1]], iter = 1:10000)

# Check out the head of sleep_chains
head(sleep_chains)


library("ggplot2")





# Use plot() to construct trace plots of the m and s chains
plot(sleep_sim, density=FALSE)

#plot of the m chain
ggplot(sleep_chains , aes(x = iter, y = m)) + 
  geom_line()# Use ggplot() to construct a trace 

# Trace plot the first 100 iterations of the m chain
ggplot(sleep_chains[1:1000, ], aes(x = iter, y = m)) + 
  geom_line()