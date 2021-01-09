K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()

#One way of getting around the limitations of quadrat analysis is to compare the observed distribution of points with the Poisson random model for a whole range of different distance radii. This is what Ripley’s K function computes.

#We can conduct a Ripley’s K test on our data very simply with the spatstat package using the kest() function.

#The plot for K has a number of elements that are worth explaining. First, the Kpois(r) line in Red is the theoretical value of K for each distance window (r) under a Poisson assumption of Complete Spatial Randomness. The Black line is the estimated values of K accounting for the effects of the edge of the study area.

#Where the value of K falls above the line, the data appear to be clustered at that distance. Where the value of K is below the line, the data are dispersed. From the graph, we can see that up until distances of around 1300 metres, Blue Plaques appear to be clustered in Harrow, however, at around 1500 m, the distribution appears random and then dispersed between about 1600 and 2100 metres.