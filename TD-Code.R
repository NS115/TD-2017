## Select data ##
sea_ice_extent <- read.csv(".../Ross Sea Ice Extent.csv")
colony_size <- read.csv(".../Cape Bird Middle.csv")

## Remove missing values ##
is.na(colony_size) # Check for missing values
new_colony_size = na.omit(colony_size)

## Figure 1 ##
par(mar=c(5,4,4,5)+.1)
plot(new_colony_size$Colony.Size ~ new_colony_size$Year, 
     type = "l", 
     col = "red", 
     xlab="Year", 
     ylab= "Colony Size / N° individuals", 
     lwd = 2,
     main = "Evolution of Cape Bird Adélie Penguin Colony Size between 1983 and 2012", 
     cex.main = 1.5)

# adding a secondary y axis
par(new = TRUE) 
plot(sea_ice_extent$Extent ~ colony_size$Year, 
     type = "l", 
     col="blue", 
     xaxt="n", 
     yaxt="n", 
     xlab="", 
     ylab= "", 
     lwd = 2)

axis(4)
mtext("Sea Ice Extent / million square km",side=4,line=3)


# adding a legend
legend("topleft",
       col=c("red","blue"),
       lty=1,legend=c("Colony Size","Sea Ice Extent"), 
       cex = 0.60)


## Correlation Test ##

# Compute rho value 
cor (colony_size$Colony.Size, 
     sea_ice_extent$Extent, 
     use = "pairwise.complete.obs",
     method = "spearman") 

# Compute rho and p-value
cor.test (sea_ice_extent$Extent, 
          colony_size$Colony.Size, 
          alternative = "greater", 
          method = "spearman", 
          exact = NULL)


## Plot 2 ##

ice = sea_ice_extent$Extent
colony = colony_size$Colony.Size

plot (ice,colony, 
      xlab = "Sea ice extent / million square km", 
      ylab = "Colony size / N° individuals", 
      pch = 8, 
      main = "Correlation between sea ice extent and Adélie penguin colony size on Cape Bird", 
      cex.main = 1.5)

# Customise point
points(ice, y = colony, 
       type = "p", 
       pch = 8, 
       col = "sienna1") 

# Add a trendline
abline(lm(colony ~ ice), 
       col = "black", 
       lwd = 2)



