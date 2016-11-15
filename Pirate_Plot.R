#install.packages("devtools")
#install_github("ndphillips/yarrr")
library("devtools")
library("yarrr")

# We have the pirate dataset comming with:
head(pirates)
pirateplot(formula = age ~ favorite.pirate, 
           data = pirates, 
           xlab = "Favorite Pirate", 
           ylab = "Age", 
           main = "")

pirateplot(formula = beard.length ~ sex + college, data = pirates, main = "Beard lengths", xlab = "", ylab = "Beard Length")

pirateplot(formula = age ~ favorite.pirate, data = pirates, xlab = "Favorite Pirate", ylab = "Age", main = "",
           theme.o = 1)
pirateplot(formula = age ~ favorite.pirate, data = pirates, xlab = "Favorite Pirate", ylab = "Age", main = "",
           theme.o = 2)
pirateplot(formula = age ~ favorite.pirate, data = pirates, xlab = "Favorite Pirate", ylab = "Age", main = "",
           theme.o = 3)

piratepal(palette= "all", action = "show")

pirateplot(formula = age ~ favorite.pirate, data = pirates, xlab = "Favorite Pirate", ylab = "Age", main = "",
           
           # Choose your color palette, or give common color vector
           pal = "drugs",
           
           # Set transparency of the elements:
           line.o = 0.1,
           bar.o = .1,
           bean.o = .3,
           point.o = .9,
           
           # Shape of point
           point.pch = 2,
           
           #Background color
           back.col = "white" )
