library(grid)
library(ggguitar)

# Define the chords
G_M <- c(3, 2, 0, 0, 0, 3)
D_M <- c(NA, NA, 0, 2, 3, 2)
C_M <- c(NA, 3, 2, 0, 1, 0)
D_7 <- c(NA, NA, 0, 2, 1, 2)
A_7 <- c(NA, 0, 2, 0, 2, 0)
e_m <- c(0, 2, 2, 0, 0, 0)

# Generate the tab charts
GM <- tablature('G', G_M, FALSE, FALSE)
CM <- tablature('C', C_M, FALSE, FALSE)
DM <- tablature('D', D_M, FALSE, FALSE)
em <- tablature('em', e_m, FALSE, FALSE)
D7 <- tablature('D7', D_7, FALSE, FALSE)
A7 <- tablature('A7', A_7, FALSE, FALSE)

# Arrange the tab together into a single image
blank<-grid.rect(gp=gpar(col="white"))
gridExtra::grid.arrange(GM, blank, blank, blank, DM, blank, GM, blank,
                        CM, blank, GM, blank, CM, blank, GM, blank,
                        D7, blank, em, A7, GM, D7, GM, blank,
                        ncol = 8,
                        top=textGrob("Stille Nacht", gp=gpar(fontsize=15,font=8)))


library(ggguitar)
C_M <- 0="" 1="" 2="" 3="" b="" c="">
    tablature('C Major', C_M)

tablature('C Major', C_M, FALSE, FALSE)

