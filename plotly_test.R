library(plotly)
library(dplyr)

# Dataset for creating chord diagram ------------------------------------------
# Data for number of facebook posts
# See https://plot.ly/python/filled-chord-diagram/

df <- rbind(c(16, 3, 28, 0, 18),
            c(18, 0, 12, 5, 29),
            c(9, 11, 17, 27, 0),
            c(19, 0, 31, 11, 12),
            c(23, 17, 10, 0, 34))
df <- data.frame(df)

colnames(df) <- c('Emma', 'Isabella', 'Ava', 'Olivia', 'Sophia')
rownames(df) <- c('Emma', 'Isabella', 'Ava', 'Olivia', 'Sophia')


# Settings --------------------------------------------------------------------
# Over all plot settings like color and transparency

cols <- RColorBrewer::brewer.pal(nrow(df), "Set1")  # Set of colors (n = number of rows in data)
opacity <- 0.5  # Opacity of ideogram
chord.opacity <- 0.3  # Opcaity of individual chords
linecolor <- "black"
circlefill <- "#f2f2f2"
inner.radius <- 0.93
gap <- 0.02


# Function Definition: addGaps() ----------------------------------------------
addGaps <- function(theta, gap = 0.05){
    
    # Takes a vector of angles and adds a gap in-between them
    # Adds and subtracts the gap value from computed angle
    
    newtheta <- data.frame()
    
    for (i in 1:length(theta)) {
        
        if(i == 1){
            x <- 0 + gap
            y <- theta[i] - gap
            newtheta <- rbind(newtheta, c(x, y))
        }else{
            x <- theta[i - 1] + gap
            y <- theta[i] - gap
            newtheta <- rbind(newtheta, c(x, y))
        }
    }
    
    newtheta <- data.frame(theta, newtheta)
    colnames(newtheta) <- c("theta", "start", "end")
    
    return(newtheta)
}

# Function Definition: toAngular() --------------------------------------------
toAngular <- function(x, rad = 1, gap = 0.05, lower = 0, upper = 2*pi, addgaps = T){
    
    # Maps a set of numbers onto the unit circle by computing cumulative
    # sums and assigning angles to each sum
    
    cumtotals <- cumsum(x / sum(x))
    
    # Upper and lower bounds are the angle limits to which mapping
    # is limited. Ex 0 - 2PI
    delta <- ifelse(upper > lower, upper - lower, (2*pi) - lower + upper)
    theta <-  cumtotals * delta
    
    x <- rad * cos(theta)
    y <- rad * sin(theta)
    
    df <- data.frame(x, y)
    
    # Additionally, add gaps in between each sector using the addGaps() function
    if (addgaps == T) {
        gaps <- addGaps(theta, gap = gap)
        ret <- list(theta = theta,
                    coord = df,
                    gaps = gaps)
    }else{
        ret <- list(theta = theta,
                    coord = df)
    }
    
    return(ret)
}



# Create ideogram -------------------------------------------------------------
# See See https://plot.ly/python/filled-chord-diagram/

# The ideogram is constructed of the row sums i.e total interactions in each row
dat <- rowSums(df)

# Outer ring is the unit circle
outer <- toAngular(dat, gap = gap)

# Inner ring has radius < 1
inner <- toAngular(dat, rad = inner.radius, gap = gap)

# Ideogram is charted as a svg path and fed to plot_ly() as a shape
# Compute a path for each sector of the ideogram by combining the 
# coordinates of the outer and inner circles
outer.inner <- rbind(outer$gaps, inner$gaps) %>% arrange(theta)  # arrange in increasing order of theta

# Each sector of the ideogram is made of four points - 
# the start and end points of the outer and inner circles
# Hence increment by 2 and not 1
vec <- seq(1, nrow(outer.inner), by = 2)

# Create and empty dataframe
ideogram <- data.frame()

k <- 1  # Counter for each row / group

# Loop through each sector and create a svg path using the start and end
# points of the outer and inner circles
for (i in vec) {
    
    # Get starting and ending point for 'i' th sector
    start <- outer.inner$start[i]
    end <- outer.inner$end[i]
    
    # Ensure starting point is always less than ending point of sector
    if (start > end) start <- start - (2*pi)
    
    # Create a sequence of thetas along the sector
    thetas <- seq(start, end, length.out = 100)
    
    # Compute x and y coordinates
    x <- c(cos(thetas), inner.radius * cos(rev(thetas)))
    y <- c(sin(thetas), inner.radius * sin(rev(thetas)))
    
    # Add a group for easy subsetting later on
    coords <- data.frame(x, y, group = k)
    ideogram <- rbind(ideogram, coords)
    
    # Increment group number
    k <- k + 1
}

# Function definition: createPath() -------------------------------------------
createPath <- function(df){
    
    # Given x and y coordinates creates a string containing a svg path
    # that can be fed to plotly as a shape
    
    start <- paste("M", df$x[1], df$y[1])
    path <- paste("L", df$x[-1], df$y[-1], collapse = " ")
    path <- paste(start, path, "Z")
    return(path)
}

# Use group numbers assigned to each sector to subset and create a path string
ideogram.path <- by(ideogram, ideogram$group, createPath)

# Plot the ideogram (just as a check). Chord diagram is generated separately later
# Create shape list
ideogram.shapes <- list()  # Used later on

for (i in 1:nrow(df)) {
    
    # Use plotly syntax to save shapes of each sector as a list
    ideogram.shapes[[i]] <- list(type = "path",
                                 path = ideogram.path[i],
                                 fillcolor = cols[i],
                                 line = list(color = linecolor, width = 1),
                                 opacity = opacity)
}

# Just to check if things are looking okay
ideogram.plot <- plot_ly(height = 800, width = 800) %>%
    layout(
        xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
        yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
        shapes = ideogram.shapes)

ideogram.plot



# Create chords ---------------------------------------------------------------
# Divide each sector corresponding to each interaction in each row
sector.angles <- inner$gaps
angle.list <- data.frame()
for (i in 1:nrow(sector.angles)) {
    # Get starting and ending points of each sector
    start <- sector.angles$start[i]
    end <- sector.angles$end[i]
    
    # Sort each row from increasing to decreasing
    dat <- sort(df[i,])
    
    # Use toAngular() function to get thetas corrosponding to each row item
    angle <- toAngular(as.numeric(dat), lower = start, upper = end, addgaps = F)$theta
    
    # Offset by the starting point since the function returns values in 
    # the [0 - (start - end)] interval
    angle <- c(start + angle)
    
    # Collate all the data for each division of the sector 
    temp <- data.frame(from = rownames(sector.angles)[i],
                       to = names(dat),
                       value = as.numeric(dat),
                       angle,
                       x = inner.radius * cos(angle),
                       y = inner.radius * sin(angle),
                       stringsAsFactors = F)
    
    # Add the starting point to the divisions
    # If min value in a row is zero then starting point for that division
    # must be the starting point of the sector
    startrow <- data.frame(from = rownames(sector.angles)[i],
                           to = "start",
                           value = 0,
                           angle = sector.angles$start[i],
                           x = inner.radius * cos(sector.angles$start[i]),
                           y = inner.radius * sin(sector.angles$start[i]),
                           stringsAsFactors = F)
    
    angle.list <- rbind(angle.list, startrow, temp)
}

# Create unique path IDs i.e. each set of interactions gets a unique ID
# Example - A -> B and B -> A will get the same ID
k <- 1
angle.list$ID <- rep(0, nrow(angle.list))
revstr <- paste(angle.list$to, angle.list$from)

for (i in 1:nrow(angle.list)) {
    if (angle.list$ID[i] == 0) {
        from = angle.list$from[i]
        to = angle.list$to[i]
        str <- paste(from, to)
        mtch <- match(str, revstr)
        
        if (!is.na(mtch)) {
            angle.list$ID[c(i, mtch)] <- k
            k <- k + 1
        }
    }
}

# Each chord is bounded by four points: 
# 1. two actual data points corrosponding to the actual interaction i.e. A -> B (p1) and B -> A (p2)
# 2. And two previous data points to complete the polygon
# We'll create some helper functions

# Function definition: bezierCurve() ------------------------------------------
bezierCurve <- function(t1, t2){
    
    # Takes two angles as arguments and returns the x and y coordinates
    # of a quadratic bezier curve 
    
    t <- seq(0, 1, length.out = 100)
    
    p0 <- c(inner.radius * cos(t1), inner.radius * sin(t1))  # Starting point (t1)
    p2 <- c(inner.radius * cos(t2), inner.radius * sin(t2))  # Ending point (t2)
    p1 <- c(-inner.radius * cos(mean(t1, t2)), -inner.radius * sin(mean(t1, t2)))  # Control point
    
    # Curve =  (1 - t^2)*p0 + 2(t-1)t*p1 + t^2*p2
    x <- (1 - t**2) * p0[1] + 2*(1 - t)*t * p1[1] + t**2 * p2[1]
    y <- (1 - t**2) * p0[2] + 2*(1 - t)*t * p1[2] + t**2 * p2[2]
    df <- data.frame(x, y)
    
    return(df)
}

# Function definition: circleCurve() ------------------------------------------
circleCurve <- function(t1, t2){
    
    # Returns the x and y coordinates of points lying on the inner 
    # boundary of the ideogram bounded by two angles t1 and t2 
    
    t <- seq(min(t1, t2), max(t1, t2), length.out = 50)
    x <- inner.radius * cos(t)
    y <- inner.radius * sin(t)
    
    df <- data.frame(x, y)
    
    return(df)
}

# Function definition: opposite() ---------------------------------------------
opposite <- function(df){
    
    # Given a dataframe, simply returns the dataframe in reverse order
    
    n <- nrow(df)
    df <- df[n:1,]
    return(df)
}

# Function definition: chordShape() -------------------------------------------
chordShape <- function(ID){
    
    # Function to create svg path for a chord given by a unique ID (created earler)
    
    id <- which(angle.list$ID == ID)
    
    # Get color based on higher number of connects
    idx <- which.max(angle.list$value[id])
    fillcolor <- angle.list$from[id[idx]]
    fillcolor <- cols[which(rownames(df) == fillcolor)]
    
    # Append the two prior points to complete polygon
    id <- c(id, id - 1)
    t <- angle.list$angle[id]
    
    # Each chord is made of two bezier curves and two (one) curve lying on the 
    # inner boundary of the ideogram
    if(length(t) == 4){
        a <- bezierCurve(t[1], t[4])
        b <- bezierCurve(t[3], t[2])
        c <- circleCurve(t[1], t[3])
        d <- circleCurve(t[2], t[4])
        
        df <- rbind(a, d, opposite(b), c)
        
        pth <- createPath(df)
        shp <- list(type = "path",
                    path = pth,
                    fillcolor = fillcolor,
                    line = list(color = linecolor, width = 1),
                    opacity = chord.opacity)
        
    }else{
        
        # Case when there are zero interactions i.e. 
        # A -> B > 0 but B -> A = 0 or viceversa
        a <- bezierCurve(t[1], t[2])
        b <- circleCurve(t[1], t[2])
        
        df <- rbind(a, b)
        
        pth <- createPath(df)
        shp <- list(type = "path",
                    path = pth,
                    fillcolor = fillcolor,
                    line = list(color = linecolor, width = 1),
                    opacity = chord.opacity)
    }
    
    return(shp)
}

# Loop through each unique ID and create a shape for each corrosponding polygon
chord.shapes <- list()
for(i in unique(angle.list$ID)){
    if(i != 0){
        chord.shapes[[i]] <- chordShape(ID = i)
    }
}

# Create a grey circle on the inside for aesthetics
ang <- seq(0, (2*pi), length.out = 100)
x <- 1 * cos(ang)
y <- 1 * sin(ang)

pth <- createPath(df = data.frame(x, y))
inner.circle <- list(list(type = "path",
                          path = pth,
                          fillcolor = circlefill,
                          line = list(color = linecolor, width = 1),
                          opacity = 0.2))

# Add all shapes to same list
all.shapes <- c(ideogram.shapes, chord.shapes, inner.circle)
length(all.shapes)

# Plot chord diagram ----------------------------------------------------------
# Just a description of chord diagram
description <- paste0("<i>","A chord diagram is a graphical method of displaying the inter-relationships ",
                      "between data in a matrix. The data is <br> arranged radially around a circle ",
                      "with the relationships between the points typically drawn as arcs connecting ",
                      "the<br>data together - <b>Wikipedia</b>","</i>")

# Coordinates for labels
labels <- data.frame(x = 1.1 * cos(outer$theta + pi/5),
                     y = 1.1 * sin(outer$theta + pi/5),
                     text = paste0("<b>", rownames(df), "</b>"))

# Plot using plot_ly()
chord.plot <- plot_ly(width = 800, height = 800) %>%
    
    # Add labels to sectors
    add_text(data = labels, x = ~x, y = ~y, text = ~text, hoverinfo = "none",
             textfont = list(family = "serif", size = 14, color = "#999999")) %>%
    
    # Layout for shapes, annotations and axis options
    layout(
        xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, domain = c(0, 0.9)),
        yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, domain = c(0, 0.9)),
        shapes = all.shapes,
        
        annotations = list(
            list(xref = "paper", yref = "paper",
                 xanchor = "left", yanchor = "top",
                 x = 0, y = 1, showarrow = F,
                 text = "<b>Filled Chord Diagram</b>",
                 font = list(family = "serif", size = 25, color = "black")),
            
            list(xref = "paper", yref = "paper",
                 xanchor = "left", yanchor = "top",
                 x = 0, y = 0.95, showarrow = F,
                 text = description,
                 align = "left",
                 font = list(family = "arial", size = 10, color = "black"))
        ))

print(chord.plot)
