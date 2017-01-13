# library
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2) 

# have a look to the example dataset
head(demoFreq)
wordcloud2(demoFreq, size = 1,shape = 'star')
wordcloud2(demoFreq, size = 2, minRotation = -pi/2, maxRotation = -pi/2)
wordcloud2(demoFreq, size = 2, minRotation = -pi/6, maxRotation = -pi/6,
           rotateRatio = 1)

## Sys.setlocale("LC_CTYPE","eng")
wordcloud2(demoFreqC, size = 2, fontFamily = "微软雅黑",
           color = "random-light", backgroundColor = "grey")

# Gives a proposed palette
wordcloud2(demoFreq, size=1.6, color='random-dark')

# or a vector of colors. vector must be same length than input data
wordcloud2(demoFreq, size=1.6, color=rep_len( c("green","blue"), nrow(demoFreq) ) )

# Change the background color
wordcloud2(demoFreq, size=1.6, color='random-light', backgroundColor="black")

# Change the shape:
wordcloud2(demoFreq, size = 0.7, shape = 'star')

# Change the shape using your image
wordcloud2(demoFreq, figPath = "peace.png", size = 1.5, color = "skyblue", backgroundColor="black")

wordcloud2(demoFreq, size = 2.3, minRotation = -pi/6, maxRotation = -pi/6, rotateRatio = 1)

wordcloud2(demoFreqC, size = 2, fontFamily = "微软雅黑", color = "random-light", backgroundColor = "grey")

letterCloud( demoFreq, word = "R", color='random-light' , backgroundColor="black")
letterCloud( demoFreq, word = "R", color='random-light' , backgroundColor="white")
letterCloud( demoFreq, word = "PEACE", color="white", backgroundColor="pink")

