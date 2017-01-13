library(rbokeh)
figure(width = NULL, height = NULL) %>%
    ly_points(Sepal.Length, Sepal.Width, data = iris,
              color = Species, glyph = Species,
              hover = list(Sepal.Length, Sepal.Width))


