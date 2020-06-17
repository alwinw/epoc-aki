
obs_data %>% summarise(
  Events = n(),
  `Admissions` = n_distinct(AdmissionID),
  `Unique Patients` = n_distinct(`UR number`)
)

obs_data %>% 
  group_by(Admission) %>% 
  summarise(
    Events = n(),
    `Admissions` = n_distinct(AdmissionID),
    `Unique Patients` = n_distinct(`UR number`)
  )
























library(ggpubr)

# Scatter plot colored by groups ("Species")
sp <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width",
                color = "Species", palette = "jco",
                size = 3, alpha = 0.6)+
  border()                                         
# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggdensity(iris, "Sepal.Length", fill = "Species",
                   palette = "jco")
yplot <- ggdensity(iris, "Sepal.Width", fill = "Species", 
                   palette = "jco")+
  rotate()
# Cleaning the plots
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()
# Arranging the plot
ggarrange(xplot, NULL, sp, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)

# load package and data
library(ggplot2)
library(ggExtra)
data(mpg, package="ggplot2")
# mpg <- read.csv("http://goo.gl/uEeRGu")

# Scatterplot
theme_set(theme_bw())  # pre-set the bw theme.
mpg_select <- mpg[mpg$hwy >= 35 & mpg$cty > 27, ]
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")