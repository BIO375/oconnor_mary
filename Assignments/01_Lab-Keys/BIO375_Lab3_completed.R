### Lab 3. Data manipulation and graphing ####

# Answers to lab

# R for Data Science, Chapter 3 ####
# https://r4ds.had.co.nz/data-visualisation.html
# Enter your code here

# Tidyverse book Ch.3 (online version, actually Ch.1 in text version)
rm(list = ls())

#install.packages("tidyverse")
library("tidyverse")
#install.packages(c("nycflights13", "gapminder", "Lahman"))
library("nycflights13")
library("gapminder")
library("Lahman")
1+2
tidyverse_update()
dput(mtcars)
mpg
?mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
?geom_point
# ggplot template
# ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
summary(mpg)
list(mpg)
?mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = cyl, y = hwy))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = cty, size = cty))
?geom_point
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class, stroke = cyl))
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, colour = displ < 5))

#End Section 3.3

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(.~ cyl)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(~cty)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(~ drv)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ drv)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
?facet_wrap
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
?geom_smooth
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = class, y = hwy))
ggplot(data = mpg) +
  geom_line(mapping = aes(x = cty, y = hwy))
?geom_histogram
ggplot(data = mpg) +
  geom_histogram(mapping = aes(hwy), bins = 10)
ggplot(data = mpg) +
  geom_area(mapping = aes( x = hwy, y = displ))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
?geom_smooth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(se = FALSE, show.legend = FALSE)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
ggplot(data = mpg, mapping = aes(x=displ, y = hwy))+
  geom_point()+
  geom_smooth()
ggplot(data = mpg, mapping = aes(x=displ, y = hwy, group = drv))+
  geom_point()+
  geom_smooth()
ggplot(data = mpg, mapping = aes(x=displ, y = hwy, color = drv))+
  geom_point()+
  geom_smooth(se = FALSE)
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) +
  geom_point(mapping = aes(color = drv))+
  geom_smooth(se = FALSE)
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) +
  geom_point(mapping = aes(color = drv))+
  geom_smooth(mapping = aes(linetype = drv), se = FALSE)
ggplot(data = mpg, aes(x=displ, y = hwy, size = 1, stroke = 5, color = drv)) +
  geom_point(mapping = aes(shape = "21", fill = drv))
# above does not work
?aes
?diamonds
summary(diamonds)
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
?geom_bar
vignette("ggplot2-specs")
ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))
# variables computed by stat are in help section "computed variables" under each geom
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
?stat_summary
?geom_pointrange
?fun.ymin
ggplot(data = diamonds, mapping = aes(x = cut, y = depth, ymin = min(depth), ymax = max(depth))) +
  geom_pointrange()
?min
# did not get this to work
ggplot(data = diamonds, mapping = aes(x = cut, y = depth)) +
  geom_col()
?geom_col
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
??prop
ggplot(data = diamonds, aes(cut, ...prop...)) + 
  geom_bar(aes(fill = color))
?diamonds
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_jitter(aes(width=0.1))
?geom_jitter
?geom_count
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_count()
?geom_boxplot
ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot(aes(color=class))
?position_dodge
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()
install.packages("maps")
library("maps")
nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()
ggplot(data = mpg, aes(x = class, group = drv, fill = drv))+
  geom_bar()
ggplot(data = mpg, aes(x = class, group = drv, fill = drv))+
  geom_bar()+
  coord_flip()
ggplot(data = mpg, aes(x = class, group = drv, fill = drv))+
  geom_bar()+
  coord_polar()
?labs
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()
?coord_map
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() + 
  geom_abline() +
  coord_fixed

# Template
# ggplot(data = <DATA>) + 
# <GEOM_FUNCTION>(
#    mapping = aes(<MAPPINGS>),
#    stat = <STAT>, 
#    position = <POSITION>
#  ) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>
geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
ggplot(data = mpg)+
  geom_point(data = filter(mpg, cyl == 8), aes(x = displ, y = hwy))

### Assignment

# Create the sanchez.csv file in Excel, save it in datasets/demos/sanchez.csv
# and then load the sanchez.csv file
# Enter your code here

sanchez <- read_csv("datasets/demos/sanchez.csv")

# Calculate summary statistics
# Enter your code here

summ_beetle <- sanchez %>%
  group_by(COLTYPE) %>% 
  summarise(n = n(),
            mean = mean(BEETLE96),
            median = median(BEETLE96),
            IQR = IQR(BEETLE96),
            sd = sd(BEETLE96),
            var = var(BEETLE96))

View(summ_beetle)

# Add a new column of log(y+1) transformed beetle densities to the sanchez dataset
# Enter your code here

sanchez<-mutate(sanchez, logp1_beetle = log(BEETLE96+1))

# Generate histograms of beetle density by colony type before and after data 
# transformation
# Enter your code here
ggplot(sanchez) +
  geom_histogram(aes(BEETLE96), binwidth = 2)+
  facet_wrap(~COLTYPE)
ggplot(sanchez) +
  geom_histogram(aes(logp1_beetle), binwidth = 0.5)+
  facet_wrap(~COLTYPE)

# Plot boxplots of beetle density by colony type before and after data 
# transformation
# Enter your code here

ggplot(sanchez)+
  geom_boxplot(aes(x = COLTYPE, y = BEETLE96), notch = TRUE, varwidth = TRUE)
ggplot(sanchez)+
  geom_boxplot(aes(x = COLTYPE, y = logp1_beetle), notch = TRUE, varwidth = TRUE)


