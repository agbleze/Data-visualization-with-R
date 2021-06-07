## rrlation between engine size and fuel efficiency
library(tidyverse)
mpg
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg)
ggplot(data = mpg) + geom_point(mapping = aes(x = hwy, y = cyl))
ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))
