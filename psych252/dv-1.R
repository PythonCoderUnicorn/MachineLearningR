

# bar chart
library(tidyverse)

diamonds = diamonds

gdiamonds = ggplot(data =  diamonds,
       aes(x= carat,
           y= price,
           col= color)
       ) 

gdiamonds +
  stat_summary(fun = 'mean', geom = 'bar')


gdiamonds +
  geom_point()



gd = ggplot(data =  diamonds,
                   aes(x= color,
                       y= price,
                       group= cut,
                       fill= cut,
                       col= cut)
) 

gd +
  stat_summary(fun='mean', geom = 'line', size= 2)

gd +
  stat_summary(fun = 'mean', geom = 'bar', position = 'dodge')



gd2 = ggplot(
  diamonds,
  aes(x= carat,
      y= price,
      color= color)
)

gd2+
  geom_point()+
  geom_smooth(method = 'lm', color='pink')





















