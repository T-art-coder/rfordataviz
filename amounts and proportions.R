library(gapminder)
library(dplyr)
head(gapminder)


mutate(gapminder, gdp = gdpPercap * pop,
       pop_mill = round(pop / 1000000))


mutate(gapminder, after_1960 = ifelse(year > 1960, TRUE, FALSE))

mutate(gapminder, 
       after_1960 = ifelse(year > 1960, "After 1960", "Before 1960"))

mutate(gapminder, african_country = ifelse(continent = 'Africa', 
                                           "African country", "Non"))



mutate(gapminder, africa = continent == 'Africa')
gapminder


# Add an africa column that is TRUE if the country is on the African continent
mutate(gapminder, africa = continent == "Africa")

# Add a column for logged GDP per capita
mutate(gapminder, log_gdpPercap = log(gdpPercap))

# Add an africa_asia column that says “Africa or Asia” if the country is in
# Africa or Asia, and “Not Africa or Asia” if it’s not
mutate(gapminder, 
       africa_asia = 
         ifelse(continent %in% c("Africa", "Asia"), "Africa or Asia", "Not Africa or Asia"))


gapminder %>%
  filter(year == 2002) %>% 
  mutate(log_gdpPercap = log(gdpPercap))


gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(avg = mean(lifeExp), 
            )


gapminder %>% group_by(continent, year)%>%
  summarize(avg = mean(lifeExp))



library(ggplot2)
library(viridis)

continent_counts <- gapminder %>%
  group_by(continent) %>%
  summarize(countries = n_distinct(country)  )

ggplot(continent_counts, aes(x = continent, y = countries, fill= continent)) + 
  geom_col() + 
  scale_fill_viridis_d(option = 'plasma', end = 0.9)




gapminder_2007 <- gapminder %>% 
  filter(year == 2007)

ggplot(gapminder_2007, 
       aes(x = gdpPercap, y = lifeExp,
           color = continent, shape = continent, size = pop)) +
  geom_point()+
  scale_x_log10()


ggplot(gapminder_2007, 
       aes(x = gdpPercap, y = lifeExp,
           color = continent,  size = pop)) +
  geom_point()+
  scale_x_log10()
  