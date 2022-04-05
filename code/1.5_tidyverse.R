# Õppematerjal aines Suurte tekstiandmete töötlemine ja analüüs humanitaarteadustes. (koostanud Peeter Tinits 2020)

# R-i kasutamise näidis: Gapminderi andmed
# ========================================

# Graafiku videodemonstratsiooniks vaata siia:
# https://www.gapminder.org/answers/how-does-income-relate-to-life-expectancy/


# Kui te kasutate koodi oma arvutis on vaja jooksutada järgmine rida. See kontrollib, kas installitud on vajalikud paketid ja kui neid pole, siis installib need.
lapply(c("gapminder","tidyverse"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))



# Paketid sisaldavad R-is lisakäske (tidyverse komplekt) ja mõnikord ka andmestikke (gapminder).
# Pakettide aktiveerimiseks on käsk library(), (require() teeb suhteliselt sama kui näete kellegi teise analüüsides seda)
# Paketid tuleb aktiveerida iga kord kui avad R-i. Need käsud avavad R-i praeguses keskkonnas.
library(gapminder)
library(tidyverse)

# Kuigi pakett gapminder annab meile juba andmestiku ette, siis enamasti tuleb andmestik sisse lugeda failist. Näidisena siin loeme sisse gapminder.csv faili.
gapminder <- read_csv("data/gapminder.csv")



# Loome uuesti graafiku, mida näitas Hans Rosling. See näeb esmapilgul välja keeruline, aga koosneb lihtsatest osadest.
gapminder %>%
  group_by(country) %>%
  filter(year==max(year)) %>%
  ggplot(aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+
  geom_point()+
  scale_x_log10()


# Vaatame lihtsamaid käske ja kasutame neid gapminderi andmetega töötamiseks.


# Lihtsad tidyverse käsud
#
# Töömudel on järgmine
# andmed %>%
#   protsess1() %>%
#   protsess2()
#


#  %>% - vii andmed järgmisesse protsessi
# select() vali muutujad
# filter() vii andmestik läbi filtri/sõela
# arrange() järjesta andmed
# mutate() loo uus muutuja
# group_by() grupeeri andmestik


# Alustame sellest et loeme sisse tabeli.
# read_csv on peaaegu sama käsk kui read.csv aga ta on loodud tidyverse keskkonna jaoks ja teeb natuke enamat
gapminder <- read_csv("data/gapminder.csv")
#Ja võime vaadata seda vajutades sellele ülal-paremal või kasutades View() funktsiooni
view(gapminder)

#Viime andmed läbi filtri, kus riigiks peab olema Soome
gapminder %>%
  filter(country=="Finland")

# Kuidas võiks leida Saksamaa andmed?







#Salvestame muutujana
finland <- gapminder %>%
  filter(country=="Finland")

# Teeme muutuja brasiilia andmetega





# Teeme muutuja Ameerika maailmajao andmetega.







#Muudame tabeli järjestust arrange() käsuga. desc() ütleb, et teha kahanevas järjekorras.
gapminder %>% 
  filter(year == 1952) %>% 
  arrange(desc(pop))


# Järjestame soome andmed uusimast vanemani






# Järjestame Euroopa riigid tähestiku järjekorras. Võtame ainult 1952 aasta.






#Madalaim oodatav eluiga aastal 2007. Select saab valida paar tulpa.
gapminder %>% 
  filter(year == 2007) %>% 
  arrange(lifeExp) %>% 
  select(country, lifeExp)


# Valime Euroopa 1952 andmetel ainult gdp ja eluea.







#Saame teha ka uue muutuja. Selleks on mutate() käsk.
gapminder %>% 
  mutate(gdp = pop * gdpPercap)





# Visualiseerimiseks saab kasutada ggplot-i
#
#
# Selle põhikuju tidyverse-is
#
# andmed %>%
#   ggplot(mapping = aes(SEOSED)) +
#   GEOM_FUNKTSIOONID()
#
# Vaata ?ggplot2 üksikasjalikumaks infoks. Google ggplot2 aitab ka.


# Graafik, mis sarnaneb esimesele, aga nüüd ühe riigi kohta läbi aastate
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=gdpPercap))+
  geom_point()+
  labs(title="Soome rikkus 1952-1007")

#Teeme sama graafiku Brasiilia kohta





# Filter käsu puhul võime mitu tingimust korraga panna. Näiteks võime kasutada nähtud jada c() seal.
gapminder %>%
  filter(country%in%c("Brazil","Finland","Germany"))  %>%
  ggplot(aes(x=year,y=gdpPercap))+
  geom_point()+
  labs(title="Riikide rahvaarvu võrdlus")

# Võime värvida punktid riigi nime järgi sättides parameetri color
gapminder %>%
  filter(country%in%c("Brazil","Finland","Germany"))  %>%
  ggplot(aes(x=year,y=gdpPercap,color=country))+
  geom_point()+
  labs(title="Riikide rahvaarvu võrdlus")

# Vali välja kolm riiki ja vaata nende rikkuse kasvu.




# Samuti nende rahvastiku kasvu.






# Esialgne graafik, võtame lihtsalt aasta 2007, kuna me teame, et see on viimane.
gapminder %>%
  filter(year==2007) %>%
  ggplot(aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+
  geom_point()+
  scale_x_log10()


# Sama tulemuse võime saada ka valides välja read, kus aasta on sama kui maksimaalne aasta väärtus.
gapminder %>%
  filter(year==max(year)) %>%
  ggplot(aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+
  geom_point()+
  scale_x_log10()


# Mõnikord juhtub, et riikidel on erinevad maksimaalsed aastad. Sellisel juhul peaksime grupeerima andmestiku riikide kaupa ning alles seejärel võrdlema suurima aastaarvuga.
gapminder %>%
  group_by(country) %>%
  filter(year==max(year)) %>%
  ggplot(aes(x=gdpPercap,y=lifeExp,color=continent,size=pop))+
  geom_point()+
  scale_x_log10()

# Nüüd kui me oleme näinud, kuidas andmetega üldiselt saab RStudios töötada, siis vaatame edasi juba täpsemalt tekstiandmeid.



# Allikad:
# https://github.com/rstudio/webinars/blob/master/46-tidyverse-visualisation-and-manipulation-basics/ (RStudio webinar by garretgman, CC)
# http://rpubs.com/aelhabr/tidyverse-basics