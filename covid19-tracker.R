library(readr)
library(reshape2)
library(dplyr)

setwd("/Users/aamzallag/code_programming/COVID-19/")
system("git pull")
conf_cases <- read_csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
#colnames(conf_cases)[1:5]
confm <- melt(conf_cases, id.vars = c("Province/State", "Country/Region", "Lat", "Long"), 
              variable.name = "date", value.name = "n_cases")
confm$date <- as.Date(confm$date, format = "%m/%d/%y")
#filter(confm, grepl("Massa", `Province/State`))
#filter(confm, grepl("New York$", `Province/State`))

qplot(date, n_cases, 
      colour = `Province/State`, geom = "line",
      data=filter(confm, grepl("China", `Country/Region`), !grepl("Hubei", `Province/State`)))

library(plotly)
qplot(date, n_cases, 
      colour = `Province/State`, geom = "line",
      data=filter(confm, grepl("US", `Country/Region`),
                  !grepl(",", `Province/State`),
                  date > as.Date("2020-03-01"))) %>% ggplotly()

qplot(date, n_cases, 
      colour = paste(`Country/Region`, `Province/State`, sep = ", "), geom = "line",
      data=filter(confm, `Country/Region` %in% c("France", "United Kingdom", "Israel", "Italy", "Iran", "Korea, South"), 
                  date > as.Date("2020-02-24"))) %>% ggplotly()

#d_cases <- read_csv("/Users/aamzallag/code_programming/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

rec_cases <- read_csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
recm <- melt(dplyr::select(rec_cases, -Lat, -Long), id.vars = c("Province/State", "Country/Region"), 
             variable.name = "date", value.name = "recovered")
recm$date <- as.Date(confm$date, format = "%m/%d/%y")
totm <- full_join(confm, recm)
totm <- mutate(totm, conf_cases = n_cases)
totm <- mutate(totm, n_cases = conf_cases - recovered)

counties <- read_csv("~/code_programming/nytimes/covid-19-data/us-counties.csv")

pops <- read_csv("~/Downloads/co-est2019-annres.csv", skip=1)
pops <- dplyr::filter(pops, !is.na(Census), X1 != "United States")
pops$X1 <- sub("^\\.", "", pops$X1)
library(stringr)
pops <- data.frame(pops, str_split_fixed(pops$X1, ", ", 2))
grep("^\\.", invert = TRUE, pops$X1, value = TRUE)
colnames(pops)[(ncol(pops)-1):ncol(pops)] <- c("county", "state")
pops$county <- sub(" County", "", pops$county)

write_csv(pops, "~/code_programming/covid19-arnaud/covid19-tracker.output/populations_us_counties.csv")

tmp <- left_join(counties, select(pops, X2019, county, state))
table(is.na(tmp$X2019))


###
###   diff eqs
###

library(diffeqr) # https://cran.r-project.org/web/packages/diffeqr/vignettes/ode.html
diffeqr::diffeq_setup()

f <- JuliaCall::julia_eval("
function f(du,u,p,t)
  # u = p[1]*(1-u/p[2])*u
  u[1] = p[1].*(1-u[1]./p[2]).*u[1]
  u[2] = 0
 end")
u0=c(0.5, 0.5)
tspan <- list(0.0, 1.0)

sol = diffeqr::ode.solve("f", u0, tspan, c(1.5, 1.2))#, saveat = c(0.0+1:10/10))
sol
plot(sol$t,sol$u[,1],"l")

f2 <- function(u,p,t) {
   p[1]*(1-u/p[2])*u
}
tspan <- list(-0.0, 4.0)
sol = diffeqr::ode.solve(f2, 1/2, tspan, c(2, 14))
plot(sol$t,sol$u,"l")



