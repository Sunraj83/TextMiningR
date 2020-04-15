
## Reading data from dowloaded file

ER <- read.csv('/Users/Sunny/Desktop/Voting/votes.csv', header = TRUE)

ncol(ER)
names(ER)

##Disregarding Columns which are not required

ER$SBO001207 <- NULL;ER$SBO315207 <- NULL;ER$SBO115207 <- NULL;ER$SBO215207 <- NULL;
ER$SBO515207  <- NULL;ER$SBO415207  <- NULL;ER$SBO015207  <- NULL;
ER$MAN450207 <- NULL;ER$WTN220207 <- NULL;ER$RTN130207 <- NULL;
ER$RTN131207 <- NULL;ER$AFN120207 <- NULL;ER$BPS030214 <- NULL;
ER$X.1 <- NULL;ER$X <- NULL;ER$Clinton_Obama <- NULL;ER$Trump_Romney <- NULL;
ER$Trump_Prediction <- NULL; ER$Clinton_Prediction <- NULL; ER$Trump_Deviation <- NULL;
ER$Clinton_Deviation <- NULL


ncol(ER)
names(ER)

##Checking whether any row in entire data frame has na or missing value
which(is.na(ER))

##checking whether any row in the entire data set has NA, na, NaN value
nrow(ER[complete.cases(ER), ])
nrow(ER)



##Preparing Data and renaming columns for Analysis later.

ER1 <- data.frame(ER_county_ID = ER$combined_fips,
                  ER_county_nm = ER$county_name,
                  ER_states = ER$state_abbr,
                  ER_votes_Dem16 = ER$votes_dem_2016,
                  ER_votes_GOP16 = ER$votes_gop_2016,
                  ER_votes_total =  ER$total_votes_2016,
                  ER_Clinton  = ER$Clinton * 100,
                  ER_Trump = ER$Trump * 100,
                  ER_diff2016 = 100 * ER$per_point_diff_2016,
                  ER_diff2012 = 100 *  ER$per_point_diff_2012,
                  ER_Obama = ER$Obama * 100,
                  ER_Romney = ER$Romney * 100,
                  ER_votes_Dem12 = ER$votes_dem_2012,
                  ER_votes_GOP12 = ER$votes_gop_2012,
                  ER_votes_total = ER$total_votes_2012,
                  ER_Pop14 = ER$population2014,
                  ER_Pop10 = ER$population2010,
                  ER_Age_U5_14 = ER$AGE135214,
                  ER_Age_U18_14 = ER$AGE295214,
                  ER_Age_O65_14 = ER$age65plus,
                  ER_Vot_Turnarnd = (ER$total_votes_2016/((100 - (ER$AGE135214 + ER$AGE295214))                                     * ER$population2014/100)) * 100,
                  ER_Gender = ER$SEX255214,
                  ER_Ethn_White = round(ER$White * ER$population2014),
                  ER_Ethn_Black = round(ER$Black * ER$population2014),
                  ER_Amer_Ind = round(ER$RHI325214/100 * ER$population2014),
                  ER_Asian = round(ER$RHI425214/100 * ER$population2014),
                  ER_Hawai = round(ER$RHI525214/100 * ER$population2014),
                  ER_Misc = round(ER$RHI625214/100 * ER$population2014),
                  ER_Hispanic = round(ER$Hispanic/100 * ER$population2014),
                  ER_College_Pop = (ER$Edu_batchelors/100) * ER$population2014,
                  ER_House_income = ER$INC110213,
                  ER_Density = ER$Density)


### Analyzing Flipped States in 2016 presidential Elections

library(plotly)
library(ggplot2)
library(UsingR)
library(prob)
library(dplyr)
library(knitr)
library(sampling)

### Analyzing Categorical Variable ####################################################

##Identifying Winner for each county in 2016 Presidential Election
ER1$Winner2016 <- ifelse(ER1$ER_votes_Dem16>ER1$ER_votes_GOP16 ,"Clinton","Trump")

##Identifying Winner for each county in 2012 Presidential Election
ER1$Winner2012 <- ifelse(ER1$ER_votes_Dem12>ER1$ER_votes_GOP12 ,"Obama","Romney")


##Identifying Counties flipped by Trump and Clinton in 2016 election

ER1$RepubFlip <- ifelse(ER1$Winner2016 == 'Trump' & ER1$Winner2012 == 'Obama', 1, 0)
ER1$DemoFlip <- ifelse(ER1$Winner2016 == 'Clinton' & ER1$Winner2012 == 'Romney', 1, 0)


##

county_flipR <- subset(ER1,ER1$Winner2016 == 'Trump') 
county_flipR <- subset(county_flipR,county_flipR$Winner2012 == 'Obama')

## Getting subsets for Democratic Winner
county_Demo_Winner <- subset(ER1,ER1$Winner2016 == 'Clinton')

## Getting subsets for Republican Winner
county_Rep_Winner <- subset(ER1,ER1$Winner2016 == 'Trump')


##Identifying Counties flipped by Clinton in 2016 election

county_flipD = subset(ER1,ER1$Winner2016 == 'Clinton')
county_flipD = subset(county_flipD,county_flipD$Winner2012 == 'Romney')


## Configuring the levels for States to show on x axis else all 51 states will be displayed with no bars

county_flipR$ER_states <- factor(county_flipR$ER_states, levels =               c(as.character(unique(county_flipR$ER_states))))

county_flipD$ER_states <- factor(county_flipD$ER_states, levels = c(as.character(unique(county_flipD$ER_states))))


p <- plot_ly(data = county_flipR, 
             x = county_flipR$ER_states, 
             type = "histogram",
             name = 'Flipped By Republicans')%>%
  add_trace(data = county_flipD, 
            x = county_flipD$ER_states, 
            type = "histogram",
            name = 'Flipped by Democrats') %>%
  layout(yaxis = list(title = 'No. of US Counties Flipped in 2016'))%>%
  layout(xaxis = list(title = 'States')); p



cat("Total Number of Counties Flipped by Trump      :", nrow(subset(ER1, ER1$RepubFlip == 1)))



cat("Total Number of Counties Flipped by Clinton    :", nrow(county_flipD))


#######################################################
##Calculating % county flips for Republicans

ER1$LineVal <- 1

county_st <- ER1 %>%
  group_by(ER_states) %>%
  summarize_at(c("RepubFlip","DemoFlip","LineVal"), sum, na.rm  = TRUE)

county_st1 <- subset(county_st, RepubFlip >= 1)
county_st1$RFlipPct <- (county_st1$RepubFlip/county_st1$LineVal) * 100


county_st2 <- subset(county_st, DemoFlip >= 1)
county_st2$DFlipPct <- (county_st2$DemoFlip/county_st2$LineVal) * 100



county_st1$ER_states <- factor(county_st1$ER_states, levels = 
                                 c(as.character(unique(county_st1$ER_states))))

county_st2$ER_states <- factor(county_st2$ER_states, levels = 
                                 c(as.character(unique(county_st2$ER_states))))

p <- plot_ly(data = county_st1,
             x = county_st1$ER_states, 
             y = county_st1$RFlipPct,
             type = "scatter",
             size = county_st1$RFlipPct,
             name = '% Flip By Republicans')%>%
  add_trace(data = county_st2,
            x = county_st2$ER_states, 
            y = county_st2$DFlipPct,
            type = "scatter",
            size = county_st2$RFlipPct,
            name = '% Flip By Democrats')%>%
  layout(yaxis = list(title = ' % Counties Flipped in 2016'))%>%
  layout(xaxis = list(title = 'States')); p


### Flipped State Analysis 


swing.states1 <- subset(ER1, ER1$ER_states == 'WI' |ER1$ER_states == 'MI'| 
                          ER1$ER_states =='MN' |ER1$ER_states == 'OH'|
                          ER1$ER_states == 'IA'|ER1$ER_states == 'ME'|
                          ER1$ER_states == 'NH'|ER1$ER_states == 'NY')


##Identifying Counties flipped by Trump in 2016 election for WI state

county_flipRSW = subset(swing.states1,swing.states1$Winner2016 == 'Trump');
county_flipRSW = subset(county_flipRSW,county_flipRSW$Winner2012 == 'Obama')
county_Demo_WinnerSW = subset(swing.states1,swing.states1$Winner2016 == 'Clinton')
county_Rep_WinnerSW = subset(swing.states1,swing.states1$Winner2016 == 'Trump')



swing.states1$ER_states <- factor(swing.states1$ER_states, levels = 
                                    c(as.character(unique(swing.states1$ER_states))))

county_flipRSW$ER_states <- factor(county_flipRSW$ER_states, levels = 
                                     c(as.character(unique(county_flipRSW$ER_states))))

county_Demo_WinnerSW$ER_states <- factor(county_Demo_WinnerSW$ER_states, levels = 
                                           c(as.character(unique(county_Demo_WinnerSW$ER_states))))

county_Rep_WinnerSW$ER_states <- factor(county_Rep_WinnerSW$ER_states, levels = 
                                          c(as.character(unique(county_Rep_WinnerSW$ER_states))))




p3 <- plot_ly(county_flipRSW, 
              x = county_flipRSW$ER_states, 
              type = "histogram",
              name = 'flip')%>%
  add_trace(county_Demo_WinnerSW, x = county_Demo_WinnerSW$ER_states, name = "Clinton"  ) %>%
  add_trace(county_Rep_WinnerSW, x = county_Rep_WinnerSW$ER_states, name = "Trump"  ) %>%
  add_trace(swing.states1, x = swing.states1$ER_states, name  = 'Total Counties') %>%
  layout(yaxis = list(title = '# counties in Swing States'))%>%
  layout(xaxis = list(title = 'Swing States in 2016')); p3


cat("Total Number of Counties Flipped by Trump in Selected States      :", nrow(county_flipRSW))


#### Ethnic Population Split by Flipped States


ethn <- data.frame(States = swing.states1$ER_states, 
                   White.Pop = swing.states1$ER_Ethn_White,
                   Black.Pop = swing.states1$ER_Ethn_Black,
                   AmInd.Pop = swing.states1$ER_Amer_Ind,
                   Asian.Pop = swing.states1$ER_Asian,
                   haw.Pop = swing.states1$ER_Hawai,
                   Misc.Pop = swing.states1$ER_Misc,
                   Hisp.Pop = swing.states1$ER_Hispanic)

options(dplyr.print_max = 25)
options(pillar.sigfig=5)


ethn.sum <- ethn %>%
  group_by(States) %>%
  summarize_at(c("White.Pop","Black.Pop","AmInd.Pop",
                 "Asian.Pop", "haw.Pop", "Misc.Pop",
                 "Hisp.Pop"), sum, na.rm  = TRUE)

library(reshape2)
ethn.sum.long<-melt(ethn.sum,id.vars="States")


p1 <- plot_ly(subset(ethn.sum.long,ethn.sum.long$States == 'ME'), values = ~value,
              labels = ~variable, type = 'pie',hole = 0.40,
              domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
  add_trace(data = subset(ethn.sum.long,ethn.sum.long$States == 'NH'), values = ~value,
            labels = ~variable, type = 'pie',hole = 0.40,
            domain = list(x = c(0.5, 1), y = c(0, 1)))%>%
  layout(title = 'Maine State                            New Hampshire State ');p1

p2 <- plot_ly(subset(ethn.sum.long,ethn.sum.long$States == 'NY'), values = ~value,
              labels = ~variable, type = 'pie',hole = 0.40,
              domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
  add_trace(data = subset(ethn.sum.long,ethn.sum.long$States == 'WI'), values = ~value,
            labels = ~variable, type = 'pie',hole = 0.40,
            domain = list(x = c(0.5, 1), y = c(0, 1))) %>%
  layout(title = 'New York State                         Wisconsin State '); p2

p3 <- plot_ly(subset(ethn.sum.long,ethn.sum.long$States == 'MI'), values = ~value,
              labels = ~variable, type = 'pie',hole = 0.40,
              domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
  add_trace(data = subset(ethn.sum.long,ethn.sum.long$States == 'MN'), values = ~value,
            labels = ~variable, type = 'pie',hole = 0.40,
            domain = list(x = c(0.5, 1), y = c(0, 1)))%>%
  layout(title = 'Michigan State                              Minnesota State '); p3

p4 <- plot_ly(subset(ethn.sum.long,ethn.sum.long$States == 'IA'), values = ~value,
              labels = ~variable, type = 'pie',hole = 0.40,
              domain = list(x = c(0, 0.5), y = c(0, 1))) %>%
  add_trace(data = subset(ethn.sum.long,ethn.sum.long$States == 'OH'), values = ~value,
            labels = ~variable, type = 'pie',hole = 0.40,
            domain = list(x = c(0.5, 1), y = c(0, 1)))%>%
  layout(title = 'Iowa State                              Ohio State '); p4


#### College Population in Flipped States


p <- plot_ly(swing.states1, 
             x = subset((swing.states1$ER_College_Pop/swing.states1$ER_Pop14) *100, 
                        swing.states1$ER_states == 'WI'),
             type = "box", name = 'Wisconsin State') %>%
  add_trace(x = subset((swing.states1$ER_College_Pop/swing.states1$ER_Pop14) *100, 
                       swing.states1$ER_states == 'MN'),
            type = "box", name = 'Minnesota State') %>%
  add_trace(x = subset((swing.states1$ER_College_Pop/swing.states1$ER_Pop14) *100, 
                       swing.states1$ER_states == 'MI'),
            type = "box", name = 'Michigan State') %>%
  add_trace(x = subset((swing.states1$ER_College_Pop/swing.states1$ER_Pop14) *100, 
                       swing.states1$ER_states == 'IA'),
            type = "box", name = 'Iowa State') %>%
  add_trace(x = subset((swing.states1$ER_College_Pop/swing.states1$ER_Pop14) *100, 
                       swing.states1$ER_states == 'OH'),
            type = "box", name = 'Ohio State') %>%
  add_trace(x = subset((swing.states1$ER_College_Pop/swing.states1$ER_Pop14) *100, 
                       swing.states1$ER_states == 'NY'),
            type = "box", name = 'New York State') %>%
  add_trace(x = subset((swing.states1$ER_College_Pop/swing.states1$ER_Pop14) *100, 
                       swing.states1$ER_states == 'NH'),
            type = "box", name = 'New Hampshire State') %>%
  add_trace(x = subset((swing.states1$ER_College_Pop/swing.states1$ER_Pop14) *100, 
                       swing.states1$ER_states == 'ME'),
            type = "box", name = 'Maine State') %>%
  layout(xaxis = list(title = 'College Population % in Swing States')); p



### State vs Voter Turnaround

#### Assumptions:

##Age Eligible Voting population is considered for below analysis. In reality actual voting population is the registered voter population. 

##Voting Age Populaiton consists of Registered voters + Nonvoters + Noncitizens


options(dplyr.print_max = 25)
options(pillar.sigfig=5)
a2 <- ER1 %>%
  group_by(ER_states) %>%
  summarize(ER_Vot_Turnarnd = mean(ER_Vot_Turnarnd))

a3 <- ER1 %>%
  ##group_by(ER_states) %>%
  summarize(ER_Vot_Turnarnd = mean(ER_Vot_Turnarnd))

p2 <- plot_ly(data = a2,
              x = a2$ER_states,
              y = a2$ER_Vot_Turnarnd,
              type = "scatter",
              mode = 'lines+markers',
              ##    color = ('rgb(0, 0, 255)'),
              name = 'State Voter Turnaround') %>%
  add_trace(data = a3,
            y = a3$ER_Vot_Turnarnd,
            type = "scatter",
            mode = 'lines',
            ##    color = ('rgb(200, 100, 50)'),
            name = 'US Voter Turnaround'); p2


#### President Candidate Vote % Across all US Counties


p2 <- plot_ly(ER1, x = ER1$ER_Clinton, type = "box", name = 'Clinton') %>%
  add_trace(x = ER1$ER_Trump, name  = 'Trump') %>%
  add_trace(x = ER1$ER_Obama, name  = 'Obama') %>%
  add_trace(x = ER1$ER_Romney, name  = 'Romney') %>%
  layout(xaxis = list(title = 'Pres. Cand. Vote %')); p2



## Bivariate and Multivariate Analysis


### Identity Politics 



#### College Population Correlation with Vote casted


### Correlation - 2016

f <- list(
  family = "Arial",
  size = 15,
  color = 'black')

p1 <- plot_ly(ER,
              x = ~Clinton,
              y = ~Edu_batchelors,
              type = "scatter",
              name = 'Clinton') %>%
  add_trace(x = ~Trump, name = 'Trump') %>%
  layout(title = '2012-2016 Candidates Vs College Educated Pop%', titlefont = f,
         yaxis = list(title = 'County College Pop. %',zeroline = FALSE, titlefont = f),
         xaxis = list(title = 'Vote Share %',zeroline = FALSE, titlefont = f));p1

### Correlation - 2012

p2 <- plot_ly(ER,
              x = ~Obama,
              y = ~Edu_batchelors,
              type = "scatter",
              name = 'Obama') %>%
  add_trace(x = ~Romney, name = 'Romney') %>%
  layout(title = '2012-2016 Candidates Vs College Educated Pop%', titlefont = f,
         yaxis = list(title = 'County College Pop. %',zeroline = FALSE, titlefont = f),
         xaxis = list(title = 'Vote Share %',zeroline = FALSE, titlefont = f));p2


#### Household Income Correlation with Vote casted



p4 <- plot_ly(ER,
              x = ~Clinton,
              y = ~INC110213,
              type = "scatter",
              name = 'Clinton') %>%
  add_trace(x = ~Trump, name = 'Trump') %>%
  layout(title = '2012-2016 Candidates Vs Median Household Income', titlefont = f,
         yaxis = list(title = 'Median Household Income',zeroline = FALSE, titlefont = f),
         xaxis = list(title = 'Vote Share %',zeroline = FALSE, titlefont = f));p4

### Correlation - 2012

p5 <- plot_ly(ER,
              x = ~Obama,
              y = ~INC110213,
              type = "scatter",
              name = 'Obama') %>%
  add_trace(x = ~Romney, name = 'Romney') %>%
  layout(title = '2012-2016 Candidates Vs Median Household Income', titlefont = f,
         yaxis = list(title = 'Median Household Income',zeroline = FALSE, titlefont = f),
         xaxis = list(title = 'Vote Share %',zeroline = FALSE, titlefont = f));p5



#### Gender (Female Population) Correlation with Vote casted

p7 <- plot_ly(ER,
              x = ~Clinton,
              y = ~SEX255214,
              type = "scatter",
              name = 'Clinton') %>%
  add_trace(x = ~Trump, name = 'Trump') %>%
  layout(title = '2012-2016 Candidates Vs Female Voters', titlefont = f,
         yaxis = list(title = 'Female % Population',zeroline = FALSE, titlefont = f),
         xaxis = list(title = 'Vote Share %',zeroline = FALSE, titlefont = f));p7

### Correlation - 2012

p8 <- plot_ly(ER,
              x = ~Obama,
              y = ~SEX255214,
              type = "scatter",
              name = 'Obama') %>%
  add_trace(x = ~Romney, name = 'Romney') %>%
  layout(title = '2012-2016 Candidates Vs Female Voters', titlefont = f,
         yaxis = list(title = 'Female % Population',zeroline = FALSE, titlefont = f),
         xaxis = list(title = 'Vote Share %',zeroline = FALSE, titlefont = f));p8


#### Ethnicity Correlation with Vote casted


### Correlation - 2016

p10 <- plot_ly(ER,
               x = ~Clinton,
               y = ~White,
               type = "scatter",
               name = 'Clinton-White') %>%
  add_trace(x = ~Trump, name = 'Trump-White') %>%
  layout(title = 'White Population Split', titlefont = f,
         yaxis = list(title = '% White Pop.',zeroline = FALSE, titlefont = f),
         xaxis = list(title = 'Vote Share %',zeroline = FALSE, titlefont = f));p10

### Correlation - 2012

p11 <- plot_ly(ER,
               x = ~Clinton,
               y = ~Black,
               type = "scatter",
               name = 'Clinton-Black') %>%
  add_trace(x = ~Trump, name = 'Trump-Black') %>%
  layout(title = 'Black population Split', titlefont = f,
         yaxis = list(title = '% Black Pop.',zeroline = FALSE, titlefont = f),
         xaxis = list(title = 'Vote Share %',zeroline = FALSE, titlefont = f));p11


### Examining the Age Distribution in each state


#Calculating Mean Age category % of Every State

options(dplyr.print_max = 25)
options(pillar.sigfig=5)
Age.Dist <- ER %>%
  group_by(state_abbr) %>%
  summarize_at(c("AGE135214","AGE295214","age65plus"),
               mean, na.rm  = TRUE)

#Calculating Mean Age Category % of USA

Age.Dist1 <- ER %>%
  ## group_by(state_abbr) %>%
  summarize_at(c("AGE135214","AGE295214","age65plus"),
               mean, na.rm  = TRUE)

## Plotting Distribution of State Mean Age Categories

p19 <- plot_ly(Age.Dist) %>% 
  add_trace(x = ~state_abbr,
            y = ~AGE135214,
            type = "scatter",
            mode = 'lines+markers',
            color = ('rgb(0, 0, 255)'),
            name = 'Age Under 5 %')%>%
  add_trace(x = ~state_abbr,
            y = Age.Dist1$AGE135214,
            type = "scatter",
            mode = "lines",
            color = ('rgb(221,160,221)'),
            hoverinfo = "none",
            name = 'Age Under 5 % - USA')%>%
  add_trace(x = ~state_abbr,
            y = ~AGE295214,
            type = "scatter",
            mode = 'lines+markers',
            color = ('rgb(0,100,0)'),
            name = 'Age 5 to 18 %')%>%
  add_trace(x = ~state_abbr,
            y = Age.Dist1$AGE295214,
            type = "scatter",
            mode = "lines",
            color = ('rgb(221,160,221)'),
            hoverinfo = "none",
            name = 'Age 5 to 18 % - USA')%>%
  add_trace(x = ~state_abbr,
            y = (100 - (Age.Dist$AGE135214 + Age.Dist$AGE295214 + Age.Dist$age65plus)),
            type = "scatter",
            mode = 'lines+markers',
            color = ('rgb(255,0,0)'),
            name = 'Age 18 to 65 %')%>%
  add_trace(x = ~state_abbr,
            y = (100 - (Age.Dist1$AGE135214 + Age.Dist1$AGE295214 + Age.Dist1$age65plus)),
            type = "scatter",
            mode = "lines",
            color = ('rgb(221,160,221)'),
            hoverinfo = "none",
            name = 'Age 18 to 65 % - USA')%>%
  add_trace(x = ~state_abbr,
            y = ~age65plus,
            type = "scatter",
            mode = 'lines+markers',
            color = ('rgb(148,0,211)'),
            name = 'Age 65+ %')%>%
  add_trace(x = ~state_abbr,
            y = Age.Dist1$age65plus,
            type = "scatter",
            mode = "lines",
            color = ('rgb(221,160,221)'),
            hoverinfo = "none",
            name = 'Age 65+ % - USA')%>%
  layout(title = 'State Average Age Distribution',
         xaxis = list(title = 'State'),
         yaxis = list(title = 'Population % - Age')); p19



#### Analyzing Point Differenential between 2016 and 2012 presidential elections.


##summary(ER1$ER_diff2016)
x1 <- density(ER1$ER_diff2016)
x2 <- density(ER1$ER_diff2012)

p2 <- plot_ly(ER1, x = ER1$ER_diff2016, 
              type = "histogram",
              name = '2016') %>%
  add_trace(x = ER1$ER_diff2012,
            type = "histogram",
            name = '2012') %>%
  add_trace(x = x1$x, y = x1$y, type = "scatter", 
            mode = "lines", fill = "blue", yaxis = "y2", name = "Density") %>% 
  add_trace(x = x2$x, y = x2$y, type = "scatter", 
            mode = "lines", fill = "orange", yaxis = "y2", name = "Density") %>% 
  layout(  xaxis = list(title = 'Point Differential: Democrats - Republicans'),
           yaxis = list(title = 'No. Of Counties')) %>% 
  layout(yaxis2 = list(overlaying = "y", side = "right")); p2



## Central Limit Theorem Application


### Getting Distribution of Income across US States



p1 <- plot_ly(ER1, x = ER1$ER_House_income, 
              histnorm = "probability", 
              type = "histogram",
              name = 'Income') %>%
  layout(xaxis = list(title = 'Median Household Income'),
         yaxis = list(title = 'Density'));p1

m <- mean(ER1$ER_House_income)
std <-sd(ER1$ER_House_income)


##### Drawing 100 Samples for various sample sizes of 10,20,30,40


set.seed(150)
par(mfrow = c(2,2))
samples <- 100
##sample.size <- 35

xbar <- numeric(samples)


for (sample.size in c(10,20,30,40)) {
for (i in 1: samples) {
xbar[i] <- mean(sample(ER1$ER_House_income,sample.size))
}



m1 = mean(xbar)
std1 <- sd(xbar)

hist(xbar,
breaks = 20,prob= TRUE,
main = paste("Sample Size =", sample.size),
xlab = "House Hold Income",col="Green")
curve(dnorm(x, mean=m1, sd=std1), 
col="red", lwd=2, add=TRUE, yaxt="n")

cat("Sample Size = ", sample.size, " Mean = ", mean(xbar),
    " SD = ", sd(xbar), "\n")

}


cat("Sample Size = ", "OD", " Mean = ", m,
    " SD = ", std, "\n")






## Sampling Techniques



### 1. Simple Random Sampling without Replacement


set.seed(125)

S <- srswr(50,nrow(ER1))

##S[S != 0]
rows <- (1:nrow(ER1))[S !=0]

srswr.sample.ER1 <- ER1[rows,]

##Show the frequencies for each region (REG). 

a <- table(srswr.sample.ER1$ER_states)


###

##  Summarizing Voter Turnaround % at County Level for Simple Random Sampling Data

options(dplyr.print_max = 25)
options(pillar.sigfig=5)
a3 <- srswr.sample.ER1 %>%
group_by(ER_states) %>%
summarize(ER_Vot_Turnarnd = mean(ER_Vot_Turnarnd))


##  Summarizing Voter Turnaround % at County Level for Original Data

options(dplyr.print_max = 25)
options(pillar.sigfig=5)
a2 <- ER1 %>%
group_by(ER_states) %>%
summarize(ER_Vot_Turnarnd = mean(ER_Vot_Turnarnd))


## Merging Original Sumamrized Data with Simple Random Sampling Sumamrized Data

a4 <- merge(a2,a3,by="ER_states")


##Comparing Original Data with Simple Random Sampling of sample size 50

p30 <- plot_ly(a4) %>% 
add_trace(x = a4$ER_states,
y = a4$ER_Vot_Turnarnd.x,
type = "scatter",
mode = 'lines',
name = 'Original Data') %>% 
add_trace(x = a4$ER_states,
y = a4$ER_Vot_Turnarnd.y,
type = "scatter",
mode = 'lines',
name = 'Simple Random Sampling of 50');p30



### 2. Systematic Sampling


set.seed(125)

N <- nrow(ER1)
n <- 50

k <- ceiling(N/n)

r <- sample(k,1)

# select every kth item

s1 <- seq(r, by = k, length = n)

sys.sample.ER1 <- ER1[s1, ]
##head(sys.sample.ER1)

for(i in 1:n) {
if (s1[i] > N)
s1[i] <- N
}

sys.sample.ER1 <- ER1[s1, ]

##  Summarizing Voter Turnaround % at County Level for Simple Random Sampling Data

options(dplyr.print_max = 25)
options(pillar.sigfig=5)
a5 <- sys.sample.ER1 %>%
group_by(ER_states) %>%
summarize(ER_Vot_Turnarnd = mean(ER_Vot_Turnarnd))


## Merging Original Sumamrized Data with Simple Random Sampling Sumamrized Data

a6 <- merge(a2,a5,by="ER_states")

##Comparing Original Data with Simple Random Sampling of sample size 50

p31 <- plot_ly(a6) %>% 
add_trace(x = a6$ER_states,
y = a6$ER_Vot_Turnarnd.x,
type = "scatter",
mode = 'lines',
name = 'Original Data') %>% 
add_trace(x = a6$ER_states,
y = a6$ER_Vot_Turnarnd.y,
type = "scatter",
mode = 'lines',
name = 'Systematic Sampling of 50');p31



### 3. Stratified sampling 

set.seed(125)
#

## Setting Categories for Voter Turaround % based on sample size of 50, so that atleast 1
## sample get picked up from Stratified sampling

ER1$VoterTurn_Cat <- 
ifelse(ER1$ER_Vot_Turnarnd < 40, "0% - 40%",
ifelse(ER1$ER_Vot_Turnarnd >= 40 & ER1$ER_Vot_Turnarnd < 50, "40% - 50%",
ifelse(ER1$ER_Vot_Turnarnd >= 50 & ER1$ER_Vot_Turnarnd < 60, "50% - 60%",
ifelse(ER1$ER_Vot_Turnarnd >= 60 & ER1$ER_Vot_Turnarnd < 70, "60% - 70%",
ifelse(ER1$ER_Vot_Turnarnd >= 70 & ER1$ER_Vot_Turnarnd < 80, "70% - 80%", 
ifelse(ER1$ER_Vot_Turnarnd >= 80 , "80% and Above",""))))))     

##order.index                                                                            
order.index <- order(ER1$VoterTurn_Cat)

##Arranging Data as per Index
data1 <- ER1[order.index, ]

##Getting Frequencies for each Category
freq <- table(data1$VoterTurn_Cat)

##Some States have more than 100% turnaround. Voter Fraud can be ruled out here. I guess County stat are from 2014 abd 2016 election happened in 2016. SO some under 18 might be eligible for votingv in 2016.

##Getting Proportions based on sample size 50
sizes <- (50 * freq / sum(freq))

st <- strata(data1, stratanames = c("VoterTurn_Cat"),
size = round(sizes),
method = "srswor",description = TRUE)

stf.sample.ER1<- getdata(data1, st)
##head(stf.sample.ER1)

## Summarizing Actual Voter Turnaround for sample data group by states

options(dplyr.print_max = 25)
options(pillar.sigfig=5)
a7 <- stf.sample.ER1 %>%
group_by(ER_states) %>%
summarize(ER_Vot_Turnarnd = mean(ER_Vot_Turnarnd))

## Merginf Stratified sample and Original sumamrized data for Voter Turnaround %

a8 <- merge(a2,a7,by="ER_states")

p32 <- plot_ly(a8) %>% 
add_trace(x = a8$ER_states,
y = a8$ER_Vot_Turnarnd.x,
type = "scatter",
mode = 'lines',
name = 'Original Data') %>% 
add_trace(x = a8$ER_states,
y = a8$ER_Vot_Turnarnd.y,
type = "scatter",
mode = 'lines',
name = 'Stratified Sampling of 50');p32


### End of Final Project Assignment  ###


