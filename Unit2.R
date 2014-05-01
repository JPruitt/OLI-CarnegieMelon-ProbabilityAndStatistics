## Unit 2: Producing Data

## Sampling

load("./student_survey.RData")
population

## The data have been loaded into the variable "population." 
## The dataset includes the following variables:
## Course: natural science, social science, or business
## Handed: right-handed or left-handed
## Sex: female or male
## Verbal: SAT Verbal scores up to 800
## Age: in years
## First, we will take a simple random sample of the data. For the sake of consistency, 
## we will make the random sample the same size (192) as the nonrandom sample of business 
## statistics students that will be examined later.

random_sample = population[sample(1:length(population$Course),192),]
random_sample

## A. Now we will determine whether the four variables' behavior for the random sample 
## is comparable to their behavior for the population.

## 1. To compare the proportion of right-handed students in the sample to those in the population, 
## create 2 pie charts, one for handedness in the population and one for handedness in the random sample.

random_sample_percent = 100*summary(random_sample$Handed)/length(random_sample$Handed)
random_sample_percent
pop_percent = 100*summary(population$Handed)/length(population $Handed)
pop_percent
par(mfrow=c(1,2))
pie(pop_percent,labels=paste(c("left=","right="),round(pop_percent,0),"%"),main="Population")
pie(random_sample_percent,labels=paste(c("left=","right="),
                                       round(random_sample_percent,0),"%"),main="Random Sample")

## Consider the distributions to be comparable if the sample proportion comes within about 5% of 
## the population proportion. Does it? (Use the text box in the first Learn By Doing exercise below 
## to record your answer.)

## 2. To compare the proportion of female students in the sample to the proportion in the population, 
## create 2 pie charts, one for sex in the population and one for sex in the random sample.

random_sample_percent = 100*summary(random_sample$Sex)/length(random_sample$Sex)
random_sample_percent
pop_percent = 100*summary(population$Sex)/length(population $Sex)
pop_percent
par(mfrow=c(1,2))
pie(pop_percent,labels=paste(c("female=","male="),round(pop_percent,0),"%"),main="Population")
pie(random_sample_percent,labels=paste(c("female=","male="),
                                       round(random_sample_percent,0),"%"),main="Random Sample")

## Consider the distributions to be comparable if the sample proportion comes within about 5% of 
## population proportion. Does it? (Use the text box in the first Learn By Doing exercise below to 
## record your answers.)

## 3. Create 2 descriptive statistics summary tables—one for SAT Verbal score in the population and 
## one for SAT Verbal score in the sample

summary(population$Verbal) 
summary(random_sample$Verbal)

## Since SAT scores tend to follow a normal (symmetric) distribution, you can focus on means to make 
## a comparison. Consider the distributions to be comparable if the sample mean SAT Verbal score comes 
## within about 10 points of the population mean. Does it? (Use the text box in the first Learn By 
## Doing exercise below to record your answers.)

## 4. Create 2 more descriptive statistics summary tables—one for age in the population and one for age 
## in the sample. 

summary(population$Age) 
summary(random_sample$Age)

## Since Age tends to follow a right-skewed distribution, you should focus on medians to make a 
## comparison. Consider the distributions to be comparable if the sample median age comes within 
## about .5 years of the population median. Does it?

## C. How representative is the (nonrandom) sample of students in the business statistics course, 
## in actuality? In order to answer this question, we will need to extract this group from the population.

business = population[population$Course=="Business",]
business 

## Next, we explore whether the four variables' behavior for the (nonrandom) sample of business 
## statistics students is comparable to their behavior for the population:

business_percent = 100*summary(business$Handed)/length(business$Handed)
business_percent
pop_percent = 100*summary(population$Handed)/length(population$Handed)
pop_percent
par(mfrow=c(1,2))
pie(pop_percent,labels=paste(c("left=","right="),round(pop_percent,0),"%"),main="Population")
pie(business_percent,labels=paste(c("left=","right="),
                                       round(business_percent,0),"%"),main="business Sample")

business_percent = 100*summary(business$Sex)/length(business$Sex)
business_percent
pop_percent = 100*summary(population$Sex)/length(population $Sex)
pop_percent
par(mfrow=c(1,2))
pie(pop_percent,labels=paste(c("female=","male="),round(pop_percent,0),"%"),main="Population")
pie(business_percent,labels=paste(c("female=","male="),
                                       round(business_percent,0),"%"),main="business Sample")

summary(population$Verbal) 
summary(business$Verbal)

summary(population$Age) 
summary(business$Age)

## The male to female ratio in the business sample is bias toward males.

## Designing Studies
## Causation and Experiments

load("./computers.RData")

## The data have been loaded into the variable "computers." This dataset has more than 20,000 
## entries, so we will not display it. To verify that the data have been loaded into the variable 
## computers, copy the following command to R: 

summary(computers)

## Our dataset contains the values of the three possible lurking variables:
  
## age: in years
## gender: female or male
## comp: hours per week of computer use

## The company must rely upon sampling to study its customers' preferences, since the entire 
## population cannot be assigned to treatments. Therefore, we will first choose a simple random 
## sample (SRS) of 450 people for the subjects in the study.

## To choose the sample, copy the following command to R:

random_sample = computers[sample(1:length(computers $age),450),]
summary(random_sample)

## By looking at the numbers of males and females in the sample, we see that the sample indeed 
## has 450 entries.

## Now we will randomly assign our SRS of 450 subjects to treatment groups, one for each of the 
## three versions of the ISP's software. Let's denote the versions "1," "2," and "3," and create 
## a categorical variable to identify the treatment for each subject.

## To use R to randomly assign the 450 subjects to one of three treatments, copy the following command: 

group = floor(runif(450,min=1,max=4)) 
random_sample = cbind(random_sample,group)

## We are finally reaching the goal of this activity. We will now examine whether the 
## randomization was successful in making our three treatment groups similar with respect 
## to the variables age, gender, and comp. In other words, we will now examine whether the 
## distributions of these variables in the three groups are similar or not.

## To compare the distribution of age among the three treatment groups, we'll create side-by-side 
## boxplots of age by treatment. 

boxplot(random_sample[group==1,]$age,random_sample[group==2,]$age,random_sample[group==3,]$age)

## To compare the distribution of gender among the three treatment groups, we'll look at a two-way 
## table of conditional percents:

two_way_table = table(random_sample$group,random_sample$gender) 
100*two_way_table / rowSums(two_way_table)

## To compare the distribution of comp (the hours per week of computer use) among the three 
## treatment groups, we'll create side by side boxplots of comp by treatment. 
## Follow the instructions above, making the obvious necessary changes.

boxplot(random_sample[group==1,]$comp,random_sample[group==2,]$comp,random_sample[group==3,]$comp)

load("./depression.RData")

head(depression, 10)

table(depression$Treat)
plot(factor(depression$Treat), depression$AcuteT)

summary(depression)

tbl<-table(data.frame(depression$Treat, depression$Outcome))
tbl
100*tbl/rowSums(tbl)

plot(factor(depression$Treat), depression$Time)
tapply(depression$Time, factor(depression$Treat), summary)
