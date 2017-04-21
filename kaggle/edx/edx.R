library(ggplot2)
library(grid)
library(gridExtra)

setwd('/home/dbgustlr92/kaggle/edx')
edx_csv <- read.csv('appendix.csv')

str(edx_csv)

# The number of course by Institution 
ggplot(edx_csv, aes(edx_csv$Institution)) + geom_bar(aes(fill=edx_csv$Institution)) +
  xlab('Institution') + ylab('Count') + guides(fill=FALSE)


# The number of course by Year and Subject
harvardX <- edx_csv[edx_csv$Institution=="harvardX",]
mitX <- edx_csv[edx_csv$Institution!="harvardX",]
ggplot(harvardX, aes(harvardX$Year)) + geom_bar(aes(fill=harvardX$Course.Subject), position='dodge') + xlab('Year') + ylab('Count') + labs(fill='Institution')
ggplot(mitX, aes(mitX$Year)) + geom_bar(aes(fill=mitX$Course.Subject), position='dodge') + xlab('Year') + ylab('Count') + labs(fill='Institution')

# The number of course by Subject and Institution
ggplot(edx_csv, aes(edx_csv$Course.Subject)) + geom_bar(aes(fill=edx_csv$Institution)) +
  xlab('Subject') + ylab('Count)') + labs(fill='Institution') +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=0))

# Posting in Forum and Total Course Hours is affected Certificated?
ggplot(edx_csv, aes(edx_csv$Total.Course.Hours..Thousands., edx_csv$X..Posted.in.Forum)) +
  geom_point(aes(colour=edx_csv$X..Certified)) +scale_colour_gradient(high = "green") +
  labs(list(x='Total Course Hours', y='Posted in Forum(%)', colour='Certificated(%)'))

# Density of Total Course Hours by Subject  
ggplot(edx_csv, aes(edx_csv$Total.Course.Hours..Thousands., colour=edx_csv$Course.Subject)) + 
  geom_density()

# Calculate slope and intercept of line of best fit
coef(lm(Total.Course.Hours..Thousands. ~ X..Certified, data = edx_csv))
ggplot(edx_csv, aes(x=edx_csv$Total.Course.Hours..Thousands., y=edx_csv$X..Certified)) +
  geom_point(colour='grey') + geom_abline(intercept = 108.384224, slope = -1.722101, colour='red') +
  labs(x='Total Course Hours', y='Certified(%)') + geom_smooth()

# Relationship between Total Course Hours and Participants
ggplot(edx_csv, aes(x=edx_csv$Total.Course.Hours..Thousands., y=edx_csv$Participants..Course.Content.Accessed.)) +
  geom_point(aes(colour=edx_csv$Course.Subject)) + scale_y_continuous(labels=scaleFUN) +
  labs(x='Total Course Hours', y='Number of Participants', colour='Subject')



str(edx_csv)
