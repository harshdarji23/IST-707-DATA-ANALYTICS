#This is a tutorial uses R package "arules" to analyze the market basket data. 

# Load the libraries

# library manuals

# https://cran.r-project.org/web/packages/arules/arules.pdf
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
# http://www.salemmarafi.com/code/market-basket-analysis-with-r/

# install.packages(arules)
# install.packages(arulesViz)
library(arules)
library(arulesViz)


# Load the dataset

groceries <- read.transactions("/Users/byu/Desktop/Data/groceries.csv", format="basket", sep="," )


# We can explore the data before we make any rules. The following script is going to create an item frequency plot for the top 20 items.
# "type" indicates whether item frequencies should be displayed relative or absolute

itemFrequencyPlot(groceries,topN=20,type="absolute")


# Mine rules with the Association Rule algorithm. 
# It is required to set the minimum support and confidence values.

rules <- apriori(groceries, parameter = list(supp = 0.001, conf = 0.8))


# Show the top 5 rules, rounding with 2 digits
# From the top 5 rules, we could notice that the rule {bottled beer, soups} => {whole milk} is strong, indicating bottled beer and soups are frequently bought together with whole milk. So we would suggest to place these items close to each other.

options(digits=2)
inspect(rules[1:5])


# Get summary info about all rules

summary(rules)


# Sort rules so that we can view the most relevant rules first. For example, sort rules with "confidence":

rules<-sort(rules, by="confidence", decreasing=TRUE)


# If we want to target items to generate rules. For example, the frequently bought items with “whole milk”:
# "minlen" is to avoid empty left hand side items.

rules<-apriori(data=groceries, parameter=list(supp=0.001,conf = 0.08, minlen=2), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by='confidence')
inspect(rules[1:5])


# Likewise, we could set "whole milk" on the left hand side as well.

rules<-apriori(data=groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])


# Visualize the rules
# if you encounter "tcl/tk not available" error on mac, see if this page can help you
# http://planspace.org/2013/01/17/fix-r-tcltk-dependency-problem-on-mac/

#plot(rules,method="graph",interactive=TRUE,shading=NA)

# get the top 10 rules sorted by lift

subrules2 <- head(sort(rules, by="lift"), 10)

# plot these rules

plot(subrules2, method="graph")

# plot these rules interactively

plot(subrules2, method="graph", interactive=TRUE)
