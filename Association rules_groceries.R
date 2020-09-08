#Setting woring directory
setwd('C:\\Users\\singh\\Documents\\Association Rules')

#Loading required packages
library(arules)

Groceries <- read.csv('groceries.csv',header = FALSE)
head(Groceries)
Groceries <- read.transactions('groceries.csv', sep=',')
class(groceries)

inspect(head(Groceries, 3))

size(head(Groceries))
LIST(head(Groceries, 3))

#Most frequent items

frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
inspect(frequentItems)
#  items                         support   
#  {other vegetables,whole milk} 0.07483477
#  {whole milk}                  0.25551601
#  {other vegetables}            0.19349263
#  {rolls/buns}                  0.18393493
#  {yogurt}                      0.13950178
#  {soda}                        0.17437722
itemFrequencyPlot(Groceries, topN=10, type="absolute", main="Item Frequency") 

#Getting the product recommendation rules

rules_1 <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.5)) 
rules_1
#set of 5668 rules 
#Support = 0.001, confidence = 0.5
rules_conf <- sort (rules_1, by="confidence", decreasing=TRUE)
rules_conf
inspect(head(rules_conf))

rules_lift <- sort (rules_1, by="lift", decreasing=TRUE) 
inspect(head(rules_lift))
#  lhs                        rhs                  support confidence    coverage     lift count
#[1] {Instant food products,                                                                      
#  soda}                  => {hamburger meat} 0.001220132  0.6315789 0.001931876 18.99565    12
#[2] {popcorn,                                                                                    
 # soda}                  => {salty snack}    0.001220132  0.6315789 0.001931876 16.69779    12
#[3] {baking powder,                                                                              
 # flour}                 => {sugar}          0.001016777  0.5555556 0.001830198 16.40807    10
#[4] {ham,                                                                                        
 # processed cheese}      => {white bread}    0.001931876  0.6333333 0.003050330 15.04549    19
#[5] {Instant food products,                                                                      
 # whole milk}            => {hamburger meat} 0.001525165  0.5000000 0.003050330 15.03823    15
#[6] {curd,                                                                                       
 # other vegetables,                                                                           
  #whipped/sour cream,                                                                         
  #yogurt}                => {cream cheese}   0.001016777  0.5882353 0.001728521 14.83409    10

#The rules with confidence of 1 (see rules_conf above) imply that, whenever the LHS item was purchased, the RHS item was also purchased 100% of the time.

#We will now try rules with different support and confidenc interval


Grocery.rules_A <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.25, minlen = 2))
Grocery.rules_A
#set of 17391 rules 

Grocery.rules_B <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.5, minlen = 2))
Grocery.rules_B
#set of 5668 rules 

Grocery.rules_C <- apriori(Groceries, parameter = list(support = 0.002, confidence = 0.5, minlen = 2))
Grocery.rules_C
#set of 1098 rules 

Grocery.rules_3 <- apriori(Groceries, parameter = list(support = 0.002, confidence = 0.25, minlen = 2))
Grocery.rules_3
#set of 4024 rules


Grocery.rules_4 <- apriori(Groceries, parameter = list(support = 0.003, confidence = 0.25, minlen = 2))
Grocery.rules_4
#set of 1771 rules 


Grocery.rules_5 <- apriori(Groceries, parameter = list(support = 0.003, confidence = 0.5, minlen = 2))
Grocery.rules_5
#set of 421 rules

#Controlling the number Of rules in Output 

rules <- apriori(Groceries, parameter = list (supp = 0.001, conf = 0.5, maxlen=3)) 
rules


#REMOVING REDUNDANT RULES

subsetRules <- which(colSums(is.subset(rules, rules)) > 1) # get subset rules in vector
length(subsetRules)  
rules <- rules[-subsetRules] 
rules


#Find what factors influenced purchase of product X

rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="whole milk"), control = list (verbose=F)
rules_conf <- sort(rules, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf))

#Finding out what products were purchased after/along with product X
rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.15,minlen=2), appearance = list(default="rhs",lhs="whole milk"), control = list (verbose=F)) # those who bought 'milk' also bought..
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf))
                  
                                                      
                  
