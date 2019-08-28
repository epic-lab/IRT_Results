# IRT_Results
#The problem with analyzing all the factors together is that in the "GRM Function" assumes that all of the items on the #measure are assessing the same underlying construct - and that this construct can be #estimated by averaging across all of #the items within the questionnaire. Thus, you are using “ sexual #anxiety” items to estimate “sexual responsibility”, #“sexual responsibility” items to estimate “sexual #competence”, and so on. A better practice might be to evaluate the items #associated with each of the four #scales, and check to see how well they predict the scale with which they are expected to #be associated.

###Sexual Anxiety IRT 

discovery_n = round(nrow(data)*.9)	# get the 90% N rounded to the nearest whole number
discovery = data[1:discovery_n,]  	# defines a new dataset (discovery) as containing rows 1 through n_discovery from the full dataset (msscq)


#Subset just the items for the Sexual Anxiety factor short form 

myvars<-c("Q70","Q81","Q41","Q37")
AnxietySF<-discovery[myvars]



#Fit a graded response model since it handles ordinal polytomous data 
#Constrained models assume that each item have the same level of discrimination.
#Perform ANOVA to determine which model is best, the results suggest an unconstrained model 

fit.1<-grm(AnxietySF, constrained = TRUE)
fit.2<-grm(AnxietySF, constrained = FALSE)
anova(fit.1,fit.2)



#Plot each items' response categorical curve 
#An item is better at discriminating between individuals when the curves are peaked and dispersed across all levels of the #latent trait. For example, an item with high discrimination would have 6 peaks dispersed from low levels of the latent #trait to high levels of the latent trait. 

fit.AnxietySF<- grm(AnxietySF, constrained = FALSE)
summary(fit.AnxietySF)
plot(fit.AnxietySF)
