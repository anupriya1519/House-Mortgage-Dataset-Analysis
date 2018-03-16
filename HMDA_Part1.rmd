---
title: "Exploratory Data Analysis Home Mortgage NY"
output: html_notebook
---


####1. Introduction

The Home Mortgage Disclosure Act (HMDA) requires many financial institutions to maintain, report, and publicly disclose information about mortgages.This dataset covers all mortgage decisions made in 2015 for the state of New York.

####2. Understanding the problem - Business Perspective
Before we dive into solve the problem, let us first understand the business related to this dataset. 


####2.1 What is HMDA ?

Each year thousands of banks and other financial institutions report data about mortgages to the public, thanks to the Home Mortgage Disclosure Act, or "HMDA" for short. These public data are important because:

--Help show whether lenders are serving the housing needs of their communities;
--Give public officials information that helps them make decisions and policies; and
--Shed light on lending patterns that could be discriminatory

####2.2 Loan Origination Journey
My friend Rose wants to buy a home but she doen't have enough money to pay in cash, so she applies for a loan at her bank. Bank collects all the information related to her finances and the property she is wiiling to buy.These information helps the bank to make a decison whether or not to lend her money, and the terms of the loan. The bank reviews Rose's background and decides that she mets their criteria, and she gets approved.Once all the papers are signed, Rose closes the loan. or in mortgage-speak, the loan is "originated.".Therefore the last stage of the loan is Loan Origination.

#### In the following steps we will learn how to build a classification tree to find out the deciding variables or the most important variables on which a loan application depends.

Step1- It includes loading the dataset and reading.

```{r}
hmda<- read.csv('D:/Rutgers Study Material/Rutgers Study Material/DADM/Project/ny-home-mortgage/ny_hmda_2015.csv')
# Just to check if dataset is properly loaded or not, we will use head
head(hmda,2)
mortgage<-head(hmda,45000)
```

Step2- Gaining some insights about the data.

```{r}
# 'names' will return all the column names in the dataset
names(mortgage)

# we will look at the structure and dimension of the dataset
str(mortgage)
dim(mortgage)
```


There are 78 coulmns and 45000 rows of data.

Step3- This is the most important step, data cleansing. 

```{r}
#Function to check if there are any NA values in the dataset
sapply(mortgage,function(x) sum(is.na(x)))
```

Lots of NA values are recorded. 

```{r}
# The most up-to-date method for handling missing data is to use multiple imputations.
# Load the mice package
library(mice)
# pattern of missing data
md.pattern(mortgage)
```

```{r}

# multiple imputations
imp <- mice(mortgage, m=1, maxit=2, method='cart', seed=500)


```
```{r}

# completed data
home <- mice::complete(imp)
```

Here we use 'cart'(classification and regression trees) as the imputation method. Now R does not need to do any X matrix inversion.

```{r}
summary(home)
```

#### 3.Understanding the problem - Data Perspective
The data provided can be grouped into the following subjects

Location describes the State, metro area and census tract of the property

Property Type describes the Property Type and Occupancy of the property.Property type values include One-to-four family dwelling,Manufactured housing and Multifamily dwelling. This also answers the question "Will the owner use the property as their primary residence ?" . The values include Owner occupied as principal dwelling , Not owner occupied as principal dwelling and Not Applicable.

Loan describes the action taken on the Loan, purpose of the Loan , Type of the loan ,Loan's lien status.

Lender describes the lender associated with the loan and the Federal agency associated with the loan.

Applicant describes the demographic information for the applicants and the co-applicants.This has the applicant sex , co- applicant sex , applicant race and ethnicity, co- applicant race and ethnicity.


#### Analyzing the data with the power of visualization


In this section, we examine the distribution of the various Actions on Loans. As discussed in the previous section, we would be interested in the loan action Loan Origination since this status signifies that the loan has been flagged off to be given to the applicant.


```{r}
homeMortgageStatus_ethnicity = home %>% group_by(action_taken_name,applicant_ethnicity_name) %>%
  summarise(CountOfActionTaken = n()) %>%
  arrange(desc(CountOfActionTaken))

homeMortgage_ethnicity = home %>% group_by(applicant_ethnicity_name) %>%
  summarise(CountOfEthnicity = n()) %>%
  arrange(desc(CountOfEthnicity))

ggplot(homeMortgage_ethnicity, aes(x = reorder(applicant_ethnicity_name, CountOfEthnicity), 
                                          y = CountOfEthnicity)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = applicant_ethnicity_name, y = 1, label = paste0("(",round(CountOfEthnicity),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'applicant_ethnicity_name', y = '%age Count Of Action Taken', title = 'Actions in Loans') +
  coord_flip() + 
  theme_bw()
```

The Not Hispanic or Latino ethnic community applies for the largest percentage of the loans.

```{r}
homeMortgageStatus_applicant_race1 = home %>% group_by(action_taken_name,applicant_race_name_1) %>%
  summarise(CountOfActionTaken = n()) %>%
  arrange(desc(CountOfActionTaken))

homeMortgage_applicant_race1 = home %>% group_by(applicant_race_name_1) %>%
  summarise(CountOfRace1 = n()) %>%
  arrange(desc(CountOfRace1))

ggplot(homeMortgage_applicant_race1, aes(x = reorder(applicant_race_name_1, CountOfRace1), 
                                   y = CountOfRace1)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = applicant_race_name_1, y = 1, label = paste0("(",round(CountOfRace1),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Race Name', y = 'Count Of Action Taken', title = 'Actions in Loans by Race') +
  coord_flip() + 
  theme_bw()
```

```{r}
actionStatus = "Loan originated"
breaks = seq(0,400,50)

home %>%
  filter(action_taken_name == actionStatus ) %>%
ggplot(aes(applicant_income_000s)) +
  scale_x_continuous(limits = c(0, 400),breaks=breaks ) +
  geom_histogram(binwidth = 10,,fill = c("red")) +
  labs(x = 'Income in Thousands', y = 'Count', title = 'Loan Originated Applicant Income distribution') +  theme_bw()
```
We observe that MOST of the loans which are originated have applicants with income around Sixty Thousand to Seventy Five thousand dollars.


#### Loan Purpose Types
We investigate the different loan Purpose Types associated with the loans.
Loan Purpose Types distribution
 
```{r}
home %>%
  filter(!is.na(loan_purpose_name)) %>%
  group_by(loan_purpose_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  mutate(percentage = ( CountLoanPurpose/sum(CountLoanPurpose) ) *100 ) %>%
  mutate(loan_purpose_name = reorder(loan_purpose_name, percentage)) %>%
  
  ggplot(aes(x = loan_purpose_name,y = percentage)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = loan_purpose_name, y = 1, label = paste0("( ",round(percentage),"% )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Loan Purpose', y = 'Count', title = 'Loans Purpose Types') +
  coord_flip() + 
  theme_bw()
```
Home Purchase and Refinancing are the major Loan Purpose types.

####Counties and Loan distribution
We display the Counties and the Loans Type distribution.


```{r}
Top10Counties = hmda %>%
  filter(!is.na(county_name)) %>%
  group_by(county_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  mutate(percentage = ( CountLoanPurpose/sum(CountLoanPurpose) ) *100 ) %>%
  mutate(county_name = reorder(county_name, percentage)) %>%
  arrange(desc(percentage)) %>%
  head(10)

hmda %>%
  filter(!is.na(county_name)) %>%
  filter(county_name %in% Top10Counties$county_name) %>%
  group_by(county_name,action_taken_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  
  
  ggplot(aes(x = county_name,y = CountLoanPurpose,fill = action_taken_name)) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'County Name', y = 'Count', title = 'County Distribution with Action Types') +
  theme_bw() + theme(legend.position="top") 
```

####Loan purpose types and their actions
The following bar graph shows the Loan Purpose Types along with the different actions.

```{r}
hmda %>%
  filter(!is.na(loan_purpose_name)) %>%
  group_by(loan_purpose_name,action_taken_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  
  ggplot(aes(x = loan_purpose_name,y = CountLoanPurpose,fill =(action_taken_name))) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'Loan Purpose', y = 'Count', title = 'Loans Purpose Types Distribution with Action Types') +
  theme_bw()
```


####Modelling using Classification and Regression Trees
We predict whether the status of the Loan would be Loan originated or not. The following tree shows the conditions which would be used to determine whether the would be Loan originated or not.


Select Columns for modelling
Here we select the columns which would be required for modelling.We make the columns as factors so that they can be used for the CART model.

```{r}
selectedCols = c("action_taken","applicant_ethnicity",        
"applicant_income_000s","applicant_race_1","co_applicant_ethnicity",
"co_applicant_sex", "county_code","hoepa_status","lien_status",
"loan_purpose","loan_type","msamd",                                              
"owner_occupancy","preapproval",
"property_type","purchaser_type","loan_amount_000s")


homeMortgage_selectedCols = hmda %>% select(selectedCols) %>%
  mutate(isLoanOriginated = FALSE)  %>%
  mutate(isLoanOriginated = replace(isLoanOriginated, action_taken == 1, TRUE)) %>%
  select(-action_taken)

homeMortgage_selectedCols$applicant_ethnicity = as.factor(homeMortgage_selectedCols$applicant_ethnicity)
homeMortgage_selectedCols$applicant_race_1 = as.factor(homeMortgage_selectedCols$applicant_ethnicity)
homeMortgage_selectedCols$co_applicant_ethnicity = as.factor(homeMortgage_selectedCols$co_applicant_ethnicity)
homeMortgage_selectedCols$co_applicant_sex = as.factor(homeMortgage_selectedCols$co_applicant_sex)
homeMortgage_selectedCols$county_code = as.factor(homeMortgage_selectedCols$county_code)
homeMortgage_selectedCols$hoepa_status =  as.factor(homeMortgage_selectedCols$hoepa_status)
homeMortgage_selectedCols$lien_status =  as.factor(homeMortgage_selectedCols$lien_status)
homeMortgage_selectedCols$loan_purpose =  as.factor(homeMortgage_selectedCols$loan_purpose)
homeMortgage_selectedCols$loan_type =  as.factor(homeMortgage_selectedCols$loan_type)
homeMortgage_selectedCols$owner_occupancy =  as.factor(homeMortgage_selectedCols$owner_occupancy)  
homeMortgage_selectedCols$preapproval =  as.factor(homeMortgage_selectedCols$preapproval)  
homeMortgage_selectedCols$property_type =  as.factor(homeMortgage_selectedCols$property_type) 
homeMortgage_selectedCols$purchaser_type =  as.factor(homeMortgage_selectedCols$purchaser_type) 
```


####Build and Visualize the CART model
We build and visualize the CART model. Through this model, we can examine the most important features which impact the decision for Loan Origination.

```{r}
set.seed(3000)
split = sample.split(homeMortgage_selectedCols$isLoanOriginated, SplitRatio = 0.8)
Train = subset(homeMortgage_selectedCols, split==TRUE)
Test = subset(homeMortgage_selectedCols, split==FALSE)
   
# CART model
homeMortgageTree = rpart(isLoanOriginated ~., method="class", data = Train, control=rpart.control(minbucket=5))

prp(homeMortgageTree)
```


Performance of the model:
```{r}
library(ROCR)
roc_pred <- prediction(pred[,1], Test$isLoanOriginated)
plot(performance(roc_pred, measure="tpr", x.measure="fpr"), colorize=TRUE)
```



```{r}
plot(performance(roc_pred, measure="lift", x.measure="rpp"), colorize=TRUE)
```

Here we can see that the model is not doing very well. The tighter the ROC curve hugs towards the left the better is the model.

Sensitivity/specificity curve and precision/recall curve:

```{r}
plot(performance(roc_pred, measure="sens", x.measure="spec"), colorize=TRUE)
plot(performance(roc_pred, measure="prec", x.measure="rec"), colorize=TRUE)
```


####Conclusion: 
Lien_stat, Purchase, Loan_Pur, Loan_type, County_c and Loan_amount are the most important variables to decide whether a mortgage application will be accepted or not. 




