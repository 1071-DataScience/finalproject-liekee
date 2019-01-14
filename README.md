#  Can Love be predictable?

Please Check the /final_project.md for our full markdown!

### Groups
*  Lieke van Uden,  107300601
*  Felix Ude, 106266012


### Goal
The goal of our project is that we want to test whether love is predictable. Our dataset, collected by Columbia Business School, consists of data about the results of speed dates. The data was gathered from participants from 2002-2004. Each participant contributes in multiple speed dates at the organised events. At the end of each speed date the participants should answer the question if they would like to see their data again. If both participants of the date say yes, then it is a match. We would like to predict these matches with our model. 

The dataset consists of some demographics, dating habits, and lifestyle information of the participants. Also, after each speed data the participant are asked to rate six key attributes of their date: attractiveness, sincerity, intelligence, fun, ambition and share interests. We created features by calculating the differences between certain variables of the two participants in a speed date. For example, we calculated the age difference, income difference, and interest difference for all the speed dates. All these features are used to predict whether there will be a match between two participants. 


### demo 
There is a script file in \code for you to check out. The skip flag is for skipping the cross-validation process and just use the parameters we found, since the process is very time consuming. 
```R
Rscript code/final_project_script.R --input input.csv --output output.csv --skip 1
```
For a full markdown see /final_project.md

## Folder organization and its related information

### docs
* Your presentation, 1071_datascience_FP_<yourID|groupName>.ppt/pptx/pdf, by **Jan. 15**
  - Documentation for the variables of our dataset

### data
Source
The dataset is collected by Columbia Business School professors Ray Fisman and Sheena Iyengar. The dataset can be found on Kaggle: https://www.kaggle.com/annavictoria/speed-dating-experiment/home

Input Format
After cleaning we have 310 unique participants in our data, and 129 dimensions. 

Preprocessing
We handled the missing data (NA) for the categorical variables by setting the corresponding columns to the value “Other”. After that we matched the participants whom went on a date with each other and merged these entries. Such that in each row we have the information about both participants of the speed date. After that, we did some feature creation. For each date, we calculated the difference in certain variables of the participants for numeric variables and created a dummy variable in case of categorical variables, which indicates whether the variable for both partners was the same or not. For example, we calculated the age difference, income difference and interests difference. This could be an indication whether two participant are more similar and have the same interests or that they have total different interests and demographics. This could influence the results of the speed date.  


### code
We used a Support Vector Machine and picked the preliminary within a 10 k-fold cross-validation. 
We split our data in a training and test set with a 0.8/02. ration. The k-fold cross-validation was then performed onto the Training Set. The model with the best parameters where then tested with the test set. 
The decision for this method was based on some prelimenary tests and SVM seems to work well in our case since we are dealing with a high number of dimensions. We received an AUC of 0.84, which is higher than the Null-Model (in which case all predictions where 0 for a match) with the standard AUC of 0.5.
For the full code with comments please see the /final_project.md!

### results

We decided that the overall accuracy for the prediction of a match is not the most relevant. We concluded that the Recal is the most important performance metric, since we think that it is better to predict a match that is not a match than the other way around, so that the participants won't lose their opportunity of meeting again. This let us choose a lower threshhold, than if we had tried to optimize our estimator for the highest accuracy. In the end our accuracy was 74.81%, however we achieved a Recall of 79.03%.
