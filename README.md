# BitrexInducedUnpalatability

6 experiment were conducted (with different sets of spiders), the numbers refer to the concentration of bitrex. 0 (no training, no bitrex involved), 1 (training), 1.5 (no training), 2 (no training), 3 (no training), 3_Final (training) refer to different concentration of bitrex (in different experiment, i.e. different set of spiders) and when using these different concentration, some spiders were given prior exposure (aka training) or not.

chronologically, the experiments were not conducted following the gradient of concentration but in this order:  
1 (training)  
2 (no training)  
3 (no training)  
1.5 (no training)  
3_Final (training)  
0 (no training, no bitrex involved at all)

## 1) Raw Data  
Contains all Databases:  
VideoAnalyses_0_control_BitrexTermites.accdb (basal drop rate test),   VideoAnalyses_1_5BitrexTermites.accdb (1.5%),   VideoAnalyses_1BitrexTermites.accdb (1%, with training),   VideoAnalyses_2BitrexTermites.accdb (2%),   VideoAnalyses_3_Final_BitrexTermites.accdb (3% final test with training),   and VideoAnalyses_3BitrexTermites.accdb (3% intermediate with no training)  


## 2) Data extraction
### need to run on windows, R 32 bits for package RODBC to call database
#### if problem with making connection ebtween R and Access, consider installing this:
##### https://www.microsoft.com/en-ca/download/details.aspx?id=13255
generate csv file to use for data analysis

## 3) Extracted Data
### Folder 'AllAttacks'  
'AllAttacks.csv'(all attacks for 1%), 'AllAttacks1_5.csv' (all attacks for 1.5%), 'AllAttacks2.csv'
(all attacks for 2%), 'AllAttacks3.csv'(all attacks for first round of 3%, with no training), and 'AllAttacks3_final.csv'(for all attacks of the last round of 3%, with training)
We use these to analyse drop rate


### Folder 'FirstAttacks' 
similar structure as above  
We use these to analyses to delay to attack


### Folder 'FocalAttacks' 
In each test, there were four termites, two of one treatment (color and palatibility), and two of the other.
a focal termite group (of two termites) is a randomly selected group in each test.  
We use these to test for the likelihood of attack toward one treatment or another for the first attack
(regardless of color and different palatability)
file names are structured as above  



## 3) Data analyses  
Call CVS file and run stats



# TODO: 

* check wish fisher exact test is apropriate to compare drop rate when palatable prey, before or after attacking a non palatable prey. This will tell us if the contamination is on the mouth parts or not.

* We still have to create a script for the termite movement videos (just average gridlines crossed comparing each treatment?), and for the other bitrex prey tests (crickets, caped termites, fruit flies). During the caped termite tests, many spiders attacked and dropped the unpalatable prey multiple times, so for analysis we should look at the first attack drop rate and compare it to other parallel experiments where similar prey were given but without bitrex. We still are going to describe, anecdotally that some spiders have attacked multiple and always (?) dropped. 
