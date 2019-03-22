# BitrexInducedUnpalatability

## Termite and bitrex experiments
6 experiment were conducted with termites (with different sets of spiders), the numbers refer to the concentration of bitrex. 0 (no training, no bitrex involved), 1 (training), 1.5 (no training), 2 (no training), 3 (no training), 3_Final (training) refer to different concentration of bitrex (in different experiment, i.e. different set of spiders) and when using these different concentration, some spiders were given prior exposure (aka training) or not.

chronologically, the experiments were not conducted following the gradient of concentration but in this order:  
1 (training)  
2 (no training)  
3 (no training)  
1.5 (no training)  
3_Final (training)  
0 (no training, no bitrex involved at all)


## milkweed bug experiment
Another experiment was conducted with milkweed bugs either raise on milkweed seed or sunflower seed


## movement tests
the movement rates of termites sprayed with water or bitrex were compared
the movement rates of bugs (SF, MW, pinated MW) were compared

## other prey tests
colored crickets, caped termites, fruit flies. Only palatable or unpalatable prey were provided (not simultaneously)


### 1) Raw Data  
Contains all Access Databases woth the raw data


### 2) Data extraction
##### need to run on windows, R 32 bits for package RODBC to call database
##### if problem with making connection ebtween R and Access, consider installing this:
##### https://www.microsoft.com/en-ca/download/details.aspx?id=13255
generate all csv files to use for data analysis

### 3) Extracted Data
#### Folder 'AllAttacks'  
We use these to analyse drop rate

#### Folder 'FirstAttacks' 
We use these to analyses to delay to attack

#### Folder 'FocalAttacks' 
In each test, there were four termites (or for the bug test, 2 bugs), two of one treatment (color and palatibility), and two of the other.
a focal termite group (of two termites) is a randomly selected group in each test.  
We use these to test for the likelihood of attack toward one treatment or another for the first attack
(regardless of color and different palatability)
We sample a focal, run test, save staistics, 1000 times, then average model estimates, CI, LRT, p value.

### 4) Data analyses  
Call CVS file and run stats
Organized per questions/model, testing it in all experiments at once

### 5) Figures
the model estimates are plotted, by removing the intercept, having a factor palatability or a combination of the factor palatability and prior exposure, and having the other factor, color, as a scaled continuous variable.

for attack likelihood: either using all first attacks or a focal first attack (replicated 1000 times and averages)




# TODO: 

* For the other bitrex prey tests (crickets, caped termites, fruit flies). During the caped termite tests, many spiders attacked and dropped the unpalatable prey multiple times, so for analysis we should look at the first attack drop rate and compare it to other parallel experiments where similar prey were given but without bitrex. We still are going to describe, anecdotally that some spiders have attacked multiple and always (?) dropped. 

