# BitrexInducedUnpalatability

## Data extraction: need to run on windows, R 32 bits for package RODBC to call database
generate csv file to use for data analysis

## Data analysis: call CVS file and run stats



Folder 'AllAttacks' contains: 'AllAttacks.csv'(all attacks for 1%), 'AllAttacks1_5.csv' (all attacks for 1.5%), 'AllAttacks2.csv'
(all attacks for 2%), 'AllAttacks3.csv'(all attacks for first round of 3%, with no training), and 'AllAttacks3_final.csv'(for all attacks of the last round of 3%, with training)

Folder 'DataAnalysis' , 'DataExtractionHandling' , and  'FocalTermiteAttack' follow the same format as above 

Folder 'VideoAnalyses' contains ALL databases: VideoAnalyses_0_control_BitrexTermites.accdb (basal drop rate test), VideoAnalyses_1_5BitrexTermites.accdb (1.5%), VideoAnalyses_1BitrexTermites.accdb (1%, with training), VideoAnalyses_2BitrexTermites.accdb (2%), VideoAnalyses_3_Final_BitrexTermites.accdb (3% final test with training), and VideoAnalyses_3BitrexTermites.accdb (3% intermediate with no training)



#IMPORTANT: 

 1. The script ran for 3% final tests does not include the training. I couldn't run the same 1% script on the data since that script was specified to the first dataset, so I ran the intermediate script. We got the results for the drop rate, but not the effect of the training, color, etc.

 2. There is no script for the control basal drop rate test. The database will say which color termite was attacked and dropped/consumed, but I have no way of analyzing the drop rate since the script wants to analyze a focal termite.

3. We still have to create a script for the termite movement videos (just average gridlines crossed comparing each treatment?), and for the other bitrex prey tests (crickets, caped termites, fruit flies). During the caped termite tests, many spiders attacked and dropped the unpalatable prey multiple times, so for analysis we should look at the total drop rate and compare it to our other datasets. 

 4. We need to analyze the drop rate of palatable termites when a spider has attacked a palatable termite first, compared to when the spider attacked a bitrex termite first and then went on to attack a palatable termite after. This will tell us if the contamination is on the mouth parts or not.

5. The other data (other than drop rate), such as if they were more likely to attack a particular treatment, delay to attack, color preference, or if training/prior exposure matters (for the 1% and final 3% tests) needs to be accounted for- the data is accessable with the script, I just haven't written it all out yet.