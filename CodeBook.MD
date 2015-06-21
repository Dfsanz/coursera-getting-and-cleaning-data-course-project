### Introduction

This document provides a description of the raw data set, attribute information and transformations run on the data.  A full description of the raw data set can be found here: http://bit.ly/1mEvWTG

The information is also provided below for convenience.

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

A video of the experiment including an example of the 6 recorded activities with one of the participants can be seen in the following link: https://www.youtube.com/watch?v=XOEN9W05_4A


### Attribute Information

For each record in the dataset it is provided: 
* Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
* Triaxial Angular velocity from the gyroscope. 
* A 561-feature vector with time and frequency domain variables. 
* Its activity label. 
* An identifier of the subject who carried out the experiment.

### Data Transformations

1) Data sets are merged

The following data sets are merged using the rbind and cbind functions:

Input Files:

* x_test.txt
* y_test.txt
* subject_test.txt
* x_train.txt
* y_train.txt
* subject_train.txt
* features.txt

Output Variable:

all.data.merged

2) We apply a filter using the grep function to the all.data.merged data set produced above so that only the mean and standard deviation calculations of the data set are retained

Input Variable:

all.data.merged

Output Variable:

all.data.filtered

3) We iterate through each row from the all.data.filtered data set and substitue the activity number with more descriptive labels contained in the activity_labels.txt file.

4) We substitute abbrevations contained within column heads in the all.data.filtered data set with more descriptive labels.

5) We take the mean of each variable and group by subject and activity.  We then generate the tidydata.txt file and write this file to disk.


