# data

This directory holds all the data files used in the main code. 2D calculations and selected derived 3D outputs were calculated using the 2D-calculations script and added to the data collected. After that, these files were not touched and therefore contain all relevant, high-quality, nano-CT data for apatite dataset.  

20220704_apatite-data is the main data file 
20220718_size-distribution is a data file used to make Fig02
date-eu-hecalc is a data file used to make Fig07 (taken from table D01)

The percent difference files are simply the numbers required to plot the percent difference lines on the regression plots. 

**Explanation of column names for apatite-data below:**

- id: id #
- sample: unique sample #
- gem: gem value assigned from Fig. 3
- gc: geometric classification (letter from GEM) 
- ri: roughness index (number from GEM) 
- np_obs: number of pyramidal terminations stored as text
- np_num_obs: number of pyramidal terminations stored as a number 
- geo: geometry (GEM A and B = hexagonal; GEM C = ellipsoid)
- broken: notes on whether grain is broken or not
- notes: descriptive notes 
- size bin: size bin the maximum width falls in 
- size_cat: if max width < 50, small. if max width 50-100, medium. if max width > 100, large. 
- mount: mount number from CT (see Figure 5)

For columns N-AL, the following suffixes are appended to the parameter: 
- [parameter]_3d: the value obtained from CT 
- [parameter]_both: the value obtained from using the min and max width in 2D calculations 
- [parameter]_max: the value obtained from using the max width only in 2D calculations 
- [parameter]_max_2np: the value obtained from using the max width only and assuming 2 Np in 2D calculations

Parameters: isotope specific FTs, FTcombined, volume, surface area, Rs (SA/V), RFT

- l1: first measured length
- w1: max width
- l2: second measured length
- w2: min width
- w_max: checks to make sure width listed in w1 is the maximum 
- l_avg: average length
- w_avg: average width 
- grain: apatite
 
