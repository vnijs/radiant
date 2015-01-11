Brand maps based on (dis)similarity data can be analyzed using Multi-Dimensional Scaling (MDS).

#### Example 1

The city data (city) contains information on distances in miles between 10 major cities in the US. Distances for 45 (10 x 9 / 2) from-to city pairs are provided. These data are used to illustrate that MDS can take simple data on distances (or on brand dissimilarities as we will see) and create a 2-dimensional map that accurately depicts the relative city (or brand) positions. 

Click the 'examples' radio button on the Data > Manage page and click 'Load examples' then choose the city data from the Datasets drop-down. Select 'from' as ID 1, 'to' as ID 2, and 'distance' as the Dissimilarity measure. The original distances are shown in (lower triangular) matrix form in the figure below. If the analysis is successful we expect cities that are close (e.g., Washington DC and New York) should also be located close together on the map and cities that are far apart (e.g., Seattle and Miami) should also be positioned far apart in the map.

The basic measure of (lack of) fit for MDS is called Stress. If MDS cannot create a map that accurately describes the original data this will result in high stress. Stress values of .1 are generally considered fair, .05 is good, and .01 or lower is excellent. High stress values indicate that a dimensionality of three (or higher) is needed to accurately depict the available data. For the city data the stress value is equal to .006 which is outstanding. In the Summary tab we also see the coordinates that will be used to create the two-dimensional map show in the Plots tab. 

![mds city - summary](figures/mds_city_summary.png)

In the graph from the screen grab of the Plots tab shown below the relative locations of Los Angeles, Boston, etc. look wrong. This is due to the fact the MDS program has no information on North, South, East and West. We can ‘flip’ the plot in Radiant to see if map becomes easier to recognize and interpret. 

![mds city - plots org](figures/mds_city_plots_org.png)

To create the plot below we clicked the check-boxes for Dimension 1 and Dimension 2. After ‘flipping’ the plot along both the horizontal and vertical axis we see that the relative locations of the cities look quite good. Note that this map is ‘flat’, i.e., there is no correction for the curvature of the earth.

![mds city - plots flip](figures/mds_city_plots_flip.png)

#### Example 2

The following plot is based on similarity data for a set of toothpaste brands (tpbrands, available as one of the example datasets). Respondents were asked the following question: "Please rate the following pairs of toothpaste brands on the basis of their similarity (1 = very similar, 7 = very dissimilar)." for all pairwise combinations of 10 brands, i.e., 45 comparisons. MDS will try to create a map that reproduces as accurately as possible the original dissimilarities (or perceptual distances) provided by the 50 respondents. The original dissimilarity ratings are shown in (lower triangular) matrix form in the figure below. From these data we can already see that the respondents perceive some brands to be very similar (e.g., Ultra Brite and Pepsodent have an average dissimilarity score of 1.11) and others to be very dissimilar (e.g., Crest and Sensodyne). The stress value for a two-dimensional solution is quite good (.058). 

![mds toothpaste - summary](figures/mds_tpbrands_summary.png)

The coordinates shown in the Summary tab are used to plot the brands in two-dimensions in the Plots tab. In the plot we see that Aqua Fresh and Colgate as well as Ultra Brite and Pepsodent are located very close together. This is consistent with the original data. Sensodyne and Crest, however, are positioned at opposite ends of the plot. Again, this is consistent with the original data and provides visual confirmation that MDS was able to create a plot that fits the data well.

From the plot a manager might conclude that the brands that are closest together in the map are perceived by consumers as close substitutes and, hence, close competitors in the minds of consumers in this market segment. A manager for Aqua Fresh or Macleans, in contrast, might focus less on Sensodyne when developing a competitive positioning plan for her brand. An important limitation of the brand maps based on (dis)similarity data is that the axes are difficult to interpret. For example, why are Close-up and Crest located at opposite ends of the horizontal axes? The researcher could respondents to explain the meaning of the axes or else obtain additional attribute information for the brands and correlate/overlay these on to the plot to facilitate interpretation. Such attribute data could, however, also be used to create a brand map without the need for (dis)similarity ratings (see Maps > Attribute).

![mds toothpaste - plots org](figures/mds_tpbrands_plots.png)

&copy; Vincent Nijs (2014) <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a>