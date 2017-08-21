#Factor Analysis

The goal of Factor Analysis is to reduce the dimensionality of the data without significant loss of information. The tool tries to achieve this goal by looking for structure in the correlation matrix of the variables included in the analysis. The researcher will usually try to link each of the original variables to an underlying factor and then provide a descriptive label for each factor (i.e., a name).

###Input:
Variables, matrix format (default=correlation), number of factors, factor extracting method (default = minimum residual), factor rotation method (default = varimax), bootstrap number (optional)

###Output:
On Pre-Factor tab: Data identification, Bartlett test result, Variable collinearity, KMO-MSA test, and eigenvalues.
On Factor tab: Data identification, Factor loadings, Variable communality, Factor scores.
On Bootstrap FA tab: Data identification, Bootstrap eigenvalues, Bootstrap factor loadings.
On Factor Plot tab: Factor scores plot, Factor biplot

## Example: Toothpaste

Click the 'examples' radio button on the Data  page and click 'Load examples' then choose the toothpaste  data from the Datasets dropdown. The data set contains information from 60 consumers who were asked to respond to six questions to determine their attitudes towards toothpaste. The scores shown for variables v1-v6 indicate the level of agreement with the statement indicated on a 7-point scale where 1 = strongly disagree and 7 = strongly agree.

![facfull shopping - summary](figures/fa1.png)

The first step in factor analysis is to determine if the data has the appropriate characteristics for the analysis.
Data that show a limited amount of correlation between the variables of interest are generally not appropriate for factor analysis. We will use three criteria to test if the data are suited for factor analysis: Bartlett test of sphericity, Variable collinear, and KMO-MSA test.
1. Bartlett test: to examine the correlation matrix. If the variables do not have high correlated among themselves, data is not appropriate for factoring. Select another variables/dataset.
2. Variable collinearity: Check the collinearity of data by variable's correlation. Remove any variable with a correlation 1 or -1 from the analysis.
3. Kaiser-Meyer-Oklin Measure Sampling Adequance. The MSA value is to measure the homogenity of variables. If variable's MSA value < 0.5 then remove the variable. The overall KMO value is to determine if the data sufficient for factor analysis. It's suggested that the overall KMO value should be > 0.8, but > 0.6 is tolerable. The overall KMO can sometimes be increased by deleting the variables whose MSA value is low.
As can be seen in the output in the Pre-Factor tab from Factor analysis, Bartlett’s test statistic is large and significant (p-value very close to 0) as desired. The Kaiser-Meyer-Olkin (KMO) > 0.6 and thus acceptable. The variable collinearity values are all above 0.4 so all variables can be used in the analysis.

The next step is to determine the number of factors needed to capture structure underlying the data and variables. There are three ways to determine how many factors to retain. (1) By eigenvalue greater-than-one rule, number of factors = number of eigenvalues that > 1. (2) By the importance of factors, the number of factors is sufficient if can explain mostly 80% total sample variance. (3) By Scree plot, look for the 'elbow' visible.
From the three ways provided, we can choose 2 factors to analyzed.

Once we have determined the number of factors we can extract and rotate them. The factors are rotated to generate a solution where, to the extent possible, a variable has a high loading on only one factor. This is an important benefit because it makes it easier to interpret what the factor represents. While there are numerous algorithms to rotate a factor loadings matrix the most commonly used is Varimax rotation.

![facfull shopping - summary](figures/fa2.png)

The numbers in the table are the correlations of the six variables with the two Factors. For example, variable v1 has a correlation of .962 with Factor 1 and a correlation of -.031 with Factor 2. As such v1 will play a big role in naming Factor 1 but an insignificant role in naming Factor 2. 

The rotated factor loadings will be used to determine labels or names for the different factors. We need to identify and highlight the highest factor loading, in absolute value, in each row. This is most easily done by setting number in the Format loadings input to .4 and checking the Sort box. Together, the variables highlighted in each column (i.e., for each factor) will help us to understand what the factor represents. Questions 1, 3 and 5 reflect the importance of health issues while questions 2, 4, and 6 reflect aesthetics issues. Plausible names for the factors might therefor be:

Factor 1: Health benefits
Factor 2: Social benefits

The best way to see what rotation does is to select the method from list and inspect what changes in the output. To start click on the Plots tab and set the radio button to None. The image shown below depicts the loadings of the variables on the two factors. Variable 5 falls somewhat in between the lines for factor 1 and factor 2. When we select Varimax rotation, however, the label for v5 lines up nicely with the horizontal axis. This change in alignment is also reflected in the factor loadings. The unrotated factor loadings for v5 are -0.869 for factor 1 and -0.351 for factor 2. The rotated factor loadings for v5 are -0.934 for factor 1 and -0.079 for factor 2.

![facfull shopping - summary](figures/fa4.png)

The final step is to generate the factor scores. You can think of these scores as a weighted average of the variables that are linked to a factor. They approximate the scores that a respondent would have provided if we could have asked about the factor in a single question, i.e., the respondents inferred ratings on the factors. 

For Bootstrap FA, we must determine the number of resampling in the sidebar. The output from this analysis is bootstrap mean of eigenvalues with standard errors, and the bootstrap mean of factor loadings with standard errors.
![facfull shopping - summary](figures/fa4.png)

#### Summary

1. Determine if the data are appropriate for factor analysis using Bartlett, KMO, and Collinearity (see pre-factor analysis)
2. Determine the number of factors (see the scree-plot and eigenvalues > 1 produced through pre-factor analysis)
3. Extract the (rotated) factor solution to produce: 
	- Factor loadings: Correlations between attributes and factors
	- Factor scores: Inferred ratings on the new factors
5. Identify the highest factor loading, in absolute value, in each row (i.e., for each variable)
4. Interpret the factors using the strongest factor loadings and label them


&copy; Vincent Nijs (2014), with modification <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a>