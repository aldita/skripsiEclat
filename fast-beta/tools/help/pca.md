#Principal Component Analysis

A principal component explaining the variance-covariance structure of a set of variables through a few linear combinations of these variables.

###Input:
Variables, matrix format

###Output:
On Analysis tab: Principal component loadings, Proportion of variance explained, Scree plot
On Biplot tab: Biplot

## Example: Toothpaste

Click the 'examples' radio button on the Data > Manage page and click 'Load examples' then choose the toothpaste  data from the Datasets dropdown. The data set contains information from 60 consumers who were asked to respond to six questions to determine their attitudes towards toothpaste. The scores shown for variables v1-v6 indicate the level of agreement with the statement indicated on a 7-point scale where 1 = strongly disagree and 7 = strongly agree.

![pca toothpaste - summary](figures/pc1.png)

As can be seen in the output in the Analysis  tab from Principal Component Analysis, application will automatically compute the principal component loadings. From the picture above, we can say the 1st component is 0.5617V1 - 0.1818V2 + 0.5665V3 - 0.2067V4 - 0.5257V5 - 0.1069V6


The next step is to determine the number of principal components. There are some methods to determine how many components to retain:
(1) Amoount of total sample variance explained. The number of components is sufficient if can explain mostly 80% total sample variance.
(2) Relative size of eigenvalue, by eigenvalue greater-than-one rule. The number of components = the number of eigenvalues that > 1.
(3) Scree plot, by look for an elbow (bend) in the scree plot.The number of components is taken to be the point at which the remaining eigenvalues are relatively small and about the same size.

On the Biplot tab, there is Plot of component scores and component loadings in the same graph, from the first two principal components

![pca toothpaste - summary](figures/pc2.png)

&copy; Vincent Nijs (2014) with modification <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png" /></a>