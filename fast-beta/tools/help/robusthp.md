<h5><b> INTRODUCTION </b></h5>
Robust Hurdle Poisson (RHP) is a robust count regression analysis for count data with excess zeros (true zeros). There are two models in RHP: Logit model and Truncated Poisson model.
<br/>

<h5><b>STEPS OF ROBUST HURDLE POISSON ANALYSIS</b></h5>
1. In the sidebar, select one dependent variable which consist of more than equal to zero.
2. In the sidebar, select at least two independent variables. Press CTRL + click the variable.
3. Click tab menu 'Data Identification' for view:
<ul>
<li> 'Frequency Table of Dependent Variable': important to view relative frequency of zeros values in data. </li>
<li>  'Cross Table': view crosstab of categorical variables. </li>
<li> 'Summary Data': view summary (minimum, maximum, mean, quartil) of each variables. </li>
</ul>
4. Click tab menu 'Data Visualization' for view:
<ul>
<li> 'Scatter Plot with Density and Correlation': important to view density curve of dependent variable, ensure that dependent variable distribution is right-skewed (poisson distribution). </li>
<li> 'Box Plot': important to view outlier and comparing distributions. </li>
</ul>
5. Click tab menu 'Outlier Detection' for view:
<ul>
<li> 'Logit': ensure there are outliers in logit , dot-red in plot cook's distance is indicate outlier. </li>
<li> 'Truncated Poisson': ensure there are outliers in truncated poisson, dot-red in plot cook's distance is indicate outlier. </li>
</ul>
6. Click tab menu 'Summary' for view:
<ul>
<li> 'Hurdle Poisson': estimate parameter and p-value of Hurdle Poisson. </li>
<li> 'Robust Hurdle Poisson': estimate parameter and p-value of Robust Hurdle Poisson. </li>
</ul>
7. (optional) Click tab menu 'Testing' for view:
<ul>
<li> 'Quasi Deviance Test' : testing for ensure at least one of independent variable that significant. </li>
<li> 'Sensitivity Analysis': curve for compare stability of p-value in Hurdle Poisson and Robust Hurdle Poisson. <br/>
	Note: When you click tab menu 'Testing', in the sidebar panel there is 'Check Sensitivity Analysis'. Select one independent variable to be checked in sensitivity analysis. </li>
</ul>
8. (optional) When you click tab menu 'Summary' or tab menu 'Testing', in the sidebar panel will appear 'Download' button. Download button can be used for download report of doing Robust Hurdle Poisson Analysis.

<h5><b> DATA EXAMPLE </b></h5>
You can use <b>babel</b> data for doing Robust Hurdle Poisson Analysis. The data describe cigarette consumption in Bangka Belitung Province. Data source: BPS SUSENAS KOR 2015. <br/>
Variables:
<ol>
	<li> y: independent variable, cigarette consumption individual per week </li>
	<li> x1: the highest education level attained. 1 (<= junior high school), 0 (>= senior high school) </li>
	<li> x2: gender. 1 (man), 0 (woman) </li>
	<li> x3: region, 1 (urban), 0 (rural) </li>
	<li> x4: marital status, 1 (married), 0 (single) </li>
	<li> x5: job status, 1 (informal), 0 (other) </li>
	<li> x6: age </li>
	<li> x7: income per capita </li>
</ol>

<i>If you want to close this help and back to the analysis, click cross symbol on the top-right.</i>