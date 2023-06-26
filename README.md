# Spotify_Prediction_Algo
Different classification algorithms that predict the popularity of a song on Spotify given it's various musical attributes including both categorial and numerical predictors. Spotify data was obtained through Kaggle, a data analysis platform where users can collaborate, share, and work on data-related projects. The response variable is "Popularity" which was converted to a categorial ordinal variable. Colinearity among variables were assessed by creating a correlation matrix heatmap:

![image](https://github.com/ankith-py/Spotify_Prediction_Algo/assets/87325426/803afcfa-0531-4ae6-b217-c03a7e946b47)

Multiclass Classification Algorithms developed in this project: Linear Discriminant Analysis, Ordinal Regression, Trees, Pruned Tree, & Support Vector Machines.

Other machine learning methods (specifically regression algorithms) were utilized during this project such as the plot shown below. Please refer to the R code for more information.

![image](https://github.com/ankith-py/Spotify_Prediction_Algo/assets/87325426/2f789b81-7488-4399-8c07-7d2a8dd54c55)

Prediction accuracy was obtained through cross validation at a 80/20 train-test split via 100 simulations, and error rates are visualized through base RStudio boxplots as shown below:

![image](https://github.com/ankith-py/Spotify_Prediction_Algo/assets/87325426/1b90ca2f-895a-405a-8b1a-c547b470db96)


**Please note that these conclusions should not be extended beyond this sample.**
