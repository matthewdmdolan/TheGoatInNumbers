# TheGoatInNumbers

**Overview **

This statistical analysis of Lionel Messi's soccer data provides a variety of insights:

Messi's participation per season has remained consistently high, indicating his key role in the team.
His goal-scoring rate varies per season, potentially reflecting varying performance levels or external factors affecting his scoring.
Messi's goals-per-game ratio fluctuates seasonally, suggesting changes in his scoring efficiency over time.
Most of Messi's goals come from the center of the box, implying strong positioning and goal-scoring abilities in these areas.
Messi has scored most frequently against teams like "Rayo Vallecano", "Osasuna", "Levante", and "Valencia". This suggests that Messi might be particularly effective against these teams or that these teams' defenses are less successful in containing Messi.
Messi has provided the most assists to Luis Suarez, indicating a strong on-field partnership between them.
After analyzing this historical data, a logistic regression model was trained to predict whether Messi's shots would result in a goal. The model's accuracy was found to be 90.09%, which means it correctly predicted the outcome 90.09% of the time.

However, the model's ROC-AUC score was 0.6606, which is only slightly above average. This suggests that, despite a high accuracy rate, the model could be better at differentiating between goals and non-goals. In other words, while the model is correct most of the time, it struggles to confidently distinguish between positive and negative cases.

In conclusion, Messi's soccer data reveals important trends about his performance, and the predictive model developed performs reasonably well. However, model improvements are possible to achieve better differentiation between successful and unsuccessful shots.

**Limitations**

This project, while providing valuable insights and demonstrating the potential of using machine learning for sports analytics, had certain limitations. Primarily, the analysis was constrained by the limited availability of comprehensive, free data sources tracking players over their career spans. As a result, the data might not fully capture the nuances and depth of Lionel Messi's performance throughout his distinguished career. Moreover, the scope of feature engineering was limited in this project. Although feature engineering is a crucial aspect of any machine learning task and can significantly enhance model performance, this project was primarily focused on exploring the capabilities of the Shiny framework and kick-starting the development of web applications in R. Therefore, more sophisticated data processing and feature extraction techniques weren't the main priority, and their absence may affect the quality of insights derived and the predictive power of the model.

Further to this, it's a relatively rudimentary web application, which is a product of r's place in my workflow. Python remains my firm favourite for the vast majority of tasks, but the ability to utilise r's brilliant statistical analysis packages and produce a quick web application to share with end users is something that I wanted to explore.
