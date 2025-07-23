# Track Popularity Prediction Analysis: Answers

## Question 4: Multiple Linear Regression to Predict Track Popularity

### a. Top Three Important Features for Song Popularity (for a Label Company)

Based on our regression analysis, the top three features that make a song more likely to be popular are:

1. **Danceability** (coefficient: +10.51, p-value < 0.001)
   - Songs with higher danceability scores are significantly more popular
   - For each 0.1 increase in danceability (on a 0-1 scale), popularity increases by approximately 1.05 points
   - This suggests that songs with good rhythm and beat that are easy to dance to resonate more with listeners

2. **Acousticness** (coefficient: +2.89, p-value < 0.001)
   - Songs with more acoustic elements tend to be more popular
   - This indicates that despite the prevalence of electronic production, acoustic elements still connect with audiences
   - The effect is smaller than danceability but still statistically significant

3. **Genre: Pop** (coefficient: +9.69, p-value < 0.001)
   - Pop music has the strongest positive genre effect on popularity
   - Pop songs are predicted to be about 9.7 points more popular than the reference genre (EDM)
   - Rock (coef: +9.41) and Latin (coef: +8.24) genres also show strong positive effects

**Recommendation for Label Companies:**
When scouting for new talent, prioritize artists who create songs with high danceability, incorporate some acoustic elements, and align with pop, rock, or Latin genres. These characteristics are statistically associated with higher popularity scores on streaming platforms.

### b. Goodness of Fit Assessment

Our model achieved an R-squared value of 0.091, meaning it explains approximately 9.1% of the variance in track popularity. The adjusted R-squared is 0.090, and the F-statistic is highly significant (p < 0.001).

**Assessment:**
This is a relatively poor fit, indicating that audio features and genre alone cannot adequately "quantify art" in terms of predicting popularity. While our model identified statistically significant relationships, 90.9% of the variance in popularity remains unexplained by these features.

**Why the model doesn't fit well:**
1. **Missing components of "art"**: The model lacks critical qualitative factors that influence popularity:
   - Lyrical content and storytelling quality
   - Artist reputation and marketing
   - Cultural relevance and timing
   - Visual elements (music videos, artist image)
   - Social media presence and promotion
   - Collaborations with other artists
   - Radio airplay and playlist placement

2. **Subjectivity and trends**: Music popularity is highly subjective and trend-dependent, making it difficult to quantify through audio features alone. What makes a song popular can change rapidly based on cultural shifts.

3. **Non-linear relationships**: The relationship between audio features and popularity may be complex and non-linear, with interactions that our linear model cannot capture.

**Conclusion on "quantifying art":**
While certain technical aspects of music can be quantified and do influence popularity to some extent, our analysis suggests that the artistic essence of music that truly drives popularity cannot be fully captured by measurable audio features. The low R-squared value supports the notion that art remains largely unquantifiable through technical metrics alone.

### c. Multicollinearity Evaluation

We evaluated multicollinearity by examining correlations between predictor variables. Our analysis found:

- No high correlations (|r| > 0.7) between any of the audio features used in the model
- The correlation matrix visualization (feature_correlation_heatmap.png) confirms the absence of strong correlations between predictors

**Conclusion on multicollinearity:**
Our regression model does not suffer from significant multicollinearity issues. This means that:

1. The coefficient estimates are stable and reliable
2. Each feature provides unique information to the model
3. The interpretation of individual feature effects is valid
4. There is no need to remove or combine features due to redundancy

The absence of multicollinearity strengthens our confidence in the identified relationships between audio features and track popularity, even though the overall model explains only a small portion of the variance in popularity.

## Summary of Key Findings

1. **Genre matters significantly**: Pop, rock, and Latin genres are associated with higher popularity scores
2. **Danceability is crucial**: Songs that are easy to dance to tend to be more popular
3. **Surprising negative effects**: High energy and instrumentalness are associated with lower popularity
4. **Model limitations**: Audio features alone explain only 9.1% of popularity variance
5. **No multicollinearity**: The predictors provide independent information to the model

These findings suggest that while certain technical aspects of music do influence popularity, the majority of what makes a song popular lies beyond measurable audio characteristics, reinforcing the idea that music remains an art form that cannot be fully reduced to quantifiable features.