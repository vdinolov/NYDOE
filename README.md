# Erasing the Opportunity Gap in NYC Public Schools

## The "Why"
College is the single best indicator of higher lifetime income. College completion starts with a high-quality PreK-12 education and access to resources like quality healthcare and housing. Unfortunately, not everyone has access due to generational poverty or larger, systemic issues. This leads to gaps in college completion rates by race, with Whites completing college at higher rates than Black and Hispanic/Latino students. From local education agencies to postsecondary institutions, all decision-makers have a stake in student success. The future of our economy depends on higher rates of student achievement from cradle to college so all students succeed. 

The SHSAT (Specialized High School Test) provides students with access to a higher quality education, but clear barriers exist for historically underserved students. The goal of this project is to increase the diversity of students taking the SHSAT. By focusing efforts in under-performing areas that are historically underrepresented in SHSAT registration, a path to specialized high schools will become more available for economically and racially diverse students.

## About the Data 
Two datasets were pulled by PASSNYC from the New York Department of Education (NYDOE). The first is a database of thousands of schools across New York City and the second is SHSAT registration and testers. The raw data is comprised of 1,272 rows and 161 columns. Each row is an observation of variables such as state test scores, student demographics, school success indices, and location. This data is public data used to identify students within New York City’s underperforming school districts. 

Data Source: https://www.kaggle.com/passnyc/data-science-for-good

Packages used for this project: cowplot, DT,  GGally, ggplot2, RColorBrewer, tidyverse, viridis

## Outcome 
New York State is often recognized for its high-quality public education. Yet, income and access stand between students and their entrance to specialized high schools. Disparities in income become even clearer when comparing students by race and ethnicity. 1,272 NYC schools were analyzed to identify trends in student academic performance as indicated by state assessments. These scores were compared based on the percentage of economically disadvantaged students at a given school and the school communities’ average household income. The following trends were identified:

* Schools with more Black and Hispanic or Latino Students and English Language Learners have a higher ENI and lower average school income estimates.

* Schools with more White Students have a lower ENI and higher average school income estimates.

* Student performance is strongly correlated with race and income as White students had higher average scores on state-wide assessments and higher school income estimates.
	
An infographic was designed using Adobe Illustrator with visualizations created in R Studio that present the above information. 
