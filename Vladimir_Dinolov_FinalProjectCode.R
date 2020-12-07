library(tidyverse) 
library(viridis)
library(cowplot)
library(DT)
library(GGally)
options(warn = -1)

data <- read_csv('C:/Users/Vlad/Desktop/Data Science/Information Visualization/Project/2016 School Explorer.csv')
shsat <- read_csv('C:/Users/Vlad/Desktop/Data Science/Information Visualization/Project/D5 SHSAT Registrations and Testers.csv')

glimpse(data) #data meets the formula with 1,272 rows and 161 columns 

options(repr.plot.width=10, repr.plot.height=6)
data <- data[,-c(1:3)]
d <- head(data)

#creating table to explore the data interactively
datatable(head(data) , class = 'cell-border stripe', rownames = TRUE)

#removing percent (%) and commas (,)
data[, sapply(data, class) == 'character'] <- 
  as.data.frame(apply(data[, sapply(data, class) == 'character'],2, function(x) gsub("[%$,]", "", x)), 
                stringsAsFactors = FALSE)

#creating vectors instead of lists
data[,c(12,14:24,26,28,30,32,34,37,38)] <- as.numeric(unlist(data[,c(12,14:24,26,28,30,32,34,37,38)]))

#changing names to have periods instead of spaces or dashes
data <- data %>% dplyr::rename_all(funs(make.names(.)))
glimpse(data)

#creating scatter plot of economic need index vs. school income 
options(repr.plot.width=10, repr.plot.height=6)
ggplot(data, aes(Economic.Need.Index, School.Income.Estimate, color = Community.School.)) +
  geom_point()+scale_color_manual(values=c("#6CACE4", "#E69F00"))+ 
  theme_bw()+
  xlab("Economic Need Index") + ylab("School Income Estimate")+ggtitle('Economic Need Vs School Income')

#Percent of schools in each city
theme1 <- theme_bw()+theme(legend.position = 'none', plot.title = element_text(hjust = 0.5))
data %>% group_by(City) %>% summarise(count = length(City))%>% 
  mutate(percentage = prop.table(count)*100)%>%top_n(5,wt=percentage)%>%
  ggplot(aes(reorder(City,percentage), percentage, fill = percentage))+geom_col()+
  scale_fill_viridis(direction = -1)+
  theme_bw()+coord_flip()+
  geom_text(aes(label = sprintf("%.2f%%", percentage)), hjust = -0.2,
            vjust = 0.5, size =3)+ theme1+  xlab("") + ylab("Percent")+
  ggtitle("Percent of Schools in each City")

#community schools in each city
ggplot(data, aes(City, fill = Community.School.)) +
  geom_bar()+scale_fill_manual(values=c("#6CACE4", "#E69F00"))+theme_bw()+coord_flip()+
  xlab("City") + ylab("Number of Schools")+ggtitle('Number of Community School In Each City')

#plot by ENI and SIE
data %>% group_by(City)%>%na.omit()%>%
  summarise(AverageENI = mean(Economic.Need.Index), AverageSIE= mean(School.Income.Estimate))%>%
  ggplot(aes(AverageENI,AverageSIE, label = City))+geom_point()+geom_label()+ theme_bw()

#ethnicity by city 
data %>% gather(c(16:19,21) ,key = "Ethnicity", value = "Total")%>%
  group_by(City,Ethnicity) %>% na.omit()%>%
  summarise(Percent = round(mean(Total),2)) %>%
  ggplot(aes(City, Percent, fill = Ethnicity))+geom_col(position = 'fill')+
  scale_fill_brewer(palette="Greens")+
  coord_flip()+theme_bw()

#plots by student demographics and ENI, SIE, Attendance, and Absence 
plot_grid(ggplot(data, aes(Percent.ELL, Economic.Need.Index))+
            geom_point(color='#6CACE4', alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Asian, Economic.Need.Index))+
            geom_point(color='#6CACE4', alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.White, Economic.Need.Index))+
            geom_point(color='#6CACE4', alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Hispanic, Economic.Need.Index))+
            geom_point(color='#6CACE4', alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Black, Economic.Need.Index))+ 
            geom_point(color='#6CACE4', alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          align = 'h')

plot_grid(ggplot(data, aes(Percent.ELL, School.Income.Estimate))+
            geom_point(color="#E69F00", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Asian, School.Income.Estimate))+
            geom_point(color="#E69F00", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.White, School.Income.Estimate))+
            geom_point(color="#E69F00", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Hispanic, School.Income.Estimate))+
            geom_point(color="#E69F00", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Black, School.Income.Estimate))+ 
            geom_point(color="#E69F00", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          align = 'h')

plot_grid(ggplot(data, aes(Percent.ELL, Student.Attendance.Rate))+
            geom_point(color="#238b45", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Asian, Student.Attendance.Rate))+
            geom_point(color="#238b45", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.White, Student.Attendance.Rate))+
            geom_point(color="#238b45", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Hispanic, Student.Attendance.Rate))+
            geom_point(color="#238b45", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Black, Student.Attendance.Rate))+ 
            geom_point(color="#238b45", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          align = 'h')

plot_grid(ggplot(data, aes(Percent.ELL, Percent.of.Students.Chronically.Absent))+
            geom_point(color="#74c476", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Asian, Percent.of.Students.Chronically.Absent))+
            geom_point(color="#74c476", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.White, Percent.of.Students.Chronically.Absent))+
            geom_point(color="#74c476", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Hispanic, Percent.of.Students.Chronically.Absent))+
            geom_point(color="#74c476", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Black, Percent.of.Students.Chronically.Absent))+ 
            geom_point(color="#74c476", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          align = 'h')

#ethnicity by rigorous instruction alignment ratings 
data$Rigorous.Instruction.Rating <- factor((data$Rigorous.Instruction.Rating),
                                           level = c("Not Meeting Target",
                                                     "Approaching Target",
                                                     "Meeting Target",
                                                     "Exceeding Target"))
data %>% gather(c(16:19,21) ,key = "Ethnicity", value = "Total")%>%
  group_by(Ethnicity,Rigorous.Instruction.. ,Rigorous.Instruction.Rating ) %>% na.omit()%>%
  filter(Rigorous.Instruction.Rating != 'N/A')%>%
  summarise(Percent.Ethnicities = round(mean(Total),2)) %>%
  ggplot(aes(Rigorous.Instruction.., Percent.Ethnicities, fill = Rigorous.Instruction.Rating, shape = Ethnicity))+
  geom_point(aes(color = Rigorous.Instruction.Rating))+ facet_wrap(~Ethnicity)+
  scale_color_viridis(direction = -1, discrete = TRUE)+
  theme_bw()

#ethnicity by teacher collaboration ratings 
data$Collaborative.Teachers.Rating <- factor((data$Collaborative.Teachers.Rating),
                                             level = c("Not Meeting Target",
                                                       "Approaching Target",
                                                       "Meeting Target",
                                                       "Exceeding Target"))
data %>% gather(c(16:19,21) ,key = "Ethnicity", value = "Total")%>%
  group_by(Ethnicity,Collaborative.Teachers.. ,Collaborative.Teachers.Rating ) %>% na.omit()%>%
  filter(Collaborative.Teachers.Rating != 'N/A')%>%
  summarise(Percent.Ethnicities = round(mean(Total),2)) %>%
  ggplot(aes(Collaborative.Teachers..,Percent.Ethnicities, fill = Collaborative.Teachers.Rating, shape = Ethnicity))+
  geom_point(aes(color = Collaborative.Teachers.Rating))+ facet_wrap(~Ethnicity)+
  scale_color_viridis(direction = -1, discrete = TRUE)+
  theme_bw()


#ethnicity by supportive environment rating  
data$Supportive.Environment.Rating <- factor((data$Supportive.Environment.Rating),
                                             level = c("Not Meeting Target",
                                                       "Approaching Target",
                                                       "Meeting Target",
                                                       "Exceeding Target"))
data %>% gather(c(16:19,21) ,key = "Ethnicity", value = "Total")%>%
  group_by(Ethnicity,Supportive.Environment.. ,Supportive.Environment.Rating ) %>% na.omit()%>%
  filter(Supportive.Environment.Rating != 'N/A')%>%
  summarise(Percent.Ethnicities = round(mean(Total),2)) %>%
  ggplot(aes(Supportive.Environment..,Percent.Ethnicities, fill = Supportive.Environment.Rating, shape = Ethnicity))+
  geom_point(aes(color = Supportive.Environment.Rating))+ facet_wrap(~Ethnicity)+
  scale_color_viridis(direction = -1, discrete = TRUE)+
  theme_bw()

#ethnicity by school leadership rating  
data$Effective.School.Leadership.Rating <- factor((data$Effective.School.Leadership.Rating),
                                                  level = c("Not Meeting Target",
                                                            "Approaching Target",
                                                            "Meeting Target",
                                                            "Exceeding Target"))
data %>% gather(c(16:19,21) ,key = "Ethnicity", value = "Total")%>%
  group_by(Ethnicity,Effective.School.Leadership..,Effective.School.Leadership.Rating ) %>% na.omit()%>%
  filter(Effective.School.Leadership.Rating != 'N/A')%>%
  summarise(Percent.Ethnicities = round(mean(Total),2)) %>%
  ggplot(aes(Effective.School.Leadership..,Percent.Ethnicities, fill = Effective.School.Leadership.Rating, shape = Ethnicity))+
  geom_point(aes(color = Effective.School.Leadership.Rating))+ facet_wrap(~Ethnicity)+
  scale_color_viridis(direction = -1, discrete = TRUE)+
  theme_bw()

#Ethnicity by Community Score 
data$Strong.Family.Community.Ties.Rating <- factor((data$Strong.Family.Community.Ties.Rating),
                                                   level = c("Not Meeting Target",
                                                             "Approaching Target",
                                                             "Meeting Target",
                                                             "Exceeding Target"))
data %>% gather(c(16:19,21) ,key = "Ethnicity", value = "Total")%>%
  group_by(Ethnicity,Strong.Family.Community.Ties..,Strong.Family.Community.Ties.Rating ) %>% na.omit()%>%
  filter(Strong.Family.Community.Ties.Rating != 'N/A')%>%
  summarise(Percent.Ethnicities = round(mean(Total),2)) %>%
  ggplot(aes(Strong.Family.Community.Ties..,Percent.Ethnicities, fill = Strong.Family.Community.Ties.Rating, shape = Ethnicity))+
  geom_point(aes(color = Strong.Family.Community.Ties.Rating))+ facet_wrap(~Ethnicity)+
  scale_color_vi

#Ethnicity by Trust Rating 
data$Trust.Rating <- factor((data$Trust.Rating),
                            level = c("Not Meeting Target",
                                      "Approaching Target",
                                      "Meeting Target",
                                      "Exceeding Target"))
data %>% gather(c(16:19,21) ,key = "Ethnicity", value = "Total")%>%
  group_by(Ethnicity,Trust..,Trust.Rating ) %>% na.omit()%>%
  filter(Trust.Rating != 'N/A')%>%
  summarise(Percent.Ethnicities = round(mean(Total),2)) %>%
  ggplot(aes(Trust..,Percent.Ethnicities, fill = Trust.Rating, shape = Ethnicity))+
  geom_point(aes(color = Trust.Rating))+ facet_wrap(~Ethnicity)+
  scale_color_viridis(direction = -1, discrete = TRUE)+
  theme_bw()

#Ethnicity by ELA Scores
plot_grid(ggplot(data, aes(Percent.ELL, Average.ELA.Proficiency))+
            geom_point(color="#238b45", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Asian, Average.ELA.Proficiency))+
            geom_point(color="#238b45", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.White, Average.ELA.Proficiency))+
            geom_point(color="#238b45", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Hispanic, Average.ELA.Proficiency))+
            geom_point(color="#238b45", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Black, Average.ELA.Proficiency))+ 
            geom_point(color="#238b45", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          align = 'h')

#Ethnicity by Math Scores
plot_grid(ggplot(data, aes(Percent.ELL, Average.Math.Proficiency))+
            geom_point(color="#E69F00", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Asian, Average.Math.Proficiency))+
            geom_point(color="#E69F00", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.White, Average.Math.Proficiency))+
            geom_point(color="#E69F00", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Hispanic, Average.Math.Proficiency))+
            geom_point(color="#E69F00", alpha = 0.5)+ geom_smooth(method = lm)+theme_bw(),
          ggplot(data, aes(Percent.Black, Average.Math.Proficiency))+ 
            geom_point(color="#E69F00", alpha = 0.5)+geom_smooth(method = lm)+theme_bw(),
          align = 'h')

#Grade 3 ELA by City
options(repr.plot.width=12, repr.plot.height=8)
data %>% gather(c(39:48) ,key = "score4sbygroup", value = "Total") %>%
  group_by(City,score4sbygroup)%>%
  filter(score4sbygroup== "Grade.3.ELA...All.Students.Tested" |score4sbygroup== "Grade.3.ELA.4s...All.Students")%>%
  summarise(count = sum(Total)) %>% mutate(percent = prop.table(count)*100)%>%
  ggplot(aes(score4sbygroup,percent, fill = score4sbygroup))+geom_col()+facet_wrap(~City)+theme_bw()+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  xlab(" ") + ylab("")+ ggtitle("Percent of Grade 3 Students scored 4 in ELA in each City")

#Grade 3 Math Scores by City
data %>% gather(c(49:50) ,key = "score4sbygroup", value = "Total") %>%
  group_by(City,score4sbygroup)%>%
  filter(score4sbygroup== "Grade.3.Math...All.Students.tested" |score4sbygroup== "Grade.3.Math.4s...All.Students")%>%
  summarise(count = sum(Total)) %>% mutate(percent = prop.table(count)*100)%>%
  ggplot(aes(score4sbygroup,percent, fill = score4sbygroup))+geom_col()+facet_wrap(~City)+theme_bw()+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  xlab(" ") + ylab("")+ ggtitle("Percent of Grade 3 Students scored 4 in Math in each City")

#Grade 8 Math Scores by City 
data %>% gather(c(149:150) ,key = "score4sbygroup", value = "Total") %>%
  group_by(City,score4sbygroup)%>%
  #filter(score4sbygroup== "Grade.8.ELA...All.Students.Tested" |score4sbygroup== "Grade.8.ELA.4s...All.Students")%>%
  summarise(count = sum(Total)) %>% mutate(percent = prop.table(count)*100)%>%
  ggplot(aes(score4sbygroup,percent, fill = score4sbygroup))+geom_col()+facet_wrap(~City)+theme_bw()+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  xlab(" ") + ylab("")+ ggtitle("Percent of Grade 8 Students scored 4 in Math in each City")

#Grade 8 ELA Scores by City
data %>% gather(c(139:140) ,key = "score4sbygroup", value = "Total") %>%
  group_by(City,score4sbygroup)%>%
  #filter(score4sbygroup== "Grade.8.Math...All.Students.Tested" |score4sbygroup== "Grade.8.Math.4s...All.Students")%>%
  summarise(count = sum(Total)) %>% mutate(percent = prop.table(count)*100)%>%
  ggplot(aes(score4sbygroup,percent, fill = score4sbygroup))+geom_col()+facet_wrap(~City)+theme_bw()+
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  xlab(" ") + ylab("")+ ggtitle("Percent of Grade 8 Students scored 4 in ELA in each City")
