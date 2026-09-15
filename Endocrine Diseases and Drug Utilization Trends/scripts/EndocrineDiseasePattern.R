# Install & Load Packages

# install.packages("tidyverse")
# install.packages("gtsummary")
# install.packages("gt")
# install.packages("gridExtra")
# install.packages("sjPlot")
# install.packages("cowplot")
# install.packages("naniar")
# install.packages("extrafont")
# install.packages("ggpubr")
# install.packages("corrplot")
#install.packages("broom")
#install.packages("broom.helpers")


library(tidyverse)
library(gtsummary)
library(gt)
library(gridExtra)
library(sjPlot)
library(cowplot)
library(naniar)
library(extrafont)
library(ggpubr)
library(corrplot)
library(broom)
library(broom.helpers)


# Read Data

Endocrine <- readxl::read_excel("../data/EndocrineDiseasePattern.xlsx", sheet = 1)

shapiro.test(Endocrine$Age)

# 1. Dimension of a dataset, rows and columns

dim(Endocrine)
nrow(Endocrine)
ncol(Endocrine)

# 2. Show column names (Variables Name)

names(Endocrine)

# 3. Check data structure of every variables

str(Endocrine)
glimpse(Endocrine) #Best

# 4. Check for missing value

is.na(Endocrine) 
sum(is.na(Endocrine))

# 4a. Visualize missing data with `naniar`

miss_var_which(Endocrine)
miss_var_which(Endocrine)
pct_miss_var(Endocrine)
gg_miss_var(Endocrine) # Best
vis_miss(Endocrine)    # Also very good one

# 5. Check for any duplicate value

duplicated(Endocrine)
sum(duplicated(Endocrine))

# 6. Examine first few row

head(Endocrine, n = 10)

# 7. Examine last few row

tail(Endocrine, n = 10)

# 8. Summarize data

summary(Endocrine)

# 9. Modification from character or factor

Endocrine <- Endocrine |>
  mutate_if(is.character, as.factor)

glimpse(Endocrine) # Check data structure after conversion



# 10. Numeric Data
# integer => Frequency / Percentage  ~ Discrete 
# double => Unit / Fraction / Decimal ` ~ Continuous 

# 10a. Mean

Endocrine$Age

min(Endocrine$Age)
max(Endocrine$Age)
Range <- max(Endocrine$Age) - min(Endocrine$Age)
Range
mean(Endocrine$Age)
sd(Endocrine$Age)

# 10b. Median Way

median(Endocrine$Age)
quantile(Endocrine$Age, 0.25)
quantile(Endocrine$Age, 0.75)
inter_quartile_range <- quantile(Endocrine$Age, 0.75) - quantile(Endocrine$Age, 0.25)
inter_quartile_range


# 11. Categorical Data (Factor ~ fct)
# 3 types => Dichotomous, Nominal, and Ordinal

summary(Endocrine$Gender)
table(Endocrine$Gender) # Easy way # Count
prop.table(table(Endocrine$Gender)) #Frequency fraction
percnt <- (prop.table(table(Endocrine$Gender))*100) #Frequency Percent

percnt


# 12. Categorization => `Numeric Data` to `Categorical`

names(Endocrine)

Endocrine <- Endocrine |>
  mutate(Age_Group = case_when(
    Age < 50 ~ "<50",
    Age >= 50 & Age <= 59 ~ "50-59",
    Age >= 60 ~ ">60"
  ))

Endocrine


# 13.  Final Modification from character or factor

Endocrine <- Endocrine |>
  mutate_if(is.character, as.factor)

glimpse(Endocrine)


# 14. Publication Ready Descriptive Tables => Demographic characteristics
# Create with (statistic = "Age" ~ "{mean} ± ({sd})")
# 14. Table 1. Socio-demographic characteristics of the study participants (N=600)

names(Endocrine)

Endocrine |>
  select(3, 4, 7, 5, 6) |> 
  tbl_summary(statistic = "Age" ~ "{mean} ± ({sd})") |>
  as_gt() |>
  gtsave("../tables/Table1.docx")


#================================================================


# Read Data

EndocrineDiseases <- readxl::read_excel("../data/EndocrineDiseasePattern.xlsx", sheet = 2)


# 15. Table 2a. Pattern of endocrine-related diseases among the study participants (N=600)

names(EndocrineDiseases)

EndocrineDiseases <- EndocrineDiseases |>
  mutate_if(is.character, as.factor)


glimpse(EndocrineDiseases)

EndocrineDiseases |>
  select(1:8) |> 
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)")) |>
  as_gt() |>
  gtsave("../tables/Table2a.docx")


#==============================================================

# Read Data

Comorbidities <- readxl::read_excel("../data/EndocrineDiseasePattern.xlsx", sheet = 3)


# 15. Table 2b. Pattern of Comorbidities among the study participants (N=600)

names(Comorbidities)

Comorbidities <- Comorbidities |>
  mutate_if(is.character, as.factor)


glimpse(Comorbidities)

Comorbidities |>
  select(1:4) |> 
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)")) |>
  as_gt() |>
  gtsave("../tables/Table2b.docx")

#==================================================================


# Read Data

Medications <- readxl::read_excel("../data/EndocrineDiseasePattern.xlsx", sheet = 4)


# 15. Table 3. Distribution of endocrine disease & other comorbid disease medications (N=600)

names(Medications)

Medications <- Medications |>
  mutate_if(is.character, as.factor)


glimpse(Medications)

Medications |>
  select(1:22) |> 
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)")) |>
  as_gt() |>
  gtsave("../tables/Table3.docx")

#==================================================================


# Read Data

AssocChisq <- readxl::read_excel("../data/EndocrineDiseasePattern.xlsx", sheet = 6)


# 16. Table 5. Association between Gender and Disease Pattern among Study Participants (N=600)

names(AssocChisq)

AssocChisq <- AssocChisq |>
  mutate_if(is.character, as.factor)


glimpse(AssocChisq)

AssocChisq |>
  select(Gender, 2:13) |>
  tbl_summary(by = Gender) |>
  add_overall() |>
  add_p() |>
  bold_p(t = 0.05) |>
  as_gt() |>                 
  gtsave("../tables/Table5.docx")


#==================================================================


# Read Data

AssocAge <- readxl::read_excel("../data/EndocrineDiseasePattern.xlsx", sheet = 7)


# 17. Table 6. Association between Age Group and Disease Pattern among Study Participants (N=600)

AssocAge <- AssocAge |>
  mutate(Age_Group = case_when(
    Age < 50 ~ "<50 Years",
    Age >= 50 & Age <= 59 ~ "50-59 Years",
    Age >= 60 ~ ">60 Years"
  ))

names(AssocAge)

AssocAov <- AssocAge |>
  mutate_if(is.character, as.factor)


glimpse(AssocAge)


AssocAge |>
  select(Age_Group, 2:13) |>
  tbl_summary(by = Age_Group) |>
  add_overall() |>
  add_p() |>
  bold_p(t = 0.05) |>
  as_gt() |>                 
  gtsave("../tables/Table6.docx")


#============================================================


# Read Data

tGender <- readxl::read_excel("../data/EndocrineDiseasePattern.xlsx", sheet = 8)


# 18. Table 7. Association between Gender and Endocrine Diseases & other comorbidities Medications (N=600)

names(tGender)

tGender <- tGender |>
  mutate_if(is.character, as.factor)


glimpse(tGender)


tGender |>
  select(Gender, 2:23) |>
  tbl_summary(by = Gender) |>
  add_overall() |>
  add_p(test.args = all_tests("t.test") ~ list(workspace = 2e9)) |>
  bold_p(t = 0.05) |>
  as_gt() |>                 
  gtsave("../tables/Table7.docx")

#================================================================

# Distribution of Patient Ages

ggplot(Endocrine, aes(x = Age,
                      fill = Gender)) +
  geom_histogram(binwidth = 5,  alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Age)), 
             linetype = "dashed", linewidth = 0.3) +
  facet_wrap(vars(Gender), nrow = 1) +
  labs(x = "Age (Years)",          
       y = "Number of Patients", 
       title = "Distribution of Patient Ages by Gender",
       subtitle = "Black Dashed Line Indicates Mean Age",
       caption = "Figure 1. Distribution of Patient Ages by Gender among Study Participants") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12,
                                  hjust = 0.5),
        plot.caption = element_text(size = 11,
                                     hjust = 0.5),
        legend.position = "top")

ggsave("../figures/Distribution of Patient Ages by Gender.png", 
       units = "in", width = 10, height = 10, dpi = 1200)
ggsave("../figures/Distribution of Patient Ages by Gender.pdf",
       units = "in", width = 10, height = 10, dpi = 1200)

#===================================================================

# Distribution of Number of Prescribed Drug Scores by Gender


Vizualizatons <- readxl::read_excel("../data/EndocrineDiseasePattern.xlsx", sheet = 9)


Vizualizatons <- Vizualizatons |>
  mutate(Age_Group = case_when(
    Age < 50 ~ "<50 Years",
    Age >= 50 & Age <= 59 ~ "50-59 Years",
    Age >= 60 ~ ">60 Years"
  ))



Vizualizatons


ggplot(Vizualizatons,
       aes(x = Gender, y = Drug_Score, 
           fill = Gender)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_viridis_c() +
  scale_fill_discrete(name = "Gender") +
  labs(x = "Gender",          
       y = "Number of Prescribed Drug Scores", 
       title = "Distribution of Number of Prescribed Drug Scores by Gender",
       caption = "Figure 3. Distribution of Number of Prescribed Drug Scores by Gender") +
  geom_hline(yintercept = mean(Vizualizatons$Drug_Score), linetype = 2) + 
  stat_compare_means(label.x = 1.45, method = "t.test") +
  theme_bw() +
  theme(plot.title = element_text(size = 16,
                                  hjust = 0.5),
        plot.caption = element_text(size = 11,
                                    hjust = 0.5),
        legend.position = "top")


ggsave("../figures/Figure 2. Distribution of Number of Prescribed Drug Scores by Gender.png", 
       units = "in", width = 10, height = 10, dpi = 1200)
ggsave("../figures/Figure 2. Distribution of Number of Prescribed Drug Scores by Gender.pdf",
       units = "in", width = 10, height = 10, dpi = 1200)



#  Distribution of Number of Prescribed Drug Scores among Age Groups


P1 <- ggplot(Vizualizatons,
             aes(x = Age_Group, y = Drug_Score, 
                 fill = Age_Group)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "Age Groups") +
  labs(x = "Age Groups",          
       y = "Number of Prescribed Drug Scores", 
       title = "Distribution of Number of Prescribed Drug Scores among Age Groups",
       caption = "Distribution of Number of Prescribed Drug Scores among Age Groups",
       tag = "A") +
  geom_hline(yintercept = mean(Vizualizatons$Drug_Score), linetype = 2) + 
  stat_compare_means(label.x = 1.95, method = "anova") +
  theme_bw() +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5),
        plot.caption = element_text(size = 11,
                                    hjust = 0.5),
        legend.position = "top")

P1

ggsave("../figures/Figure3. Distribution of Number of Prescribed Drug Scores among Age Groups.png", 
       units = "in", width = 10, height = 10, dpi = 1200)
ggsave("../figures/Figure 3. Distribution of Number of Prescribed Drug Scores among Age Groups.pdf",
       units = "in", width = 10, height = 10, dpi = 1200)


#  Distribution of Number of Prescribed Drug Scores among BMIs


P2 <- ggplot(Vizualizatons,
             aes(x = BMI, y = Drug_Score, 
                 fill = BMI)) +
  geom_boxplot() +
  geom_jitter() +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "BMI") +
  labs(x = "BMI",          
       y = "Number of Prescribed Drug Scores", 
       title = "Distribution of Number of Prescribed Drug Scores among BMIs",
       caption = "Distribution of Number of Prescribed Drug Scores among BMIs",
       tag = "B") +
  geom_hline(yintercept = mean(Vizualizatons$Drug_Score), linetype = 2) + 
  stat_compare_means(label.x = 1.95, method = "anova") +
  theme_bw() +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5),
        plot.caption = element_text(size = 11,
                                    hjust = 0.5),
        legend.position = "top")

P2

ggsave("../figures/Figure 4. Distribution of Number of Prescribed Drug Scores among BMIs.png", 
       units = "in", width = 10, height = 10, dpi = 1200)
ggsave("../figures/Figure 4. Distribution of Number of Prescribed Drug Scores among BMIs.pdf",
       units = "in", width = 10, height = 10, dpi = 1200)


# Now, Make Grid

comboPlot <- cowplot::plot_grid(P1, P2, nrow = 1, labels = c("A", "B"))
comboPlot

# Adding a Caption

FinalPlot <- ggdraw(comboPlot) + 
  draw_label("Figure 4. Comparison of the Distribution of Prescribed Drug Scores among Different Age Groups and BMI Categories",
             x = 0.5, y = 0, vjust = 1, size = 11) +
  theme(plot.margin = unit(c(0,0,1,0), "cm"))

#Final Export

ggsave2("../figures/Figure 4. Comparison of the Distribution of Prescribed Drug Scores among Different Age Groups and BMI Categories.png", 
        units = "in", width = 15, height = 10, dpi = 1200)
ggsave2("../figures/Figure 3. Comparison of the Distribution of Prescribed Drug Scores among Different Age Groups and BMI Categories.pdf", 
        units = "in", width = 15, height = 10, dpi = 1200)


# Drug Utilization: Study vs WHO Reference


ComparisonData <- data.frame(
  Indicator = c("Avg Drugs No", "Injections", "Antibiotics", 
                "Generics", "EDL"),
  Source = rep(c("Current Study", "WHO Standard"), each = 5),
  Percentage = c(3.2, 24, 2, 0, 45.03,  
               1.8, 24.1, 26.8, 100.0, 100.0))

ComparisonData


ggplot(ComparisonData,
       aes(x = Indicator, y = Percentage, 
           fill = Source)) +
  geom_col() +
  geom_text(aes(label = Percentage),
            vjust = - 0.5,
            size = 3.5) +
  scale_color_viridis_d() +
  scale_fill_discrete(name = "Source") +
  facet_wrap(vars(Source), ncol = 2) +
  labs(x = "Indicators",          
       y = "Percentage (%)", 
       title = "Drug Utilization: Study vs. WHO Reference",
       subtitle = "Comparison of key prescribing indicators",
       caption = "Figure 2. Drug Utilization: Study vs. WHO Reference") +
  theme_bw() +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 12,
                                  hjust = 0.5),
        plot.caption = element_text(size = 11,
                                    hjust = 0.5),
        legend.position = "top")

ggsave("../figures/Figure 4. Drug Utilization Study vs. WHO Reference.png", 
       units = "in", width = 10, height = 10, dpi = 1200)
ggsave("../figures/Figure 4. Drug Utilization Study vs. WHO Reference.pdf",
       units = "in", width = 10, height = 10, dpi = 1200)



