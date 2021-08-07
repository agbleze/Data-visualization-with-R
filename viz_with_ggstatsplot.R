runExample() 


#######################

set.seed(123)
library(ggstatsplot)
# model object
mod <- lm(formula = mpg ~ cyl * am, data = mtcars)
# to get a plot
ggcoefstats(x = mod, output = "plot")
# to get a tidy dataframe
ggcoefstats(x = mod, output = "tidy")
# to get a glance summary
ggcoefstats(x = mod, output = "glance")


########### regregg model plot ###

#regmod <- model_tsall_log_butAvgsession %>%

 model_tsall_log_butAvgsession <- tslm(log(data_calendAdjust_ts[,15] + 1) ~ Avg_ECR + Avg_Users + Avg_bounce_rate + Avg_session_duration +
                                          Avg_sessionPer_user + Avg_Pg_session, data = data_calendAdjust_ts )

 model_tsall_log_butAvgsession
  ggcoefstats(x = model_tsall_log_butAvgsession, output = "plot", statistic = "z")
  
  datmode = data.frame(model_tsall_log_butAvgsession)
  ggcoefstats(x = datmode, output = "plot")
  
  
  ggcoefstats(
    x = stats::lm(formula = log(data_calendAdjust_ts[,15] + 1) ~ Avg_ECR + Avg_Users + Avg_bounce_rate + Avg_session_duration +
               Avg_sessionPer_user + Avg_Pg_session, data = data_calendAdjust_ts ),
   # sort = "ascending", # sorting the terms of the model based on estimate values
    ggtheme = ggplot2::theme_gray(), # changing the default theme
  #  stats.label.color = c("#CC79A7", "darkgreen", "#0072B2", "darkred", "black", "red"),
    title = "Regression analysis: Predicting the influence of various KPI on Revenue",
   # subtitle = "What is the influence of various KPI have "
 # output = "glance"
  
  )  

  # setup
  set.seed(123)
  
  # data
  df <- dplyr::filter(
    movies_long,
    genre %in% c(
      "Action",
      "Action Comedy",
      "Action Drama",
      "Comedy",
      "Drama",
      "Comedy Drama"
    )
  )
  
  # plot
  ggcoefstats(
    x = stats::lm(formula = rating ~ genre, data = df),
    sort = "ascending", # sorting the terms of the model based on estimate values
    ggtheme = ggplot2::theme_gray(), # changing the default theme
    stats.label.color = c("#CC79A7", "darkgreen", "#0072B2", "darkred", "black", "red"),
    title = "Movie ratings by their genre",
    subtitle = "Source: www.imdb.com"
  )  
  
  
#   
#   
# (regmod <- model_tsall_log_butAvgsession %>%
#     tbl_regression(label = list(Avg_ECR ~ "Ecommence Conversion Rate",
#                                 Avg_Users ~ "Number of Users",
#                                 Avg_bounce_rate ~ "Bounce Rate",
#                                 Avg_session_duration ~ "Session duration",
#                                 Avg_sessionPer_user ~ "Number of sessions per user",
#                                 Avg_Pg_session ~ "Number of Pages per session")) %>%
#     bold_p()%>%
#     add_glance_source_note() %>%
#     plot()
# )



####################
# for reproducibility
set.seed(123)
library(ggstatsplot)
# for plot
if (require("ggcorrplot")) {
  ggcorrmat(iris)
}
# to get the correlation analyses results in a dataframe
ggcorrmat(
  data = ggplot2::msleep,
  cor.vars = sleep_total:bodywt,
  partial = TRUE,
  output = "dataframe"
)


############
# to get reproducible results from bootstrapping
set.seed(123)
library(ggstatsplot)
# the most basic function call
grouped_ggwithinstats(
  data = VR_dilemma,
  x = modality,
  y = score,
  grouping.var = order,
  type = "np", # non-parametric test
  # additional modifications for **each** plot using `ggplot2` functions
  ggplot.component = ggplot2::scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    limits = c(0, 1)
  )
)


#####################################

# to get reproducible results from bootstrapping
set.seed(123)
library(ggstatsplot)

# simple function call with the defaults
ggbetweenstats(mtcars, am, mpg)

# more detailed function call
ggbetweenstats(
  data = morley,
  x = Expt,
  y = Speed,
  type = "n",
  plot.type = "box",
  xlab = "The experiment number",
  ylab = "Speed-of-light measurement",
  pairwise.comparisons = TRUE,
  p.adjust.method = "fdr",
  outlier.tagging = TRUE,
  outlier.label = Run
)

ggbetweenstats(
  data = spreaduser_type,
  x = "New Users",
  y = "Returning Users",
  type = "n",
  plot.type = "box",
 # xlab = "The experiment number",
#  ylab = "Speed-of-light measurement",
  pairwise.comparisons = TRUE,
  p.adjust.method = "fdr",
  outlier.tagging = TRUE,
  #outlier.label = Run
)

ggbetweenstats(mtcars, am, mpg, type = "n")

ggbetweenstats(spreaduser_type, "New Users", "Returning Users")
ggbetweenstats(data_select_sum, x = "Total_revenue", y = "Total_ECR")
#View(data_select_sum)

data_calendAdjust_ts%>%
  data.frame()%>%
  ggbetweenstats(x = "Avg_ECR", y = "Avg_revenue")

tryi <- data.frame(data_calendAdjust_ts)
ggbetweenstats(tryi, x = "Avg_revenue", y = "Avg_ECR", type = "n")
ggwithinstats(tryi, x = "Avg_revenue", y = "Avg_ECR", type = "p")
