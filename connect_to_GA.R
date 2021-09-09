install.packages("devtools")
devtools::install_github("jdeboer/ganalytics")

library(ganalytics)
gaToken <- GoogleApiCreds(userName = "lagbleze@st.ug.edu.gh", list(client_id = "803758101865-7nolvt9anore6ju7tk2jo9jsth6b0ghr.apps.googleusercontent.com",
                                                        client_secret = "O9rWZzHwepmUPxPyZn3vp13Q"))
save(gaToken, file = "gaToken")

myQuery <- GaQuery(246669950, gaToken)
GetGaData(myQuery)
