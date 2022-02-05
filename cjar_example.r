#devtools::install_github('searchdiscovery/cjar')
library(plotly)
library(cjar)
library(dplyr)
library(Rtsne)
cja_auth_with('jwt')
cja_auth()

dv = "dv_6161b8cb41212d45c47ce83d"

user_summaries = cja_freeform_table(
  dataId = dv,
  date_range = c(as.POSIXct("2022-01-01"), as.POSIXct("2022-01-31")),
  dimensions = c("adobe_personid"),
  metrics = c("csv_downloads", "shares", "right_clicks", "visits"),
  top=50000
)

transformed_users = Rtsne(user_summaries, dims=2)
plot_data = as.data.frame(transformed_users$Y)

library(dbscan)
cluster_model2 = dbscan(plot_data, minPts = 15, eps=2.3)
clustered_data2 = plot_data
clustered_data2$cluster = cluster_model2$cluster
p = plot_ly(
  data = clustered_data2, 
  x = ~V1, 
  y = ~V2,
  color = ~as.factor(cluster)
) %>% layout(
  xaxis = list(
    domain = c(-60,60),
    title = "tSNE V1"
  ),
  yaxis = list(
    domain = c(-50,50),
    title = "tSNE V2",
    scaleanchor = "x"
  )
)
p


clustered_user_rollup = cbind(user_summaries, clustered_data2)
plot_ly(
  data = clustered_user_rollup,
  x = ~V1,
  y = ~V2,
  #z = ~as.numeric(hits),
  #z = ~as.numeric(visits),
  #z = ~as.numeric(email_sign_ups),
  z = ~as.numeric(shares),
  #z = ~as.numeric(internal_searches),
  #z = ~as.numeric(orders),
  #z = ~as.numeric(product_views),
  color = ~as.factor(cluster)
) %>% layout( 
  height = 650,
  scene = list(
    xaxis = list(
      domain = c(-60,60),
      title = "tSNE V1"
    ),
    yaxis = list(
      domain = c(-50,50),
      title = "tSNE V2",
      scaleanchor = "x"
    ),
    zaxis = list(
      title = "Revenue"
    )
  )
)
