##### Regression Code #####

library(GGally)
library(chron)
library(cvTools)
load(path + '/R/cleanData')

#removing NA and empty entries#
by_session = by_session[-which(by_session$uptime == ""), ]
by_session = by_session[-which(is.na(by_session$max_viewer)), ]
by_session$US = by_session$geo == 'US'
by_session = subset(by_session, select = -c(geo))

cleaned_timestamp = c()
for (i in 1:nrow(by_session)) {
  timestamp = toString(by_session$uptime[i])
  ts = unlist(strsplit(timestamp, "\\s+"))
  cleaned_timestamp[i] = chron(paste(ts[2], ts[3], ts[5]), ts[4], format = c('mon d y', 'h:m:s'))
}

by_session$uptime = cleaned_timestamp


# sample mean predictions #
sample_mean = mean(by_session$max_viewer)
RMSE_sm = sqrt(mean((by_session$max_viewer - sample_mean)^2))

# observe correlation of covariates#
cor(data.matrix(by_session))

# linear regression #
by_session.reg = subset(by_session, select = -c(session_id, channel_id, language_channel, timezone, producer))
by_session.reg = as.data.frame(data.matrix(by_session.reg))
fm = lm(max_viewer ~ ., data = by_session.reg)
cv.lm = cvFit(fm, data=by_session.reg, y=by_session.reg$max_viewer, K=5)