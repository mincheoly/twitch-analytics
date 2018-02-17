##### Classification Code ######

library(chron)
library(ROCR)
library(boot)

# Clean Data ----
by_session = by_session[-which(by_session$uptime == ""), ]

cleaned_timestamp = rep(chron(), nrow(by_session))
primetime = vector(length=nrow(by_session))
for (i in 1:nrow(by_session)) {by
  timestamp = toString(by_session$uptime[i])
  ts = unlist(strsplit(timestamp, "\\s+"))
  temp = chron(paste(ts[2], ts[3], ts[5]), ts[4], format = c('mon d y', 'h:m:s'))
  time = strsplit(ts[4], ':')
  if (as.numeric(time[[1]][1]) > 18 && as.numeric(time[[1]][1]) < 22) {
    primetime[i] = 1;
  }
  cleaned_timestamp[i] = temp
}

by_session$uptime = cleaned_timestamp
by_session$primetime = primetime
classification_data = by_session
classification_data$mature = as.logical(classification_data$mature)*1
classification_data$producer = as.logical(classification_data$producer)*1
classification_data$channel_subscription = as.logical(classification_data$channel_subscription)*1
classification_data$featured = as.logical(classification_data$featured)*1
classification_data$viewed = (classification_data$max_viewer > 3)*1


# Primetime Classification Task -----

# Baseline

fm_baseline = glm(primetime ~ max_viewer + video_bitrate + uptime_sec + channel_view_count + featured + mature + channel_subscription, family='binomial',data=classification_data)

prob = predict(fm_baseline1, classification_data, type='response')
pred = prediction(prob, classification_data$primetime)

fnr.perf = performance(pred, measure = "fnr")
tpr.perf = performance(pred, measure = "tpr")
tnr.perf = performance(pred, measure = "tnr")
fpr.perf = performance(pred, measure = "fpr")
plot(fnr.perf)
plot(tpr.perf)
plot(fpr.perf)
plot(fpr.perf)

# Get ROC curve and AUC
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Model 1

fm_model_1 = glm(primetime ~ max_viewer + uptime_sec + channel_view_count + featured + mature + max_viewer:channel_view_count + mature:featured + max_viewer:uptime_sec, family='binomial',data=classification_data)

prob = predict(fm_model_1, classification_data, type='response')
pred = prediction(prob, classification_data$primetime)

acc.perf = performance(pred, measure = "acc")
plot(acc.perf)

fnr.perf = performance(pred, measure = "fnr")
tpr.perf = performance(pred, measure = "tpr")
tnr.perf = performance(pred, measure = "tnr")
fpr.perf = performance(pred, measure = "fpr")
plot(fnr.perf)
plot(tpr.perf)
plot(fpr.perf)
plot(tnr.perf)

# Get ROC curve and AUC
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Divide into train/validate
val_rows = sample(nrow(classification_data), 50000)
val_set = classification_data[val_rows, ]
train_set = classification_data[-val_rows, ]
fm_model_1 = glm(primetime ~ max_viewer + uptime_sec + channel_view_count + featured + mature + max_viewer:channel_view_count + mature:featured + max_viewer:uptime_sec, family='binomial',data=train_set)
prob = predict(fm_model_1, val_set, type='response')
preds = (prob > 0.15)*1
correct = 0
for (i in 1:nrow(val_set)){
  if (!is.na(preds[i]) && !is.na(val_set$primetime[i])){
    if (preds[i] == val_set$primetime[i]) {
      correct = correct + 1
    }
  }
}
accuracy = correct/50000
accuracy
