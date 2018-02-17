library(tidyverse)
library(GGally)

# Read and clean data --------------------------------------------------------------

load("C:/Users/minch_000/Desktop/226 project/.RData")

stream_data <- select(week_data, -accessed_at_utc, -channel_login, -video_height, 
                      -video_width, -embed_count, -site_count, -audio_codec, 
                      -video_codec, -session_count)
levels(stream_data$mature) <- c("False", "True")
stream_data$viewers <- as.numeric(as.character(stream_data$viewers))

# Finding associated covariates
assoc_data <- select(stream_data, viewers, video_bitrate, uptime_sec, channel_view_count, featured, mature)
sample_assoc_data <- assoc_data[sample(nrow(assoc_data), 100),]
ggpairs(sample_assoc_data)



# Group by session for future uses
by_session <- group_by(stream_data, session_id)

# Find out how having a "Mature" flag correlates with max # of viewers
# of a given session.
by_mature <- group_by(stream_data, session_id) %>%
                summarise(
                  max_viewer = max(viewers),
                  mature = mature[1]
                ) %>%
                filter(
                  !is.na(max_viewer),
                  !is.na(mature)
                ) %>% 
                group_by(mature) %>%
                summarise(
                  max_viewer_avg = mean(max_viewer)
                )
by_mature

# A tibble: 2 × 2
#   mature max_viewer_avg
#   <fctr>          <dbl>
# 1  False       13.87001
# 2   True       25.28092

# Association between the mature and the featured flag
by_feature_mature <-  select(by_session,
                        featured, 
                        mature
                      ) %>%
                      filter(featured == 'True') %>% # only consider featured sessions
                      group_by(mature) %>%
                      summarise(
                        count = n()
                      )
by_feature_mature

# A tibble: 2 × 2
#   mature  count
#   <fctr>  <int>
# 1  False 134098
# 2   True  51780

# Find out how having a "Featured" flag correlates with max # of viewers
# of a given session.
by_featured <- group_by(stream_data, session_id, featured) %>%
  summarise(
    max_viewer = max(viewers)
  ) %>%
  filter(
    !is.na(max_viewer),
    !is.na(featured), 
    featured == 'False' | featured == 'True'
  ) %>% 
  group_by(featured) %>%
  summarise(
    max_viewer_avg = mean(max_viewer)
  )
by_featured

# A tibble: 2 × 2
#   featured max_viewer_avg
#     <fctr>          <dbl>
# 1    False       8.734332
# 2     True     538.790890

# There were a total of 134098 + 51780 = 185878 sessions that were featured.
# Out of these, 51780, or 28% 
