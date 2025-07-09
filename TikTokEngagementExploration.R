library(tidyverse)
library(ggplot2)

#load the data
tiktok_data <- read.csv("tiktok_dataset.csv")

#drop rows without any label
cleaned_tiktok_data <- tiktok_data[tiktok_data$claim_status != "", ]

#segregate claim and opinion rows
claim_rows <- cleaned_tiktok_data[cleaned_tiktok_data$claim_status == "claim", ]
opinion_rows <- cleaned_tiktok_data[cleaned_tiktok_data$claim_status == "opinion", ]


#calculate averages
mean_claim_like_count <- mean(as.numeric(claim_rows$video_like_count))
mean_opinion_like_count <- mean(as.numeric(opinion_rows$video_like_count))

mean_claim_share_count <- mean(as.numeric(claim_rows$video_share_count))
mean_opinion_share_count <- mean(as.numeric(opinion_rows$video_share_count))

mean_claim_comment_count <- mean(as.numeric(claim_rows$video_comment_count))
mean_opinion_comment_count <- mean(as.numeric(opinion_rows$video_comment_count))

#plot averages
mean_engagement <- data.frame(
  claim_status = c("Claim", "Claim", "Claim", "Opinion", "Opinion", "Opinion"),
  metric = c("Likes", "Shares", "Comments", "Likes", "Shares", "Comments"),
  mean_value = c(mean_claim_like_count, mean_claim_share_count, mean_claim_comment_count, mean_opinion_like_count, mean_opinion_share_count, mean_opinion_comment_count)
)

ggplot(mean_engagement, aes(x = metric, y = mean_value, fill = claim_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10(labels = scales::comma) +
  labs(title = "Average Engagement by Content Type (Log Scale)",
       x = "Engagement Metric",
       y = "Mean Count (log scale)",
       fill = "Content Type") +
  theme_minimal()

# make sure its statistically significant (t-test)
t.test(as.numeric(video_like_count) ~ claim_status, data = cleaned_tiktok_data)
t.test(as.numeric(video_share_count) ~ claim_status, data = cleaned_tiktok_data)
t.test(as.numeric(video_comment_count) ~ claim_status, data = cleaned_tiktok_data)


#check if video length and type of account affect engagement (ANOVA) 
anova_result <- aov(as.numeric(cleaned_tiktok_data$video_like_count) ~ cleaned_tiktok_data$claim_status * as.numeric(cleaned_tiktok_data$video_duration_sec) * cleaned_tiktok_data$verified_status, data = cleaned_tiktok_data)
summary(anova_result)
