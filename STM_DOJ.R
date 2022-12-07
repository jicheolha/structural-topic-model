## Load STM
library(stm)

## Navigate 
#localpath = '/Users/jicheolha/Dropbox/DOJTextAnalysis/'
#localpath = '/Users/jetson/Dropbox/Research/FraudPerspective/DOJTextAnalysis/'
setwd(localpath)

## Read Data
data = read.csv('data/DOJAll_PR_Tagged.csv')

## Process Data
processed = textProcessor(data$pr, metadata = data)
out = prepDocuments(processed$documents, processed$vocab, processed$meta)
docs = out$documents
vocab = out$vocab
meta = out$meta

## Clean Date Covariate
new_date = data$date[-processed$docs.removed]
datedrop <- which(is.na(new_date))
newdate_dropdate <- new_date[-datedrop]
docs_dropdate <- docs[-datedrop]
meta_dropdate <- meta[-datedrop,]

## Models
#model_0 = stm(docs_dropdate, vocab, K = 5, prevalence = ~s(newdate_dropdate), data = meta_dropdate, init.type = "Spectral", max.em.its = 150, set.seed(0), verbose = False)
model_1 = stm(docs_dropdate, vocab, K = 5, prevalence = ~s(newdate_dropdate), data = meta_dropdate, init.type = "Spectral", max.em.its = 150, set.seed(1), verbose = FALSE)
model_2 = stm(docs_dropdate, vocab, K = 10, prevalence = ~s(newdate_dropdate), data = meta_dropdate, init.type = "Spectral", max.em.its = 150, set.seed(2), verbose = FALSE)
model_3 = stm(docs_dropdate, vocab, K = 15, prevalence = ~s(newdate_dropdate), data = meta_dropdate, init.type = "Spectral", max.em.its = 150, set.seed(3), verbose = FALSE)
model_4 = stm(docs_dropdate, vocab, K = 20, prevalence = ~s(newdate_dropdate), data = meta_dropdate, init.type = "Spectral", max.em.its = 150, set.seed(4), verbose = FALSE)

## Model Plots
#setwd('results/stm/model0')
pdf('model_0.pdf')
plot(model_0, n = 5, xlim = c(0, 0.5))
dev.off()

#setwd('results/stm/model1')
pdf('model_1.pdf')
plot(model_1, n = 5, xlim = c(0, 0.5))
dev.off()

#setwd('results/stm/model2')
pdf('model_2.pdf')
plot(model_2, n = 5, xlim = c(0, 0.5))
dev.off()

#setwd('results/stm/model3')
pdf('model_3.pdf')
plot(model_3, n = 5, xlim = c(0, 0.5))
dev.off()

#setwd('results/stm/model4')
pdf('model_4.pdf')
plot(model_4, n = 5, xlim = c(0, 0.5))
dev.off()

## Word Clouds
#setwd('results/stm/model3/cloud')
pdf('cloud_1.pdf')
cloud(model_3, topic = 1, scale = c(3, 1))
dev.off()

pdf('cloud_2.pdf')
cloud(model_3, topic = 2, scale = c(3, 1))
dev.off()

pdf('cloud_3.pdf')
cloud(model_3, topic = 3, scale = c(3, 1))
dev.off()

pdf('cloud_4.pdf')
cloud(model_3, topic = 4, scale = c(3, 1))
dev.off()

pdf('cloud_5.pdf')
cloud(model_3, topic = 5, scale = c(3, 1))
dev.off()

pdf('cloud_6.pdf')
cloud(model_3, topic = 6, scale = c(3, 1))
dev.off()

pdf('cloud_7.pdf')
cloud(model_3, topic = 7, scale = c(3, 1))
dev.off()

pdf('cloud_8.pdf')
cloud(model_3, topic = 8, scale = c(3, 1))
dev.off()

pdf('cloud_9.pdf')
cloud(model_3, topic = 9, scale = c(3, 1))
dev.off()

pdf('cloud_10.pdf')
cloud(model_3, topic = 10, scale = c(3, 1))
dev.off()

pdf('cloud_11.pdf')
cloud(model_3, topic = 11, scale = c(3, 1))
dev.off()

pdf('cloud_12.pdf')
cloud(model_3, topic = 12, scale = c(3, 1))
dev.off()

pdf('cloud_13.pdf')
cloud(model_3, topic = 13, scale = c(3, 1))
dev.off()

pdf('cloud_14.pdf')
cloud(model_3, topic = 14, scale = c(3, 1))
dev.off()

pdf('cloud_15.pdf')
cloud(model_3, topic = 15, scale = c(3, 1))
dev.off()

## Display Topics and FREX Weightings
#labelTopics(model_0)
labelTopics(model_1)
labelTopics(model_2)
labelTopics(model_3)
labelTopics(model_4)

## Top Most Representative Documents
modeled_documents = data$pr[-processed$docs.removed][-datedrop]
findThoughts(model_3, modeled_documents, topics = 5, n = 2)
findThoughts(model_3, modeled_documents, topics = 9, n = 2)
findThoughts(model_3, modeled_documents, topics = 2, n = 2)
findThoughts(model_3, modeled_documents, topics = 6, n = 2)
findThoughts(model_3, modeled_documents, topics = 12, n = 2)

## Contrasts Plots
#setwd('results/stm/model3/contrast')

pdf('contrast_1_5.pdf')
contrast_plot = plot(model_3, type = "perspectives", topics = c(1,5), main = "Topics 1 and 5")
dev.off()

pdf('contrast_5_12.pdf')
contrast_plot = plot(model_3, type = "perspectives", topics = c(5,12), main = "Topics 5 and 12")
dev.off()

pdf('contrast_5_9.pdf')
contrast_plot = plot(model_3, type = "perspectives", topics = c(5,9), main = "Topics 5 and 9")
dev.off()

pdf('contrast_2_9.pdf')
contrast_plot = plot(model_3, type = "perspectives", topics = c(2,9), main = "Topics 2 and 9")
dev.off()

pdf('contrast_5_8.pdf')
contrast_plot = plot(model_3, type = "perspectives", topics = c(5,8), main = "Topics 5 and 8")
dev.off()

## Topic Correlations
pdf('topic_correlations_0.pdf')
topic_correlations = plot(topicCorr(model_0))
dev.off()


pdf('topic_correlations_1.pdf')
topic_correlations = plot(topicCorr(model_1))
dev.off()


pdf('topic_correlations_2.pdf')
topic_correlations = plot(topicCorr(model_2))
dev.off()

pdf('topic_correlations_3.pdf')
topic_correlations = plot(topicCorr(model_3))
dev.off()


pdf('topic_correlations_4.pdf')
topic_correlations = plot(topicCorr(model_4))
dev.off()

# Topic Proportion as a Function of Time
prep = estimateEffect(~s(date), stmobj = model_3, metadata = meta_dropdate, uncertainty = "Global")
plot(prep, "date", method = "continuous", topics = 7, model = model_3, printlegend = FALSE, xaxt = 'n', xlab = "Time")
yearseq = seq(from = as.Date("2000-01-01"), to = as.Date("2022-01-01"), by = "year")
axis(side = 1, at = as.numeric(yearseq) + 25551, labels = yearseq)

#print(yearseq)
#print(as.numeric(yearseq))

# Optimal K
K = searchK(docs_dropdate, vocab, K = c(2:20), prevalence =~ s(newdate_dropdate), data = meta)

pdf('K_2_20.pdf')
plot(K)
dev.off()

pdf('K_to_100.pdf')
plot(K_2)
dev.off()
K_2 = searchK(docs_dropdate, vocab, K = c(20,40,60,80,100), prevalence =~ s(newdate_dropdate), data = meta)
