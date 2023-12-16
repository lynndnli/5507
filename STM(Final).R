# Loading required libraries (sorted and deduplicated)
library(tidyr)
library(quanteda)
library(dplyr)
library(furrr)
library(ggplot2)
library(ggraph)
library(igraph)
library(jiebaR)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(showtext)
library(stm)
library(stminsights)
library(sysfonts)
library(tidytext)
library(tidyverse)
library(tidystm)
library(visNetwork)
library(corrplot)
# Initializing showtext package
showtext_auto(enable = TRUE)

#######################################################################
################# 1. Document-feature Matrix Building #################
#######################################################################

# Reading CSV file and building a corpus
df <- read.csv(file = '/Users/Lynn/Downloads/codes/PP_merged_1213.csv')
df$index <- seq(1:nrow(df))

# Tokenizing and constructing a document-feature matrix
jieba_tokens <- corpus(df, docid_field = "index", text_field = "tokenz") %>%
  tokenizers::tokenize_regex(pattern = " ") %>%
  tokens()

# Assigning document variables
docvars(jieba_tokens, "Label") <- as.character(df$Label)
docvars(jieba_tokens, "title") <- as.character(df$title)
docvars(jieba_tokens, "note_desc") <- as.character(df$note_desc)

# Creating and trimming the document-feature matrix
jieba_dfm <- dfm(jieba_tokens)
dfm_trim <- dfm_trim(jieba_dfm, min_docfreq = 0.0003, max_docfreq = 0.99, 
                     docfreq_type = "prop", verbose = TRUE)
dfm_trim
# Displaying top features
topfeatures(dfm_trim, n = 20, scheme = "docfreq")

# Converting to STM format
stmdfm <- convert(dfm_trim, to = "stm", docvars = docvars(jieba_tokens))
out <- list(documents = stmdfm$documents, vocab = stmdfm$vocab, meta = stmdfm$meta)

#######################################################################
########################## 2. Model Selection #########################
#######################################################################

# Trying different models and comparing their performance
many_models <- tibble(K = (2:15)) %>%  # Modified to directly use the required range
  mutate(topic_model = future_map(K, ~stm(out$documents, out$vocab, K = .,
                                          data = out$meta, init.type = "Spectral",
                                          seed = 2022, prevalence = ~Label)))
heldout <- make.heldout(dfm_trim)

# Evaluating and diagnosing models
k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfm_trim),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, dfm_trim),
         bound = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

# Visualizing model diagnostics by number of topics
k_result %>%
  transmute(K, `Lower bound` = lbound, Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)", y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "")

# Comparing exclusivity and semantic coherence
k_result %>%
  unnest(c(exclusivity, semantic_coherence)) %>% 
  filter(K %in%  (2:15)) %>%
  group_by(K) %>% 
  summarize(exclusivity = mean(exclusivity),
            semantic_coherence = mean(semantic_coherence)) %>% 
  mutate(K = as.factor(K)) %>%
  ggplot(aes(x = semantic_coherence, y = exclusivity, color = as.factor(K))) +
  geom_point(size = 1, alpha = 0.7, show.legend = FALSE) +
  ggrepel::geom_text_repel(
    aes(label = as.factor(K)),
    size = 5,
    hjust = 0, 
    show.legend = FALSE
  )+
  labs(x = "Semantic Coherence (mean)", y = "Exclusivity (mean)",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "")

# Outputting model matrix to CSV
matrix <- k_result %>%
  unnest(c(exclusivity, semantic_coherence)) %>% 
  group_by(K) %>% 
  summarize(exclusivity = mean(exclusivity),
            semantic_coherence = mean(semantic_coherence))
write.table(matrix , file = "model_matrix.csv", sep=",", row.names=FALSE)

# Selecting model with 15 topics
model_7 <- many_models %>% 
  filter(K == 7) %>% 
  pull(topic_model) %>% 
  .[[1]]

model_9 <- many_models %>% 
  filter(K == 9) %>% 
  pull(topic_model) %>% 
  .[[1]]

model_12 <- many_models %>% 
  filter(K == 12) %>% 
  pull(topic_model) %>% 
  .[[1]]

model_4 <- many_models %>% 
  filter(K == 4) %>% 
  pull(topic_model) %>% 
  .[[1]]



# Extracting top terms from topics
T_9 <- labelTopics(model_9, n = 20)
capture.output(T_9, file = "T_9.txt")

T_12 <- labelTopics(model_12, n = 20)
capture.output(T_12, file = "T_12.txt")

T_7 <- labelTopics(model_7, n = 20)
capture.output(T_7, file = "T_7.txt")
# Analyzing thoughts in the model
findThoughts(model_7, texts = out[["meta"]][["title"]], n = 4)
findThoughts(model_7, texts = out[["meta"]][["note_desc"]], n = 4)
findThoughts(model_35, texts = out[["meta"]][["content_desc"]], n = 4)

# Evaluating topic quality
Q_7 <- tibble(
  topic = 1:7,
  exclusivity = exclusivity(model_7),
  semantic_coherence = semanticCoherence(model_7, out$documents)
) %>% 
  ggplot(aes(semantic_coherence, exclusivity, label = topic)) +
  geom_point() +
  geom_text(nudge_y = .01) +
  theme_bw()

write.table(Q_7[["data"]] , file = "Q_7.csv", sep=",", row.names=FALSE)

#######################################################################
########################## 3. Estimate Effect #########################
#######################################################################

# Assigning names to topics and preparing data for effects analysis
topicNames <- labelTopics(model_7)
k <- 7 
topic <- data.frame(
  topicnames = c("TOPIC 1: Gender Discrimination",
                 "TOPIC 2: Personal Relationships",
                 "TOPIC 3: Character Aesthetics",
                 "TOPIC 4: Empowerment of Female Player",
                 "TOPIC 5: Casual Gaming",
                 "TOPIC 6: Gender Stereotypes",
                 "TOPIC 7: Strategic Gameplay"),

                 
  TopicNumber = 1:k,
  TopicProportions = colMeans(model_7$theta)
)

# Estimating effects of variables on topics
prep <- estimateEffect(1:k ~ Label, model_7, meta = out$meta, uncertainty = "Global")

# Plotting the effect of sentiment on topic prevalence
par(mfrow=c(1,1))
Result <- plot(prep, "Label", method = "difference", topics = c(1:7), model = model_7, 
               cov.value1 = "M", cov.value2 = "F",
               xlab = "More Likely Female - Not Significant - More Likely Male",
               main = "Effect of Sentiment on Topic Prevalence",
               xlim = c(-.15, .15), labeltype = "custom",
               custom.labels = c(1:7))

# Extracting effect data and exporting to CSV
effect <- extract.estimateEffect(prep, c("Label"), model = model_7, 
                                 method = "difference", cov.value1 = "M", cov.value2 = "F")
write.csv(effect,'gender_effect_7.csv')

# Reordering topics based on expected topic proportion
rank <- order(unlist(Result$means))
topic <- topic[rank,]

# Plotting differences in sentiment between male and female
par(mfrow = c(1,1),mar = c(6, 2, 2, 2))
STMresults <- plot(prep, "Label", method = "difference", cov.value1 = "M", 
                   cov.value2 = "F", 
                   topics = topic$TopicNumber,
                   verbose.labels = F, 
                   ylab = "", 
                   labeltype = "custom",
                   xlab = "More Likely Female                                                           - Not Significant                          - More Likely Male",
                   custom.labels  = topic$topicnames, 
                   main = "",
                   xlim = c(-.16,.08),
                   width = 100)

model_7_dt <- make.dt(model_7, meta = NULL)
plot(model_7)

cloud(model_7, topic = 7, scale = c(2, 0.75))

mod.out.corr <- topicCorr(model_7)
plot(mod.out.corr)

require(devtools)
install_version("igraph", version = "1.2.6", repos = "http://cran.us.r-project.org")
#######################################################################
######################## 4. Topical Correlation #######################
#######################################################################

# Analyzing topic correlations
# mod.out.corr <- topicCorr(model_35, cutoff = .01)
mod.out.corr <- topicCorr(model_7, cutoff = .01)

# Visualizing topic correlations
corrplot(mod.out.corr$cor, order="hclust", hclust.method="ward.D2", 
         method = "circle", type = "lower", diag = F)

# Further topic correlation analysis with different method
mod.out.corr <- topicCorr(model_7, method = "huge")

# Preparing data for network analysis
links2 <- as.matrix(mod.out.corr$posadj)
net2 <- graph_from_adjacency_matrix(links2, mode = "undirected")
net2 <- igraph::simplify(net2, remove.loops = TRUE) 

links <- igraph::as_data_frame(net2, what="edges")
nodes <- igraph::as_data_frame(net2, what="vertices")

# Detecting communities in the network
clp <- cluster_label_prop(net2)
nodes$community <- clp$membership

# Generating colors for nodes based on the effect of sentiment
means <- as.data.frame(unlist(STMresults$means))
means$means[is.na(means$means)] <- 0  # Replace NA with 0 or another appropriate value
colorIntensity <- abs(means$means) / 0.095
colorIntensity[colorIntensity > 1] <- 1  # Ensuring the values are between 0 and 1
color <- colorRamp(c("white", "#FFB200"))(colorIntensity)

colnames(means) <- "means"
color <- colorRamp(c("white","#FFB200"))(abs(means$means)/0.095)
means$colorDem <- rgb(color[,1],color[,2],color[,3],  maxColorValue=255)

color <- colorRamp(c("white","#277BC0"))(abs(means$means)/0.1)
means$colorRep <- rgb(color[,1],color[,2],color[,3],  maxColorValue=255)

means$color <- ifelse(means$means>0,means$colorDem,means$colorRep)

# Configuring nodes for the visNetwork visualization
nodes$shape <- "dot"  
nodes$shadow <- TRUE
nodes$title <- apply(topicNames$prob, 1, function(x) paste0(x, collapse = " + "))[rank] 
nodes$label <- topic$topicnames
nodes$size <- (topic$TopicProportions / max(topic$TopicProportions)) * 40
nodes$borderWidth <- 2

# Setting node colors
nodes$color.background <- means$color
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
nodes$id <- topic$TopicNumber

# Creating the network visualization with visNetwork
visNetwork(nodes, links, width="100%",  height="1200px", main="Topic Sentiment Correlation Network") %>%
  visOptions(highlightNearest = list(enabled = TRUE, algorithm = "hierarchical")) %>%
  visInteraction(navigationButtons = TRUE)

# Exporting network data to CSV
write.csv(links,'stance_edges.csv')
write.csv(nodes,'stance_nodes.csv')

# Extracting and exporting topic correlations
stm_corrs <- get_network(model = model_10,
                         method = 'huge',
                         labels = paste('Topic', 1:10),
                         cutoff = 0.01,
                         cutiso = FALSE)
write.table(stm_corrs , file = "gender_corr.csv", sep=",", row.names=FALSE)



# extract theta value 
# Assuming 'data' is your original dataset and it includes a column 'tokenz'

# Assuming 'data' is your original dataset with a column 'note_id'

# Step 1: Filter note_id's based on the documents included in the STM model
# Assuming the 'note_id's are in the same order as the documents in the STM model
filtered_note_ids <- data$note_id[!is.na(data$tokenz)]

# Ensure the length matches the number of rows in the theta dataframe
filtered_note_ids <- filtered_note_ids[1:nrow(model_7$theta)]

# Step 2: Extract theta values and add the filtered note_id's
theta_values <- as.data.frame(model_7$theta)
theta_values$note_id <- filtered_note_ids

# Step 3: Export to CSV
write.csv(theta_values, "theta_values_with_note_id.csv", row.names = FALSE)


# Load the necessary libraries
library(readr)

# Read the datasets
theta_values <- read_csv("/mnt/data/theta_values_with_note_id.csv")
main_data <- read_csv("/mnt/data/PP_merged_with_scores1213.csv")

# Merge the datasets by 'note_id'
merged_data <- merge(main_data, theta_values, by = "note_id")

# You can now work with the merged_data
# Optionally, you can write the merged data to a new CSV file
write.csv(merged_data, "/mnt/data/merged_data.csv", row.names = FALSE)




