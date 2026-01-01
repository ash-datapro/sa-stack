library(readr)
library(dplyr)
library(tibble)
library(purrr)

# point this at the folder that contains:
# datasetSentences.txt, datasetSplit.txt, dictionary.txt, sentiment_labels.txt,
# original_rt_snippets.txt, SOStr.txt, STree.txt, README.txt
treebank_path = "~/Desktop/Project/sentiment-api/data/input-data/stanfordSentimentTreebank/stanfordSentimentTreebank"

files = list.files(treebank_path, full.names = TRUE)
names(files) = basename(files)

# 1) Tab-separated with a header row
datasetSentences = read_tsv(
  files["datasetSentences.txt"],
  skip = 1,
  col_names = c("sentence_index", "sentence"),
  show_col_types = FALSE
)

datasetSplit = read_csv(
  files["datasetSplit.txt"],
  skip = 1,
  col_names = c("sentence_index", "splitset_label"),
  show_col_types = FALSE
)

# 2) Pipe-delimited
dictionary = read_delim(
  files["dictionary.txt"],
  delim = "|",
  col_names = c("phrase", "phrase_id"),
  show_col_types = FALSE
)

sentiment_labels = read_delim(
  files["sentiment_labels.txt"],
  delim = "|",
  skip = 1,
  col_names = c("phrase_id", "sentiment"),
  show_col_types = FALSE
) %>%
  mutate(phrase_id = as.integer(phrase_id),
         sentiment = as.double(sentiment))

# 3) Plain text (one item per line)
original_rt_snippets = read_lines(files["original_rt_snippets.txt"])
SOStr = read_lines(files["SOStr.txt"])
STree = read_lines(files["STree.txt"])
README = read_lines(files["README.txt"])

# Optional: keep everything in one list
sst_treebank = list(
  datasetSentences = datasetSentences,
  datasetSplit = datasetSplit,
  dictionary = dictionary,
  sentiment_labels = sentiment_labels,
  original_rt_snippets = original_rt_snippets,
  SOStr = SOStr,
  STree = STree,
  README = README,
  files = files
)

# Quick sanity checks
map(sst_treebank[c("datasetSentences", "datasetSplit", "dictionary", "sentiment_labels")], nrow)


sst_treebank = list(datasetSentences = datasetSentences, 
                    datasetSplit = datasetSplit, 
                    dictionary = dictionary, 
                    sentiment_labels = sentiment_labels, 
                    original_rt_snippets = original_rt_snippets, 
                    SOStr = SOStr, STree = STree, README = README, files = files)

saveRDS(sst_treebank, file = "sst_treebank.rds")