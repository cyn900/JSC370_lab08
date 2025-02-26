Lab 08 - Text Mining/NLP
================

# Learning goals

- Use `unnest_tokens()` and `unnest_ngrams()` to extract tokens and
  ngrams from text
- Use dplyr and ggplot2 to analyze and visualize text data
- Try a theme model using `topicmodels`

# Lab description

For this lab we will be working with the medical record transcriptions
from <https://www.mtsamples.com/> available at
<https://github.com/JSC370/JSC370-2025/tree/main/data/medical_transcriptions>.

# Deliverables

1.  Questions 1-7 answered, knit to pdf or html output uploaded to
    Quercus.

2.  Render the Rmarkdown document using `github_document` and add it to
    your github site. Add link to github site in your html: <https://github.com/cyn900/JSC370_lab08>

### Setup packages

You should load in `tidyverse`, (or `data.table`), `tidytext`,
`wordcloud2`, `tm`, and `topicmodels`.

## Read in the Medical Transcriptions

Loading in reference transcription samples from
<https://www.mtsamples.com/>

``` r
library(tidytext)
library(tidyverse)
```

    ## Warning: package 'lubridate' was built under R version 4.3.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(wordcloud2)
library(tm)
```

    ## Warning: package 'tm' was built under R version 4.3.3

    ## Loading required package: NLP

    ## Warning: package 'NLP' was built under R version 4.3.3

    ## 
    ## Attaching package: 'NLP'
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     annotate

``` r
library(topicmodels)
```

    ## Warning: package 'topicmodels' was built under R version 4.3.3

``` r
mt_samples <- read_csv("https://raw.githubusercontent.com/JSC370/JSC370-2025/main/data/medical_transcriptions/mtsamples.csv")
```

    ## New names:
    ## Rows: 3682 Columns: 6
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (5): description, medical_specialty, sample_name, transcription, keywords dbl
    ## (1): ...1
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

``` r
mt_samples <- mt_samples |>
  select(description, medical_specialty, transcription)

head(mt_samples)
```

    ## # A tibble: 6 × 3
    ##   description                                    medical_specialty transcription
    ##   <chr>                                          <chr>             <chr>        
    ## 1 A 23-year-old white female presents with comp… Allergy / Immuno… "SUBJECTIVE:…
    ## 2 Consult for laparoscopic gastric bypass.       Bariatrics        "PAST MEDICA…
    ## 3 Consult for laparoscopic gastric bypass.       Bariatrics        "HISTORY OF …
    ## 4 2-D M-Mode. Doppler.                           Cardiovascular /… "2-D M-MODE:…
    ## 5 2-D Echocardiogram                             Cardiovascular /… "1.  The lef…
    ## 6 Morbid obesity.  Laparoscopic antecolic anteg… Bariatrics        "PREOPERATIV…

------------------------------------------------------------------------

## Question 1: What specialties do we have?

We can use `count()` from `dplyr` to figure out how many different
medical specialties are in the data. Are these categories related?
overlapping? evenly distributed? Make a bar plot.

``` r
mt_samples %>%
  count(medical_specialty, sort = TRUE) %>%
  ggplot(aes(x = n, y = reorder(medical_specialty, n))) +  # Reordering by count for a better visual arrangement
  geom_bar(stat = "identity", fill = "#619CFF") +  # Using a single color for all bars
  labs(title = "Count of Medical Specialty in mt_samples",
       x = "Frequency",
       y = "Medical Specialty") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10),  # Ensuring the font size is readable
        axis.text.x = element_text(size = 10),
        panel.grid.major.y = element_blank(),  # Remove horizontal grid lines for a cleaner look
        panel.grid.minor.y = element_blank(),
        legend.position = "none")
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Some categories are related and overlapped. Specialties like
“Neurosurgery” and “Neurology” are closely related but focus on surgical
and non-surgical treatments, respectively. There might be overlaps where
broader categories, such as “Cardiovascular / Pulmonary,” encompass more
specific fields like “Cardiology” and “Pulmonology.” Moreover, the
“Surgery” is are much higher than others, this means dataset skewed
towards surgical cases.

------------------------------------------------------------------------

## Question 2: Tokenize

- Tokenize the the words in the `transcription` column
- Count the number of times each token appears
- Visualize the top 20 most frequent words with a bar plot
- Create a word cloud of the top 20 most frequent words

### Explain what we see from this result. Does it makes sense? What insights (if any) do we get?

``` r
# Tokenizing the words in the 'transcription' column and counting them
tokens <- mt_samples %>%
  select(transcription) %>%
  unnest_tokens(word, transcription) %>%
  count(word, sort = TRUE)

# Visualizing the top 20 most frequent words with a bar plot
top_words <- tokens %>%
  top_n(20, n)  # explicitly specifying the column 'n' for clarity

ggplot(top_words, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  coord_flip() +  # Flip the axes to make it easier to read
  labs(title = "Top 20 Most Frequent Words", x = "Word", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend to simplify the plot
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Creating a word cloud of the top 20 most frequent words
wordcloud2(top_words)
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

The result from the bar plot indeed makes sense statistically, as it
accurately reflects the count of each word, and it’s common to see stop
words dominate such frequency distributions in unfiltered text data.
However, this type of result may not be particularly helpful for gaining
insights into the specific content of medical transcriptions.

------------------------------------------------------------------------

## Question 3: Stopwords

- Redo Question 2 but remove stopwords
- Check `stopwords()` library and `stop_words` in `tidytext`
- Use regex to remove numbers as well
- Try customizing your stopwords list to include 3-4 additional words
  that do not appear informative

### What do we see when you remove stopwords and then when you filter further? Does it give us a better idea of what the text is about?

``` r
head(stopwords("english"))
```

    ## [1] "i"      "me"     "my"     "myself" "we"     "our"

``` r
length(stopwords("english"))
```

    ## [1] 174

``` r
head(stop_words)
```

    ## # A tibble: 6 × 2
    ##   word      lexicon
    ##   <chr>     <chr>  
    ## 1 a         SMART  
    ## 2 a's       SMART  
    ## 3 able      SMART  
    ## 4 about     SMART  
    ## 5 above     SMART  
    ## 6 according SMART

``` r
custom_stop_words <- c("left", "right", "else","also", "will","without")  # Add any word you find uninformative

tokens <- mt_samples %>%
  select(transcription) %>%
  unnest_tokens(word, transcription) %>%
  filter(!word %in% stopwords("english"), !word %in% custom_stop_words) %>%
  filter(!grepl("^[0-9]+$", word))  # Use regex to remove words that are purely numbers

word_counts <- tokens %>%
  count(word, sort = TRUE) %>%
  top_n(20, n)  # Select top 20 for visualization

# Bar plot of the top 20 words
ggplot(word_counts, aes(x = reorder(word, n), y = n, fill = word)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Frequent Words in Medical Transcriptions (Filtered)",
       x = "Words",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Word cloud of the top 20 words
wordcloud2(word_counts)
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

After removing stop words and some common words that do not help with
understanding the content, I can gain better insights into what the
transcriptions are about. They are patient-centric and contain
information regarding history, diagnoses, and details regarding
procedures.

------------------------------------------------------------------------

## Question 4: ngrams

Repeat question 2, but this time tokenize into bi-grams. How does the
result change if you look at tri-grams? Note we need to remove stopwords
a little differently. You don’t need to recreate the wordclouds.

``` r
stopwords2 <- stopwords("en")
sw_start <- paste0("^", paste(stopwords2, collapse=" |^"), "$")
sw_end <- paste0("", paste(stopwords2, collapse="$| "), "$")

tokens_bigram <- mt_samples %>%
  select(transcription) %>%
  unnest_tokens(bigram, transcription, token = "ngrams", n = 2) %>%
  filter(!grepl(sw_start, bigram), !grepl(sw_end, bigram))  # Filter out bi-grams with stopwords at the start or end

bigram_counts <- tokens_bigram %>%
  count(bigram, sort = TRUE) %>%
  top_n(20, n)

ggplot(bigram_counts, aes(x = reorder(bigram, n), y = n, fill = bigram)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Bi-grams in Medical Transcriptions",
       x = "Bi-grams",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
tokens_trigram <- mt_samples %>%
  select(transcription) %>%
  unnest_tokens(trigram, transcription, token = "ngrams", n = 3) %>%
  filter(!grepl(sw_start, trigram), !grepl(sw_end, trigram))  # Adjusted regex if needed for tri-grams

trigram_counts <- tokens_trigram %>%
  count(trigram, sort = TRUE) %>%
  top_n(20, n)

# Visualization for tri-grams
ggplot(trigram_counts, aes(x = reorder(trigram, n), y = n, fill = trigram)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Tri-grams in Medical Transcriptions",
       x = "Tri-grams",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

In general,Bi-grams and Tri-grams provide more information regarding the
context. The bi-gram analysis mainly highlights frequent two-word
combinations such as “blood pressure” and “year old,” which, while
informative, mostly focus on common medical terms and patient
descriptors. Tri-grams convey more specific information, showing
sequences of actions and more detailed descriptions of medical and
surgical processes.

------------------------------------------------------------------------

## Question 5: Examining words

Using the results from the bigram, pick a word and count the words that
appear before and after it, and create a plot of the top 20.

``` r
library(stringr)

selected_word <- "patient"

# Filter bi-grams to find occurrences of the selected word
tokens_filtered <- tokens_bigram %>%
  filter(str_detect(bigram, pattern = paste0("\\b", selected_word, "\\b"))) %>%
  mutate(before_word = str_extract(bigram, paste0("^(.*?)\\s", selected_word)),
         after_word = str_extract(bigram, paste0(selected_word, "\\s(.*?)$")))

# Count the words before "patient"
before_counts <- tokens_filtered %>%
  count(before_word, sort = TRUE) %>%
  filter(!is.na(before_word), before_word != "") %>%
  top_n(20, n)

# Count the words after "patient"
after_counts <- tokens_filtered %>%
  count(after_word, sort = TRUE) %>%
  filter(!is.na(after_word), after_word != "") %>%
  top_n(20, n)

combined_counts <- bind_rows(mutate(before_counts, word = before_word, type = "Before"),
                             mutate(after_counts, word = after_word, type = "After")) %>%
  arrange(desc(n))

# Plot the results
ggplot(combined_counts, aes(x = reorder(word, n), y = n, fill = type)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  labs(title = paste("Top Contexts Around the Word '", selected_word, "'", sep = ""),
       x = "Context Words",
       y = "Frequency",
       fill = "Context") +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
This graph illustrates the top context words that frequently appear
before and after the word “patient” in medical transcriptions. The words
shown before “patient” predominantly involve states or actions related
to the patient (like “tolerated” and “denies”), while the words after
primarily describe patient conditions or actions taken (like “will” and
“states”), providing insights into the typical discussions surrounding
patients in medical settings.

------------------------------------------------------------------------

## Question 6: Words by Specialties

Which words are most used in each of the specialties? You can use
`group_by()` and `top_n()` from `dplyr` to have the calculations be done
within each specialty. Remember to remove stopwords. How about the 5
most used words?

``` r
data_processed <- mt_samples %>%
  # Unnest the words from the 'transcription' column
  unnest_tokens(word, transcription) %>%
  # Remove stopwords
  anti_join(data.frame(word = stopwords("en")), by = "word") %>%
  filter(!str_detect(word, "^\\d+$")) %>%  # Regex to remove words that are entirely numeric
  # Count words within each specialty
  count(medical_specialty, word, sort = TRUE) %>%
  # Group by medical specialty and slice the top 5 words
  group_by(medical_specialty) %>%
  top_n(5, n) %>%
  ungroup()
```

## Question 7: Topic Models

See if there are any themes in the data by using a topic model (LDA).

- you first need to create a document term matrix
- then you can try the LDA function in `topicmodels`. Try different k
  values.
- create a facet plot of the results from the LDA (see code from
  lecture)

``` r
# My computer run out of memory if the size is to large
sample_size <- 100  # Adjust based on your memory constraints

sample_indices <- sample(1:nrow(mt_samples), sample_size)
  
transcripts_dtm <- mt_samples[sample_indices,] %>%
  select(transcription) %>%
  unnest_tokens(word, transcription) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "^\\d+$")) %>%  # Regex to remove words that are entirely numeric
  count(document = row_number(), word) %>%
  cast_dtm(document, word, n)

transcripts_dtm_matrix <- as.matrix(transcripts_dtm)

transcripts_lda <- LDA(transcripts_dtm_matrix, k = 6, control = list(seed = 1234))

# Extract the terms for each topic
terms_lda <- tidy(transcripts_lda, matrix = "beta")

# Top terms per topic for visualization
top_terms <- terms_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup()

# Create the facet plot
ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(x = "Terms", y = "Beta", title = "Top Terms in Each Topic from LDA Model") +
  theme_minimal()
```

![](08lab-text-mining_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
