from nltk.tokenize import RegexpTokenizer
from nltk.corpus import stopwords
from langdetect import detect
import numpy as np
import re


def news_clean(documents, terms, news_file):
    """

    :param term:
    :return:
    """

    #add classifier to determine if news article is material
    tokenizer = RegexpTokenizer(r'[a-zA-Z]{3,}')
    stop_words = set(stopwords.words('english'))

    terms = np.unique(terms)
    for t in terms:
        if ' ' in t:
            print(t)
            subterm = t.split(sep=' ' or '&' or '%' or '+')
            for t in subterm:
                stop_words.add(t)
        else:
            stop_words.add(t)

    cleaned_docs = []

    for articles in documents:
        for article in articles:
            language = detect(article)
            if language == 'en':
                article = re.sub(r'http\S+', '', article)
                article = article.lower()
                word_tokens = tokenizer.tokenize(article)
                word_tokens = [w for w in word_tokens if not w in stop_words]
                if len(word_tokens) > 20:
                    print(word_tokens, file=news_file)
                    cleaned_docs.append(word_tokens)

    '''documents = open('titles.txt', 'r') #Not used for now, maybe for later analysis on titles in wordcloud?
    with open('titles_cleaned.txt', 'w') as text_file:
        for article in documents:
            word_tokens = tokenizer.tokenize(article)
            word_tokens = [w for w in word_tokens if not w in stop_words]
            print(word_tokens, file=text_file)'''

    return cleaned_docs #Is not used further, as we use text files