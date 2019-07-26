from Social_cleaner import media_clean
from News_cleaner import news_clean
import pickle
import os


#When running first time, install this
import nltk
nltk.download('punkt')
nltk.download('stopwords')


class data_cleaner(object):
    def __init__(self, news='', tweets='', reddit=''):
        self.cleaned_news = []
        self.cleaned_twitter = []
        self.cleaned_reddit = []
        self.terms = []

    def clean(self):
        self.terms.extend(['said','also','company','cookies','would','com','twitter','market'])

        save_file = open('news.txt', 'a')
        self.cleaned_news = news_clean(self.news, self.terms, save_file)
        print('news is cleaned')
        save_file = open('twitter.txt', 'a')
        self.cleaned_twitter = media_clean(self.tweets, self.terms, save_file)
        print('twitter is cleaned')
        save_file = open('reddit.txt', 'a')
        self.cleaned_reddit = media_clean(self.reddit, self.terms, save_file)
        print('reddit is cleaned')

        save_file.close()

    def data_load(self):
        if os.path.isfile('reddit_dump'):
            comment_collection = open('reddit_dump', 'rb')
            self.reddit = pickle.load(comment_collection)
            comment_collection.close()
            os.remove('reddit_dump')

        if os.path.isfile('news_dump'):
            news_collection = open('news_dump', 'rb')
            self.news = pickle.load(news_collection)
            news_collection.close()
            os.remove('news_dump')

        if os.path.isfile('twitter_dump'):
            twitter_collection = open('twitter_dump', 'rb')
            self.tweets = pickle.load(twitter_collection)
            twitter_collection.close()
            os.remove('twitter_dump')

        if os.path.isfile('term_dump'):
            term_collection = open('term_dump', 'rb')
            self.terms = pickle.load(term_collection)
            term_collection.close()
            os.remove('term_dump')