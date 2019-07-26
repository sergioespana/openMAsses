#! /usr/bin/python

import os, sys
import re
from Data_collector import data_collector
from Data_cleaner import data_cleaner
from multiprocessing import Process, freeze_support
from news_analyzer import news_analyzer


def main():
    if os.path.isfile('search_terms.csv') == False:
       raise IOError('no file with search terms is provided, create a search_terms.csv file')

    if len(sys.argv) < 2:
        raise IOError('no program option is selected, check if collect or analyze is called')

    if sys.argv[1] == 'collect':
        f1 = open('twitter.txt', 'w')
        f2 = open('news.txt', 'w')
        f3 = open('reddit.txt', 'w')

        f1.close()
        f2.close()
        f3.close()
        file = open('search_terms.csv')

        hashtags = []
        news = []
        usernames = []
        reddits = []

        i = 1  # much ugly, need to find more sophisticated method
        for line in file:
            line = re.sub(r'[^a-zA-Z ,_]+', '', line)

            if i == 2:
                hashtags = line.split(sep=',')
            if i == 3:
                usernames = line.split(sep=',')
            if i == 4:
                news = line.split(sep=',')
            if i == 5:
                reddits = line.split(sep=',')
            i += 1

        #then it collects the data
        try:
            spider = data_collector(number=int(sys.argv[2]))
        except:
            spider = data_collector(number=200)
        for hashtag in hashtags:
            print(hashtag)
            spider.collect(hashtag=str(hashtag), number=5000)
        for username in usernames:
            print(username)
            spider.collect(username=str(username), number=5000)
        for new in news:
            print(new)
            spider.collect(searchterm=str(new), number=200)
        for reddit in reddits:
            print(reddit)
            spider.collect(redditsearch=str(reddit), number=10000)
        spider.dump_data()

        janitor = data_cleaner()
        janitor.data_load()
        janitor.clean()
        print('all data is cleaned')

        file.close()
        print('termfile is removed')
        #os.remove('search_terms.csv')
        os.remove('data.txt')
        os.remove('titles.txt')

    if sys.argv[1] == 'analyze':
        print('starting LDA analysis on news')
        'initiate class news analyzer'
        analyzer = news_analyzer()

        'run the news analysis'
        analyzer.news_analyze()

if __name__ == "__main__":
    main()