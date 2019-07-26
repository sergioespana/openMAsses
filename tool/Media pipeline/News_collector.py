import requests
from bs4 import BeautifulSoup
from time import sleep
import re
from newsplease import NewsPlease #Thanks to https://github.com/fhamborg/news-please
from nytimesarticle import articleAPI
import praw
import praw.models
import os


#https://dlab.berkeley.edu/blog/scraping-new-york-times-articles-python-tutorial

def news_collect(term, number, content_file, title_file):
    """"
    """

    news_init = news_collector(term=term, number=number)
    news_init.url_scraper()
    reader = news_article_reader(term=term, content_file=content_file, title_file=title_file)
    news_documents = reader.read()

    return news_documents

class news_collector():
    def __init__(self, term, number):
        self.term = term
        self.googleterm = term.replace(' ', '%')
        self.yahooterm = term.replace(' ', '+')
        self.number = number
        self.org_google_url = 'https://www.google.com/search?q={0}&source=lnms&tbm=nws&hl=en'.format(self.googleterm) #gebruikt % sign to connect
        self.google_url = 'https://www.google.com/search?q={0}&source=lnms&tbm=nws&hl=en'.format(self.googleterm)
        self.org_yahoo_url = "https://news.search.yahoo.com/search?p={0}".format(self.yahooterm) #gebruikt + to connect
        self.yahoo_url = "https://news.search.yahoo.com/search?p={0}".format(self.yahooterm)
        url_file = open('news_urls.txt', 'w+')
        url_file.close()

    def url_scraper(self):
        numpage = int(round(int(self.number) / 10))
        if numpage < 1:
            numpage = 1
            print('minimum number of articles to get is 10, default setting (10) is used.')
        for page in range(numpage):
            self.update_url(page)
            self.google_url_scraper()
            self.yahoo_url_scraper()
            self.nyt_url_scraper(self.googleterm, page)
            sleep(2)
        #self.reddit_url_scraper()

    def google_url_scraper(self):
        url_file = open('news_urls.txt','a')
        response = requests.get(self.google_url)
        if response is not None:
            soup = BeautifulSoup(response.text, 'html.parser')
            headline_results = soup.find_all('h3', class_='r')
            for item in headline_results:
                for link in item.find_all('a'):
                    link = link.get('href')
                    link = link.replace('/url?q=','')
                    link = re.sub(r'&sa\S+', '', link)
                    print(link, file=url_file)
        else:
            print('google url blocked, moving to next iteration')
        url_file.close()

    def yahoo_url_scraper(self):
        url_file = open('news_urls.txt', 'a')
        response = requests.get(self.yahoo_url)
        if response is not None:
            soup = BeautifulSoup(response.text, 'html.parser')
            headline_results = soup.find_all('h4', class_='fz-16 lh-20 mah-42 ov-h')
            for item in headline_results:
                for link in item.find_all('a'):
                    link = link.get('href')
                    link = link.replace('/url?q=','')
                    link = re.sub(r'&sa\S+', '', link)
                    print(link, file=url_file)
        else:
            print('yahoo url blocked, moving to next iteraton')
        url_file.close()

    def update_url(self,page):
        self.google_url = self.org_google_url + '&start={0}'.format(page*10)
        self.yahoo_url = self.org_yahoo_url + '&b={0}'.format((page*10)+1)

    def nyt_url_scraper(self, term, page):
        url_file = open('news_urls.txt', 'a')
        try:
            newslinks = search_NYT(term, page)
            for link in newslinks:
                print(link, file=url_file)
        except:
            print('NYT blocked, moving to next iteration')
        url_file.close()

    def reddit_url_scraper(self): #breaks for unknown reason, removed from current build
        url_file = open('news_urls.txt', 'a')
        try:
            reddit = praw.Reddit(client_id='ANother',
                                 client_secret='Key',
                                 user_agent='For me')
        except:
            print('reddit praw broken, skipping reddit')

        try:
            for submission in reddit.subreddit('all').search(query=self.googleterm, sort='relevance',limit=self.number):
                print(submission.url, file=url_file)
        except:
            print('reddit submission unavailable, skipping this iteration')
        url_file.close()

class news_article_reader():
    def __init__(self, content_file, title_file, term=''):
        self.content_file = content_file
        self.title_file = title_file
        self.term = term

    def read(self):
        url_file = open('news_urls.txt','r')
        articles = []
        i = 0

        for line in url_file:
            try:
                article = NewsPlease.from_url(line, timeout=3)
                print(article.title, file=self.title_file)
                article.text = article.text.replace('\n', '')
                print(article.text, file=self.content_file)
                articles.append(article.text)
                sleep(1)
                i += 1
                if i % 10 == 0:
                    if i != 0:
                        print (i, 'articles collected so far')
            except:
                print('failure, next article')

        #clean workspace
        print(i, 'news articles collected on', self.term,'\n')
        url_file.close()
        os.remove('news_urls.txt')

        return articles

def search_NYT(term, page):
    api = articleAPI('61rH3OU4Xztv1bMXWnYZNkvPxIP6twOZ')
    articles = api.search(q='{0}'.format(term), page=page)
    news = parse_articles(articles)
    return news

def parse_articles(articles): # https://dlab.berkeley.edu/blog/scraping-new-york-times-articles-python-tutorial
    '''
    This function takes in a response to the NYT api and parses
    the articles into a list of dictionaries
    '''
    news = []
    for i in articles['response']['docs']:
        dic = {}
        dic['id'] = i['_id']
        dic['headline'] = i['headline']['main'].encode("utf8")
        #dic['desk'] = i['news_desk']
        dic['date'] = i['pub_date'][0:10] # cutting time of day.
        #dic['section'] = i['section_name']
        #if i['snippet'] is not None:
         #   dic['snippet'] = i['snippet'].encode("utf8")
        #dic['source'] = i['source']
        #dic['type'] = i['type_of_material']
        dic['url'] = i['web_url']
        #dic['word_count'] = i['word_count']
        # locations
        #locations = []
        #for x in range(0,len(i['keywords'])):
        #    if 'glocations' in i['keywords'][x]['name']:
        #        locations.append(i['keywords'][x]['value'])
        #dic['locations'] = locations
        # subject
        #subjects = []
        #for x in range(0,len(i['keywords'])):
        #    if 'subject' in i['keywords'][x]['name']:
        #        subjects.append(i['keywords'][x]['value'])
        #dic['subjects'] = subjects
        news.append(dic['url']) #not news, just the urls
    return(news)