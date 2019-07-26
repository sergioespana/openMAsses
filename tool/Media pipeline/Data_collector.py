from Social_collector import twitter_collect_hashtag, twitter_collect_username, reddit_collect
from News_collector import news_collect
import pickle


class data_collector(object):
    def __init__(self, number= 10):
        self.news_docs = []
        self.tweets = []
        self.reddit = []
        self.terms = []
        self.title_file = open('titles.txt', 'w')  # IMPLENET THIS PROPERLY
        self.save_file = open('data.txt', 'w')

    def collect(self, hashtag='', username='', searchterm='', redditsearch='', sep_save='off', number=1):
        """
        """
        if type(number) != int:
            try:
                round(int(number))
            except:
                return

        if hashtag != '':
            hashtag = hashtag.replace(' ','')
            if sep_save == 'on':
                self.save_file = open('tweets_hashtag_{0}.txt'.format(username), 'w')
            self.tweets.append(twitter_collect_hashtag(hashtag, number, self.save_file))
            self.terms.append(hashtag)

        elif username != '':
            if sep_save == 'on':
                self.save_file = open('tweets_username_{0}.txt'.format(username), 'w')
            self.tweets.append(twitter_collect_username(username, number, self.save_file))
            self.terms.append(username)

        elif searchterm != '':
            if sep_save == 'on':
                self.save_file = open('news_{0}.txt'.format(searchterm), 'w')
            self.news_docs.append(news_collect(searchterm, number, self.save_file, self.title_file))
            self.terms.append(searchterm)

        elif redditsearch != '':
            redditsearch = redditsearch.replace(' ', '%')
            if sep_save == 'on':
                self.save_file = open('reddit_{0}.txt'.format(redditsearch), 'w')
            self.reddit.append(reddit_collect(redditsearch, number, self.save_file)) #Not really tweets, but reddit comments are similar in nature
            self.terms.append(redditsearch)

    def dump_data(self):
        if self.terms:
            term_file = open('term_dump', 'wb')
            pickle.dump(self.terms, file=term_file)
            term_file.close()
        if self.reddit:
            comment_file = open('reddit_dump', 'wb')
            pickle.dump(self.reddit, file=comment_file)
            comment_file.close()
        if self.news_docs:
            news_file = open('news_dump', 'wb')
            pickle.dump(self.news_docs, file=news_file)
            news_file.close()
        if self.tweets:
            twitter_file = open('twitter_dump', 'wb')
            pickle.dump(self.tweets, file=twitter_file)
            twitter_file.close()
        print('data dumped on', self.terms)

        self.save_file.close()
        self.title_file.close()