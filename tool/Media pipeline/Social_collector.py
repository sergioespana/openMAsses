import re
import tweepy
import praw
import GetOldTweets.got3 as got #Thanks to: https://github.com/Jefferson-Henrique/GetOldTweets-python

def twitter_collect_hashtag(hashtag, number, save_file):
    """" Function requesting twitter hashtag data,
    returns a text fiile with name: tweets_hashtag_{hashtag}
    """
    consumer_key = 'Request a key'
    consumer_secret = 'From twitter'
    access_key = 'This one is kept safe'
    access_secret = 'And is just for my own use'

    # Create the api endpoint
    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    api = tweepy.API(auth)

    documents = []
    try:
        for tweet in tweepy.Cursor(api.search, lang='en', q='#' + hashtag, rpp=100).items(int(number)):
            try:
                if tweet not in documents:
                    print(str(tweet.text.encode('utf-8')), file=save_file)
                    documents.append(str(tweet.text.encode('utf-8')))
            except:
                print('odd tweet, skipping one')
            sleep(1)
    except:
        print('failure error in tweet collector, pushing back empty documents')

    print(len(documents), 'tweets were collected on hashtag', hashtag, '\n')

    return documents

def twitter_collect_username(username, number, save_file):
    """

    :param username:
    :param number:
    :param save_file:
    :return:
    """

    documents = []
    try:
        tweetCriteria = got.manager.TweetCriteria().setUsername(username).setMaxTweets(int(number))
        tweets = got.manager.TweetManager.getTweets(tweetCriteria)
    except:
        print('broken manager, returning empty documents')
        return documents

    i = 0
    for tweet in tweets:
        try:
            print(tweet.text.encode('utf-8'), file=save_file)
            documents.append(tweet.text)
            i += 1
        except:
            print('odd tweet, skipping one one')

    print(i, 'tweets were collected on username', username, '\n')

    return documents

def reddit_collect(term, number, save_file):
    documents = []

    try:
        reddit = praw.Reddit(client_id='Same',
                             client_secret='Story',
                             user_agent='As above')
    except:
        print('reddit connection broken, returning empty documents')
        return documents

    for submission in reddit.subreddit('all').search(query=term, sort='relevance', limit=int(number)):
        submission.comments.replace_more(limit=None)
        for comment in submission.comments.list():
            if len(documents) > int(number):
                break
            try:
                print(comment.body.encode('utf-8'), file=save_file)
                documents.append(comment.body)
            except:
                print('odd reddit comment, skipping this one')

    term = term.replace(' ', '%')
    print(len(documents), 'reddit comments are downloaded on', term, '\n')
    return documents