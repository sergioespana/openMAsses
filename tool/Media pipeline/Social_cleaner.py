from nltk.tokenize import RegexpTokenizer
from nltk.corpus import stopwords
import re
from langdetect import detect


def media_clean(documents, terms, save_file):
    """

    :param type:
    :param term:
    :return:
    """

    tokenizer = RegexpTokenizer(r'[a-zA-Z]{3,}', discard_empty=True)
    stop_words = set(stopwords.words('english'))
    for t in terms:
        stop_words.add(t)

    cleaned_docs = []
    for tweets in documents:
        for line in tweets:
            try:
                if detect(line) == 'en':
                    line = re.sub(r'http\S+','',line)
                    line = re.sub(r'\\x\S+','',line)
                    line = re.sub(r'#\S+','',line)
                    line = re.sub(r'@\S+','',line)
                    line = re.sub(r'b\'','',line)
                    line = re.sub(r'b\"','',line)
                    line = re.sub(r'\\n\S+','',line)
                    line = tokenizer.tokenize(line.lower())
                    line = [w for w in line if not w in stop_words]
                    print(line, file=save_file)
                    cleaned_docs.append(line)
            except:
                print('not a tweet or comment, skip')


    return cleaned_docs