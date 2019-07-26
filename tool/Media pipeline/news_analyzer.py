from gensim.corpora.dictionary import Dictionary
from gensim.models import ldamodel
from gensim.models.coherencemodel import CoherenceModel
from gensim.test.utils import datapath
from nltk.tokenize import RegexpTokenizer
from collections import Counter


class news_analyzer:

    def __init__(self):
        'open analysis file to write new results'
        newsanalysis = open('C:\\Users\Melchior\PycharmProjects\ThesisV3\\venv\\analysis.txt', 'w')
        newsanalysis.close()

    def news_analyze(self):
        news_file = open('news.txt','r')

        read_file = news_file.readlines()
        tokenizer = RegexpTokenizer(r'[a-zA-Z]{3,}', discard_empty=True)
        wordList = []
        for line in read_file:
            wordList.append(tokenizer.tokenize(line))

        dict = Dictionary(wordList)
        dict.filter_extremes(no_below=5, no_above=0.20)
        corpus = [dict.doc2bow(text) for text in wordList]

        max_score = 0
        for i in range(1,25):
            model = ldamodel.LdaModel(corpus=corpus,num_topics=i,id2word=dict,iterations=50)

            coherence_c_v = CoherenceModel(model=model, texts=wordList, dictionary=dict, coherence='c_v',
                                           processes=1)
            model_score = coherence_c_v.get_coherence()
            print(model_score)
            if model_score > max_score:
                max_score = model_score
                temp_file = datapath('model')
                model.save(temp_file)
                number_of_topics = i


        model.load(temp_file)
        print(temp_file)
        for i in range(number_of_topics):
            top_terms = []
            top_probs = []
            for term in model.get_topic_terms(i-1, topn=15):
                if term[0] in dict:
                    # print(d.get(term[0]))
                    top_terms.append(dict.get(term[0]))
                    top_probs.append(dict.get(term[1]))

            analysis_file = open('C:\\Users\Melchior\PycharmProjects\ThesisV3\\venv\\analysis.txt', 'a')
            print(top_terms, file=analysis_file)
            analysis_file.close()

        # Counts the number of dominant topics, given 1 article has 1 dominant topic
        topic_distributions = model.get_document_topics(bow=corpus)
        print(topic_distributions)
        dominant_topics = []
        for distrubution in topic_distributions:
            print(distrubution)
            if len(distrubution) > 1:
                dominant_probability = 0
                for probability in distrubution:
                    if probability[1] > dominant_probability:
                        dominant_probability = probability[1]
                        dominant_topic = probability[0]
                dominant_topics.append(dominant_topic)
            else:
                dominant_topics.append(distrubution[0][0])
        print(Counter(dominant_topics))  # x articles have a dominant topic y