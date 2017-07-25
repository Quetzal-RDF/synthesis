# import modules & set up logging
import gensim, logging
import codecs
import re
import os
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)


class MySentences(object):
    def __init__(self, dirname):
        self.dirname = dirname
        self.regex = re.compile(r"[<][^>]+[>]", re.IGNORECASE)
        self.stoplist = set('for a of the and by returns are at string null from to in'.split())

    def __iter__(self):

        for fname in os.listdir(self.dirname):
            if not fname.endswith(".doc"):
                continue
            for line in codecs.open(os.path.join(self.dirname, fname), encoding='utf-8',errors='ignore' ):
                    line = re.sub(self.regex,'', line)
                    line = line.replace(',', ' ')
                    line = line.lower()
                    # yield line.split()
                    yield [word for word in line.split() if word not in self.stoplist]


sentences = MySentences('../embeddingData') # a memory-friendly iterator
model = gensim.models.Word2Vec(sentences, window=5, size=100, min_count=1)
model.save("../embeddingData/functions.word2vec")

