import gensim, logging

logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)


def doVector(model):
    funcs = ['or', \
             'and', \
             'not', \
             'concatenate', \
             'quotient', \
             'remainder', \
             'absolute', \
             'ceiling', \
             'floor', \
             'truncate', \
             'sign', \
             'if', \
             'substring', \
             'index', \
             'length', \
             'plus', \
             'minus', \
             'subtract', \
             'divide', \
             'exponent', \
             'factorial', \
             'logarithm', \
             'sqrt', \
             'lower', \
             'overlay', \
             'trim', \
             'upper', \
             'matches', \
             'today',
             'extract' \
             'less', \
             'greater', \
             'equal', \
             'average', \
             'group', \
             'count', \
             'minimum', \
             'maximum', \
             'sum', \
             '<', \
             '>', \
             '=', \
             'replace' \
             ];

    for f in funcs:
        try:
            print(f + str(model.most_similar(positive=[f], topn=10)))
        except:
            pass

model = gensim.models.Word2Vec.load("../embeddingdata/functions.word2vec")
doVector(model)
model = gensim.models.KeyedVectors.load_word2vec_format("../pretrainedmodels/GoogleNews-vectors-negative300.bin", binary=True)
doVector(model)
model = gensim.models.KeyedVectors.load_word2vec_format("../pretrainedmodels/glove.6B.300d.w2vformat.txt")
doVector(model)