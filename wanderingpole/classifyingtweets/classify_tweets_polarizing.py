import os
import re
from tqdm import tqdm
# from p_tqdm import p_map
from simpletransformers.classification import ClassificationModel
from pymongo import MongoClient

# mongo
with open('mongo_uri.txt', 'r') as _file:
    mongo_uri = _file.read()

db = MongoClient(mongo_uri).wanderingpole

# db = MongoClient('mongodb://192.168.176.156').wanderingpole

def clean_text(_string):

    re_url = r"((http|https)\:\/\/)?[a-zA-Z0-9\.\/\?\:@\-_=#]+\.([a-zA-Z]){2,6}([a-zA-Z0-9\.\&\/\?\:@\-_=#])*"
    _string = re.sub(re_url, '', _string)

    re_retweet = r"(RT\s@\w+:)|(&amp;)"
    _string = re.sub(re_retweet, '', _string)
    return _string


def process_docs(list_docs):
    """reads and classifies tweets from a mongodb"""
    # make sure there are no empty tweets
    list_docs = [_doc for _doc in list_docs if 'text' in _doc]
    list_docs = [_doc for _doc in list_docs if len(_doc['text']) > 0]
    # pull the tweets
    text = [_doc['text'] for _doc in list_docs]
    print('cleaning')
    text = [clean_text(tt) for tt in tqdm(text)]
    # make sure there are no empty tweets
    list_docs = [_doc for _doc in list_docs if 'text' in _doc]
    list_docs = [_doc for _doc in list_docs if len(_doc['text']) > 0]
    # classify them
    print('classifying')
    results, model_outputs = model.predict(text)
    # write back into db
    for nn, _doc in enumerate(list_docs):
        db.tweets.update_one(
            {
                '_id': _doc['_id']
            },
            {
                '$set': {
                    'polarizing': int(results[nn]),
                    'polarizing_outputs': [float(mo) for mo in model_outputs[nn]]
                }
            }
        )

if __name__ == "__main__":
    # load the model
    batch_size = 512

    model = ClassificationModel('roberta'
                            , 'D:\Dropbox\Twitter\ModelOutput'
                            , num_labels=2
                            , args={
                                'eval_batch_size':batch_size
                                , 'n_gpu':1
                            })

    count = db.tweets.count_documents(
        {
            'polarizing': {'$exists': False},
            'text': {'$exists': True}
        }
    )

    print(f'{count} documents to process')

    cursor = db.tweets.find(
        {
            'polarizing': {'$exists': False},
            'text': {'$exists': True}
        }
    )

    hold_ee = []
    print('starting process')
    for _doc in tqdm(cursor, total=count):
        # collect the doc
        hold_ee.append(_doc)
        # process batch_size at a time
        if len(hold_ee) >= batch_size*10:
            process_docs(hold_ee)
            hold_ee = []
    # process whatever's left over
    if len(hold_ee) > 0:
        process_docs(hold_ee)


# db.tweets.update_many(
#     {},
#     {
#         '$unset': {
#             'model_outputs': 1,
#             'uncivil': 1
#         }
#     }
# )


# count = db.tweets.count_documents(
#     {
#         'uncivil': {'$exists': False},
#         'full_text': {'$exists': True}
#     }
# )
