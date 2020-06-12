import os
from tqdm import tqdm
from simpletransformers.classification import ClassificationModel
from pymongo import MongoClient

# mongo
with open('mongo_uri.txt', 'r') as _file:
    mongo_uri = _file.read()

db = MongoClient(mongo_uri).wanderingpole


def process_docs(list_docs):
    """reads and classifies tweets from a mongodb"""
    # make sure there are no empty tweets
    list_docs = [_doc for _doc in list_docs if 'text' in _doc]
    list_docs = [_doc for _doc in list_docs if len(_doc['text']) > 0]
    # pull the tweets
    text = [_doc['text'] for _doc in list_docs]
    # classify them
    results, model_outputs = model.predict(text)
    # write back into db
    for nn, _doc in enumerate(list_docs):
        db.tweets.update_one(
            {
                '_id': _doc['_id']
            },
            {
                '$set': {
                    'incivil': int(results[nn]),
                    'model_outputs': [float(mo) for mo in model_outputs[nn]]
                }
            }
        )

if __name__ == "__main__":
    # load the model
    model = ClassificationModel('roberta'
                            , 'wanderingpole/model/'
                            , num_labels=2
                            , args={'eval_batch_size':1024})

    cursor = db.tweets.find(
        {
            'incivil': {'$exists': False}
        }
    )

    hold_ee = []
    for _doc in tqdm(cursor):
        # collect the doc
        hold_ee.append(_doc)
        # process 10k at a time
        if len(hold_ee) >= 1024:
            process_docs(hold_ee)
            hold_ee = []
    # process whatever's left over
    if len(hold_ee) > 0:
        process_docs(hold_ee)
