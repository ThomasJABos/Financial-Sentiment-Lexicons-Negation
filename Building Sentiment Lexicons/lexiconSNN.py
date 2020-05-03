# -*- coding: utf-8 -*-
"""
Created on Sun Mar 22 14:39:40 2020

@author: tjabo
"""

import codecs as cd
import numpy as np
import pandas as pd 
try:
    import cPickle as pickle
except:
    import pickle
import collections

def readinfo(fname):
    with cd.open(fname, 'rb') as f:
        word_idx_map = pickle.load(f)
    return word_idx_map

# compute sentiment strenghts
data = np.load("epoch_5Words_current.npy")
ss = data[:,1] - data[:,0]

# get vocabulary
info = readinfo("info.tw")
word_idx_map = info['vocab']

sorted_x = sorted(word_idx_map.items(), key=lambda kv: kv[1]) #create sorted list
sorted_dict = collections.OrderedDict(sorted_x) # create sorted dict
keys = list(sorted_dict.keys())
lexicon = np.vstack((keys[1:len(keys)], ss[1:len(keys)])).T
pd.DataFrame(lexicon).to_csv(
    "C:/Users/tjabo/PycharmProjects/Thesis/lexiconSNNNeg2.csv", 
    header=None, index=None) # save to csv file