#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 22 20:17:29 2017
Author: Cynthia Zaldivar
Data Science
Final Project, Parts a and b
"""

import time
import pandas as pd
import numpy as np
from collections import Counter

start_time = time.time()

test = pd.read_table("zip.test", header=None, delim_whitespace=True)
train = pd.read_table("zip.train", header=None, delim_whitespace=True)
train = train[0:1000]  # First 1000 rows

test_class = np.array(test[0])
test_data = test.drop(test.columns[[0]], axis=1)
test_data = np.array(test_data)

train_class = train[0].astype(int)
train_class = np.array(train_class)
train_data = train.drop(train.columns[[0]], axis=1)
train_data = np.array(train_data)

'''
Returns most common item in a list
'''


def majority(my_list):
    counter = Counter(my_list)
    value, count = counter.most_common(1)[0]
    return value


predicted_classes = []

'''
Function for K nearest neighbors
'''


def KNN(testbatch, training, k):
    kmins = np.zeros((len(testbatch), k))
    distance = np.zeros((len(testbatch), len(training)))
    for i in range(len(testbatch)):
        for j in range(len(training)):
            distance[i][j] = np.sqrt(np.sum((testbatch[i] - training[j])**2))

        # Get K minimum indices for each i
        kmins[i] = distance[i].argsort()[:k]

    kmins = kmins.astype(int)

    # Majority Vote
    for i in range(len(kmins)):
        classes = train_class[:][kmins[i]]
        classes = classes.tolist()
        predicted_classes.append(majority(classes))

    return predicted_classes

predictions = KNN(test_data, train_data, 25)

'''
Calculate the error of the predictions
'''
errors = 0
for i in range(len(predictions)):
        if predictions[i] - test_class[i] != 0:
            errors += 1

print("KNN using the first 1000 rows, K = 25:")
print("The error rate is ", 100*errors/len(predictions), "%", sep="")
print("Run time: %s seconds" % (time.time() - start_time))

