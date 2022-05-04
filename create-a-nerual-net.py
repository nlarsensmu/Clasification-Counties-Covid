#!/usr/bin/env python
# coding: utf-8

# In[372]:


import os
import glob
import pandas as pd
from tqdm.auto import tqdm
import time
import numpy as np
import numpy.ma as ma

import tensorflow as tf
from tensorflow import keras
from tensorflow.keras import layers

from keras.models import Sequential
from keras import layers
from keras.optimizers import RMSprop
from tensorflow.keras.optimizers import Adam
from sklearn.metrics import accuracy_score
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from tensorflow.keras.layers import concatenate
from tensorflow.keras.layers import Conv2D, MaxPooling2D, Flatten
from sklearn.model_selection import StratifiedKFold

from tensorflow.keras.layers import Dense, Activation, Input, GRU, SimpleRNN, Dropout
from tensorflow.keras.models import Model
from tensorflow.keras.initializers import GlorotUniform
from numpy.random import seed
seed(1)
tf.random.set_seed(2)


# In[2]:


train_path = 'C:\\Users\\Steven\\Desktop\\New folder\\train.csv'
test_path = 'C:\\Users\\Steven\\Desktop\\New folder\\validation.csv'
train = pd.read_csv(train_path)
#train.drop([0], axis=1)
test = pd.read_csv(test_path)

train['num_risk_deaths'] =  train['risk_deaths'].apply(lambda x: 0 if x == 'low' else 1)
train = train.drop(['deaths','confirmed_cases','risk_cases', 'state', 'county_name',
           'confirmed_cases_per1000','deaths_per1000', 'risk_deaths', 'county_fips_code'], axis=1)


test['num_risk_deaths'] =  test['risk_deaths'].apply(lambda x: 0 if x == 'low' else 1)
test = test.drop(['deaths','confirmed_cases','risk_cases', 'state', 'county_name',
           'confirmed_cases_per1000','deaths_per1000', 'risk_deaths', 'county_fips_code'], axis=1)


# In[3]:


np_train = train.to_numpy()
np_test = test.to_numpy()

X_train = np_train[:,:-1]
y_train = np_train[:, -1]
X_test = np_test[:,:-1]
y_test = np_test[:, -1]


# In[4]:


X_train.shape[1]


# In[357]:


input_all = Input(shape=(X_train.shape[1],))
x = Dense(units=32, activation='relu',kernel_initializer='random_normal')(input_all)
x = Dense(units=64, activation='relu',kernel_initializer='random_normal')(x)
x = Dense(units=128, activation='relu',kernel_initializer='random_normal')(x)
x = Dense(units=256, activation='relu',kernel_initializer='random_normal')(x)
x = Dense(units=128, activation='relu',kernel_initializer='random_normal')(x)
x = Dense(units=64, activation='relu',kernel_initializer='random_normal')(x)
x = Dense(units=32, activation='relu',kernel_initializer='random_normal')(x)
prediction = Dense(units=1, activation='sigmoid')(x)

model = Model(inputs=input_all, outputs=prediction)


# In[358]:


from tensorflow.keras.optimizers import Adam
model.compile(optimizer=Adam(learning_rate=0.0001),
              loss='bce',
              metrics=['accuracy'])
model.summary()


# In[363]:


get_ipython().run_cell_magic('time', '', "model = None\n\ninput_all = Input(shape=(X_train.shape[1],))\nx = Dense(units=32, activation='relu',\n          kernel_initializer='glorot_uniform')(input_all)\nx = Dense(units=64, activation='relu',kernel_initializer='glorot_uniform')(x)\nx = Dense(units=128, activation='relu',kernel_initializer='glorot_uniform')(x)\nx = Dense(units=256, activation='relu',kernel_initializer='glorot_uniform')(x)\nprediction = Dense(units=1, activation='sigmoid')(x)\n\nmodel = Model(inputs=input_all, outputs=prediction)\n\nmodel.compile(optimizer=Adam(learning_rate=0.00001,\n                            beta_1=0.8,\n                            beta_2=0.99,\n                            epsilon=1e-06,),\n              loss='bce',\n              metrics=['accuracy'])\n\n\nclass_weight = [5,  50]\n\nhistory = model.fit(X_train, y_train, validation_data=(X_test, y_test),\n                    epochs=120,  batch_size=50, \n                    class_weight=class_weight, \n                    verbose=False)\nyhat = model.predict(X_train)\nprint(min(yhat), max(yhat), np.mean(yhat))\nyhat = model.predict(X_test)\nprint(min(yhat), max(yhat), np.mean(yhat))")


# In[364]:


def plot_history(history):
    loss = history.history['loss']
    val_loss = history.history['val_loss']

    epochs = range(1, len(loss) + 1)

    plt.figure(figsize=(12.8,4.8))
    plt.plot(epochs, loss, 'r', label='Training loss')
    plt.plot(epochs, val_loss, 'b', label='Validation loss')
    plt.title('Training and Validation Loss')
    plt.legend()
    plt.show()
    
def plot_acc(history):
    acc = history.history['accuracy']
    val_acc = history.history['val_accuracy']

    epochs = range(1, len(acc) + 1)

    plt.figure(figsize=(12.8,4.8))
    plt.plot(epochs, acc, 'r', label='Training accuracy')
    plt.plot(epochs, val_acc, 'b', label='Validation accuracy')
    plt.title('Training and Validation accuracy')
    plt.legend()
    plt.show()


# In[365]:


import matplotlib.pyplot as plt
loss = history.history['loss']
val_loss = history.history['val_loss']

epochs = range(1, len(loss) + 1)

plot_history(history)

yhat = model.predict(X_train).round()

print('Train:', accuracy_score(y_train, yhat)*100)

yhat = model.predict(X_test).round()
print('Validation:', accuracy_score(y_test, yhat)*100)


# In[366]:


from sklearn.metrics import recall_score
yhat = (model.predict(X_test)).round()
print(recall_score(y_test, yhat, pos_label=0))
print(recall_score(y_test, yhat, pos_label=1))

yhat = (model.predict(X_test) + 0.30).round()
print(recall_score(y_test, yhat, pos_label=0))
print(recall_score(y_test, yhat, pos_label=1))


# In[414]:


get_ipython().run_cell_magic('time', '', "model = None\nseed(1)\ntf.random.set_seed(2)\nfrom tensorflow.python.keras import backend as K\nsession_conf = tf.compat.v1.ConfigProto(intra_op_parallelism_threads=1, inter_op_parallelism_threads=1)\nsess = tf.compat.v1.Session(graph=tf.compat.v1.get_default_graph(), config=session_conf)\nK.set_session(sess)\n\ninput_all = Input(shape=(X_train.shape[1],))\nx = Dense(units=32, activation='relu',\n          kernel_initializer='glorot_uniform')(input_all)\nx = Dense(units=64, activation='relu',kernel_initializer='glorot_uniform')(x)\nx = Dense(units=128, activation='relu',kernel_initializer='glorot_uniform')(x)\nx = Dense(units=256, activation='relu',kernel_initializer='glorot_uniform')(x)\nprediction = Dense(units=1, activation='sigmoid')(x)\n\nmodel = Model(inputs=input_all, outputs=prediction)\n\nmodel.compile(optimizer=Adam(learning_rate=0.0001,\n                            beta_1=0.8,\n                            beta_2=0.99,\n                            epsilon=1e-07,),\n              loss='bce',\n              metrics=['accuracy'])\n\n\nclass_weight = [15,  50]\n\nhistory = model.fit(X_train, y_train, validation_data=(X_test, y_test),\n                    epochs=120,  batch_size=50, \n                    class_weight=class_weight, \n                    verbose=False)\nyhat = model.predict(X_train)\nprint(min(yhat), max(yhat), np.mean(yhat))\nyhat = model.predict(X_test)\nprint(min(yhat), max(yhat), np.mean(yhat))\n\nloss = history.history['loss']\nval_loss = history.history['val_loss']\n\nepochs = range(1, len(loss) + 1)\n\nplot_history(history)\n\nyhat = model.predict(X_train).round()\n\nprint('Train:', accuracy_score(y_train, yhat)*100)\n\nyhat = model.predict(X_test).round()\nprint('Validation:', accuracy_score(y_test, yhat)*100)")


# In[420]:


from sklearn.metrics import recall_score
yhat = (model.predict(X_test)).round()
print(recall_score(y_test, yhat, pos_label=0))
print(recall_score(y_test, yhat, pos_label=1))

yhat = (model.predict(X_test) + 0.075).round()
print(recall_score(y_test, yhat, pos_label=0))
print(recall_score(y_test, yhat, pos_label=1))


# In[416]:


yhat = (model.predict(X_test))
np.mean(yhat)


# In[ ]:




