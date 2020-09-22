# -*- coding: utf-8 -*-


import os
from tensorflow.keras.layers import SimpleRNN, Activation, Dense, RepeatVector
import numpy as np
#random.seed(1)
import matplotlib.pyplot as plt
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import LSTM, TimeDistributed, Dense
from tensorflow.keras.optimizers import Adam
import pandas as pd
import random
#np.random.seed(1)
#E:/論文/final/all_add season.csv
data = pd.read_table("C:\\Users\\jeff\\Desktop\\老闆的\\data.csv",sep=",")
central = data[data['Central'] == 0]
central = central[central['Chiayi'] == 0]
central = central[central['Hsinchu'] == 0]
central = central.iloc[26016:52032,:]

temp=np.array(central.iloc[:,3:38])
train_X=temp[0:18000,:]
train_Y=temp[168:18168,:]
test_X=temp[18001:25848,:]
test_Y=temp[18169:26016,:]
#train_X = np.reshape(train_X, (train_X.shape[0], 1, 31))
#test_X = np.reshape(test_X, (test_X.shape[0], 1, 31))

X_train = []   #預測點的前 1 週的資料
y_train = []   #預測點
for i in range(1, 17999):  #訓練集總數
    X_train.append(train_X[i-1:i+1,:])
    y_train.append(train_Y[i,:])
X_train, y_train = np.array(X_train), np.array(y_train)


X_test = []   #預測點的前 1 週的資料
y_test = []   #預測點
for i in range(1, 7846):  #訓練集總數
    X_test.append(test_X[i-1:i+1,:])
    y_test.append(test_Y[i,:])
X_test, y_test = np.array(X_test), np.array(y_test)



TIME_STEPS=2
BATCH_SIZE=6000
INPUT_SIZE=35
OUTPUT_SIZE=35
CELL_SIZE=256
LR=0.05
#BATCH_INDEX=0

model=Sequential()
model.add(LSTM(CELL_SIZE,batch_input_shape=(None, TIME_STEPS, INPUT_SIZE),return_sequences=False,stateful=False,))
#model.add(Dense(44),Activation='relu')
model.add(Dense(35))
#model.add(RepeatVector(1))
#model.add(TimeDistributed(Dense(OUTPUT_SIZE, activation='softmax')))
#model.add(TimeDistributed(Dense(OUTPUT_SIZE),input_shape=(1, 256)))
adam=Adam(LR)
model.compile(optimizer=adam,loss='mean_squared_error')
#absolute
from tensorflow.keras.callbacks import ModelCheckpoint
checkpoint=ModelCheckpoint("C:/Users/jeff/Desktop/老闆的/data2.h5",monitor='val_loss',verbose=1,save_best_only=True,model='min')
callbacks_list=[checkpoint]

model.fit(X_train,y_train,validation_data=(X_test,y_test),batch_size=BATCH_SIZE,epochs=150,callbacks=callbacks_list)
from tensorflow.keras.models import load_model
load_model=load_model("C:/Users/jeff/Desktop/老闆的/data2.h5")

result = model.predict(temp[25846:25848,:].reshape(1,2,35))
for t in range(25848, 26015): 
    para = model.predict(temp[t-1:t+1,:].reshape(1,2,35))
    result = np.vstack((result,para))


import matplotlib.pyplot as plt
 
plt.figure(figsize=(10,4))
plt.subplot(1,1,1)
plt.plot(model.history.history['loss'])
plt.plot(model.history.history['val_loss'])
plt.legend(['loss','val_loss'])
plt.ylabel('loss')
plt.xlabel('epoch')
plt.ylim(0, 25)

train_loss = pd.DataFrame(model.history.history['loss'])
val_loss = pd.DataFrame(model.history.history['val_loss'])
loss = pd.concat([train_loss,val_loss],axis=1)
loss.to_csv("C:/Users/jeff/Desktop/老闆的/taipei2/loss_MSE2.csv",index=0,header=0)
#loss.to_csv("C:/Users/jeff/Desktop/老闆的/chiayi/loss_MAE.csv",index=0,header=0)

gg = pd.DataFrame(result)
pd.DataFrame(result).to_csv("C:/Users/jeff/Desktop/老闆的/taipei2/loss_taipei2.csv",index=0,header=0)
#result.to_csv("C:/Users/jeff/Desktop/老闆的/loss_central.csv",sep=',',na_rep='NA',index=0)