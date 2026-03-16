import numpy as np
import pandas as pd
#from joblib.testing import param
from scipy.stats import alpha
from mgwr.gwr import GWR, MGWR
from mgwr.sel_bw import Sel_BW
import random
import math
import statsmodels.api as sm
from scipy.stats import t


data = pd.read_csv('Input/demeandataGeorgia.csv')


Y = data['bachelor'].values.reshape(-1, 1)
X = data[["lnpop", "black","fb","rural", "poverty", "income"]].values



Y = (Y - Y.mean()) / Y.std()
X = (X - X.mean(axis = 0)) / X.std(axis = 0)
x = data['x'].values
y = data['y'].values
coords = list(zip(x, y))



time = 5
units = 159
X_global = np.kron(np.eye(units), np.ones((time, 1)))


mysel = Sel_BW(coords = coords, y = Y, X_loc = X, time= time,  multi=True, constant=True)
opt_bws = mysel.search()


model = MGWR(coords, Y, X, mysel, time=time, hat_matrix=True, constant=True).fit()
sig = model.filter_tvals(alpha=0.05)



np.savetxt('Output/parameters_pool_G.csv', model.params, delimiter=',')
np.savetxt('Output/sig_pool_G.csv', sig, delimiter=',')

print('OPTIMAL BANDWIDTH:', opt_bws)



