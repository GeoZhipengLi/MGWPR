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


data = pd.read_csv('Input/Pscenario.csv')


Y = data['y'].values.reshape(-1, 1)
X = data[["x1","x2","x3","x4"]].values


Y = (Y - Y.mean()) / Y.std()
X = (X - X.mean(axis = 0)) / X.std(axis = 0)
x = data['i'].values
y = data['j'].values
coords = list(zip(x, y))



time = 3
units = 900


mysel = Sel_BW(coords = coords, y = Y, X_loc = X, time= time,  multi=True, constant=True)
opt_bws = mysel.search()


model = MGWR(coords, Y, X, mysel, time=time, hat_matrix=True, constant=True).fit()
sig = model.filter_tvals(alpha=0.05)

bws = opt_bws


np.savetxt('Output/parameters_pool.csv', model.params, delimiter=',')
np.savetxt('Output/sig_pool.csv', sig, delimiter=',')

print('OPTIMAL BANDWIDTH:', opt_bws)



