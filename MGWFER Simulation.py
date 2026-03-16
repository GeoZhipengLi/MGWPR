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


Y = data['y_within'].values.reshape(-1, 1)
Y_raw = data['y'].values.reshape(-1, 1)
Y_std = Y.std()
X = data[["x1_within","x2_within","x3_within","x4_within"]].values
X_scale = (Y.std(axis=0) / X.std(axis = 0)).reshape(1,-1)
X_std = X.std(axis = 0).reshape(1,-1)
X_raw = data[["x1","x2","x3","x4"]].values

Y = (Y - Y.mean()) / Y.std()
X = (X - X.mean(axis = 0)) / X.std(axis = 0)
x = data['i'].values
y = data['j'].values
coords = list(zip(x, y))



time = 3
units = 900


mysel = Sel_BW(coords = coords, y = Y, X_loc = X, time= time,  multi=True, constant=False)
opt_bws = mysel.search()


model = MGWR(coords, Y, X, mysel, time=time, hat_matrix=True, constant=False).fit()
sig = model.filter_tvals(alpha=0.05)
cct = model.CCT
bse = model.bse


Y_mean = data['y_mean'].values.reshape(-1, 1)
X_mean = data[["x1_mean","x2_mean","x3_mean","x4_mean"]].values

bws = opt_bws

ai = model.ai(X_mean, Y_mean, X_scale).ravel()
std_ai = model.std_ai(X_mean, time, bws, coords, X, X_scale, Y_std).ravel()

t_value = ai / std_ai
p_values = 2 * (1 - t.cdf(np.abs(t_value), df= (units * time - units - X.shape[1])))
adjusted_level = 0.05
significance = [ int(p < adjusted_level) for p in p_values]

ais = pd.DataFrame({
            "ai": ai,
            "std_ai": std_ai,
            "t_value": t_value,
            "p_values": p_values,
            "significance": significance
        })



np.savetxt('Output/parameters.csv', model.params, delimiter=',')
np.savetxt('Output/sig.csv', sig, delimiter=',')
ais.to_csv('Output/ai.csv', index = False)

print('OPTIMAL BANDWIDTH:', opt_bws)


