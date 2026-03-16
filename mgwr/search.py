#Bandwidth optimization methods
from pandas import concat
from tqdm import tqdm
__author__ = "Taylor Oshan"

import numpy as np
from copy import deepcopy
import numpy as np
from sklearn.linear_model import LinearRegression
import multiprocessing
import pandas as pd


def golden_section(a, c, delta, function, tol, max_iter, int_score=False,
                   verbose=False):
    """
    Golden section search routine
    Method: p212, 9.6.4
    Fotheringham, A. S., Brunsdon, C., & Charlton, M. (2002).
    Geographically weighted regression: the analysis of spatially varying relationships.

    Parameters
    ----------
    a               : float
                      initial max search section value
    b               : float
                      initial min search section value
    delta           : float
                      constant used to determine width of search sections
    function        : function
                      obejective function to be evaluated at different section
                      values
    int_score       : boolean
                      False for float score, True for integer score
    tol             : float
                      tolerance used to determine convergence
    max_iter        : integer
                      maximum iterations if no convergence to tolerance

    Returns
    -------
    opt_val         : float
                      optimal value
    opt_score       : kernel
                      optimal score
    output          : list of tuples
                      searching history
    """
    b = a + delta * np.abs(c - a)
    d = c - delta * np.abs(c - a)
    score = 0.0
    diff = 1.0e9
    iters = 0
    output = []
    dict = {}
    while np.abs(diff) > tol and iters < max_iter:
        iters += 1
        if int_score:
            b = np.round(b)
            d = np.round(d)

        if b in dict:
            score_b = dict[b]
        else:
            score_b = function(b)
            dict[b] = score_b
            if verbose:
                print("Bandwidth: ", np.round(b, 2), ", score: ",
                      "{0:.2f}".format(score_b))

        if d in dict:
            score_d = dict[d]
        else:
            score_d = function(d)
            dict[d] = score_d
            if verbose:
                print("Bandwidth: ", np.round(d, 2), ", score: ",
                      "{0:.2f}".format(score_d))

        if score_b <= score_d:
            opt_val = b
            opt_score = score_b
            c = d
            d = b
            b = a + delta * np.abs(c - a)

        else:
            opt_val = d
            opt_score = score_d
            a = b
            b = d
            d = c - delta * np.abs(c - a)

        diff = score_b - score_d
        score = opt_score
        output = list(dict.items())
    return np.round(opt_val, 2), opt_score, output


def equal_interval(l_bound, u_bound, interval, function, int_score=False, verbose=False):
    """
    Interval search, using interval as stepsize

    Parameters
    ----------
    l_bound         : float
                      initial min search section value
    u_bound         : float
                      initial max search section value
    interval        : float
                      constant used to determine width of search sections
    function        : function
                      obejective function to be evaluated at different section
                      values
    int_score       : boolean
                      False for float score, True for integer score

    Returns
    -------
    opt_val         : float
                      optimal value
    opt_score       : kernel
                      optimal score
    output          : list of tuples
                      searching history
    """
    a = l_bound
    c = u_bound
    b = a + interval
    if int_score:
        a = np.round(a, 0)
        c = np.round(c, 0)
        b = np.round(b, 0)

    output = []

    score_a = function(a)
    if verbose:
        print("Bandwidth:", a, ", score:", "{0:.2f}".format(score_a))

    output.append((a, score_a))

    opt_val = a
    opt_score = score_a

    while b < c:
        score_b = function(b)
        if verbose:
            print("Bandwidth:", b, ", score:", "{0:.2f}".format(score_b))
        output.append((b, score_b))

        if score_b < opt_score:
            opt_val = b
            opt_score = score_b
        b = b + interval

    score_c = function(c)
    if verbose:
        print("Bandwidth:", c, ", score:", "{0:.2f}".format(score_c))

    output.append((c, score_c))

    if score_c < opt_score:
        opt_val = c
        opt_score = score_c

    return opt_val, opt_score, output


def multi_bw(init, y, X, XT, n, k, family, tol, max_iter, rss_score, gwr_func,
             bw_func, sel_func, multi_bw_min, multi_bw_max, bws_same_times,
             verbose=False):
    """
    Multiscale GWR bandwidth search procedure using iterative GAM backfitting
    """
    if init is None:
        # init is the initial bandwidth, defaulting to None.
        # Note that X already includes intercepts.
        bw = sel_func(bw_func(y, X))
        #print(bw)
        optim_model = gwr_func(y, X, bw)
    else:
        bw = init
        optim_model = gwr_func(y, X, init)
    bw_gwr = bw
    err = optim_model.resid_response.reshape((-1, 1))
    # parm is a N*K matrix which contains all local coefficients.
    param = optim_model.params

    # XB is the set of smoothing functions. The column k represents the k-th smoothing function, which contains n elements.
    # Note that XB is an N*K matrix.
    XB = np.multiply(param, X)
    if rss_score:
        rss = np.sum((err)**2)
    iters = 0
    scores = []
    delta = 1e6
    # BWs stores all sets of bandwidths obtained during the back-fitting algorithm.
    # So, the theoretical maximum number of elements in BWs is max_iter.
    BWs = []
    bw_stable_counter = 0
    # bws is the temporary list which contains k bandwidths for k variables in an iteration.
    bws = np.empty(k)

    gwr_sel_hist = []

    if XT != []:
        XTshape = XT.shape
        XT = np.delete(XT, 0, axis=1)
        XT.reshape(XTshape[0], XTshape[1] - 1)
        XT_coefficients = []

    try:
        from tqdm.auto import tqdm  #if they have it, let users have a progress bar
    except ImportError:

        def tqdm(x, desc=''):  #otherwise, just passthrough the range
            return x

    for iters in tqdm(range(max_iter), desc='Backfitting'):
        new_XB = np.zeros_like(X)
        params = np.zeros_like(X)

        square_sum = np.sum(err ** 2)
        # print(square_sum)

        for j in range(k):
            # print(j)
            temp_y = XB[:, j].reshape((-1, 1))
            temp_y = temp_y + err
            temp_X = X[:, j].reshape((-1, 1))
            bw_class = bw_func(temp_y, temp_X)

            if bw_stable_counter >= bws_same_times:
                # If in backfitting, all bws not changing in bws_same_times (default 5) iterations
                bw = bws[j]
            else:
                # If the condition is not met, meaning parameters have not converged to the stable values, the back-fitting algorithm continues.
                bw = sel_func(bw_class, multi_bw_min[j], multi_bw_max[j])
                # print(multi_bw_min[j], multi_bw_max[j])
                gwr_sel_hist.append(deepcopy(bw_class.sel_hist))

            optim_model = gwr_func(temp_y, temp_X, bw)
            err = optim_model.resid_response.reshape((-1, 1))
            param = optim_model.params.reshape((-1, ))
            new_XB[:, j] = optim_model.predy.reshape(-1)
            params[:, j] = param
            bws[j] = bw
            print("Bandwidth of Covariate", j + 1, ":",  bw)

        if XT != []:
            # calibrate a linear regression for time fixed effects
            if iters == 0 :
                temp_y_XT = np.zeros(err.shape[0]).reshape((-1, 1))
                # XT_model = LinearRegression()
                # XT_model.fit(XT, err)
                # predictions = XT_model.predict(XT)
                # err = err - predictions
                # temp_y_XT = predictions
                # XT_coefficients.append(deepcopy(XT_model.coef_))
            else:
                XT_model = LinearRegression(fit_intercept=False)
                temp_err =  temp_y_XT + err
                XT_model.fit(XT, temp_err)
                predictions = XT_model.predict(XT)
                err = temp_err - predictions
                temp_y_XT = predictions
                XT_coefficients.append(deepcopy(XT_model.coef_))



        if (iters > 1) and np.all(BWs[-1] == bws):
            bw_stable_counter += 1
        else:
            bw_stable_counter = 0

        num = np.sum((new_XB - XB)**2) / n
        den = np.sum(np.sum(new_XB, axis=1)**2)
        score = (num / den)**0.5
        XB = new_XB

        if rss_score:
            predy = np.sum(np.multiply(params, X), axis=1).reshape((-1, 1))
            new_rss = np.sum((y - predy)**2)
            score = np.abs((new_rss - rss) / new_rss)
            rss = new_rss
        scores.append(deepcopy(score))
        delta = score
        BWs.append(deepcopy(bws))

        if verbose:
            print("Current iteration:", iters, ",SOC:", np.round(score, 7))
            print("Bandwidths:", ', '.join([str(bw) for bw in bws]))

        if delta < tol:
            break

    opt_bws = BWs[-1]

    if XT != []:
        opt_XT_coef = XT_coefficients[-1]
        print(opt_bws)
        print(opt_XT_coef)

    return (opt_bws, np.array(BWs), np.array(scores), params, err, gwr_sel_hist, bw_gwr)

"""
def process_iteration(args):
    iters, max_iter, BWs, bw_stable_counter, bws, new_XB, XB, err, X, k, n, sel_func, multi_bw_min, multi_bw_max, gwr_func, bw_func, gwr_sel_hist, verbose, tol, rss_score, scores = args

    new_XB = np.zeros_like(X)
    params = np.zeros_like(X)

    for j in range(k):
        temp_y = XB[:, j].reshape((-1, 1))
        temp_y = temp_y + err
        temp_X = X[:, j].reshape((-1, 1))
        bw_class = bw_func(temp_y, temp_X)

        if bw_stable_counter >= bws_same_times:
            # If in backfitting, all bws not changing in bws_same_times (default 5) iterations
            bw = bws[j]
        else:
            bw = sel_func(bw_class, multi_bw_min[j], multi_bw_max[j])
            gwr_sel_hist.append(deepcopy(bw_class.sel_hist))

        optim_model = gwr_func(temp_y, temp_X, bw)
        err = optim_model.resid_response.reshape((-1, 1))
        param = optim_model.params.reshape((-1, ))
        new_XB[:, j] = optim_model.predy.reshape(-1)
        params[:, j] = param
        bws[j] = bw

    if (iters > 1) and np.all(BWs[-1] == bws):
        bw_stable_counter += 1
    else:
        bw_stable_counter = 0

    num = np.sum((new_XB - XB)**2) / n
    den = np.sum(np.sum(new_XB, axis=1)**2)
    score = (num / den)**0.5
    XB = new_XB

    if rss_score:
        predy = np.sum(np.multiply(params, X), axis=1).reshape((-1, 1))
        new_rss = np.sum((y - predy)**2)
        score = np.abs((new_rss - rss) / new_rss)
        rss = new_rss
    scores.append(deepcopy(score))
    delta = score
    BWs.append(deepcopy(bws))

    if verbose:
        print("Current iteration:", iters, ",SOC:", np.round(score, 7))
        print("Bandwidths:", ', '.join([str(bw) for bw in bws]))

    return (iters, max_iter, BWs, bw_stable_counter, bws, new_XB, XB, err, X, k, n, sel_func, multi_bw_min, multi_bw_max, gwr_func, bw_func, gwr_sel_hist, verbose, tol, rss_score, scores)
"""

# def multi_bw(init, y, X, n, k, family, tol, max_iter, rss_score, gwr_func,
#              bw_func, sel_func, multi_bw_min, multi_bw_max, bws_same_times,
#              verbose=False):
#     bw_stable_counter = 0
#     scores = []
#     BWs = []
#     gwr_sel_hist = []
#
#     try:
#         from tqdm.auto import tqdm  # if they have it, let users have a progress bar
#     except ImportError:
#
#         def tqdm(x, desc=''):  # otherwise, just passthrough the range
#             return x
#
#     XB = np.multiply(params, X)
#     if rss_score:
#         rss = np.sum((err)**2)
#
#     args = [(iters, max_iter, BWs, bw_stable_counter, bws, new_XB, XB, err, X, k, n, sel_func, multi_bw_min, multi_bw_max,
#              gwr_func, bw_func, gwr_sel_hist, verbose, tol, rss_score, scores) for iters in range(1, max_iter + 1)]
#
#     with multiprocessing.Pool() as pool:
#         results = pool.map(process_iteration, args)
#
#     for result in results:
#         iters, max_iter, BWs, bw_stable_counter, bws, new_XB, XB, err, X, k, n, sel_func, multi_bw_min, multi_bw_max, gwr_func, bw_func, gwr_sel_hist, verbose, tol, rss_score, scores = result
#         if delta < tol:
#             break
#
#     opt_bws = BWs[-1]
#     return (opt_bws, np.array(BWs), np.array(scores), params, err, gwr_sel_hist, bw_gwr)
