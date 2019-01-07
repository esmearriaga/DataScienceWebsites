import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy import linalg

from sklearn.decomposition import PCA, FactorAnalysis
from sklearn.covariance import ShrunkCovariance, LedoitWolf
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import GridSearchCV

data=pd.read_csv('OnlineNewsPopularity.csv')

X = data[2:60]
y = data[61]
