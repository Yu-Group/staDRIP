from __future__ import print_function
import torch
import pandas as pd
import math
import numpy as np
from torch import nn, optim
from torch.nn import functional as F
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import cross_val_score

def load_data():
    """
    load rnaseq + protein + drug_response data
    """
    data = {}
    for d in ['rnaseq', 'protein', 'drug_resp']:
        for split in ['', '_valid', '_test']:
            if d == 'rnaseq':
                data[f'{d}{split}'] = pd.read_csv(f"processed_data_for_xvae/{d}_5000{split}.csv", index_col=0)
            else:
                data[f'{d}{split}'] = pd.read_csv(f"processed_data_for_xvae/{d}{split}.csv", index_col=0)
    return data

def train_valid_data(data, topK, bootstrap, seed):
    """
    convert training + valid data to tensors
    """

    K = topK
    n = len(np.array(data['rnaseq']))
    np.random.seed(seed)
    if bootstrap:
        train_idx = np.random.choice(n, n)
    else:
        train_idx = np.arange(n)
    rnaseq_input = torch.tensor(np.array(data['rnaseq'])[train_idx,:K], dtype=torch.float)
    prot_input = torch.tensor(np.array(data['protein'])[train_idx,:], dtype=torch.float)
    rnaseq_input_valid = torch.tensor(np.array(data['rnaseq_valid'])[:,:K], dtype=torch.float)
    prot_input_valid = torch.tensor(np.array(data['protein_valid']), dtype=torch.float)
    rnaseq_input_test = torch.tensor(np.array(data['rnaseq_test'])[:,:K], dtype=torch.float)
    prot_input_test = torch.tensor(np.array(data['protein_test']), dtype=torch.float)
    x = torch.cat((rnaseq_input, prot_input), 1)
    x_valid = torch.cat((rnaseq_input_valid, prot_input_valid), 1)
    x_test = torch.cat((rnaseq_input_test, prot_input_test), 1)

    return x, x_valid, train_idx


def test_data(data, topK):
    """
    convert test data to tensors
    """
    K = topK
    rnaseq_input_test = torch.tensor(np.array(data['rnaseq_test'])[:,:K], dtype=torch.float)
    prot_input_test = torch.tensor(np.array(data['protein_test']), dtype=torch.float)
    x_test = torch.cat((rnaseq_input_test, prot_input_test), 1)
    
    return x_test