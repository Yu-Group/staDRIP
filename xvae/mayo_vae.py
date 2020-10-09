from __future__ import print_function
import torch
import pandas as pd
import math
from copy import deepcopy
import numpy as np
from torch import nn, optim
from torch.nn import functional as F
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import cross_val_score
import mayo_data
import random
from tqdm import tqdm



class XVAE(nn.Module):
    """
    X-Shaped VAE for data integration
    
    Reference:  Simidjievski, Nikola, et al. "Variational autoencoders for cancer data integration: design principles and computational practice." Frontiers in genetics 10 (2019): 1205.
    """
    def __init__(self, D1_in, D2_in, H11, H12, H2):
        """
        Parameters:
        
            D1_in: int
                input dimension (number of features) of the first dataset
                
            D2_in: int
                input dimension (number of features) of the second dataset
                
            H11: int
                dimension of first hidden layer corresponding to the first dataset
                
            H12: int
                dimension of first hidden layer corresponding to the second dataset
                
            H2: int
                dimension of second (integrated) hidden layer.
        """
        super(XVAE, self).__init__()
        self.D1 = D1_in
        self.D2 = D2_in
        self.fc11 = nn.Linear(D1_in, H11)
        self.fc12 = nn.Linear(D2_in, H12)
        self.H1 = H11 + H12
        self.fc21 = nn.Linear(H11 + H12, H2)
        self.fc22 = nn.Linear(H11 + H12, H2)
        self.fc31 = nn.Linear(H2, H11)
        self.fc32 = nn.Linear(H2, H12)
        self.fc41 = nn.Linear(H11, D1_in)
        self.fc42 = nn.Linear(H12, D2_in)
        self.bn01 = nn.BatchNorm1d(D1_in)
        self.bn02 = nn.BatchNorm1d(D2_in)
        self.bn11 = nn.BatchNorm1d(H11)
        self.bn12 = nn.BatchNorm1d(H12)
        self.bn2 = nn.BatchNorm1d(H2)
        self.dropout = nn.Dropout(0.2)

    def encode(self, x):
        x1 = x[:, :self.D1]
        x2 = x[:, self.D1:]
        h11 = self.dropout(self.bn11(F.elu(self.fc11(x1))))
        h12 = self.dropout(self.bn12(F.elu(self.fc12(x2))))    
        h1 = torch.cat((h11, h12), 1)
        #mu, logvar = self.fc21(h1), self.fc22(h1)
        return self.fc21(h1), self.fc22(h1)

    def reparameterize(self, mu, logvar):
        std = torch.exp(0.5*logvar)
        eps = torch.randn_like(std)
        return mu + eps*std

    def decode(self, z):
        h31 = F.elu(self.fc31(self.dropout(self.bn2(z))))
        h32 = F.elu(self.fc32(self.dropout(self.bn2(z))))
        h41 = F.relu(self.fc41(self.dropout(self.bn11(h31))))
        h42 = self.fc42(self.dropout(self.bn12(h32)))
        h4 = torch.cat((h41, h42), 1)
        return h4
    
    def forward(self, x):
        mu, logvar = self.encode(x)
        z = self.reparameterize(mu, logvar)
        return self.decode(z), mu, logvar
    
def train(epoch, train_loader, model, optimizer):
    """
    train the xvae for one epoch 
    """
    
    np.random.seed(1)
    use_cuda = torch.cuda.is_available()
    device = torch.device("cuda:0" if use_cuda else "cpu")
    model.train()
    train_loss = 0
    for batch_idx, data in enumerate(train_loader):
        data = data.to(device)
        optimizer.zero_grad()
        recon_batch, mu, logvar = model(data)
        loss = loss_function(recon_batch, data, mu, logvar)
        loss.backward()
        train_loss += loss.item()
        optimizer.step()
    if epoch % 500 == 0:
        print('====> Epoch: {} Average loss: {:.4f}'.format(
              epoch, train_loss / len(train_loader.dataset)))
        


        
def loss_function(recon_x, x, mu, logvar):
    
    loss_fn = torch.nn.MSELoss(reduction='sum')
    MSE = loss_fn(recon_x, x)
    
    beta = 1
    KLD = -0.5 * torch.sum(1 + logvar - mu.pow(2) - logvar.exp()) * beta

    return MSE + KLD


def xvae_train(data, topK, H11, H12, H2, seed, epochs, bootstrap, bs_seed):
    """
    train xvae model for data integration, and rf model for downstream classification
    
    Parameters:    
    =========================================================================
    
        data: dict
            input -omics data and drug responses
            
        topK: int
            use the topK rnaseq features with highest variance
            
        H11: int
            dimension of first hidden layer corresponding to the rnaseq data
            
        H12: int
            dimension of first hidden layer corresponding to the protein data
            
        H2: int
            dimension of second (integrated) hidden layer.
            
        seed: int
            manual seed for pytorch
            
        epochs: int
            number of epochs to run
            
        bootstrap: Boolean
            whether or not to bootstrap training data
            
        bs_seed: int
            random seed for bootstrapping
            
    Returns:
    =========================================================================
        
        model: xvae()
            fitted xvae model
            
        rfs: dict
            dictionary of downstream rf classifiers, indexed by drug_id (from 0 to 23)
            
    """
    
    torch.manual_seed(seed)
    x, x_valid, train_idx = mayo_data.train_valid_data(data, topK, bootstrap, bs_seed)
    x_test = mayo_data.test_data(data, topK)
    model = XVAE(D1_in=topK, D2_in=214, H11=H11, H12=H12, H2=H2)
    optimizer = optim.Adam(model.parameters(), lr=1e-3)
    train_loader = torch.utils.data.DataLoader(torch.cat((x, x_valid), 0), 
                                               #x,
                                               batch_size=60,
                                               shuffle=True)

    for epoch in range(1, epochs + 1):
        train(epoch, train_loader, model, optimizer)
        #if epoch % 200 == 0:
    model.eval()
    rfs = {}
    np.random.seed(30)
    with torch.no_grad():
        recon, mu, logvar = model(x)
        train_array = model.reparameterize(mu, logvar).data.numpy()
        for drug_id in range(24):
            y = np.array(data['drug_resp'])[train_idx, drug_id]
            y_notna = np.where(np.isnan(y) == False)[0]
            resp = y[y_notna]
            rnaseq_rf_shallow = RandomForestRegressor(n_estimators=100, random_state=1)
            rnaseq_rf_shallow.fit(train_array[y_notna,:], resp)
            rfs[drug_id] = rnaseq_rf_shallow

    return model, rfs


def xvae_predict(vae_model, rfs, x_test, y_test):
    
    """
    prediction drug response using xvae + rf
    
    Parameters:
    =========================================================================
    
        vae_model: xvae()
            xvae model

        rfs: dict
            dictionary of downstream rf classifiers, indexed by drug_id (from 0 to 23)
            
        x_test: torch.tensor
            input features
            
        y_test: np.array
            actual responses
            
    Returns:
    =========================================================================
    
        y_pred: np.array
            predicted responses
            
        r2: np.array
            r-square of predictions
    
    """

    torch.manual_seed(30)
    np.random.seed(30)
    vae_model.eval()
    recon_test, mu_test, logvar_test = vae_model(x_test)
    y_pred, r2 = np.zeros((len(x_test), 24)), np.zeros(24)
    for drug_id in range(24):
        y_notna_test = np.where(np.isnan(y_test[:,drug_id]) == False)[0]
        pred_test = np.zeros(len(x_test))
        for samples in range(100):
            test_array = vae_model.reparameterize(mu_test, logvar_test).data.numpy()
            pred_test = pred_test + rfs[drug_id].predict(test_array)
        pred_test = pred_test/100
        resp_test = y_test[y_notna_test,drug_id]
        r2_test = 1 - np.mean((pred_test[y_notna_test] - resp_test) ** 2)/np.var(resp_test)
        y_pred[:,drug_id] = pred_test
        r2[drug_id] = r2_test
    return y_pred, r2
