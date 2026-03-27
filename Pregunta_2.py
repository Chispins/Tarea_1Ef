import numpy as np
import pandas as pd

data = pd.read_excel('bbdd_Tarea1.xlsx')

n = data.shape[0]

x1 = data.iloc[:,1]
x2 = data.iloc[:,2]

x1 = np.array(x1)
x2 = np.array(x2)

x1 = x1.reshape((n, 1))
x2 = x2.reshape((n, 1))


X = np.concatenate([np.ones((n, 1)), x1, x2], axis=1)
X_sin_constante = np.concatenate((x1, x2), axis=1)

B0, B1, B2 = 10, 0.4, 0.6
Betas = np.array((B0, B1, B2))
Betas = Betas.reshape((3, 1))

Betas_sin_constante = np.array((B1, B2))
Betas_sin_constante = Betas_sin_constante.reshape((2, 1))

error = np.random.normal(0, 0.0625, size=(n, 1))
y_real = X @ Betas
y_observado = X @ Betas + error  # Y con constante

Beta_gorro = np.linalg.inv(X.T @ X)@X.T @ y_observado
Beta_gorro_sc = np.linalg.inv(X_sin_constante.T @ X_sin_constante)@X_sin_constante.T @ y_observado

beta_gorro_MC = []
beta_gorro_sc_MC = []
for i in range(1000):
    itero = y_real + np.random.normal(0, 0.0625, size=(n, 1))
    itero_beta_gorro = np.linalg.inv(X.T @ X) @ X.T @ itero
    itero_beta_gorro_sc = np.linalg.inv(X_sin_constante.T @ X_sin_constante) @X_sin_constante.T @ itero

    beta_gorro_MC.append(itero_beta_gorro)
    beta_gorro_sc_MC.append(itero_beta_gorro_sc)
beta_gorro_MC_np = np.array(beta_gorro_MC)
beta_gorro_sc_MC_np = np.array(beta_gorro_sc_MC)

Betas_MC_sc = [beta_gorro_sc_MC_np[:, 0].mean(),beta_gorro_sc_MC_np[:, 1].mean()]
Betas_MC = [beta_gorro_MC_np[:, 0].mean(), beta_gorro_MC_np[:, 1].mean(), beta_gorro_MC_np[:, 2].mean()]