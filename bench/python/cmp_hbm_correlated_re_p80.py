"""Phase 80.2b: 非中心化相関ランダム傾き RE の PyMC 参照実装。
hanalyze の designHBMProgram (相関傾き branch) と同一データ・同一 prior 構造で
fit し、β(Intercept,temp)/σ/τ/R の事後を突合する。

モデル (両者一致):
  β ~ Normal(0, 10)               (固定効果 Intercept, temp)
  σ ~ HalfNormal(5)               (観測ノイズ)
  τ_c ~ HalfNormal(5), R ~ LKJ(2) (= LKJCholeskyCov(eta=2, sd_dist=HalfNormal(5)))
  z_g^c ~ N(0,1)  (非中心化 raw)
  b_g = chol @ z_g,  chol = diag(τ)·Lcorr
  μ_i = β0 + β1·temp_i + b_{g(i)}^0 + b_{g(i)}^1·temp_i
  y_i ~ Normal(μ_i, σ)
"""
import numpy as np
import pymc as pm
import arviz as az

# --- hanalyze ranSlope テストと同一データ ---
temp  = np.array([-1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1], float)
lot   = np.array([0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1])          # A=0, B=1
noise = np.array([0.05, -0.03, 0.02, -0.04, 0.01, -0.02,
                  0.03, -0.01, 0.04, -0.05, 0.02, -0.03])
slope = np.where(lot == 0, 5.0, 1.0)                            # A: 5, B: 1
y     = 2 + slope * temp + noise                               # 真: Intercept=2, 平均傾き=3

nG, k = 2, 2
X = np.column_stack([np.ones(12), temp])                       # [Intercept, temp]

with pm.Model() as model:
    beta  = pm.Normal("beta", 0.0, 10.0, shape=2)
    sigma = pm.HalfNormal("sigma", 5.0)
    # LKJCholeskyCov = diag(τ)·Lcorr、 τ~HalfNormal(5)・R~LKJ(2) (hanalyze と同一分解)
    chol, corr, stds = pm.LKJCholeskyCov(
        "Sigma", n=k, eta=2.0,
        sd_dist=pm.HalfNormal.dist(5.0), compute_corr=True)
    z = pm.Normal("z", 0.0, 1.0, shape=(nG, k))                # 非中心化 raw latent
    b = pm.Deterministic("b", z @ chol.T)                      # b[g] = chol @ z_g
    mu = X @ beta + b[lot, 0] + b[lot, 1] * temp
    pm.Normal("y", mu=mu, sigma=sigma, observed=y)

    idata = pm.sample(1000, tune=1000, chains=4, target_accept=0.95,
                      random_seed=20260707, progressbar=False)

def summ(name, arr):
    a = np.asarray(arr).ravel()
    print(f"[PYMC] {name:16s} mean={a.mean():.6f} sd={a.std():.6f}")

post = idata.posterior
summ("Intercept",  post["beta"].values[..., 0])
summ("temp",       post["beta"].values[..., 1])
summ("sigma",      post["sigma"].values)
summ("tau0",       post["Sigma_stds"].values[..., 0])
summ("tau1",       post["Sigma_stds"].values[..., 1])
summ("corr[0,1]",  post["Sigma_corr"].values[..., 0, 1])

# 収束診断
print("\n[PYMC] R-hat / ESS (beta, sigma):")
print(az.summary(idata, var_names=["beta", "sigma"],
                 kind="diagnostics")[["r_hat", "ess_bulk"]].to_string())
ndraw = post["sigma"].values.size
print(f"\n[PYMC] ndraws={ndraw}")
