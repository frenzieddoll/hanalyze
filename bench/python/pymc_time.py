"""速度比較: Haskell (hanalyze) と条件を揃えた PyMC 計測。
4 chains・tune=1000・draws=1000・max_treedepth=10・target_accept=0.8・cores=1 (逐次1コア)。
compile 時間と sample 時間を分離して報告する。"""
import time, numpy as np, pymc as pm

temp  = np.array([-1,1,-1,1,-1,1,-1,1,-1,1,-1,1], float)
lot   = np.array([0,0,0,0,0,0,1,1,1,1,1,1])
noise = np.array([0.05,-0.03,0.02,-0.04,0.01,-0.02,0.03,-0.01,0.04,-0.05,0.02,-0.03])
y     = 2 + np.where(lot==0,5.0,1.0)*temp + noise
nG,k  = 2,2
X = np.column_stack([np.ones(12), temp])

t0 = time.perf_counter()
with pm.Model() as model:
    beta  = pm.Normal("beta", 0.0, 10.0, shape=2)
    sigma = pm.HalfNormal("sigma", 5.0)
    chol, corr, stds = pm.LKJCholeskyCov("Sigma", n=k, eta=2.0,
        sd_dist=pm.HalfNormal.dist(5.0), compute_corr=True)
    z = pm.Normal("z", 0.0, 1.0, shape=(nG,k))
    b = z @ chol.T
    mu = X @ beta + b[lot,0] + b[lot,1]*temp
    pm.Normal("y", mu=mu, sigma=sigma, observed=y)
    t1 = time.perf_counter()
    idata = pm.sample(1000, tune=1000, chains=4, cores=1,
                      target_accept=0.8, max_treedepth=10,
                      random_seed=20260707, progressbar=False,
                      compute_convergence_checks=False)
    t2 = time.perf_counter()

print(f"[PYMC-TIME] model+compile: {t1-t0:.2f}s")
print(f"[PYMC-TIME] sample (4ch x 1000+1000, cores=1, ta=0.8): {t2-t1:.2f}s")
print(f"[PYMC-TIME] total: {t2-t0:.2f}s")
# 収束/tree-depth 状況の目安
sd = idata.sample_stats
print(f"[PYMC-TIME] mean tree_depth={float(sd['tree_depth'].mean()):.2f} "
      f"max={int(sd['tree_depth'].max())}  divergences={int(sd['diverging'].sum())}")
