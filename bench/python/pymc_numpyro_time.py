"""速度切り分け: 同一モデルを numpyro (JAX/XLA・全 NUTS ループを JIT) で計測。
default PyTensor バックエンド (Python NUTS ループ) との差 = 「Python ループ overhead」か
「AD/コンパイル方式」かの切り分け。config は完全一致:
4 chains・tune=1000・draws=1000・max_treedepth=10・target_accept=0.8。"""
import time, numpy as np, pymc as pm

temp  = np.array([-1,1,-1,1,-1,1,-1,1,-1,1,-1,1], float)
lot   = np.array([0,0,0,0,0,0,1,1,1,1,1,1])
noise = np.array([0.05,-0.03,0.02,-0.04,0.01,-0.02,0.03,-0.01,0.04,-0.05,0.02,-0.03])
y     = 2 + np.where(lot==0,5.0,1.0)*temp + noise
nG,k  = 2,2
X = np.column_stack([np.ones(12), temp])

with pm.Model() as model:
    beta  = pm.Normal("beta", 0.0, 10.0, shape=2)
    sigma = pm.HalfNormal("sigma", 5.0)
    chol, corr, stds = pm.LKJCholeskyCov("Sigma", n=k, eta=2.0,
        sd_dist=pm.HalfNormal.dist(5.0), compute_corr=True)
    z = pm.Normal("z", 0.0, 1.0, shape=(nG,k))
    b = z @ chol.T
    mu = X @ beta + b[lot,0] + b[lot,1]*temp
    pm.Normal("y", mu=mu, sigma=sigma, observed=y)

    t0 = time.perf_counter()
    idata = pm.sample(1000, tune=1000, chains=4,
                      target_accept=0.8,
                      nuts_sampler="numpyro",

                      progressbar=False,
                      idata_kwargs={"log_likelihood": False},
                      compute_convergence_checks=False,
                      random_seed=20260707)
    t1 = time.perf_counter()

print(f"[NUMPYRO] total wall (compile+sample, 4ch x 1000+1000, seq, ta=0.8): {t1-t0:.2f}s")
b0 = idata.posterior["beta"].values[...,0].ravel()
b1 = idata.posterior["beta"].values[...,1].ravel()
sg = idata.posterior["sigma"].values.ravel()
print(f"[NUMPYRO] Intercept mean={b0.mean():.4f} sd={b0.std():.4f}")
print(f"[NUMPYRO] temp      mean={b1.mean():.4f} sd={b1.std():.4f}")
print(f"[NUMPYRO] sigma     mean={sg.mean():.6f} sd={sg.std():.6f}")
try:
    sd = idata.sample_stats
    td = sd["tree_depth"] if "tree_depth" in sd else sd.get("num_steps")
    print(f"[NUMPYRO] mean tree_depth/steps={float(td.mean()):.2f}")
except Exception as e:
    print("[NUMPYRO] (tree stats n/a)", e)
