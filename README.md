# Compulsivity Simulations

Computational model of compulsive ritual formation and ERP treatment, implemented as an Advantage Actor-Critic (AAC) reinforcement learning agent.

## Model

**File:** `model/advantage_actor_critic.R`  
**Entry point:** `sim.agent(subject, cfg)`

The agent exists in a 2-state environment (safe / dangerous) defined by an internal threat belief `p_harm`. Triggers accumulate over time and push the agent into a dangerous state. The agent selects among 10 actions using a softmax policy; repeated actions in the dangerous state become ritualized through perseveration costs that increase their relative selection probability. A critic estimates the value of the current state; the actor updates action preferences via the advantage signal.

### Key parameters

| Parameter | Description |
|---|---|
| `eta` | Environmental controllability — scales how much the agent updates action preferences from outcomes |
| `betas` | Self-control matrix (states × actions) — scales action preference updates per action |
| `v_harm` | Valence of the negative outcome |
| `freq_c` | Probability of a negative outcome occurring at each timestep |
| `pr` | Repetition (perseveration) cost per action — higher values make an action less likely to be repeated |
| `f_p` | Forgetting rate for perseveration costs |
| `trigger_frequency` | How often (every N timesteps) an environmental trigger fires |
| `trigger_strength` | Amplitude of each trigger |
| `natural_relax_rate` | Decay rate of the accumulated threat signal `h` |
| `cutoff` | `p_harm` threshold for entering the dangerous state (default 0.05) |
| `treatment_cost` | Multiplier applied to perseveration costs during ERP treatment |
| `exposure_intensity` | Divides `trigger_frequency` during treatment (more frequent triggers = more exposure) |

### Dynamics

- **Threat accumulation:** `h = (h + trigger) * natural_relax_rate * (1 - p[action])`
- **Harm belief:** `p_harm = 2 / (1 + exp(-h)) - 1`  (sigmoid, 0 to 1)
- **Actor update:** `theta[s,a] += eta * beta[s,a] * advantage * gradient`
- **Perseveration:** `cost[s,a] -= pr[s,a]` after each action; decays by `(1 - f_p)` each timestep
- **ERP treatment:** during treatment, `trigger_frequency` is divided by `exposure_intensity` (more frequent exposures) and all perseveration costs in the dangerous state are multiplied by `treatment_cost`

---

## Experiments

### Experiment 1 — Ritual formation

Demonstrates how environmental controllability shapes ritual formation. Two agents are simulated: one with high `eta` (high controllability, stronger learning) and one with low `eta`.

**Run:** `source("Exp1/simulate.R")`  
**Visualize:** `Exp1/visualization/plot_action_proportions.R`  
**Output:** `Exp1/data/df_high_eta.rdata`, `Exp1/data/df_low_eta.rdata`

---

### Experiment 2 — Environmental manipulations

100 agents per condition. Three between-agent conditions vary the environment: baseline, reduced harm valence (`v_harm`), and increased compulsive trigger frequency (`freq_c`). Bayesian regression tests whether manipulation predicts ritualistic action frequency.

**Run:** `source("Exp2/simulate.R")` then `source("Exp2/plot.R")`  
**Visualize:** `Exp2/visualization/compare_repetitions.R`, `Exp2/visualization/plot_regression.R`  
**Output:** `Exp2/data/`

---

### Experiment 3 — Between-action variability

200 agents (two samples of 100). Perseveration cost `pr` and self-control `betas` vary across actions (not subjects) — all subjects share the same action-level parameter vector.

- **Sample 3A:** `pr` varies across actions (`abs(N(0.5, 0.5))`), `betas = 1`. Tests whether actions with higher repetition cost are more likely to become the ritual.
- **Sample 3B:** `pr = 0`, `betas` varies across actions (`Beta(1, 1)`). Tests whether actions with lower self-control are more likely to become the ritual.

**Run:** `source("Exp3/simulate.R")`  
**Visualize:**
- `Exp3/visualization/plot_cost_ritual.R` — ritual frequency as a function of action's `pr`
- `Exp3/visualization/plot_beta_ritual.R` — ritual frequency as a function of action's `beta`
- `Exp3/visualization/3A.R`, `Exp3/visualization/3B.R` — Bayesian regression results

**Output:** `Exp3/data/3A.rdata`, `Exp3/data/3B.rdata`

---

### Experiment 4 — ERP treatment simulation

100 agents simulated across 3 periods: before, during, and after treatment. `pr` varies across actions (shared vector, same as Exp3A). `treatment_cost` and `exposure_intensity` vary across subjects. Agents are classified into three mutually exclusive treatment outcomes (proportions estimated via Bayesian intercept-only models):

| Outcome | Definition |
|---|---|
| Unsuccessful Treatment | Ritual persists during the treatment period |
| Relapse | Treatment suppressed the ritual, but the original ritual returned post-treatment |
| Ritual Substitution | Treatment suppressed the ritual, but a new dominant action (freq > 50%) emerged post-treatment |

**Run:** `source("Exp4/simulate.R")`  
**Visualize:**
- `Exp4/visualization/plot_outcomes.R` — Bayesian estimates (brms) of each outcome proportion with 95% HDI
- `Exp4/visualization/plot_by_period.R` — function for plotting a single agent's P(harm) and action proportions across all 3 periods

**Output:** `Exp4/data/4.rdata`, `Exp4/data/outcome_models.rdata`

---

## Order of execution

```
source("Exp1/simulate.R")

source("Exp2/simulate.R")
source("Exp2/plot.R")

source("Exp3/simulate.R")
source("Exp3/acquire_relapse_results.R")

source("Exp4/simulate.R")
source("Exp4/visualization/plot_outcomes.R")
```

---

## Dependencies

Install manually (no lockfile):

```r
install.packages(c("tidyverse", "brms", "bayestestR", "ggplot2", "gridExtra", "scales", "patchwork"))
```

`brms` requires CmdStan. Install via:

```r
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan()
```

Tested with R 4.3, brms 2.21, cmdstanr 0.8, CmdStan 2.35.
