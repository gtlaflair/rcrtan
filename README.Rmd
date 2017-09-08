# rcrtan

[![Build Status](https://travis-ci.org/gtlaflair/rcrtan.svg?branch=master)](https://travis-ci.org/gtlaflair/rcrtan)

rcrtan provides functions for criterion-referenced test analyses as described in Brown & Hudson (2002). Currently it supports the following item and test analyses:

* Item discrimination
     + Item facility (including IF for passing test takers and failing test takers)
     + B-index
     + Agreement index
     + Item Phi
  
* Test dependability
     + Subkoviak's (1988) single administration kappa coefficient
     + Subkoviak's (1988) single administration agreement coefficient
     + Brennan's (1980) short-cut estimate of the generalizability coefficient for absolute decisions

# Installation & Documentation

Get the development version from github:

```R
install.packages('devtools')
devtools::install_github('gtlaflair/rcrtan')
```
# Item analysis functions

This package contains six unique functions for calculating item statistics and one omnibus function for estimating all of the six unique functions. The formulas have been adapted from Brown and Hudson (2002).

* `crt_iteman`: omnibus function that estimates item facility, item facility for masters, item facility for non-masters, the B-index, the agreement statistic, and item phi.

* `if_total`: estimates the item facility values for the entire group of test takers.

<!-- _k_ is the total number of dichotomously score items on a test.  -->

<!-- $$\frac{\sum_{i=1}^k X_{i}}{k}$$ -->

* `if_pass`: estimates the item facility values for the test takers who scored above the cut-score (masters).

<!-- $$if\_pass = \frac{\sum_{i=1}^k X_{i_{masters}}}{k}$$ -->

* `if_fail`: estimates the item facility values for the test takers who scored below the cut-score (non-masters).

<!-- $$if\_fail = \frac{\sum_{i=1}^k X_{i_{non-masters}}}{k}$$ -->

* `b_index`: estimates the B-index values for each item. This is the difference in item facility for masters and non-masters.

<!-- $$b\_index = if\_pass - if\_fail$$ -->

* `agree_stat`: estimates the agreement statistic. 

<!-- _P_~iT~ is the proportion ot test takers who passed the test and answered the item correctly. _Q_~i~ is the propotion of test takers who answered the item incorrectly. _P_~T~ is the proportion of test takers who passed the test. -->

<!-- $$agree\_stat = 2 * P_{iT} + Q_{i} - P_{T}$$ -->

* `item_phi`: estimates item phi. 

<!-- _P_~iT~ is the proportion ot test takers who passed the test and answered the item correctly. _P_~i~ is the proportion of test takers who answered the item correctly. _P_~T~ is the proportion of test takers who passed the test. _Q_~i~ is the proportion of test takers who answered the item incorrectly. _Q_~T~ is the proportion of examinees who failed the test. -->

<!-- $$item\_phi = P_{iT} - P_{i} * P_{T} /  \sqrt{P_{i} * Q_{i} * P_{T} * Q_{T}}$$ -->

```{r}
item_stats <- crt_iteman(data = brown_depend, items = 2:31, raw_cut_score = 21, scale = 'raw')
```

# Test dependability functions

This package contains two functions for estimating test dependability. The formulas have been adapted from Brown and Hudson (2002).

* `subkoviak`: This function uses Subkoviak's (1988) methods for estimating kappa and rho from a single test administration.

<!-- _z_ is the standardized cut-score. _c_ is the raw cut-score. _M_ is the mean of the raw test scores. _S_ is the standard deviation of the raw test scores, and 0.5 is a constant adjustment factor. This function uses the _z_ cut-score and an estimate of coefficient reliability, both rounded to the nearest $\frac{1}{10}$, in order to look up the single-test administration $\kappa$ and $\rho$ estimates. -->

<!-- $$ z = \frac{c - 0.5 - M}{S} $$ -->

```{r}
sub_estimate <- subkoviak(data = brown_depend, items = 2:31, raw_cut_score = 21, look_up = FALSE)

```

* `short_phi`: This function uses Brennan's (1980) formula for a short-cut estimate of generalizability coefficients for absolute decisions. 

<!-- _n_ is the number of test takers. _k_ is the number of items on the test. _M_~p~ is the mean of the total scores divided by the number of items. _S_~p~^2^ is the standard deviation of the total scores divided by the number of items squared. $\alpha$ is the coefficient alpha reliability estimate. -->

<!-- $$ \phi = \frac{\frac{n * S_{p}^2}{n - 1} * \alpha}{\frac{n * S_{p}^2}{n - 1} * \alpha + \frac{M_{p} * (1 - M_{p}) - S_{p}^2}{k-1}}$$ -->

```{r}
phi_estimate <- short_phi(data = brown_depend, items  = 2:31)
```
