## ----fig-basic-decision-tree, fig.cap="Basic hypothetical decision tree diagram example, where $r$ is the cost or health value associated with a node and $p$ is the probability of traversing a branch. The subscripts refer to the associated node.", echo=FALSE, , out.width="70%"----
knitr::include_graphics("figs/simple_decision_tree_DARE_annot.png")


## ----pre-run-chunks, echo=FALSE-----------------------------------------------------------------------------------------------------------
# runs these chunks here so can reference them below
tx_names <- c("Drug A", "Drug B")  # treatment names
n_treat <- length(tx_names)        # number of treatments
# define value arrays
c_success <- c_failure <- setNames(rep(NA, times = n_treat), tx_names)
q_success <- q_failure <- setNames(rep(NA, times = n_treat), tx_names)
p_success <- p_failure <- setNames(rep(NA, times = n_treat), tx_names)
# assign QALY values
q_success["Drug A"] <- 30; q_failure["Drug A"] <- 15
q_success["Drug B"] <- 25; q_failure["Drug B"] <- 23
# assign cost values
c_drug <- c("Drug A" = 2000, "Drug B" = 150)
c_success["Drug A"] <- 10000; c_failure["Drug A"] <- 20000
c_success["Drug B"] <- 5000; c_failure["Drug B"] <- 10000
# assign probabilities
p_success["Drug A"] <- 0.7
p_failure["Drug A"] <- 1 - p_success["Drug A"]
p_success["Drug B"] <- 0.95
p_failure["Drug B"] <- 1 - p_success["Drug B"]
c_total <- q_total <- setNames(rep(NA, n_treat), tx_names)
c_incr  <- q_incr  <- setNames(rep(NA, n_treat), tx_names)
# path joint probabilities
p_star <- rbind(p_success, p_failure)

# path total costs 
c_star <- cbind(c_drug + c_success, c_drug + c_failure)

# path total QALYs 
q_star <- cbind(q_success, q_failure)
c_total <- c_drug + (p_success*c_success + p_failure*c_failure)
q_total <- p_success*q_success + p_failure*q_failure
c_incr <- c_total["Drug A"] - c_total["Drug B"]
q_incr <- q_total["Drug A"] - q_total["Drug B"]
k <- 20000  # willingness to pay threshold (GBP)
icer <- c_incr/q_incr
inmb <- q_incr*k - c_incr


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-payoff-tab
#| echo: false
#| warning: false
#| message: false
#| tbl-cap: Payoff table for the basic decision tree example
tab <-
  data.frame(
    c(c_drug["Drug A"] + c_success["Drug A"], c_drug["Drug A"] + c_failure["Drug A"]),
    c(c_drug["Drug B"] + c_success["Drug B"], c_drug["Drug B"] + c_failure["Drug B"]),
    c(q_success["Drug A"], q_failure["Drug A"]),
    c(q_success["Drug B"], q_failure["Drug B"]))
tab <- data.frame(c("Success", "Failure"), tab)
colnames(tab) <- c("", "Drug A", "Drug B", "Drug A", "Drug B")
tinytable::tt(tab, digits=0) |> 
  tinytable::group_tt(
    j = list(
        "Cost (£)"=2:3,
        "QALYs"=4:5
    )
  ) 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-ce-tab
#| echo: false
#| tbl-cap: Cost-effectiveness results for willingness to pay £20,000 per QALY
k <- 20000
tab <-
  data.frame(c(c_total["Drug A"], c_total["Drug B"]),
             c(q_total["Drug A"], q_total["Drug B"]),
             c(c_incr, ""),
             c(round(q_incr, 2), ""),
             c(round(icer, 2), ""),
             c(round(q_incr*k - c_incr, 0), ""))
tab <- data.frame(c("Drug A", "Drug B"), tab)
colnames(tab) <- c("","Cost (£)", "QALYs", "$\\Delta_c$ (£)", "$\\Delta_q$ QALYs", "ICER (£/QALY)", "INMB (£)")
tinytable::tt(tab) #, digits=2)


## ----define-tx----------------------------------------------------------------------------------------------------------------------------
tx_names <- c("Drug A", "Drug B")  # treatment names
n_treat <- length(tx_names)        # number of treatments


## ----define-arrays------------------------------------------------------------------------------------------------------------------------
# define value arrays
c_success <- c_failure <- setNames(rep(NA, times = n_treat), tx_names)
q_success <- q_failure <- setNames(rep(NA, times = n_treat), tx_names)
p_success <- p_failure <- setNames(rep(NA, times = n_treat), tx_names)


## ----print-c-success----------------------------------------------------------------------------------------------------------------------
c_success


## ----assign-qalys-------------------------------------------------------------------------------------------------------------------------
# assign QALY values
q_success["Drug A"] <- 30; q_failure["Drug A"] <- 15
q_success["Drug B"] <- 25; q_failure["Drug B"] <- 23


## ----assign-costs-------------------------------------------------------------------------------------------------------------------------
# assign cost values
c_drug <- c("Drug A" = 2000, "Drug B" = 150)
c_success["Drug A"] <- 10000; c_failure["Drug A"] <- 20000
c_success["Drug B"] <- 5000; c_failure["Drug B"] <- 10000


## ----assign-probs-------------------------------------------------------------------------------------------------------------------------
# assign probabilities
p_success["Drug A"] <- 0.7
p_failure["Drug A"] <- 1 - p_success["Drug A"]
p_success["Drug B"] <- 0.95
p_failure["Drug B"] <- 1 - p_success["Drug B"]


## ----define-ib----------------------------------------------------------------------------------------------------------------------------
c_total <- q_total <- setNames(rep(NA, n_treat), tx_names)
c_incr  <- q_incr  <- setNames(rep(NA, n_treat), tx_names)


## ----calc-vals----------------------------------------------------------------------------------------------------------------------------
# path joint probabilities
p_star <- rbind(p_success, p_failure)

# path total costs 
c_star <- cbind(c_drug + c_success, c_drug + c_failure)

# path total QALYs 
q_star <- cbind(q_success, q_failure)


## -----------------------------------------------------------------------------------------------------------------------------------------
# expected values for Drug A
sum(c_star["Drug A", ]*p_star[, "Drug A"])
sum(q_star["Drug A", ]*p_star[, "Drug A"])

# expected values for Drug B
sum(c_star["Drug B", ]*p_star[, "Drug B"])
sum(q_star["Drug B", ]*p_star[, "Drug B"])


## -----------------------------------------------------------------------------------------------------------------------------------------
diag(c_star %*% p_star)
diag(q_star %*% p_star)


## ----calc-totals--------------------------------------------------------------------------------------------------------------------------
c_total <- c_drug + (p_success*c_success + p_failure*c_failure)
q_total <- p_success*q_success + p_failure*q_failure


## ----calc-ib------------------------------------------------------------------------------------------------------------------------------
c_incr <- c_total["Drug A"] - c_total["Drug B"]
q_incr <- q_total["Drug A"] - q_total["Drug B"]


## ----calc-icer----------------------------------------------------------------------------------------------------------------------------
k <- 20000  # willingness to pay threshold (GBP)
icer <- c_incr/q_incr
inmb <- q_incr*k - c_incr


## ----fig-ceplane, fig.cap='Cost-effectiveness plane with willingness to pay £20,000 per QALY indicated by the solid line. The sustainability area is shaded in grey.', fig.width=7, fig.height=5, out.width=if(knitr::is_latex_output()){"75%"} else {"55%"}, echo=FALSE, fig.align="center", fig.pos="h"----
plot(1, type = "n",
     xlim = c(0,1), ylim = c(0, 12000),
     xlab = "Incremental QALYs",
     ylab = "Incremental cost (GBP)")
     # ylab = "Incremental cost (\u00A3)")
polygon(x = c(0,1,1), y = c(0,0,20000), col = rgb(0.211,0.211,0.211,0.1), border = NA)
points(x = q_incr, y = c_incr, pch = 16, cex = 1.5)
abline(a = 0, b = 20000, lwd = 2)   # Willingness to pay per QALY threshold


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-gamma-params
#| echo: false
#| warning: false
#| message: false
#| tbl-cap: PSA Gamma distribution parameters for costs and QALYs.

# Define the data
data <- data.frame(
  c('QALYs', 'QALYs', 'QALYs', 'QALYs', 'Cost', 'Cost', 'Cost', 'Cost', 'Cost', 'Cost'),
  c('A', 'A', 'B', 'B', 'A', 'A', 'A', 'B', 'B', 'B'),
  c('$q_{succ}^A$', '$q_{fail}^A$', '$q_{succ}^B$', '$q_{fail}^B$', '$c_{drug}^A$', '$c_{succ}^A$', '$c_{fail}^A$', '$c_{drug}^B$', '$c_{succ}^B$', '$c_{fail}^B$'),
  c("30/0.3", "25/0.6", "15/0.36", "23/0.39", "2000/500", "10000/100", "20000/50", "150/666", "5000/200", "10000/100"),
  c(0.3, 0.6, 0.36, 0.39, 500, 100, 50, 666, 200, 100)
)

colnames(data) <- c("Type", "Drug", "Parameter", "$k$", "$\\mu$")

tinytable::tt(
data,
align = c('left', 'left', 'left', 'right', 'right'),
format = list(
    `Type` = "%s",
    `Drug` = "%s",
    `Parameter` = "%s"
    # `a` = "%.2f",
    # `b` = "%.2f"
  )
)


## ----c-treat-psa--------------------------------------------------------------------------------------------------------------------------
n_samples <- 500   # sample size

c_drug_psa <- rbind(
  `Drug A` = rgamma(n = n_samples, shape = c_drug["Drug A"]/500, scale = 500),
  `Drug B` = rgamma(n = n_samples, shape = c_drug["Drug B"]/666, scale = 666)
)


## ----print-c-treat-psa--------------------------------------------------------------------------------------------------------------------
round(c_drug_psa[ , 1:5], digits = 2)


## ----fig-psa-histograms, echo=FALSE, fig.align='center', fig.cap="Histograms of distribution of costs for drug treatments used in PSA. The blue curves are the underlying gamma distributions. The red vertical lines are the mean point estimates."----
par(mfrow = c(1,2))
hist(c_drug_psa["Drug A",], breaks = 30, main = "", freq = FALSE, cex.axis = 0.8, yaxt="n", las=2)
curve(dgamma(x, shape = c_drug["Drug A"]/500, scale = 500), from = 0, to = 10000, add = TRUE, col = "blue")
abline(v = c_drug["Drug A"], col = "red")

hist(c_drug_psa["Drug B",], breaks = 50, main = "", freq = FALSE, xlim = c(0,1500), cex.axis = 0.8, yaxt="n", las=2, ylab = "")
curve(dgamma(x, shape = c_drug["Drug B"]/666, scale = 666), add = TRUE, col = "blue")
abline(v = c_drug["Drug B"], col = "red")


## ----success-failure-def------------------------------------------------------------------------------------------------------------------
c_success_psa <- rbind(
  `Drug A` = rgamma(n_samples, shape = c_success["Drug A"]/100, scale = 100),
  `Drug B` = rgamma(n_samples, shape = c_success["Drug B"]/50, scale = 50)
)

c_failure_psa <- rbind(
  `Drug A` = rgamma(n_samples, shape = c_failure["Drug A"]/200, scale = 200),
  `Drug B` = rgamma(n_samples, shape = c_failure["Drug B"]/100, scale = 100)
)

q_success_psa <- rbind(
  `Drug A` = rgamma(n_samples, shape = q_success["Drug A"]/0.3, scale = 0.3),
  `Drug B` = rgamma(n_samples, shape = q_success["Drug B"]/0.36, scale = 0.36)
)

q_failure_psa <- rbind(
  `Drug A` = rgamma(n_samples, shape = q_failure["Drug A"]/0.6, scale = 0.6),
  `Drug B` = rgamma(n_samples, shape = q_failure["Drug B"]/0.39, scale = 0.39)
)


## ----total-def----------------------------------------------------------------------------------------------------------------------------
c_total_psa <- c_drug_psa + (p_success*c_success_psa + p_failure*c_failure_psa)
q_total_psa <- p_success*q_success_psa + p_failure*q_failure_psa


## ----c_incr_psa---------------------------------------------------------------------------------------------------------------------------
c_incr_psa <- c_total_psa["Drug A", ] - c_total_psa["Drug B", ]
q_incr_psa <- q_total_psa["Drug A", ] - q_total_psa["Drug B", ]


## ----ICER-psa-def-------------------------------------------------------------------------------------------------------------------------
icer_psa <- mean(c_incr_psa)/mean(q_incr_psa)
inmb_psa <- k*mean(q_incr_psa) - mean(c_incr_psa)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------
inmb_psa <- k*q_incr_psa - c_incr_psa
mean_inmb <- mean(inmb_psa)
var_inmb <- var(inmb_psa)
high_inmb <- mean_inmb + 1.96*sqrt(var_inmb)/sqrt(n_samples)
low_inmb <- mean_inmb - 1.96*sqrt(var_inmb)/sqrt(n_samples)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
#| eval: false
# library(BCEA)
# bcea_psa <- bcea(t(q_total_psa), t(c_total_psa),
#                  interventions = c("Drug A", "Drug B"), ref = 1)
# contour2(bcea_psa, title = "", wtp = 20000, point = list(sise = 1),
#          xlim = c(-10,10), ylim = c(-1000, 14000), graph = "ggplot2",
#          xlab = "Incremental QALYs", ylab = "Incremental cost (GBP)")
# ceac.plot(bcea_psa, ref = 1, title = "", graph = "ggplot2",
#           xlab = "Willingness to pay (GBP)")


## ----fig-ceplane-psa, fig.cap='Cost-effectiveness plane with willingness to pay £20,000 per QALY.',echo=FALSE, fig.align="center", fig.pos="h", warning=FALSE, message=FALSE----

library(BCEA)
bcea_psa <- bcea(t(q_total_psa), t(c_total_psa),
                 interventions = c("Drug A", "Drug B"), ref = 1)
contour2(bcea_psa, title = "", wtp = 20000, point = list(sise = 1),
         xlim = c(-10,10), ylim = c(-1000, 14000), graph = "ggplot2",
         xlab = "Incremental QALYs", ylab = "Incremental cost (GBP)")


## ----fig-ceac-plot, fig.cap='Cost-effectiveness acceptability curve.',echo=FALSE, fig.align="center", fig.pos="h", warning=FALSE, message=FALSE----

ceac.plot(bcea_psa, ref = 1, title = "", graph = "ggplot2") +
          xlab("Willingness to pay (GBP)")


## ----tree-list-def------------------------------------------------------------------------------------------------------------------------
tree <-
  list("1" = c(2,3),  # root node
       "2" =  c(),    # terminal node
       "3" =  c())    # terminal node

tree


## ----dat-drugA-def------------------------------------------------------------------------------------------------------------------------
data_cost_recur <- list(
  "Drug A" = data.frame(
    node = 1:3,
    prob = c(NA, p_success["Drug A"], p_failure["Drug A"]),
    # costs
    vals = c(c_drug["Drug A"], c_success["Drug A"], c_failure["Drug A"])
  ),  
  "Drug B" = data.frame(
    node = 1:3,
    prob = c(NA, p_success["Drug B"], p_failure["Drug B"]),
    # costs
    vals = c(c_drug["Drug B"], c_success["Drug B"], c_failure["Drug B"])
  )
)  

data_qaly_recur <- list(
  "Drug A" = data.frame(
    node = 1:3,
    prob = c(NA, p_success["Drug A"], p_failure["Drug A"]),
    # QALYS
    vals = c(0, q_success["Drug A"], q_failure["Drug A"])
  ), 
  "Drug B" = data.frame(
    node = 1:3,
    prob = c(NA, p_success["Drug B"], p_failure["Drug B"]),
    # QALYS
    vals = c(0, q_success["Drug B"], q_failure["Drug B"])
  )
)  


## -----------------------------------------------------------------------------------------------------------------------------------------
data_cost_recur


## ----EV-recursive-def---------------------------------------------------------------------------------------------------------------------
ev_recursive <- function(node,   # current node
                         tree,   # list object
                         dat) {  # dataframe object
  
  # is this a terminal node?
  # if so then end early
  if (is.na(node)) {
    return(0)
  }

  # value at current node
  c_node <- dat$vals[dat$node == node]

  # tree structure starting from current node
  child <- tree[[node]]

  if (is.null(child)) {
    return(c_node)
  } else {

    # probabilities along each branch
    pL <- dat$prob[dat$node == child[1]]  # left branch
    pR <- dat$prob[dat$node == child[2]]  # right branch

    # check for NA values
    if (any(is.na(pL))) pL <- 0
    if (any(is.na(pR))) pR <- 0

    return(c_node +
             pL*ev_recursive(child[1], tree, dat) +
             pR*ev_recursive(child[2], tree, dat))
  }
}


## ----EV-recursive-forloop, eval=TRUE------------------------------------------------------------------------------------------------------
root <- names(tree)[1]  # index of root node

c_total_recur <- q_total_recur <- setNames(
  vector(mode = "numeric", length = 2L), c("Drug A", "Drug B")
)

for (i in 1:2) {
  c_total_recur[i] <- ev_recursive(node = root, tree, data_cost_recur[[i]])
  q_total_recur[i] <- ev_recursive(node = root, tree, data_qaly_recur[[i]])
}


## ----EV-recursive, eval=FALSE-------------------------------------------------------------------------------------------------------------
# c_total_recur <- purrr::map_dbl(
#   data_cost_recur, ~ev_recursive(node = root, tree, .)
# )
# q_total_recur <- purrr::map_dbl(
#   data_qaly_recur, ~ev_recursive(node = root, tree, .)
# )


## ----incr-backward------------------------------------------------------------------------------------------------------------------------
c_incr_recur <- c_total_recur["Drug A"] - c_total_recur["Drug B"]
q_incr_recur <- q_total_recur["Drug A"] - q_total_recur["Drug B"]


## ----icer-recur---------------------------------------------------------------------------------------------------------------------------
icer_recur <- c_incr_recur/q_incr_recur


## ----star-calculation, eval=FALSE, echo=TRUE----------------------------------------------------------------------------------------------
# # find set of terminal nodes
# term_id <- which(unname(sapply(tree, \(x) length(x) == 0)))
# n_term <- length(term_id)
# 
# # initialise empty matrices
# p_star <- matrix(NA, nrow = n_term, ncol = n_treat)
# c_star <- q_star <- matrix(NA, nrow = n_treat, ncol = n_term)
# 
# for (j in 1:n_treat) {
#   c_dat <- data_cost_recur[[j]]
#   q_dat <- data_qaly_recur[[j]]
# 
#   # root node probability
#   c_dat$prob[is.na(c_dat$prob)] <- 1
# 
#   for (i in 1:n_term) {
#     node_current <- term_id[i]
# 
#     p_star[i, j] <- c_dat[node_current, "prob"]
#     c_star[j, i] <- c_dat[node_current, "vals"]  # costs
#     q_star[j, i] <- q_dat[node_current, "vals"]  # QALYs
# 
#     # repeat until root node reached
#     while (node_current != 1) {
#       node_current <- which(unname(sapply(tree, \(x) node_current %in% x)))
# 
#       p_star[i, j] <- p_star[i, j]*c_dat[node_current, "prob"]
#       c_star[j, i] <- c_star[j, i] + c_dat[node_current, "vals"]
#       q_star[j, i] <- q_star[j, i] + q_dat[node_current, "vals"]
#     }
#   }
# }


## -----------------------------------------------------------------------------------------------------------------------------------------
c_star


## ----star-recursive, eval=FALSE, echo=TRUE------------------------------------------------------------------------------------------------
# val_star_recursive <- function(node, tree, dat) {
#   # value at current node
#   v_node <- dat$vals[dat$node == node]
#   parent <- which(unname(sapply(tree, \(x) node %in% x)))
# 
#   # is root node?
#   if (length(parent) == 0) {
#     return(v_node)
#   }
# 
#   # sum current value and recursive values
#   return(v_node + val_star_recursive(parent, tree, dat))
# }
# 
# # index of terminal nodes
# term_id <- which(unname(sapply(tree, \(x) length(x) == 0)))
# 
# # number of terminal nodes
# n_term <- length(term_id)
# 
# p_star <- matrix(NA, nrow = n_term, ncol = n_treat)
# c_star <- q_star2 <- matrix(NA, nrow = n_treat, ncol = n_term)
# 
# # loop through all treatments
# for (j in 1:n_treat) {
#   c_dat <- data_cost_recur[[j]]
#   q_dat <- data_qaly_recur[[j]]
# 
#   for (i in 1:n_term) {
#     node <- term_id[i]
#     c_star[j, i] <- val_star_recursive(node, tree, c_dat)
#     q_star[j, i] <- val_star_recursive(node, tree, q_dat)
#   }
# }


## -----------------------------------------------------------------------------------------------------------------------------------------
c_star


## -----------------------------------------------------------------------------------------------------------------------------------------
all_paths <- function(tree, start = 1, path = NULL) {
  if (is.null(path)) {
    path <- c(start)
  }
  if (length(tree[[as.character(start)]]) == 0) {
    return(list(path))
  }
  paths <- list()

  for (i in tree[[as.character(start)]]) {
    # append to paths
    new_path <- c(path, i)
    paths <- c(paths, all_paths(tree, i, new_path))
  }
  return(paths)
}

all_paths(tree)

