## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Simulate 4 numbers from 1 to 20, without replacement
sample(x=1:20,size=4,replace=FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-exdistr
#| fig-cap: 'A graphical example of a probability distribution for a given set of value for the parameters $\boldsymbol\theta$'
#| echo: false
#| dev: "tikz"

p1=2; p2=1; p3=.6
y=seq(-4,4,.01)
f=p1*(p2/sqrt(p3))*exp((y-p2)^2/log(p3))
tibble(y,f) %>% ggplot(aes(y,f)) + geom_line() + xlab("$y$") + ylab("Probability distribution") +
  theme_bw()
#plot(y,f,t="l",ylab="Probability distribution",xlab="",axes=F)#,cex.lab=1.8
#axis(1); axis(2)
#mtext(side=1,line=2.5,"$y$",cex=1.3)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-exdistr0
#| fig.cap: 'A graphical example of a probability distribution for a given set of value for the parameters $\\boldsymbol\\theta$'
#| echo: false
#| fig-width: 4
#| fig-height: 2
#| include: false
p1=2; p2=1; p3=.6
y=seq(-4,4,.01)
f=p1*(p2/sqrt(p3))*exp((y-p2)^2/log(p3))
# Using 'base' plotting
plot(y,f,t="l",ylab="Probability distribution",xlab="y",axes=F)
axis(1); axis(2)

# Or the more modern 'ggplot'/'tidyverse' approach
tibble(y,f) %>% ggplot(aes(y,f)) + geom_line()+
  xlab("$y$") + ylab("Probability distribution")


## ----allcombs-----------------------------------------------------------------------------------------------------------------------------
#| echo: true
combn(5,3)


## ----binomex1-----------------------------------------------------------------------------------------------------------------------------
#| echo: true
dbinom(x=12,size=33,prob=0.25)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-binomials
#| fig-cap: 'Histogram of a random sample of 10000 observations from a Binomial($\theta=0.25,n=33)$ distribution'
#| echo: true
#| fig-width: 5.5
#| fig-height: 3.5
tibble(r=rbinom(n=10000,size=33,prob=0.25)) %>% 
  ggplot(aes(r)) + geom_histogram(
    breaks=seq(0,33),color="black",fill="grey"
  ) + theme_bw() + xlab("Number of successes") + ylab("Absolute frequency") + 
  theme(axis.text=element_text(size=8)) +
  scale_x_continuous(breaks = seq(0, 33, 1), lim = c(0, 33))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-binomials2
#| fig-cap: 'Binomial distributions for the number of successes in $n = 5, 20, 100$ Bernoulli trials, each with probability $\theta$ = 0.3 of success'
#| fig-subcap: 
#|   - $n=5$
#|   - $n=20$
#|   - $n=100$
#| fig-width: 5
#| fig-height: 7
#| layout-ncol: 3
#| echo: false
#| dev: "tikz"
theta = 0.3
n = c(5,20,100)
step=c(1,4,5)
t=list()
for (i in 1:3) {
  t[[i]]=tibble(x=seq(0,n[i]),r=dbinom(x,n[i],theta)) %>% ggplot(aes(x,r)) +
    geom_bar(stat="identity") + theme_bw() + xlab("$r$") + ylab("") +
      scale_x_continuous(breaks=seq(0,n[i],step[i]))
}
t[[1]]
t[[2]]
t[[3]]


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-poissons
#| fig-cap: 'Poisson distributions representing the number of events occurring in time $T = 5, 20, 100$, when the rate at which an event occurs in a unit of time is $\lambda=0.3$: the Poisson distributions therefore correspond to $\theta$ = 1.5, 6  and 30.'
#| fig-subcap: 
#|   - $\mbox{Rate}=0.3, T=5$
#|   - $\mbox{Rate}=0.3, T=20$
#|   - $\mbox{Rate}=0.3, T=100$
#| fig-width: 5
#| fig-height: 7
#| echo: false
#| layout-ncol: 3
#| dev: "tikz"
p=0.3
step=c(1,4,5)
n=c(5,20,100)
xmax<-c(8,20,100)
t=list()
for (i in 1:3) {
  t[[i]]=tibble(x=seq(0,n[i]+2),y=dpois(x,n[i]*p)) %>% ggplot(aes(x,y)) +
    geom_bar(stat="identity") + theme_bw() + xlab("$y$") + ylab("") +
      scale_x_continuous(breaks=seq(0,n[i],step[i]))
}
t[[1]]
t[[2]]
t[[3]]


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-superhist
#| fig-cap: 'Histogram for two samples of 1000000 observations from a Binomial(0.05,200) (in red) and a Poisson(7.5=0.05 $\times$ 200) (in blue). When the two histograms overlap, the resulting colour is shaded to purple'
#| fig-width: 5
#| fig-height: 3
#| echo: false
#| message: false
n=200
prob=.05
lambda=n*prob
nsim=1000000
y1=rbinom(nsim,size=n,prob=prob)
y2=rpois(nsim,lambda=lambda)
# hist(y1, col=rgb(1,0,0,0.5),main="",xlab="",freq=FALSE,ylab="Relative frequency")
# hist(y2, col=rgb(0,0,1,0.5), add=T,freq=FALSE)
# legend("topright",c("Binomial","Poisson"),bty="n",col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)),lty=1,lwd=10)
dat=tibble(y=y1,dist="Binomial") %>% bind_rows(tibble(y=y2,dist="Poisson")) 
ggplot(data=dat,aes(y,fill=dist))+geom_histogram(color="black",aes(y=after_stat(density)),alpha = 0.1,position="identity")+
  theme_bw()+xlab("")+ylab("Relative frequency") + theme(legend.position=c(.75,.8),legend.background = element_rect(fill='transparent'),legend.title = element_blank()) + 
#  scale_fill_manual(values=c("#FF0000","#0000FF"))
  scale_fill_manual(values=c("gray2","gray98"))


## ----poissonprobs-------------------------------------------------------------------------------------------------------------------------
#| echo: true
dpois(x=8,lambda=2)


## ----poissonsims,cache=TRUE---------------------------------------------------------------------------------------------------------------
#| echo: true
# Set the pseudo-random number generator (for replicability)
set.seed(10230)
# Vector of number of simulations
n=c(100,1000,10000,100000,1000000,10000000)
# Initialise (an empty) vector of (numeric) probabilities 
prob=numeric()
# Simulates n[i] observations from a Poisson(lambda=2) and then 
# counts the proportion of simulations with value 8
for (i in 1:length(n)) {
   y=rpois(n=n[i],lambda=2)
   prob[i]=sum(y==8)/n[i]
}
# Formats and shows the output
nlab=format(n,scientific=FALSE,big.mark=",")
names(prob)=nlab
prob


## ----normhistprep,echo=FALSE,results="hide"-----------------------------------------------------------------------------------------------
#| fig-width: 7
#| fig-height: 5
y=rnorm(1000000)
h1=hist(y,20,plot=FALSE)
h2=hist(y,50,plot=FALSE)
h3=hist(y,100,plot=FALSE)
h4=hist(y,250,plot=FALSE)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-normhist
#| fig-cap: 'Histograms for a sample from a Normal distribution with $\mu=0$ and $\sigma=1$, with superimposed (in blue) the density of a Normal(0,1).' 
#| fig-subcap: 
#|   - "Bar width: 0.50 "
#|   - "Bar width: 0.20"
#|   - "Bar width: 0.10"
#|   - "Bar width: 0.05"
#| layout-ncol: 2
#| layout-nrow: 2
#| echo: false
#| dev: "tikz"
#| message: false
#| validate-yaml: false
#| fig.pos: "H"

y %>% tibble(y=.) %>% ggplot(aes(y))+geom_histogram(aes(y=after_stat(density)),breaks=h1$breaks,col="black",fill="grey") + theme_bw() + 
  stat_function(
    fun = dnorm, 
    args = list(mean=0,sd=1), 
    lwd = 2, 
    col = 'darkblue'
  ) + xlab("$y$")

y %>% tibble(y=.) %>% ggplot(aes(y))+geom_histogram(aes(y=after_stat(density)),breaks=h2$breaks,col="black",fill="grey") + theme_bw() + 
  stat_function(
    fun = dnorm, 
    args = list(mean=0,sd=1), 
    lwd = 2, 
    col = 'darkblue'
  ) + xlab("$y$")

y %>% tibble(y=.) %>% ggplot(aes(y))+geom_histogram(aes(y=after_stat(density)),breaks=h3$breaks,col="black",fill="grey") + theme_bw() + 
  stat_function(
    fun = dnorm, 
    args = list(mean=0,sd=1), 
    lwd = 2, 
    col = 'darkblue'
  ) + xlab("$y$")

y %>% tibble(y=.) %>% ggplot(aes(y))+geom_histogram(aes(y=after_stat(density)),breaks=h4$breaks,col="black",fill="grey") + theme_bw() + 
  stat_function(
    fun = dnorm, 
    args = list(mean=0,sd=1), 
    lwd = 2, 
    col = 'darkblue'
  ) + xlab("$y$")


## ----tailarea-----------------------------------------------------------------------------------------------------------------------------
#| echo: true
qnorm(p=0.975,mean=0,sd=1)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-tailareagraph
#| fig-cap: 'Tail area for a Normal(0,1) distribution'
#| fig-subcap: 
#|   - Density
#|   - Cumulative distribution
#| layout-ncol: 2
#| echo: false
#| dev: "tikz"
#| fig-width: 7
#| fig-height: 5
#| warning: false
#| message: false

ggplot(NULL, aes(c(-4,4))) +
  geom_area(stat="function",fun=dnorm,fill="grey",xlim=c(-4,qnorm(.975)),col="black") +
  geom_area(stat="function",fun=dnorm,fill="white",xlim=c(qnorm(.975),4),col="black") + 
  theme_bw() + xlab("$Y \\sim \\mbox{Normal}(0,1)$") + ylab("Density")



x=seq(-4,4,length=200)
tibble(x=x,y=pnorm(x)) %>% ggplot(aes(x,y)) + geom_line() + 
  geom_segment(aes(x=qnorm(.975),y=0,xend=qnorm(.975),yend=pnorm(qnorm(.975))),linetype=2) + 
  geom_segment(aes(x=-4,y=pnorm(qnorm(.975)),xend=qnorm(.975),yend=pnorm(qnorm(.975))),linetype=2) +
  xlab("$y$") + ylab("Cumulative probability distribution") + theme_bw() +
  annotate(geom="text",x=-4,y=pnorm(qnorm(.975)),label="$\\uparrow$ 0.975",hjust=0,vjust=2) +
  annotate(geom="text",x=-4,y=pnorm(qnorm(.975)),label="$\\downarrow$ 0.025",hjust=0,vjust=-1.5)


## ----tailarea2----------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Computes y1 so that, given Y~Normal(0,1), Pr(Y<=y1)=0.975
y1=qnorm(p=0.975,mean=0,sd=1)
# Computes y2 so that, given Y~Normal(0,1), Pr(Y<=y1)=0.025
y2=qnorm(p=0.025,mean=0,sd=1)
# Now verifies that Pr(Y<=y1) - Pr(Y<=y2) = 0.975 - 0.025 = 0.95
pnorm(q=y1,mean=0,sd=1)-pnorm(q=y2,mean=0,sd=1)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-gammas
#| fig-cap: "Some examples of Gamma distributions upon varying the parameters $a$ and $b$. For different choices of the parameters, the Gamma distribution can give rise to other distributions, including the Exponential"
#| dev: "tikz"
#| echo: false
tibble(x=seq(0,10,.01)) %>% ggplot(aes(x)) + 
  stat_function(
    fun=dgamma,args=list(shape=2,rate=2),lwd=1.0,aes(colour="Gamma(2,2)")
  ) + 
  stat_function(
    fun=dgamma,args=list(shape=1,rate=1.2),lwd=1.0,aes(colour="Gamma(1,1.2) = Exponential(1.2)")
  ) +
  # stat_function(
  #   fun=dgamma,args=list(shape=3/2,rate=1/2),lwd=1.0,aes(colour="Gamma(1.5,0.5) = Chi-squared(3)")
  # ) +
  stat_function(
    fun=dgamma,args=list(shape=12,rate=2.7),lwd=1.0,aes(colour="Gamma(12,2.7)")
  ) +
  theme_bw() + scale_colour_manual("",values = c("black", "blue","magenta")) + 
  theme(legend.position=c(.75,.8),legend.background = element_rect(fill='transparent')) + ylab("Density") + xlab("") +
  scale_fill_grey() + scale_color_grey(name="")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-betas
#| fig-cap: 'Some examples of Beta distributions upon varying the parameters $a$ and $b$. For different choices of the parameters, the Beta distribution have different shapes, central tendency and variance'
#| fig-subcap: 
#|   - $a=0.5,b=0.5$
#|   - $a=5,b=5$
#|   - $a=1,b=1$
#|   - $a=5,b=1$
#|   - $a=15,b=5$
#|   - $a=150,b=50$
#| echo: false
#| fig.pos: "H"
#| layout-ncol: 3
#| fig-width: 7
#| fig-height: 5.5
a=c(0.5,5,1,5,15,150)
b=c(0.5,5,1,1,5,50)

t=list()
for (i in 1:length(a)) {
  t[[i]]=tibble(x=seq(0,1,.0001)) %>% ggplot(aes(x)) +
    stat_function(
      fun=dbeta,args=list(shape1=a[i],shape2=b[i]),lwd=1.0
    ) +
  theme_bw() + ylab("Density") + xlab("")
}
t[[1]]
t[[2]]
t[[3]]
t[[4]]
t[[5]]
t[[6]]


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-Bayesian1
#| fig-cap: 'An (extremely simplified!) example of Bayesian inference on a probability $\theta$ for a Binomial sampling distribution model, with $r=9$ observed successes out of $n=13$ individuals'
#| dev: "tikz"
#| echo: false
r=9; n=13
tibble(
  theta=seq(0,1,.01),
  prior=dbeta(theta,9.2,13.8),
  lik=dbeta(theta,r+1,(n-r+1)),
  post=dbeta(theta,9.2+r,13.8+n-r)
) %>% ggplot() + geom_line(aes(x=theta,y=prior,color="Prior")) + 
  geom_line(aes(x=theta,y=lik,color="Likelihood ($r=9,n=13$)")) +
  geom_line(aes(x=theta,y=post,color="Posterior")) + 
  theme_bw() + 
  scale_colour_manual("",breaks=c("Prior","Likelihood ($r=9,n=13$)","Posterior"),
                      values = c("Prior"="black", "Likelihood ($r=9,n=13$)"="red", "Posterior"="blue")) + 
  theme(legend.position=c(.8,.85),legend.background=element_rect(fill='transparent')) + ylab("") + xlab("$\\theta$") + 
  scale_fill_grey() + scale_color_grey(name="")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-likelihood1
#| fig-cap: 'Binomial sampling distribution compared to three likelihood functions for different observed values of the data $r$. Each likelihood function is rescaled to have a maximum value at 1, for comparability. Panel (a) shows the sampling distribution for $\theta=0.3$, while panel (b) shows the likelihood function for three possible observed values of $r=2,4,9$ (in black, red and blue respectively)'
#| fig.subcap: 
#|    - "Binomial sampling distribution $p(r\\mid \\theta=0.3)$"
#|    - "Likelihood function $\\mathcal{L}(\\theta \\mid r=2,4,9)$"
#| layout-ncol: 2
#| dev: "tikz"
#| echo: false
theta=.3
n=13
tibble(
  r=seq(0,n),
  p=dbinom(r,size=n,prob=theta)
) %>% ggplot(aes(r,p))+geom_bar(stat="identity") + 
  theme_bw() + xlab("$r$") + ylab("Probability distribution")

tibble(
  theta=seq(0,1,.01),
  l1=theta^2*(1-theta)^(n-2),
  l2=theta^4*(1-theta)^(n-4),
  l3=theta^9*(1-theta)^(n-9)
) %>% ggplot() + 
  geom_line(aes(x=theta,y=l1/max(l1),color="$r=2,n=13$")) + 
  geom_line(aes(x=theta,y=l2/max(l2),color="$r=4,n=13$")) + 
  geom_line(aes(x=theta,y=l3/max(l3),color="$r=9,n=13$")) + 
  theme_bw() + xlab("$\\theta$") + ylab("Rescaled likelihood function") +
  # scale_colour_manual(
  #   "",breaks=c("$r=2,n=13$","$r=4,n=13$","$r=9,n=13$"),
  #   values = c("$r=2,n=13$"="black", "$r=4,n=13$"="red", "$r=9,n=13$"="blue")
  # ) + 
  theme(legend.position=c(.9,.85),legend.background=element_rect(fill='transparent')) +
  ### Should the colouring here change???
  geom_segment(aes(x=2/n,y=0,xend=2/n,yend=1),color="black",linetype=2) +
  geom_segment(aes(x=4/n,y=0,xend=4/n,yend=1),color="red",linetype=2) +
  geom_segment(aes(x=9/n,y=0,xend=9/n,yend=1),color="blue",linetype=2) + 
  scale_fill_grey() + scale_color_grey(name="") 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-loglik
#| fig-cap: 'Likelihood function (the dark curve, with values on the $y-$axis on the left hand side) and Log-likelihood function (in ligter colour, with values on the $y-$axis on the right hand side) for the Binomial example with $r=2$. Both functions are maximised at the same point along the $x-$axis'
#| dev: "tikz"
#| echo: false
# theta=seq(0.01,0.9999,.01)
# l=theta^2*(1-theta)^(n-2)
# logl=2*log(theta)+(n-2)*log(1-theta)
# tibble(theta=theta,l=l,type="Likelihood") %>% bind_rows(tibble(theta=theta,l=logl,type="log-Likelihood")) %>% 
#   ggplot(aes(theta,l))+geom_line() + 
#   facet_wrap(~type,scales='free_y') + theme_bw() + xlab("$\\theta$") + ylab("") 

theta=seq(0.01,0.9999,.01)
l=theta^2*(1-theta)^(n-2)
logl=2*log(theta)+(n-2)*log(1-theta)
scaleFactor=max(l)/min(logl)
tibble(
  theta=theta,l=l,logl=logl
) %>% 
  ggplot() + geom_line(aes(theta,l),col="gray5") + geom_line(aes(theta,max(l)-logl*scaleFactor),col="gray80") +
  theme_bw() + scale_y_continuous("Likelihood",sec.axis = sec_axis(name="log-Likelihood",trans=~min(logl)-./scaleFactor)) + xlab("$\\theta$") + ylab("") + 
  geom_segment(aes(x=2/13,y=0,xend=2/13,yend=max(l)),linetype=2,lwd=.55) +
  theme(axis.title.y.right = element_text(angle = 90)) 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Defines the likelihood function as a R function
Lik=function(theta,r,n) {
   # The function depends on three arguments:
   # 'theta' is a vector of values specifying the range of the parameter
   # 'r' is the observed number of successes 
   # 'n' is the observed sample size
   theta^r*(1-theta)^(n-r)
}
# Use 'optimise' to obtain the MLE for w=2 and n=13 in the interval (0,1), 
# ie the range of theta
optimise(Lik,r=2,n=13,interval=c(0,1),maximum=TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: simulationBin
#| echo: true
# Sets the 'seed' so that we always get the same results
set.seed(12)
# Sets the "true" value of the probability of success (assumed known)
theta=0.3
# Sets the sample size in each repeated experiment
n=13
# Sets the number of simulations
nsim=1000
# Defines a matrix "samples" with nsim rows and n columns, 
# initially with "NA" (empty) values
samples=matrix(NA,nsim,n)
# Then creates a loop to fill each row of the matrix "samples" with n 
# simulated values from a Binomial(theta, 1) (i.e. we simulate all the 
# individual Bernoulli data Y_i) 
for (i in 1:nrow(samples)) {
   samples[i,]=rbinom(n,1,theta)
}
head(samples)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-dotcharts
#| fig-cap: 'A graphical representation of a simulation exercise to describe repeated experiments from a Binomial case with $n=13$ and $\theta=0.3$. For each of the 20 simulations presented, the black dots indicate the simulated Bernoulli outcomes $y_1,\ldots,y_n$; the red diamonds indicate the sample means $\bar{y}$ and the blue dots are the rescaled sample medians $\displaystyle \frac{\mbox{Med}(\boldsymbol{y})}{n}$. The vertical dashed line indicates the true value for the parameter $\theta=0.3$'
#| echo: false
#| message: false
sel=1:20
a=rep(c(.25,0),10)
meds=apply(samples[sel,],1,median)/13
means=apply(samples[sel,],1,mean)
p=as.vector(samples[sel,]) %>% as_tibble() %>% 
  mutate(sample=as.factor(rep(1:20,each=13))) %>% 
  ggplot(aes(x=sample,y=value))+geom_dotplot(binaxis="y",stackdir="center",dotsize = .12) +
  coord_flip() + theme_bw() + ylab("Observed values") + xlab("Repeated experiments") +
  geom_point(data=tibble(y=meds,x=1:20),aes(x=x,y=y),col="blue",size=1.25) + 
  geom_point(data=tibble(y=means,x=1:20),aes(x=x,y=y),col="red",size=2.5,shape=18) + 
  geom_segment(aes(x=-Inf,y=0.3,xend=Inf,yend=0.3),linetype="dashed",linewidth=.35) 
for (i in 1:20) {
  p=p+annotate("rect",xmin=(i-.5),xmax=(i+.5),ymin=-Inf,ymax=Inf,fill="lightgrey",alpha=a[i])
}
p


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-intervals1
#| fig-cap: 'Histograms for the sampling distributions of the two statistics $f_1(\boldsymbol Y)$ and $f_2(\boldsymbol Y)$'
#| fig-subcap: 
#|   - "Sample mean"
#|   - "Sample median/$n$"
#| layout-ncol: 2
#| echo: true
tibble(x=samples %>% apply(1,mean)) %>% 
  ggplot(aes(x)) + geom_histogram(bins=10,fill="grey",col="black") + 
  theme_bw() + xlab("") + ylab("Density") + 
  geom_segment(
    aes(x=0.3,y=-Inf,xend=0.3,yend=Inf),
    linetype="dashed",lwd=0.85
  ) 

tibble(x=samples %>% apply(1,median)/n) %>% 
  ggplot(aes(x)) + geom_histogram(bins=10,fill="grey",col="black") + 
  theme_bw() + xlab("") + ylab("Density")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| echo: false
#| label: tbl-intervals2
#| tbl-cap: Summary of the sampling distributions for the two statistics
#| message: false
#| warning: false
digits=4
tab=rbind(bmhe::stats(apply(samples,1,mean)),bmhe::stats(apply(samples,1,median)/n))
rownames(tab)=c("$\\displaystyle f_1(\\boldsymbol Y)=\\bar{Y}$","$\\displaystyle f_2(\\boldsymbol Y)=\\mbox{Med}(\\boldsymbol Y)/n$")
col1=c("$\\displaystyle f_1(\\boldsymbol Y)=\\bar{Y}$","$\\displaystyle f_2(\\boldsymbol Y)=\\mbox{Med}(\\boldsymbol Y)/n$")
tab=data.frame(col1,tab)
colnames(tab)=c("","Mean","SD","2.5\\%","Median","97.5\\%")
tinytable::tt(tab,digits=4)

# if(knitr::is_latex_output()==TRUE) {
#   kableExtra::kable_styling(
#   knitr::kable(
#     tab,
#     booktabs=TRUE,
#     digits=digits, escape=F, format="pipe",
#     col.names=c("Mean","SD","2.5\\%","Median","97.5\\%")
#   )
#   ,latex_options=c("HOLD_position"))
# } else {
#   knitr::kable(
#     tab,
#     booktabs=TRUE,
#     digits=digits
#   )
# }


## ----betaprobs----------------------------------------------------------------------------------------------------------------------------
#| echo: true
q_U=qbeta(p=0.95,shape1=18.2,shape2=17.8)


## ----betaint------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Simulates 10000 values from the posterior distribution Beta(18.2,17.8)
theta=rbeta(n=10000,shape1=18.2,shape2=17.8)
# Computes the 2.5% quantile (e.g. the point leaving area of 2.5% to its left)
q_L=quantile(theta,0.025)
# Computes the 97.5% quantile (e.g. the point leaving area of 97.5% to its left)
q_U=quantile(theta,0.975)
# Displays the resulting interval estimate
c(q_L,q_U)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-posteriorinterval
#| fig-cap: 'Posterior distribution for the Binomial example. The dark horizontal line below the distribution indicate the 95% interval estimate'
#| echo: false
#| dev: "tikz"
#| message: false
#| warning: false
tibble(theta=seq(0,1,.001),y=dbeta(seq(0,1,.001),18.2,17.8)) %>% 
  ggplot(aes(theta,y)) + geom_line(col="blue") + theme_bw() +
  xlab("$\\theta$") + ylab("Posterior probability distribution") + 
  geom_segment(aes(x=q_L,y=0,xend=q_U,yend=0))


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-dotchartCI
#| fig-cap: 'Graphical representation of the concept of confidence interval. The black dots are the simulated values for the observed Bernoulli data, in each of 20 replicates of the experiment. The red diamonds indicate the computed sample mean for each replicate. The dashed vertical line is drawn in correspondence of the "true" value of the parameter $\theta=0.3$. The blue lines indicate the 95\% confidence intervals computed using the procedure described above.'
#| echo: false
#| message: false
#| warning: false
se=(apply(samples[sel,],1,sd)/sqrt(n))
est=apply(samples[sel,],1,mean)
low=est-1.96*se
upp=est+1.96*se
check=numeric()
for (i in 1:length(sel)) {
  check[i]=(upp[i]<0.3) || (low[i]>0.3)
}
howmanytimes=sum(check==1)

p=as.vector(samples[sel,]) %>% as_tibble() %>% 
  mutate(sample=as.factor(rep(1:20,each=13))) %>% 
  ggplot(aes(x=sample,y=value))+geom_dotplot(binaxis="y",stackdir="center",dotsize = .12) +
  coord_flip() + theme_bw() + ylab("Observed values") + xlab("Repeated experiments") +
  geom_point(data=tibble(y=means,x=1:20),aes(x=x,y=y),col="red",size=2.5,shape=18) + 
  geom_segment(aes(x=-Inf,y=0.3,xend=Inf,yend=0.3),linetype="dashed",size=.35) 
for (i in 1:20) {
  p=p+annotate("rect",xmin=(i-.5),xmax=(i+.5),ymin=-Inf,ymax=Inf,fill="lightgrey",alpha=a[i]) 
}
p = p + geom_segment(aes(y=low[1],x=1,yend=upp[1],xend=1),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[2],x=2,yend=upp[2],xend=2),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[3],x=3,yend=upp[3],xend=3),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[4],x=4,yend=upp[4],xend=4),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[5],x=5,yend=upp[5],xend=5),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[6],x=6,yend=upp[6],xend=6),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[7],x=7,yend=upp[7],xend=7),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[8],x=8,yend=upp[8],xend=8),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[9],x=9,yend=upp[9],xend=9),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[10],x=10,yend=upp[10],xend=10),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[11],x=11,yend=upp[11],xend=11),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[12],x=12,yend=upp[12],xend=12),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[13],x=13,yend=upp[13],xend=13),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[14],x=14,yend=upp[14],xend=14),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[15],x=15,yend=upp[15],xend=15),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[16],x=16,yend=upp[16],xend=16),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[17],x=17,yend=upp[17],xend=17),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[18],x=18,yend=upp[18],xend=18),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[19],x=19,yend=upp[19],xend=19),lwd=.55,col="blue")
p = p + geom_segment(aes(y=low[20],x=20,yend=upp[20],xend=20),lwd=.55,col="blue")
p


## ----condtext,echo=FALSE,results="asis"---------------------------------------------------------------------------------------------------
howmanywrong=sum(check==1)
if(howmanywrong==1){
  cat(paste0("However, there is one potential replicate (experiment number ",which(check==1),') in which the procedure fails to cover the true value of the parameter. This is not entirely surprising: the procedure for the confidence interval "gets it right" 19/20 or 95% of the times.'))
}   
if(howmanywrong==0) {
    
}
if(howmanywrong>1) {
  cat(paste0("However, there are ",howmanywrong," potential replicates (experiments number ",paste(which(check==1),collapse=","),') in which the procedure fails to cover the true value of the parameter. In this case, the procedure "gets it right" ',howmanytimes,"/20 or ",format(100*howmanytimes/20,digits=3),'% of the times. But if we look at the "long-run" property (i.e. out of 1000s of simulations for the replicated experiments), we actually see that this proportion quickly goes to 95%.'))
}


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: galtondata
#| echo: false
galton=read.table("Galton.txt",header=TRUE)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-datagalton
#| echo: false
#| tbl-cap: "The first few rows of Galton's dataset on height of parents and children"
# tab=head(galton)
# digits=1
# cond.table(tab,digits)
galton |> head() |> tinytable::tt(
  digits=1
)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-datagaltonplot
#| echo: false
#| fig-cap: "A graphical depiction of Galton's data on father's and children's height. Numbers represent the families within which the children and fathers are clustered"
ggplot(galton,aes(Father,Height))+
   labs(x="Father's height (inches)",y="Child's height (inches)") +
   geom_text(aes(label=Family),size=2.95)  +
   theme_bw()
   # theme_classic() +
   # theme(axis.line = element_line(size=.7, colour = "black"),
   #       legend.position = "none",
   #       panel.grid.major = element_blank(),
   #       panel.grid.minor = element_blank(),
   #       panel.border = element_blank(),
   #       panel.background = element_blank(),
   #       plot.title=element_text(size = 20),
   #       text=element_text(size = 12),
   #       axis.text.x=element_text(colour="black", size = 12),
   #       axis.text.y=element_text(colour="black", size = 12),
   #       axis.ticks.length=unit(.2, "cm")
   #       ) 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-galtonline
#| fig-cap: "Original scale"
#| dev: "tikz"
#| fig-pos: "h"
#| fig-height: 3.5
#| fig-width: 5
#| warning: false
#| echo: false
#| message: false

y.bar=mean(galton$Height)
X1.bar=mean(galton$Father)
Cxy=cov(galton$Height,galton$Father)
Vx=var(galton$Father)
beta1.hat=Cxy/Vx
beta0.hat=y.bar-beta1.hat*X1.bar

m=lm(galton$Height~galton$Father)
galton %>% 
  ggplot(aes(x=Father,y=Height))+geom_point(col="grey40") +
  geom_smooth(method='lm', formula= y~x,xseq=c(0,range(galton$Father)[2]),se=FALSE) + 
  xlim(0,range(galton$Father)[2]) + ylim(30,range(galton$Height)[2]) + theme_bw() + 
  annotate(
    "text",min(galton$Father)*.05,max(galton$Height)*.95,label=paste("$\\hat\\beta_0=",format(m$coefficients[1],digits=3,nsmall=2),"$")
  ) +
  annotate(
    "text",min(galton$Father)*.05,max(galton$Height)*.90,label=paste("$\\hat\\beta_1=",format(m$coefficients[2],digits=3,nsmall=2),"$")
  ) +
  geom_segment(
    aes(x=0,xend=15,y=beta0.hat,yend=beta0.hat),
    arrow = arrow(length=unit(0.30,"cm"),type="closed")
  ) + 
  geom_curve(
    aes(x=10,y=beta0.hat,xend=8,yend=beta0.hat+8*beta1.hat),fill="grey"
  ) +
  annotate(
    "text",15,42,label="$\\hat\\beta_1$"
  ) + xlab("Father's height (inches)") + ylab("Child's height (inches)")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-galtonline2
#| fig-cap: "Centred covariate on the $x-$axis"
#| dev: "tikz"
#| echo: false
#| fig-height: 3.5
#| fig-width: 5
m2=lm(galton$Height~scale(galton$Father,scale=FALSE))
tibble(x=scale(galton$Father,scale=F),y=galton$Height) %>% 
  ggplot(aes(x,y)) + geom_point(col="grey40") + theme_bw() +
  xlab("Centred father's height (inches)") + ylab("Child's height (inches)") + 
  geom_smooth(method='lm', formula= y~x,se=FALSE) + 
  geom_segment(aes(x=-Inf,xend=0,y=m2$coefficients[1],yend=m2$coefficients[1]),linetype="dashed",lwd=.55) +
  geom_segment(aes(x=0,xend=0,y=-Inf,yend=m2$coefficients[1]),linetype="dashed",lwd=.55) + 
  annotate(
    "text",min(scale(galton$Father,scale=F))*.95,max(galton$Height)*.95,label=paste("$\\hat\\beta_0=",format(m2$coefficients[1],digits=3,nsmall=2),"$")
  ) +
  annotate(
    "text",min(scale(galton$Father,scale=F))*.95,max(galton$Height)*.93,label=paste("$\\hat\\beta_1=",format(m2$coefficients[2],digits=3,nsmall=2),"$")
  ) 


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-galtonline3
#| fig-cap: "Galton's original data with the regression and equality lines superimposed"
#| warning: false
#| echo: false
#| message: false
tibble(x=galton$Father,y=galton$Height) %>% 
  ggplot(aes(x,y)) + geom_smooth(method='lm', formula= y~x,se=FALSE,aes(color="blue")) + 
  geom_line(aes(x,x,color="black"),lwd=.9) +
#  scale_colour_manual("",values=c("black","blue"),labels=c('Line of "equality"',"Regression line")) +
  geom_point(col="grey40") + 
  theme_bw() + xlab("Father's height (inches)") + ylab("Child's height (inches)") +
  theme(legend.position = c(.17,.85),legend.text=element_text(size=10),legend.background=element_rect(fill='transparent')) +
  scale_colour_grey(start = 0, end = .9,labels=c('Line of "equality"',"Regression line")) +
  labs(colour="")


## ----predmatcent,echo=FALSE---------------------------------------------------------------------------------------------------------------
X=cbind(rep(1,nrow(galton)),scale(galton$Father,scale=F),scale(galton$Mother,scale=F))
colnames(X)=c("Intercept","Father","Mother")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-prior1
#| fig-cap: "The assumed prior distribution for the regression coefficients. The distribution in blue indicates the prior for the intercept $\\beta_0$. The distribution in black is the same for $\\beta_1$ and $\\beta_2$. As is possible to see, most of the probability is included in a relatively large range, approximately between -20 and 20, indicating large uncertainty in the prior father's and the mother's effect"
#| dev: "tikz"
#| echo: false
tibble(x=seq(-40,140,.01)) %>% ggplot(aes(x)) + 
  stat_function(
    fun=dnorm,args=list(mean=65,sd=20),lwd=1.1,aes(colour="$p(\\beta_0$)")
  ) + 
  stat_function(
    fun=dnorm,args=list(mean=0,sd=10),lwd=1.1,aes(colour="$p(\\beta_1)=p(\\beta_2$)")
  ) + theme_bw() +
  scale_colour_manual("",values = c("grey70","black")) +
  xlab("Regression coefficients") + ylab("Prior distribution") + 
  theme(legend.position=c(.85,.9),legend.background=element_rect(fill='transparent'))


## ----runJAGS,echo=FALSE,cache=TRUE,message=FALSE,warning=FALSE,include=FALSE--------------------------------------------------------------
library(R2jags)
model.string <- "model {
for(i in 1:n) {
y[i] ~ dnorm(mu[i],tau)
mu[i] <- X[i,]%*%beta
}
for(k in 1:K) {
beta[k] ~ dnorm(mu.beta[k],tau.beta[k])
}
tau ~ dgamma(a,b)
sigma <- pow(tau,-.5)
}
"
tmpf=tempfile()
tmps=file(tmpf,"w")
cat(model.string,file=tmps)
dataJags <- list(y=galton$Height,X=X,n=nrow(X),K=ncol(X),a=0.1,b=0.1,mu.beta=c(65,0,0),tau.beta=c(20,10,10)^-2)
model <- jags(data=dataJags,parameters.to.save=c("beta","sigma"),model.file=tmpf,n.chains=2,n.iter=10000,n.burnin=5000)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-runJAGS2
#| tbl-cap: "A summary of the posterior distributions for the model parameters"
#| message: false
#| echo: false
#| warning: false
tab=model$BUGSoutput$summary[-4,c(1,2,3,7)] 
tab=data.frame(c("$\\beta_0$ (intercept)","$\\beta_1$ (slope for father's height)","$\\beta_2$ (slope for mothers's height)","$\\sigma$ (population variance)"),tab)
colnames(tab)=c("","Mean","SD","2.5\\%","97.5\\%")
tinytable::tt(tab,digits=4)
# if(knitr::is_latex_output()==TRUE) {
#   kableExtra::kable_styling(
#   knitr::kable(
#     tab,
#     booktabs=TRUE,
#     digits=4,escape=F,format="latex",
#     col.names=c("Mean","SD","2.5\\%","97.5\\%")
#   )
#   ,latex_options=c("HOLD_position"))
# } else {
#   knitr::kable(
#     tab,
#     booktabs=TRUE,
#     digits=4
#   )
# }



## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-prior3
#| fig-cap: "Histograms from the simulated values for the posterior distributions of $\\beta_1$ and $\\beta_2$"
#| fig-subcap: 
#|   - "$p(\\beta_1\\mid \\boldsymbol{y},\\boldsymbol{X})$"
#|   - "$p(\\beta_2\\mid \\boldsymbol{y},\\boldsymbol{X})$"
#| layout-ncol: 2
#| dev: "tikz"
#| message: false
#| echo: false
#| warning: false
tibble(x=model$BUGSoutput$sims.matrix[,"beta[2]"]) %>% 
  ggplot(aes(x)) + geom_histogram(col="black",fill="grey") + 
  theme_bw() + xlab("$\\beta_1$") + xlim(c(-0.05,range(model$BUGSoutput$sims.matrix[,"beta[2]"])[2])) + 
  geom_vline(xintercept=0,linetype="dashed",lwd=.55) + ylab("Posterior distribution")

tibble(x=model$BUGSoutput$sims.matrix[,"beta[3]"]) %>% 
  ggplot(aes(x)) + geom_histogram(col="black",fill="grey") + 
  theme_bw() + xlab("$\\beta_2$") + xlim(c(-0.05,range(model$BUGSoutput$sims.matrix[,"beta[3]"])[2])) + 
  geom_vline(xintercept=0,linetype="dashed",lwd=.55) + ylab("Posterior distribution")


## ----predmat,echo=FALSE-------------------------------------------------------------------------------------------------------------------
X=cbind(rep(1,nrow(galton)),galton$Father,galton$Mother)
colnames(X)=c("Intercept","Father","Mother")


## ----mlematrix,echo=TRUE------------------------------------------------------------------------------------------------------------------
# Constructs the matrix of predictors, including the first column of ones 
# the second column with the fathers' heights and the third column with
# the mothers' heights, from the original dataset
X=cbind(rep(1,nrow(galton)),galton$Father,galton$Mother)

# Constructs the vector of outcomes with the children's heights
y=galton$Height
# Now computes the MLE for all the regression coefficients
beta=solve(t(X)%*%X) %*% t(X)%*%y


## ----mlematrix2,echo=FALSE----------------------------------------------------------------------------------------------------------------
rownames(beta)=c("beta0","beta1","beta2")
colnames(beta)="MLE estimate"
beta


## ----mlematrix3,echo=TRUE-----------------------------------------------------------------------------------------------------------------
# Computes the Residual Sums of Squares
RSS=t(y-X%*%beta)%*%(y-X%*%beta)
# Computes the estimate of the standard deviation sigma
# NB: "nrow(X)"=number of rows in the matrix X, while
#     "ncol(X)"=number of columns in the matrix X
sigma2.hat=as.numeric(RSS/(nrow(X)-ncol(X)))

# Now computes the variance of the coefficients using the formula above
sigma2.beta=sigma2.hat*solve(t(X)%*%X)
# Now squares the elements on the diagonal (i.e. the variances of the three 
# coefficients), to obtain the standard deviations
sigma.beta=sqrt(diag(sigma2.beta))


## ----mlematrix4,echo=FALSE----------------------------------------------------------------------------------------------------------------
cbind(beta,sd=sigma.beta)


## ----lmmod--------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Runs the function "lm" to run the model including "Height" as the reponse 
# (that is the variable to the left of the twiddle symbol "~"), while "Father" 
# and "Mother" (the variables to the right of the twiddle) are the covariates. 
# These are recorded in the dataset called "galton"
m1=lm(formula=Height~Father+Mother,data=galton)
# Now displays the output
summary(m1)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------
coefsmle=m1$coefficients


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-logitfn
#| fig-cap: "Graphical description of the shape and features of the logit function. For $\\theta\\rightarrow 0$, then logit$(\\theta)\\rightarrow -\\infty$, while for $\\theta\\rightarrow 1$, then logit$(\\theta)\\rightarrow + \\infty$. For $\\theta=0.5$, then logit$(\\theta)=0$" 
#| cache: true
#| dev: "tikz"
#| echo: false
tibble(theta=seq(0,1,.0001)) %>% mutate(l=log(theta/(1-theta))) %>% 
  ggplot(aes(theta,l)) + geom_line() + theme_bw() + 
  xlab("$\\theta$") + ylab("$g(\\theta)=\\mbox{logit}(\\theta)$") + 
  annotate("text",x=0,y=-Inf,label="$\\downarrow - \\infty$",vjust=-1,hjust=-.15) +
  annotate("text",x=1,y=Inf,label="$\\uparrow \\infty$",vjust=2,hjust=1) + 
  geom_segment(aes(x=.5,y=-Inf,xend=.5,yend=bmhe::logit(.5)),linetype="dashed") +
  geom_segment(aes(x=-Inf,y=0,xend=.5,yend=0),linetype="dashed") 


## ----createdata---------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Creates a variable "y2" taking value 1 if the original child's height > median
y2=ifelse(galton$Height>71,1,0)
# Summarises the new variable
table(y2)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-priorinflogistic
#| fig-cap: "The prior distribution for $\\beta_0$ in panel (a) is used to encode the information that $\\theta$ is most likely between around 0.05 and 0.2, when $X^*_{1i}=X^*_{2i}=0$, indicated by the dark blue horizontal line at the bottom on panel (b)"
#| layout-ncol: 2
#| fig-subcap: 
#|   - "The prior for $\\beta_0$"
#|   - "The induced prior for $\\theta$"
#| dev: "tikz"
#| echo: false
b0=bmhe::logit(0.105)
s0=0.4
beta0=rnorm(10000,b0,s0)
tibble(beta0=beta0) %>% ggplot(aes(beta0)) + geom_histogram(aes(y = after_stat(density)),fill="grey",col="black",bins=15) + theme_bw() +
  xlab("$\\beta_0$") + ylab("Prior distribution")

tibble(theta=bmhe::ilogit(beta0)) %>% ggplot(aes(theta)) + geom_histogram(aes(y = after_stat(density)),fill="grey",col="black",bins=15) + theme_bw() +
  xlab("$\\theta$") + ylab("Prior distribution") + 
  geom_segment(aes(x=quantile(theta,.025),y=0,xend=quantile(theta,.975),yend=0),col="blue",lwd=1.5)


## ----c------------------------------------------------------------------------------------------------------------------------------------
#| echo: true
# Defines the mean and sd of the prior
b0=log(0.105/(1-0.105))
s0=0.4
# Simulates 10000 values from the prior
beta0=rnorm(10000,b0,s0)
# Rescales the prior to the scale of the main parameter theta
theta=exp(beta0)/(1+exp(beta0))
# Checks the implied 95% interval estimate for theta
cbind(quantile(theta,.025),quantile(theta,.975))


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------
options(scipen=999)


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------
options(scipen=0)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: galtonbinary
#| cache: true
#| message: false
#| warning: false
#| include: false
y2=ifelse(galton$Height>median(galton$Height),1,0)
X=cbind(rep(1,nrow(galton)),scale(galton$Father,scale=F),scale(galton$Mother,scale=F))
colnames(X)=c("Intercept","Father","Mother")

model.string <- "model {
for(i in 1:n) {
y[i] ~ dbern(theta[i])
logit(theta[i]) <- X[i,]%*%beta
}
for(k in 1:K) {
beta[k] ~ dnorm(mu.beta[k],tau.beta[k])
}
}
"
tmpf=tempfile()
tmps=file(tmpf,"w")
cat(model.string,file=tmps)
dataJags <- list(y=y2,X=X,n=nrow(X),K=ncol(X),mu.beta=c(bmhe::logit(.105),0,0),tau.beta=c(.4,2,2)^-2)
model2 <- jags(data=dataJags,parameters.to.save=c("beta"),model.file=tmpf,n.chains=2,n.iter=10000,n.burnin=5000)

## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: tbl-runJAGS3
#| tbl-cap: "A summary of the posterior distributions for the model parameters"
#| echo: false
#| message: false
#| warning: false

tab=model2$BUGSoutput$summary[-4,c(1,2,3,7)]
tab=data.frame(c("$\\beta_0$ (intercept)","$\\beta_1$ (logOR for father's height)","$\\beta_2$ (logOR for mothers's height)"),tab)
colnames(tab)=c("","Mean","SD","2.5\\%","97.5\\%")
tinytable::tt(tab,digits=4)
# 
# 
# tab=model2$BUGSoutput$summary[-4,c(1,2,3,7)]
# rownames(tab)=c("$\\beta_0$ (intercept)","$\\beta_1$ (logOR for father's height)","$\\beta_2$ (logOR for mothers's height)")
# if(knitr::is_latex_output()==TRUE) {
#   kableExtra::kable_styling(
#   knitr::kable(
#     tab,
#     booktabs=TRUE,
#     digits=4,escape=F,format="latex",
#     col.names=c("Mean","SD","2.5\\%","97.5\\%")
#   )
#   ,latex_options=c("HOLD_position"))
# } else {
#   knitr::kable(
#     tab,
#     booktabs=TRUE,
#     digits=4
#   )
# }


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------------------
beta1=model2$BUGSoutput$sims.list$beta[,2]
beta2=model2$BUGSoutput$sims.list$beta[,3]
options(scipen=9999)

## ----postproc,echo=TRUE-------------------------------------------------------------------------------------------------------------------
# Constructs the ORs from the original simulations obtained by the model
OR1=exp(beta1)
OR2=exp(beta2)
# Tail-area probability to estimate Pr(OR1<1). This is the proportion of
# simulations for OR1 that are below 1
sum(OR1<1)/length(OR1)
# Tail-area probability to estimate Pr(OR2<1). This is the proportion of
# simulations for OR2 that are below 1
sum(OR2<1)/length(OR2)


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: fig-postproc2
#| fig-cap: "Histograms for the rescaled posterior distributions for ORs"
#| layout-ncol: 2
#| fig-subcap: 
#|   - "OR for the father's height effect"
#|   - "OR for the mother's height effect"
#| warning: false
#| message: false
#| echo: false
#| dev: "tikz"
# Plots the histograms
tibble(OR1=OR1,OR2=OR2) %>% 
  ggplot() + geom_histogram(aes(x=OR1,y = stat(density)),fill="grey",col="black",bins=15) + theme_bw() +
  xlab("$\\mbox{OR}_1$") + ylab("Prior distribution") + xlim(c(.9704,1.3296)) +
  geom_vline(xintercept = 1,linetype="dashed")

tibble(OR1=OR1,OR2=OR2) %>% ggplot()+
    geom_histogram(aes(x=OR2,y = stat(density)),fill="grey",col="black",bins=15) + theme_bw() +
  xlab("$\\mbox{OR}_2$") + ylab("Prior distribution") + xlim(c(.9704,1.3296)) +
  geom_vline(xintercept = 1,linetype="dashed")


## -----------------------------------------------------------------------------------------------------------------------------------------
#| label: glmmodel
#| echo: false
# Defines the model "formula". NB: Because X already contains a column of ones,
# needs to add "-1" to tell R that the intercept is to be accounted for by that
formula=y2~X-1
# Now runs "glm" to estimate the model
model=glm(formula,data=data.frame(y2,X),family='binomial')

# Trick to have the 'call' all in one line
# https://stackoverflow.com/questions/30692433/make-the-call-in-glm-output-respect-getoptionwidth
#xx = paste(strwrap(capture.output(print(model$call))),collapse = " ")
#model$call <- capture.output(cat(xx)) 
summary(model)

