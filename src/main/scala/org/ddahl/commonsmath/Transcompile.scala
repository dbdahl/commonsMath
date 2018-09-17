package org.ddahl.commonsmath

import org.apache.commons.math3.util.FastMath._
import org.apache.commons.math3.special.Gamma._
import org.apache.commons.math3.special.Beta._
import org.apache.commons.math3.distribution._
import org.apache.commons.math3.random.Well44497b
import scala.reflect.ClassTag

object Transcompile {

  import scala.Double.NaN
  import java.lang.Double.isNaN
  private val rng = new Well44497b()
  private def r2int(x: Double) = round(x).toInt

  // Special functions
  def _gamma(x: Double): Double = gamma(x)
  def _gamma(x: Array[Double]): Array[Double] = x.map(gamma)
  def _gamma(x: Array[Int]): Array[Double] = x.map(z => gamma(z))
  def _lgamma(x: Double): Double = logGamma(x)
  def _lgamma(x: Array[Double]): Array[Double] = x.map(logGamma)
  def _lgamma(x: Array[Int]): Array[Double] = x.map(z => logGamma(z))
  def _beta(x: Double, y: Double): Double = exp(logBeta(x, y))
  def _beta(x: Array[Double], y: Array[Double]): Array[Double] = x.zip(y).map { z => exp(logBeta(z._1, z._2)) }
  def _beta(x: Array[Int], y: Array[Double]): Array[Double] = x.zip(y).map { z => exp(logBeta(z._1, z._2)) }
  def _beta(x: Array[Double], y: Array[Int]): Array[Double] = x.zip(y).map { z => exp(logBeta(z._1, z._2)) }
  def _beta(x: Array[Int], y: Array[Int]): Array[Double] = x.zip(y).map { z => exp(logBeta(z._1, z._2)) }
  def _lbeta(x: Double, y: Double): Double = logBeta(x, y)
  def _lbeta(x: Array[Double], y: Array[Double]): Array[Double] = x.zip(y).map { z => logBeta(z._1, z._2) }
  def _lbeta(x: Array[Int], y: Array[Double]): Array[Double] = x.zip(y).map { z => logBeta(z._1, z._2) }
  def _lbeta(x: Array[Double], y: Array[Int]): Array[Double] = x.zip(y).map { z => logBeta(z._1, z._2) }
  def _lbeta(x: Array[Int], y: Array[Int]): Array[Double] = x.zip(y).map { z => logBeta(z._1, z._2) }
  def _factorial(x: Double): Double = gamma(x+1)
  def _factorial(x: Array[Double]): Array[Double] = x.map(z => gamma(z+1))
  def _factorial(x: Array[Int]): Array[Double] = x.map(z => gamma(z+1))
  def _lfactorial(x: Double): Double = logGamma(x+1)
  def _lfactorial(x: Array[Double]): Array[Double] = x.map(z => logGamma(z+1))
  def _lfactorial(x: Array[Int]): Array[Double] = x.map(z => logGamma(z+1))
  def _choose(n: Double, k: Double): Double = {
    val kk = r2int(k)
    exp(logGamma(n+1) - logGamma(kk+1) - logGamma(n-kk+1))
  }
  def _choose(n: Array[Double], k: Array[Double]): Array[Double] = {
    n.zip(k.map(r2int)).map { z =>
      exp(logGamma(z._1 + 1) - logGamma(z._2 + 1) - logGamma(z._1 - z._2 + 1))
    }
  }
  def _choose(n: Array[Int], k: Array[Double]): Array[Double] = {
    n.zip(k.map(r2int)).map { z =>
      exp(logGamma(z._1 + 1) - logGamma(z._2 + 1) - logGamma(z._1 - z._2 + 1))
    }
  }
  def _choose(n: Array[Double], k: Array[Int]): Array[Double] = {
    n.zip(k).map { z =>
      exp(logGamma(z._1 + 1) - logGamma(z._2 + 1) - logGamma(z._1 - z._2 + 1))
    }
  }
  def _choose(n: Array[Int], k: Array[Int]): Array[Double] = {
    n.zip(k).map { z =>
      exp(logGamma(z._1 + 1) - logGamma(z._2 + 1) - logGamma(z._1 - z._2 + 1))
    }
  }
  def _lchoose(n: Double, k: Double): Double = {
    val kk = r2int(k)
    logGamma(n+1) - logGamma(kk+1) - logGamma(n-kk+1)
  }
  def _lchoose(n: Array[Double], k: Array[Double]): Array[Double] = {
    n.zip(k.map(r2int)).map { z =>
      logGamma(z._1 + 1) - logGamma(z._2 + 1) - logGamma(z._1 - z._2 + 1)
    }
  }
  def _lchoose(n: Array[Int], k: Array[Double]): Array[Double] = {
    n.zip(k.map(r2int)).map { z =>
      logGamma(z._1 + 1) - logGamma(z._2 + 1) - logGamma(z._1 - z._2 + 1)
    }
  }
  def _lchoose(n: Array[Double], k: Array[Int]): Array[Double] = {
    n.zip(k).map { z =>
      logGamma(z._1 + 1) - logGamma(z._2 + 1) - logGamma(z._1 - z._2 + 1)
    }
  }
  def _lchoose(n: Array[Int], k: Array[Int]): Array[Double] = {
    n.zip(k).map { z =>
      logGamma(z._1 + 1) - logGamma(z._2 + 1) - logGamma(z._1 - z._2 + 1)
    }
  }

  // sample function
  def _sample(x: Double): Array[Int] = {
    _sample(Array.range(1,r2int(x)+1))
  }

  def _sample(x: Double, size: Double): Array[Int] = {
    _sample(Array.range(1,r2int(x)+1),size)
  }

  def _sample(x: Double, replace: Boolean): Array[Int] = {
    _sample(Array.range(1,r2int(x)+1),r2int(x),replace)
  }

  def _sample(x: Double, size: Double, replace: Boolean): Array[Int] = {
    _sample(Array.range(1,r2int(x)+1),size,replace)
  }

  def _sample(x: Double, size: Double, replace: Boolean, prob: Array[Double]): Array[Int] = {
    _sample(Array.range(1,r2int(x)+1),size,replace,prob)
  }

  def _sample(x: Double, size: Double, prob: Array[Double]): Array[Int] = {
    _sample(Array.range(1,r2int(x)+1),size,false,prob)
  }

  def _sample(x: Double, replace: Boolean, prob: Array[Double]): Array[Int] = {
    _sample(Array.range(1,r2int(x)+1),r2int(x),replace,prob)
  }

  def _sample(x: Double, prob: Array[Double]): Array[Int] = {
    _sample(Array.range(1,r2int(x)+1),r2int(x),false,prob)
  }

  def _sample[T : ClassTag](x: Array[T], size: Double = NaN, replace: Boolean = false, prob: Array[Double] = null): Array[T] = {
    val p = if (prob == null) Array.fill(x.length)(1.0) else prob.map(math.max(0.0,_))
    if ( x.length != p.length ) sys.error("Length of 'x' and 'prob' must be same.")
    val s = if ( isNaN(size) ) x.length else r2int(size)
    val pairs = x.zip(p)
    var total = p.sum
    if ( ( ! replace ) && ( s > x.length ) ) sys.error("Cannot take a sample larger than the population when 'replace = FALSE'.")
    Array.fill(s) {
      val u = total * rng.nextDouble()
      var cumsum = 0.0
      var i = 0
      while ( cumsum <= u ) {
        cumsum += pairs(i)._2
        i += 1
      }
      i -= 1
      if (!replace) {
        total -= pairs(i)._2
        pairs(i) = (pairs(i)._1, 0.0)
      }
      pairs(i)._1
    }
  }

  // Generic distribution functions
  private def dI(dist: AbstractIntegerDistribution, x: Double): Double = dist.probability(r2int(x))
  private def d(dist: AbstractRealDistribution, x: Double): Double = dist.density(x)
  private def dI(dist: AbstractIntegerDistribution, x: Array[Double], log: Boolean): Array[Double] = if ( log ) x.map( xx => dist.logProbability(r2int(xx)) ) else x.map( xx => dist.probability(r2int(xx)) )
  private def d(dist: AbstractRealDistribution, x: Array[Double], log: Boolean): Array[Double] = if ( log ) x.map( xx => dist.logDensity(xx) ) else x.map( xx => dist.density(xx) )
  private def pI(dist: AbstractIntegerDistribution, q: Double): Double = dist.cumulativeProbability(r2int(q))
  private def p(dist: AbstractRealDistribution, q: Double): Double = dist.cumulativeProbability(q)
  private def pI(dist: AbstractIntegerDistribution, q: Array[Double], lower_tail: Boolean): Array[Double] = {
    if (lower_tail) q.map(qq => dist.cumulativeProbability(r2int(qq)))
    else q.map(qq => 1 - dist.cumulativeProbability(r2int(qq)))
  }
  private def p(dist: AbstractRealDistribution, q: Array[Double], lower_tail: Boolean): Array[Double] = {
    if (lower_tail) q.map(qq => dist.cumulativeProbability(qq))
    else q.map(qq => 1 - dist.cumulativeProbability(qq))
  }
  private def qI(dist: AbstractIntegerDistribution, p: Double): Double = dist.inverseCumulativeProbability(p)
  private def q(dist: AbstractRealDistribution, p: Double): Double = dist.inverseCumulativeProbability(p)
  private def qI(dist: AbstractIntegerDistribution, p: Array[Double], lower_tail: Boolean): Array[Double] = {
    if ( lower_tail ) p.map(pp => dist.inverseCumulativeProbability(pp).toDouble)
    else p.map(pp => dist.inverseCumulativeProbability(1-pp).toDouble)
  }
  private def q(dist: AbstractRealDistribution, p: Array[Double], lower_tail: Boolean): Array[Double] = {
    if ( lower_tail ) p.map(pp => dist.inverseCumulativeProbability(pp))
    else p.map(pp => dist.inverseCumulativeProbability(1-pp))
  }
  private def rI(dist: AbstractIntegerDistribution): Double = dist.sample()
  private def r(dist: AbstractRealDistribution): Double = dist.sample()
  private def rI(dist: AbstractIntegerDistribution, n: Double): Array[Double] = dist.sample(r2int(n)).map(_.toDouble)
  private def r(dist: AbstractRealDistribution, n: Double): Array[Double] = dist.sample(r2int(n))

  // Uniform distribution
  private def mkUniform(min: Double, max: Double) = new UniformRealDistribution(rng, min, max)
  private val stdUniform = mkUniform(0.0, 1.0)
  def _dunif(x: Double) = d(stdUniform,x)
  def _dunif(x: Array[Double], min: Double = 0.0, max: Double = 1.0, log: Boolean = false) = d(mkUniform(min,max),x,log)
  def _punif(q: Double): Double = p(stdUniform,q)
  def _punif(q: Array[Double], min: Double = 0.0, max: Double = 1.0, lower_tail: Boolean = true) = p(mkUniform(min,max),q,lower_tail)
  def _qunif(p: Double): Double = q(stdUniform,p)
  def _qunif(p: Array[Double], min: Double = 0.0, max: Double = 1.0, lower_tail: Boolean = true) = q(mkUniform(min,max),p,lower_tail)
  def _runif() = r(stdUniform)
  def _runif(n: Double, min: Double = 0.0, max: Double = 1.0) = r(mkUniform(min,max),n)

  // Normal distribution
  private def mkNormal(mean: Double, sd: Double) = new NormalDistribution(rng, mean, sd)
  private val stdNormal = mkNormal(0.0, 1.0)
  def _dnorm(x: Double) = d(stdNormal,x)
  def _dnorm(x: Array[Double], mean: Double = 0.0, sd: Double = 1.0, log: Boolean = false) = d(mkNormal(mean,sd),x,log)
  def _pnorm(q: Double): Double = p(stdNormal,q)
  def _pnorm(q: Array[Double], mean: Double = 0.0, sd: Double = 1.0, lower_tail: Boolean = true) = p(mkNormal(mean,sd),q,lower_tail)
  def _qnorm(p: Double): Double = q(stdNormal,p)
  def _qnorm(p: Array[Double], mean: Double = 0.0, sd: Double = 1.0, lower_tail: Boolean = true) = q(mkNormal(mean,sd),p,lower_tail)
  def _rnorm() = r(stdNormal)
  def _rnorm(n: Double, mean: Double = 0.0, sd: Double = 1.0) = r(mkNormal(mean,sd),n)

  // Gamma distribution
  private def mkGamma(shape: Double, rate: Double, scale: Double) = {
    val scale2 = if ( ( ! isNaN(rate) ) && ( ! isNaN(scale) ) ) sys.error("Only provide one of 'rate' or 'scale'.")
    else if ( isNaN(rate) && isNaN(scale) ) 1.0
    else if ( isNaN(rate) ) scale
    else 1.0/rate
    new GammaDistribution(rng, shape, scale2)
  }
  private val stdGamma = mkGamma(1.0,1.0, NaN)
  def _dgamma(x: Double) = d(stdGamma, x)
  def _dgamma(x: Array[Double], shape: Double, rate: Double = NaN, scale: Double = NaN, log: Boolean = false) = d(mkGamma(shape,rate,scale),x,log)
  def _pgamma(q: Double) = p(stdGamma, q)
  def _pgamma(q: Array[Double], shape: Double, rate: Double = NaN, scale: Double = NaN, lower_tail: Boolean = true) = p(mkGamma(shape,rate,scale),q,lower_tail)
  def _qgamma(p: Double): Double = q(stdGamma, p)
  def _qgamma(p: Array[Double], shape: Double, rate: Double = NaN, scale: Double = NaN, lower_tail: Boolean = true) = q(mkGamma(shape,rate,scale),p,lower_tail)
  def _rgamma() = r(stdGamma)
  def _rgamma(n: Double, shape: Double, rate: Double = NaN, scale: Double = NaN) = r(mkGamma(shape,rate,scale),n)

  // Exponential distribution
  private def mkExp(rate: Double) = new ExponentialDistribution(rng, 1.0/rate)
  private val stdExp = mkExp(1.0)
  def _dexp(x: Double) = d(stdExp, x)
  def _dexp(x: Array[Double], rate: Double = 1.0, log: Boolean = false) = d(mkExp(rate),x,log)
  def _pexp(q: Double) = p(stdExp, q)
  def _pexp(q: Array[Double], rate: Double = 1.0, lower_tail: Boolean = true) = p(mkExp(rate),q,lower_tail)
  def _qexp(p: Double): Double = q(stdExp, p)
  def _qexp(p: Array[Double], rate: Double = 1.0, lower_tail: Boolean = true) = q(mkExp(rate),p,lower_tail)
  def _rexp() = r(stdExp)
  def _rexp(n: Double, rate: Double = 1.0) = r(mkExp(rate),n)

  // Beta distribution
  private def mkBeta(shape1: Double, shape2: Double, ncp: Double) = {
    if ( ncp != 0.0 ) sys.error("The current implementation requires that the ncp is 0.")
    new BetaDistribution(rng, shape1, shape2)
  }
  private val stdBeta = mkBeta(1.0,1.0,0.0)
  def _dbeta(x: Double) = d(stdBeta, x)
  def _dbeta(x: Array[Double], shape1: Double, shape2: Double, ncp: Double = 0.0, log: Boolean = false) = d(mkBeta(shape1,shape2,ncp),x,log)
  def _pbeta(q: Double) = p(stdBeta, q)
  def _pbeta(q: Array[Double], shape1: Double, shape2: Double, ncp: Double = 0.0, lower_tail: Boolean = true) = p(mkBeta(shape1,shape2,ncp),q,lower_tail)
  def _qbeta(p: Double): Double = q(stdBeta, p)
  def _qbeta(p: Array[Double], shape1: Double, shape2: Double, ncp: Double = 0.0, lower_tail: Boolean = true) = q(mkBeta(shape1,shape2,ncp),p,lower_tail)
  def _rbeta() = r(stdBeta)
  def _rbeta(n: Double, shape1: Double, shape2: Double, ncp: Double = 0.0) = r(mkBeta(shape1,shape2,ncp),n)

  // Student t distribution
  private def mkT(df: Double, ncp: Double) = {
    if ( ncp != 0.0 ) sys.error("The current implementation requires that the ncp is 0.")
    new TDistribution(rng, df)
  }
  private val stdT = mkT(1.0,0.0)
  def _dt(x: Double) = d(stdT, x)
  def _dt(x: Array[Double], df: Double, ncp: Double = 0.0, log: Boolean = false) = d(mkT(df,ncp),x,log)
  def _pt(q: Double) = p(stdT, q)
  def _pt(q: Array[Double], df: Double, ncp: Double = 0.0, lower_tail: Boolean = true) = p(mkT(df,ncp),q,lower_tail)
  def _qt(p: Double): Double = q(stdT, p)
  def _qt(p: Array[Double], df: Double, ncp: Double = 0.0, lower_tail: Boolean = true) = q(mkT(df,ncp),p,lower_tail)
  def _rt() = r(stdT)
  def _rt(n: Double, df: Double, ncp: Double = 0.0) = r(mkT(df,ncp),n)

  // Chi Squared distribution
  private def mkChiSquared(df: Double, ncp: Double) = {
    if ( ncp != 0.0 ) sys.error("The current implementation requires that the ncp is 0.")
    new ChiSquaredDistribution(rng, df)
  }
  private val stdChiSquared = mkChiSquared(1.0, 0.0)
  def _dchisq(x: Double) = d(stdChiSquared, x)
  def _dchisq(x: Array[Double], df: Double, ncp: Double = 0.0, log: Boolean = false) = d(mkChiSquared(df,ncp),x,log)
  def _pchisq(q: Double) = p(stdChiSquared, q)
  def _pchisq(q: Array[Double], df: Double, ncp: Double = 0.0, lower_tail: Boolean = true) = p(mkChiSquared(df,ncp),q,lower_tail)
  def _qchisq(p: Double): Double = q(stdChiSquared, p)
  def _qchisq(p: Array[Double], df: Double, ncp: Double = 0.0, lower_tail: Boolean = true) = q(mkChiSquared(df,ncp),p,lower_tail)
  def _rchisq() = r(stdChiSquared)
  def _rchisq(n: Double, df: Double, ncp: Double = 0.0) = r(mkChiSquared(df,ncp),n)

  // F distribution
  private def mkF(df1: Double, df2: Double, ncp: Double) = {
    if ( ncp != 0.0 ) sys.error("The current implementation requires that the ncp is 0.")
    new FDistribution(rng, df1, df2)
  }
  private val stdF = mkF(1.0, 1.0, 0.0)
  def _df(x: Double) = d(stdF, x)
  def _df(x: Array[Double], df1: Double, df2: Double, ncp: Double = 0.0, log: Boolean = false) = d(mkF(df1,df2,ncp),x,log)
  def _pf(q: Double) = p(stdF, q)
  def _pf(q: Array[Double], df1: Double, df2: Double, ncp: Double = 0.0, lower_tail: Boolean = true) = p(mkF(df1,df2,ncp),q,lower_tail)
  def _qf(p: Double): Double = q(stdF, p)
  def _qf(p: Array[Double], df1: Double, df2: Double, ncp: Double = 0.0, lower_tail: Boolean = true) = q(mkF(df1,df2,ncp),p,lower_tail)
  def _rf() = r(stdF)
  def _rf(n: Double, df1: Double, df2: Double, ncp: Double = 0.0) = r(mkF(df1,df2,ncp),n)

  // Binomial distribution
  private def mkBinomial(size: Double, prob: Double) = new BinomialDistribution(rng, r2int(size), prob)
  private val stdBinomial = mkBinomial(1.0, 0.5)
  def _dbinom(x: Double) = dI(stdBinomial, x)
  def _dbinom(x: Array[Double], size: Double, prob: Double, log: Boolean = false) = dI(mkBinomial(size,prob),x,log)
  def _pbinom(q: Double) = pI(stdBinomial, q)
  def _pbinom(q: Array[Double], size: Double, prob: Double, lower_tail: Boolean = true) = pI(mkBinomial(size,prob),q,lower_tail)
  def _qbinom(p: Double): Double = qI(stdBinomial, p)
  def _qbinom(p: Array[Double], size: Double, prob: Double, lower_tail: Boolean = true) = qI(mkBinomial(size,prob),p,lower_tail)
  def _rbinom() = rI(stdBinomial)
  def _rbinom(n: Double, size: Double, prob: Double) = rI(mkBinomial(size,prob),n)

  // Negative binomial distribution
  private def mkNegativeBinomial(size: Double, prob: Double, mu: Double) = {
    if ( ! isNaN(mu) ) sys.error("Parametrization with 'mu' is not implemented.")
    new PascalDistribution(rng, r2int(size), prob)
  }
  private val stdNegativeBinomial = mkNegativeBinomial(1.0, 0.5, NaN)
  def _dnbinom(x: Double) = dI(stdNegativeBinomial, x)
  def _dnbinom(x: Array[Double], size: Double, prob: Double, mu: Double = NaN, log: Boolean = false) = dI(mkNegativeBinomial(size,prob,mu),x,log)
  def _pnbinom(q: Double) = pI(stdNegativeBinomial, q)
  def _pnbinom(q: Array[Double], size: Double, prob: Double, mu: Double = NaN, lower_tail: Boolean = true) = pI(mkNegativeBinomial(size,prob,mu),q,lower_tail)
  def _qnbinom(p: Double): Double = qI(stdNegativeBinomial, p)
  def _qnbinom(p: Array[Double], size: Double, prob: Double, mu: Double = NaN, lower_tail: Boolean = true) = qI(mkNegativeBinomial(size,prob,mu),p,lower_tail)
  def _rnbinom() = rI(stdNegativeBinomial)
  def _rnbinom(n: Double, size: Double, prob: Double, mu: Double = NaN) = rI(mkNegativeBinomial(size,prob,mu),n)

  // Poisson distribution
  private def mkPoisson(lambda: Double) = new PoissonDistribution(rng, lambda, PoissonDistribution.DEFAULT_EPSILON, PoissonDistribution.DEFAULT_MAX_ITERATIONS)
  private val stdPoisson = mkPoisson(1.0)
  def _dpois(x: Double) = dI(stdPoisson, x)
  def _dpois(x: Array[Double], lambda: Double, log: Boolean = false) = dI(mkPoisson(lambda),x,log)
  def _ppois(q: Double) = pI(stdPoisson, q)
  def _ppois(q: Array[Double], lambda: Double, lower_tail: Boolean = true) = pI(mkPoisson(lambda),q,lower_tail)
  def _qpois(p: Double): Double = qI(stdPoisson, p)
  def _qpois(p: Array[Double], lambda: Double, lower_tail: Boolean = true) = qI(mkPoisson(lambda),p,lower_tail)
  def _rpois() = rI(stdPoisson)
  def _rpois(n: Double, lambda: Double) = rI(mkPoisson(lambda),n)

  // Geometric distribution
  private def mkGeometric(prob: Double) = new GeometricDistribution(rng, prob)
  private val stdGeometric = mkGeometric(0.5)
  def _dgeom(x: Double) = dI(stdGeometric, x)
  def _dgeom(x: Array[Double], prob: Double, log: Boolean = false) = dI(mkGeometric(prob),x,log)
  def _pgeom(q: Double) = pI(stdGeometric, q)
  def _pgeom(q: Array[Double], prob: Double, lower_tail: Boolean = true) = pI(mkGeometric(prob),q,lower_tail)
  def _qgeom(p: Double): Double = qI(stdGeometric, p)
  def _qgeom(p: Array[Double], prob: Double, lower_tail: Boolean = true) = qI(mkGeometric(prob),p,lower_tail)
  def _rgeom() = rI(stdGeometric)
  def _rgeom(n: Double, prob: Double) = rI(mkGeometric(prob),n)

}

