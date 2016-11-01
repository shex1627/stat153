question1 = function(n, phi){
  data = arima.sim(n = n, list(ar = phi))
  AR1mod = ar(data, order.max = 1, method = "yw")#fits AR(1) model according to Yule-Walker
  ephi = AR1mod$ar #recovers the estimated phi value
  return(ephi)
}

q1 = function(n, phi){
  data = arima.sim(n = n, list(ar = phi))
  AR1mod = ar(data, order = 1, method = "yw")#fits AR(1) model according to Yule-Walker
  ephi = AR1mod$ar #recovers the estimated phi value
  if (length(ephi) == 0) {
    return(AR1mod)
  }
}