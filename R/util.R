format.difftime = function(x) {
	s = as.numeric(x, units = "secs")
	dd = floor(s / (60^2 * 24))
	dd.resid = s / (60^2 * 24) - dd
	hh = floor(24*dd.resid)
	hh.resid = 24*dd.resid - floor(24*dd.resid)
	mm = floor(60*hh.resid)
	mm.resid = 60*hh.resid - floor(60*hh.resid)
	ss = floor(60*mm.resid)

	if (dd > 0) {
		fmt = sprintf("%02dd:%02dh:%02dm:%02ds", dd, hh, mm, ss)
	} else if (hh > 0) {
		fmt = sprintf("%02dh:%02dm:%02ds", hh, mm, ss)
	} else if (mm > 0) {
		fmt = sprintf("%02dm:%02ds", mm, ss)
	} else {
		fmt = sprintf("%0.2f sec", s)
	}

	return(fmt)
}

printf = function(msg, ...) {
	cat(sprintf(msg, ...))
}

logger = function(msg, ...)
{
	sys.time = as.character(Sys.time())
	cat(sys.time, "-", sprintf(msg, ...))
}

grad_fwd = function(f, x, h = 1e-5, ...) {
	k = length(x)
	eye = diag(1, k)
	res = numeric(k)
	fx = f(x, ...)
	for (j in 1:k) {
		res[j] = ( f(x + h * eye[,j], ...) - fx ) / h
	}
	return(res)
}

hess_fwd = function(f, x, h = 1e-5, ...) {
	k = length(x)
	eye = diag(1, k)
	H = matrix(NA, k, k)

	fx = f(x, ...)
	fx.eps = numeric(k)
	for (j in 1:k) {
		fx.eps[j] = f(x + h * eye[,j], ...)
	}

	for (j in 1:k) {
		for (l in 1:k) {
			num = f(x + h * eye[,j] + h * eye[,l], ...) -
				fx.eps[l] - fx.eps[j] + fx
			H[j,l] = num / h^2
		}
	}
	(H + t(H)) / 2
}

is.zero.matrix = function(X, eps = 1e-12)
{
	all(abs(X) < eps)
}

is.intercept.only = function(X, eps = 1e-12)
{
	n = length(X)
	all(dim(X) == c(n,1)) & is.zero.matrix(X-1, eps = eps)
}
