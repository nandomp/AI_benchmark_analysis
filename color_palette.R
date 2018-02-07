library("colorspace")

ICCcolour <- function (n, h = c(0, -163), c. = c(100, 100), l = c(24, 75), 
    power = c(1, 1.04444444444444), fixup = TRUE, gamma = NULL, 
    alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- rep(c., length.out = 2L)
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
        C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) * 
            rval), fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}

ICCcolour2 <- function (n, h = c(130, 48), c = 100, l = c(67, 92), power = 0.644444444444444, 
          fixup = TRUE, gamma = NULL, alpha = 1, ...) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  h <- rep(h, length.out = 2L)
  c <- c[1L]
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(1, -1, length = n)
  rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L], 
                       C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L], 
                                                               h[2L])), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}

ICCcolour3 <- function (n, c = 100, l = 70, start = 0, end = 251, fixup = TRUE, 
                        gamma = NULL, alpha = 1, ...) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  rval <- hex(polarLUV(L = l, C = c, H = seq(start, end, length = n)), 
              fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}


ICCcolour4 <- function (n, c = 80, l = 69, start = 0, end = 213, fixup = TRUE, 
                        gamma = NULL, alpha = 1, ...) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  rval <- hex(polarLUV(L = l, C = c, H = seq(start, end, length = n)), 
              fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}

#library("colorspace")
#pal <- choose_palette()

ICCcolur.filter <- function (n, c = 84, l = 70, start = 0, end = 224, fixup = TRUE, 
                             gamma = NULL, alpha = 1, ...) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  rval <- hex(polarLUV(L = l, C = c, H = seq(start, end, length = n)), 
              fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}

ICC.diverging <- function (n, h = c(260, 0), c = 100, l = c(31, 90), power = 0.511111111111111, 
                           fixup = TRUE, gamma = NULL, alpha = 1, ...) 
{
  if (!is.null(gamma)) 
    warning("'gamma' is deprecated and has no effect")
  if (n < 1L) 
    return(character(0L))
  h <- rep(h, length.out = 2L)
  c <- c[1L]
  l <- rep(l, length.out = 2L)
  power <- rep(power, length.out = 2L)
  rval <- seq(-1, 1, length = n)
  rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L], 
                       C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L], 
                                                               h[2L])), fixup = fixup, ...)
  if (!missing(alpha)) {
    alpha <- pmax(pmin(alpha, 1), 0)
    alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), 
                    width = 2L, upper.case = TRUE)
    rval <- paste(rval, alpha, sep = "")
  }
  return(rval)
}



