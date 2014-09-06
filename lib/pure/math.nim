#
#
#            Nimrod's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

##   Constructive mathematics is naturally typed. -- Simon Thompson
## 
## Basic math routines for Nimrod.
## This module is available for the `JavaScript target
## <backends.html#the-javascript-target>`_.

include "system/inclrtl"

{.push debugger:off .} # the user does not want to trace a part
                       # of the standard library!

{.push checks:off, line_dir:off, stack_trace:off.}

when defined(Posix) and not defined(haiku):
  {.passl: "-lm".}
when not defined(js):
  import times

const
  M_E* : float64 = 2.7182818284590452354 # e
  M_LOG2E* : float64 = 1.4426950408889634074 # log_2 e
  M_LOG10E* : float64 = 0.43429448190325182765 # log_10 e
  M_LN2* : float64 = 0.69314718055994530942 # log_e 2
  M_LN10* : float64 = 2.30258509299404568402 # log_e 10
  M_PI* : float64 = 3.14159265358979323846 # pi
  M_PI_2* : float64 = 1.57079632679489661923 # pi/2
  M_PI_4* : float64 = 0.78539816339744830962 # pi/4
  M_1_PI* : float64 = 0.31830988618379067154 # 1/pi
  M_2_PI* : float64 = 0.63661977236758134308 # 2/pi
  M_2_SQRTPI* : float64 = 1.12837916709551257390 # 2/sqrt(pi)
  M_SQRT2* : float64 = 1.41421356237309504880 # sqrt(2)
  M_SQRT1_2* : float64 = 0.70710678118654752440 # 1/sqrt(2)
  PI* = 3.1415926535897932384626433 ## the circle constant PI (Ludolph's number)
  E* = 2.71828182845904523536028747 ## Euler's number

  MaxFloat64Precision* = 16 ## maximum number of meaningful digits
                            ## after the decimal point for Nimrod's
                            ## ``float64`` type.
  MaxFloat32Precision* = 8  ## maximum number of meaningful digits
                            ## after the decimal point for Nimrod's
                            ## ``float32`` type.
  MaxFloatPrecision* = MaxFloat64Precision ## maximum number of 
                                           ## meaningful digits
                                           ## after the decimal point 
                                           ## for Nimrod's ``float`` type.

proc binom*(n, k: int): int {.noSideEffect.} = 
  ## computes the binomial coefficient
  if k <= 0: return 1
  if 2*k > n: return binom(n, n-k)
  result = n
  for i in countup(2, k):
    result = (result * (n + 1 - i)) div i
    
proc fac*(n: int): int {.noSideEffect.} = 
  ## computes the faculty/factorial function.
  result = 1
  for i in countup(2, n):
    result = result * i

proc isPowerOfTwo*(x: int): bool {.noSideEffect.} =
  ## returns true, if `x` is a power of two, false otherwise.
  ## Zero and negative numbers are not a power of two.
  return (x != 0) and ((x and (x - 1)) == 0)

proc nextPowerOfTwo*(x: int): int {.noSideEffect.} =
  ## returns `x` rounded up to the nearest power of two.
  ## Zero and negative numbers get rounded up to 1.
  result = x - 1 
  when defined(cpu64):
    result = result or (result shr 32)
  when sizeof(int) > 16:
    result = result or (result shr 16)
  when sizeof(int) > 8:
    result = result or (result shr 8)
  result = result or (result shr 4)
  result = result or (result shr 2)
  result = result or (result shr 1)
  result += 1 + ord(x<=0)

proc countBits32*(n: int32): int {.noSideEffect.} =
  ## counts the set bits in `n`.
  var v = n
  v = v -% ((v shr 1'i32) and 0x55555555'i32)
  v = (v and 0x33333333'i32) +% ((v shr 2'i32) and 0x33333333'i32)
  result = ((v +% (v shr 4'i32) and 0xF0F0F0F'i32) *% 0x1010101'i32) shr 24'i32

proc sum*[T](x: openArray[T]): T {.noSideEffect.} = 
  ## computes the sum of the elements in `x`. 
  ## If `x` is empty, 0 is returned.
  for i in items(x): result = result + i
  
proc mean*(x: openArray[TReal]): TReal {.noSideEffect.} = 
  ## computes the mean of the elements in `x`. 
  ## If `x` is empty, NaN is returned.
  result = sum(x) / toFloat(len(x)) # float64 and float32 are implicitly convertible
  
proc variance*(x: openArray[TReal]): TReal {.noSideEffect.} = 
  ## computes the variance of the elements in `x`. 
  ## If `x` is empty, NaN is returned.
  result = 0.0
  var m = mean(x)
  for i in 0 .. high(x):
    var diff = x[i] - m
    result = result + diff*diff
  result = result / toFloat(len(x))

proc random*(max: int): int {.gcsafe.}
  ## returns a random number in the range 0..max-1. The sequence of
  ## random number is always the same, unless `randomize` is called
  ## which initializes the random number generator with a "random"
  ## number, i.e. a tickcount.

proc random*(max: float): float {.gcsafe.}
  ## returns a random number in the range 0..<max. The sequence of
  ## random number is always the same, unless `randomize` is called
  ## which initializes the random number generator with a "random"
  ## number, i.e. a tickcount. This has a 16-bit resolution on windows
  ## and a 48-bit resolution on other platforms.

proc randomize*() {.gcsafe.}
  ## initializes the random number generator with a "random"
  ## number, i.e. a tickcount. Note: Does nothing for the JavaScript target,
  ## as JavaScript does not support this.
  
proc randomize*(seed: int) {.gcsafe.}
  ## initializes the random number generator with a specific seed.
  ## Note: Does nothing for the JavaScript target,
  ## as JavaScript does not support this.

when not defined(JS):
  proc fmin* (x, y: float64): float64 {.importc, header: "<math.h>".}
  proc fmax* (x, y: float64): float64 {.importc, header: "<math.h>".}
  proc fdim* (x, y: float64): float64 {.importc, header: "<math.h>".}
  proc fma* (x, y, z: float64): float64 {.importc, header: "<math.h>".} # x*y + z
  proc ceil* (x: float64): float64 {.importc, header: "<math.h>".}
  proc floor* (x: float64): float64 {.importc, header: "<math.h>".}
  proc trunc* (x: float64): float64 {.importc, header: "<math.h>".}
  proc nearbyint* (x: float64): float64 {.importc, header: "<math.h>".}
  proc round* (x: float64): float64 {.importc, header: "<math.h>".}
    ## converts a float to an int by rounding.  
  proc lround* (x: float64): clong {.importc, header: "<math.h>".}
  proc llround* (x: float64): clonglong {.importc, header: "<math.h>".}
  proc modf* (x: float64, intpart: var float64): float64 {.importc, header: "<math.h>".}
  proc fmod* (numer: float64, denom: float64): float64 {.importc, header: "<math.h>".}
  proc drem* (numer: float64, denom: float64): float64 {.importc, header: "<math.h>".}
  proc remainder* (numer: float64, denom: float64): float64 {.importc, header: "<math.h>".}
  proc fabs* (x: float64): float64 {.importc, header: "<math.h>".}
  proc nan* (tagp: cstring): float64 {.importc, header: "<math.h>".}
  proc nextafter* (x, y: float64): float64 {.importc, header: "<math.h>".}
  proc frexp* (x: float64, e: var cint): float64 {.importc, header: "<math.h>".}
    ## Split a number into mantissa and exponent.
    ## `frexp` calculates the mantissa m (a float greater than or equal to 0.5
    ## and less than 1) and the integer value n such that `x` (the original
    ## float value) equals m * 2**n. frexp stores n in `exponent` and returns
    ## m.
  proc ldexp* (x: float64, e: cint): float64 {.importc, header: "<math.h>".}
  proc sin* (x: float64): float64 {.importc, header: "<math.h>".}
  proc cos* (x: float64): float64 {.importc, header: "<math.h>".}
  proc tan* (x: float64): float64 {.importc, header: "<math.h>".}
  proc sincos* (x: float64, s: var float64, c: var float64) {.importc, header: "<math.h>".}
  proc asin* (x: float64): float64 {.importc, header: "<math.h>".}
  proc acos* (x: float64): float64 {.importc, header: "<math.h>".}
  proc atan* (x: float64): float64 {.importc, header: "<math.h>".}
  proc atan2* (x, y: float64): float64 {.importc, header: "<math.h>".}
    ## Calculate the arc tangent of `y` / `x`.
    ## `atan2` returns the arc tangent of `y` / `x`; it produces correct
    ## results even when the resulting angle is near pi/2 or -pi/2
    ## (`x` near 0).
  proc exp* (x: float64): float64 {.importc, header: "<math.h>".}
    ## computes e**x.
  proc exp2* (x: float64): float64 {.importc, header: "<math.h>".}
  proc log* (x: float64): float64 {.importc, header: "<math.h>".}
  proc log10* (x: float64): float64 {.importc, header: "<math.h>".}
  proc log2* (x: float64): float64 {.importc, header: "<math.h>".}
  proc logb* (x: float64): float64 {.importc, header: "<math.h>".}
  proc ilogb* (x: float64): cint {.importc, header: "<math.h>".}
  proc pow* (x, y: float64): float64 {.importc, header: "<math.h>".}
    ## computes x to power raised of y.
  proc sqrt* (x: float64): float64 {.importc, header: "<math.h>".}
    ## computes the square root of `x`.
  proc cbrt* (x: float64): float64 {.importc, header: "<math.h>".}
  proc hypot* (x, y: float64): float64 {.importc, header: "<math.h>".}
    ## same as ``sqrt(x*x + y*y)``.
  proc expm1* (x: float64): float64 {.importc, header: "<math.h>".}
  proc log1p* (x: float64): float64 {.importc, header: "<math.h>".}
  proc sinh* (x: float64): float64 {.importc, header: "<math.h>".}
  proc cosh* (x: float64): float64 {.importc, header: "<math.h>".}
  proc tanh* (x: float64): float64 {.importc, header: "<math.h>".}
  proc asinh* (x: float64): float64 {.importc, header: "<math.h>".}
  proc acosh* (x: float64): float64 {.importc, header: "<math.h>".}
  proc atanh* (x: float64): float64 {.importc, header: "<math.h>".}
  proc erf* (x: float64): float64 {.importc, header: "<math.h>".}
  proc erfc* (x: float64): float64 {.importc, header: "<math.h>".}
  proc lgamma_r* (x: float64, signp: var cint): float64 {.importc, header: "<math.h>".}
  proc gamma* (x: float64): float64 {.importc, header: "<math.h>".}
  proc tgamma* (x: float64): float64 {.importc, header: "<math.h>".}
  proc j0* (x: float64): float64 {.importc, header: "<math.h>".}
  proc j1* (x: float64): float64 {.importc, header: "<math.h>".}
  proc jn* (n: cint, x: float64): float64 {.importc, header: "<math.h>".}
  proc y0* (x: float64): float64 {.importc, header: "<math.h>".}
  proc y1* (x: float64): float64 {.importc, header: "<math.h>".}
  proc yn* (n: cint, x: float64): float64 {.importc, header: "<math.h>".}

  # single precision
  proc fminf* (x, y: float32): float32 {.importc, header: "<math.h>".}
  proc fmaxf* (x, y: float32): float32 {.importc, header: "<math.h>".}
  proc fdimf* (x, y: float32): float32 {.importc, header: "<math.h>".}
  proc fmaf* (x, y, z: float32): float32  {.importc, header: "<math.h>".} # x*y + z
  proc ceilf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc floorf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc truncf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc nearbyintf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc roundf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc lroundf* (x: float32): clong {.importc, header: "<math.h>".}
  proc llroundf* (x: float32): clonglong {.importc, header: "<math.h>".}
  proc modff* (x: float32, intpart: var float32): float32 {.importc, header: "<math.h>".}
  proc fmodf* (numer: float32, denom: float32): float32 {.importc, header: "<math.h>".}
  proc dremf* (numer: float32, denom: float32): float32 {.importc, header: "<math.h>".}
  proc remainderf* (numer: float32, denom: float32): float32 {.importc, header: "<math.h>".}
  proc fabsf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc nanf* (tagp: cstring): float32 {.importc, header: "<math.h>".}
  proc nextafterf* (x, y: float32): float32 {.importc, header: "<math.h>".}
  proc frexpf* (x: float32, e: var cint): float32 {.importc, header: "<math.h>".}
  proc ldexpf* (x: float32, e: cint): float32 {.importc, header: "<math.h>".}
  proc sinf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc cosf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc tanf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc sincosf* (x: float32, s: var float32, c: var float32) {.importc, header: "<math.h>".}
  proc asinf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc acosf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc atanf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc atan2f* (x, y: float32): float32 {.importc, header: "<math.h>".}
  proc expf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc exp2f* (x: float32): float32 {.importc, header: "<math.h>".}
  proc logf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc log10f* (x: float32): float32 {.importc, header: "<math.h>".}
  proc log2f* (x: float32): float32 {.importc, header: "<math.h>".}
  proc logbf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc ilogbf* (x: float32): cint {.importc, header: "<math.h>".}
  proc powf* (x, y: float32): float32 {.importc, header: "<math.h>".}
  proc sqrtf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc cbrtf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc hypotf* (x, y: float32): float32 {.importc, header: "<math.h>".}
  proc expm1f* (x: float32): float32 {.importc, header: "<math.h>".}
  proc log1pf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc sinhf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc coshf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc tanhf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc asinhf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc acoshf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc atanhf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc erff* (x: float32): float32 {.importc, header: "<math.h>".}
  proc erfcf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc lgammaf_r* (x: float32, signp: var cint): float32 {.importc, header: "<math.h>".}
  proc gammaf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc tgammaf* (x: float32): float32 {.importc, header: "<math.h>".}
  proc j0f* (x: float32): float32 {.importc, header: "<math.h>".}
  proc j1f* (x: float32): float32 {.importc, header: "<math.h>".}
  proc jnf* (n: cint, x: float32): float32 {.importc, header: "<math.h>".}
  proc y0f* (x: float32): float32 {.importc, header: "<math.h>".}
  proc y1f* (x: float32): float32 {.importc, header: "<math.h>".}
  proc ynf* (n: cint, x: float32): float32 {.importc, header: "<math.h>".}

  #
  let float64_epsilon*: float64 = nextafter (1.0'f64, 2.0'f64) - 1.0'f64
    ## machine epsilon for single precision
  let float32_epsilon*: float32 = nextafterf (1.0'f32, 2.0'f32) - 1.0'f32
    ## machine epsilon for double precision
    
  #
  proc fmin* (x, y: float32): float32 = fminf (x, y)
  proc fmax* (x, y: float32): float32 = fmaxf (x, y)
  proc fdim* (x, y: float32): float32 = fdimf (x, y)
  proc fma* (x, y, z: float32): float32 = fmaf (x, y, z)
  proc ceil* (x: float32): float32 = ceilf (x)
  proc floor* (x: float32): float32 = floorf (x)
  proc trunc* (x: float32): float32 = truncf (x)
  proc nearbyint* (x: float32): float32 = nearbyint (x)
  proc round* (x: float32): float32 = round (x)
  proc lround* (x: float32): clong = lround (x)
  proc llround* (x: float32): clonglong = llround (x)
  proc modf* (x: float32, intpart: var float32): float32 = modf (x, intpart)
  proc fmod* (numer: float32, denom: float32): float32 = fmodf (numer, denom)
  proc drem* (numer: float32, denom: float32): float32 = dremf (numer, denom)
  proc remainder* (numer: float32, denom: float32): float32 = remainderf (numer, denom)
  proc fabs* (x: float32): float32 = fabsf (x)
  proc nextafter* (x, y: float32): float32 = nextafterf (x, y)
  proc frexp* (x: float32, e: var cint): float32 = frexpf (x, e)
  proc ldexp* (x: float32, e: cint): float32 = ldexpf (x, e) 
  proc sin* (x: float32): float32 = sinf (x)
  proc cos* (x: float32): float32 = cosf (x)
  proc tan* (x: float32): float32 = tanf (x)
  proc sincos* (x: float32, s: var float32, c: var float32) = sincosf (x, s, c)
  proc asin* (x: float32): float32 = asinf (x)
  proc acos* (x: float32): float32 = acosf (x)
  proc atan* (x: float32): float32 = atanf (x)
  proc atan2* (x, y: float32): float32 = atan2f (x, y)
  proc arcsin* (x: float32): float32 = asinf (x)
  proc arccos* (x: float32): float32 = acosf (x)
  proc arctan* (x: float32): float32 = atanf (x)
  proc arctan2* (x, y: float32): float32 = atan2f (x, y)
  proc arcsin* (x: float64): float64 = asin (x)
  proc arccos* (x: float64): float64 = acos (x)
  proc arctan* (x: float64): float64 = atan (x)
  proc arctan2* (x, y: float64): float64 = atan2 (x, y)
  proc exp* (x: float32): float32 = expf (x)
  proc exp2* (x: float32): float32 = exp2f (x)
  proc log* (x: float32): float32 = logf (x)
  proc log10* (x: float32): float32 = log10f (x)
  proc log2* (x: float32): float32 = log2f (x)
  proc logb* (x: float32): float32 = logbf (x)
  proc ilogb* (x: float32): cint = ilogbf (x)
  proc pow* (x, y: float32): float32 = powf (x, y)
  proc sqrt* (x: float32): float32 = sqrtf (x)
  proc cbrt* (x: float32): float32 = cbrtf (x)
  proc hypot* (x, y: float32): float32 = hypotf (x, y)
  proc expm1* (x: float32): float32 = expm1f (x)
  proc log1p* (x: float32): float32 = log1pf (x)
  proc sinh* (x: float32): float32 = sinhf (x)
  proc cosh* (x: float32): float32 = coshf (x)
  proc tanh* (x: float32): float32 = tanhf (x)
  proc asinh* (x: float32): float32 = asinhf (x)
  proc acosh* (x: float32): float32 = acoshf (x)
  proc atanh* (x: float32): float32 = atanhf (x)
  proc erf* (x: float32): float32 = erff (x)
  proc erfc* (x: float32): float32 = erfcf (x)
  proc lgamma_r* (x: float32, signp: var cint): float32 = lgammaf_r (x, signp)
  proc gamma* (x: float32): float32 = gammaf (x)
  proc tgamma* (x: float32): float32 = tgammaf (x)
  proc j0* (x: float32): float32 = j0f (x)
  proc j1* (x: float32): float32 = j1f (x)
  proc jn* (n: cint, x: float32): float32 = jnf (n, x)
  proc y0* (x: float32): float32 = y0f (x)
  proc y1* (x: float32): float32 = y1f (x)
  proc yn* (n: cint, x: float32): float32 = ynf (n, x)

  proc ln*(x: float64): float64 = log (x)
    ## computes ln(x).
  proc ln*(x: float32): float32 = log (x)
    ## computes ln(x).    

when not defined(JS):  
  # C procs:
  proc gettime(dummy: ptr cint): cint {.importc: "time", header: "<time.h>".}
  proc srand(seed: cint) {.importc: "srand", header: "<stdlib.h>".}
  proc rand(): cint {.importc: "rand", header: "<stdlib.h>".}
  
  when not defined(windows):
    proc srand48(seed: clong) {.importc: "srand48", header: "<stdlib.h>".}
    proc drand48(): float {.importc: "drand48", header: "<stdlib.h>".}
    proc random(max: float): float =
      result = drand48() * max
  when defined(windows):
    proc random(max: float): float =
      # we are hardcodeing this because
      # importcing macros is extremely problematic
      # and because the value is publicly documented
      # on MSDN and very unlikely to change
      const rand_max = 32767
      result = (float(rand()) / float(rand_max)) * max
  proc randomize() =
    randomize(cast[int](epochTime()))

  proc randomize(seed: int) =
    srand(cint(seed))
    when declared(srand48): srand48(seed)
  proc random(max: int): int =
    result = int(rand()) mod max

else:
  proc mathrandom(): float {.importc: "Math.random", nodecl.}
  proc floor*(x: float): float {.importc: "Math.floor", nodecl.}
  proc ceil*(x: float): float {.importc: "Math.ceil", nodecl.}
  proc random(max: int): int =
    result = int(floor(mathrandom() * float(max)))
  proc random(max: float): float =
    result = float(mathrandom() * float(max))
  proc randomize() = discard
  proc randomize(seed: int) = discard
  
  proc sqrt*(x: float): float {.importc: "Math.sqrt", nodecl.}
  proc ln*(x: float): float {.importc: "Math.log", nodecl.}
  proc log10*(x: float): float = return ln(x) / ln(10.0)
  proc log2*(x: float): float = return ln(x) / ln(2.0)

  proc exp*(x: float): float {.importc: "Math.exp", nodecl.}
  proc round*(x: float): int {.importc: "Math.round", nodecl.}
  proc pow*(x, y: float): float {.importc: "Math.pow", nodecl.}
  
  proc frexp*(x: float, exponent: var int): float =
    if x == 0.0:
      exponent = 0
      result = 0.0
    elif x < 0.0:
      result = -frexp(-x, exponent)
    else:
      var ex = floor(log2(x))
      exponent = round(ex)
      result = x / pow(2.0, ex)

  proc arccos*(x: float): float {.importc: "Math.acos", nodecl.}
  proc arcsin*(x: float): float {.importc: "Math.asin", nodecl.}
  proc arctan*(x: float): float {.importc: "Math.atan", nodecl.}
  proc arctan2*(y, x: float): float {.importc: "Math.atan2", nodecl.}
  
  proc cos*(x: float): float {.importc: "Math.cos", nodecl.}
  proc cosh*(x: float): float = return (exp(x)+exp(-x))*0.5
  proc hypot*(x, y: float): float = return sqrt(x*x + y*y)
  proc sinh*(x: float): float = return (exp(x)-exp(-x))*0.5
  proc sin*(x: float): float {.importc: "Math.sin", nodecl.}
  proc tan*(x: float): float {.importc: "Math.tan", nodecl.}
  proc tanh*(x: float): float =
    var y = exp(2.0*x)
    return (y-1.0)/(y+1.0)

type
  TFloatClass* = enum ## describes the class a floating point value belongs to.
                      ## This is the type that is returned by `classify`.
    fcNormal,    ## value is an ordinary nonzero floating point value
    fcSubnormal, ## value is a subnormal (a very small) floating point value
    fcZero,      ## value is zero
    fcNegZero,   ## value is the negative zero
    fcNan,       ## value is Not-A-Number (NAN)
    fcInf,       ## value is positive infinity
    fcNegInf     ## value is negative infinity

proc classify* (x: float64): TFloatClass = 
  ## classifies a floating point value. Returns `x`'s class as specified by
  ## `TFloatClass`.
    
  # JavaScript and most C compilers have no classify:
  if x == 0.0:
    if 1.0/x == Inf:
      return fcZero
    else:
      return fcNegZero
  if x*0.5 == x:
    if x > 0.0: return fcInf
    else: return fcNegInf
  if x != x: return fcNan
  var ex: cint
  let fx: float64 = frexp (x, ex)
  if ex <= -1021 and fx < 0.5: return fcSubnormal # 2^-1022 is smallest normal
  return fcNormal
  # XXX: fcSubnormal is not detected!
  
proc classify* (x: float32): TFloatClass = 
  ## classifies a floating point value. Returns `x`'s class as specified by
  ## `TFloatClass`.
    
  # JavaScript and most C compilers have no classify:
  if x == 0.0:
    if 1.0/x == Inf:
      return fcZero
    else:
      return fcNegZero
  if x*0.5 == x:
    if x > 0.0: return fcInf
    else: return fcNegInf
  if x != x: return fcNan
  var ex: cint = 0
  let fx: float32 = frexp (x, ex)
  if ex <= -125 and fx < 0.5 : return fcSubnormal # 2^-126 is smallest normal
  return fcNormal
  # XXX: fcSubnormal is not detected!
  
proc `mod`*(x, y: float): float =
  result = if y == 0.0: x else: x - y * (x/y).floor

proc random*[T](x: TSlice[T]): T =
  ## For a slice `a .. b` returns a value in the range `a .. b-1`.
  result = random(x.b - x.a) + x.a

proc random[T](a: openarray[T]): T =
  ## returns a random element from the openarray `a`.
  result = a[random(a.low..a.len)]

type
  TRunningStat* {.pure,final.} = object  ## an accumulator for statistical data
    n*: int                              ## number of pushed data
    sum*, min*, max*, mean*: float       ## self-explaining
    oldM, oldS, newS: float

proc push*(s: var TRunningStat, x: float) = 
  ## pushes a value `x` for processing
  inc(s.n)
  # See Knuth TAOCP vol 2, 3rd edition, page 232
  if s.n == 1:
    s.min = x
    s.max = x
    s.oldM = x
    s.mean = x
    s.oldS = 0.0
  else:
    if s.min > x: s.min = x
    if s.max < x: s.max = x
    s.mean = s.oldM + (x - s.oldM)/toFloat(s.n)
    s.newS = s.oldS + (x - s.oldM)*(x - s.mean)

    # set up for next iteration:
    s.oldM = s.mean
    s.oldS = s.newS
  s.sum = s.sum + x
  
proc push*(s: var TRunningStat, x: int) = 
  ## pushes a value `x` for processing. `x` is simply converted to ``float``
  ## and the other push operation is called.
  push(s, toFloat(x))
  
proc variance*(s: TRunningStat): float = 
  ## computes the current variance of `s`
  if s.n > 1: result = s.newS / (toFloat(s.n - 1))

proc standardDeviation*(s: TRunningStat): float = 
  ## computes the current standard deviation of `s`
  result = sqrt(variance(s))

{.pop.}
{.pop.}

#
when isMainModule and not defined(JS):
  var pi32 : float32 = PI
  var pi64 : float64 = Pi
  assert ( pi32 != pi64 ) 
  assert ( classify (pi32) == fcNormal )
  assert ( classify (pi64) == fcNormal )
  var i32 : float32 = pi32 / 0.0;
  var i64 : float64 = pi64 / 0.0;
  assert ( classify (i32) == fcInf )
  assert ( classify (i64) == fcInf )
  assert ( classify (-i32) == fcNegInf )
  assert ( classify (-i64) == fcNegInf )
  assert ( 0*i32 != 0*i32 and 0*i32 != 0*i64 )
  assert ( classify (0*i32) == fcNaN )
  assert ( classify (0*i64) == fcNaN )
  assert ( classify (ldexp (1.0'f32, -126)) == fcNormal )
  assert ( classify (ldexp (0.99999'f32, -126)) == fcNormal )
  assert ( classify (ldexp (1.0'f64, -1022)) == fcNormal )
  assert ( classify (ldexp (0.9999999999'f64, -1022)) == fcNormal )
  assert ( mean([1.0'f32, 2.0'f32]) == 1.5'f32 )
  assert ( mean([1.0'f64, 2.0'f64]) == 1.5'f64 )
  assert ( variance([1.0'f32, 2.0'f32]) == 0.25'f32 )
  assert ( variance([1.0'f64, 2.0'f64]) == 0.25'f64 )
  block:
    var x: float32 = sin (0.0'f32)
    x = x - 1.0'f32 / 0.0'f32
    assert ( x == NegInf )
  block:
    var x : float64 = M_PI
    var y : float32 = float32 (M_PI)
    var z : float64 = float64 (y)
    assert ( x != z )
  # for sanity checks on base 2 IEEE systems
  proc machEpsilon [T] () : T =
    result = 1.0
    while (1.0 + result / 2.0 > 1.0):
      result = result / 2.0
  assert ( machEpsilon [float32] () == float32_epsilon )
  assert ( machEpsilon [float64] () == float64_epsilon )
  assert ( float64_epsilon < 1E-14 )
  assert ( float32_epsilon > 1E-8 )
  assert ( nan("") != nanf("") )
  assert ( sin(0'f64) == 0'f64 )
  assert ( sinf(0) == 0 )
  assert ( tan(0'f64) == 0'f64 )
  assert ( tanf(0) == 0 )
  assert ( asin(0'f64) == 0'f64 )
  assert ( asinf(0) == 0 )
  assert ( atan(0'f64) == 0'f64 )
  assert ( atanf(0) == 0 )
  assert ( cos(0'f64) == 1'f64 )
  assert ( cosf(0) == 1 )
  assert ( acos(1'f64) == 0'f64 )
  assert ( acosf(1) == 0 )
  assert ( sinh(0'f64) == 0'f64 )
  assert ( sinhf(0) == 0 )
  assert ( cosh(0'f64) == 1'f64 )
  assert ( coshf(0) == 1 )
  assert ( asinh(0'f64) == 0'f64 )
  assert ( asinhf(0) == 0 )
  assert ( acosh(1'f64) == 0'f64 )
  assert ( acoshf(1) == 0 )
  assert ( tanh(0'f64) == 0'f64 )
  assert ( tanhf(0) == 0 )
  assert ( atanh(0'f64) == 0'f64 )
  assert ( atanhf(0) == 0 )
  assert ( atan2(0'f64,1'f64) == 0'f64 )
  assert ( atan2f(0,1) == 0 )
  assert ( exp2(1'f64) == 2'f64 )
  assert ( exp2f(1) == 2 )
  assert ( log(1'f64) == 0'f64 )
  assert ( log10(10'f64) == 1'f64 )
  assert ( log2(2'f64) == 1'f64 )
  assert ( sqrt(4'f64) == 2'f64 )
  assert ( sqrtf(4) == 2 )
  assert ( cbrt(8'f64) == 2'f64 )
  assert ( cbrtf(8) == 2 )
  assert ( fabs(-1'f64) == 1'f64 )
  assert ( fabsf(-1) == 1 )
  assert ( expm1(0'f64) == 0'f64 )
  assert ( expm1f(0) == 0 )
  assert ( log1p(0'f64) == 0'f64 )
  assert ( log1pf(0) == 0 )
  assert ( hypot(1,0'f64) == 1'f64 )
  assert ( hypotf(0,1) == 1 )
  assert ( ceil(2.1) == 3 )
  assert ( ceilf(2.1) == 3 )
  assert ( floor(2.1) == 2 )
  assert ( floorf(2.1) == 2 )
  assert ( trunc(2.6) == 2 )
  assert ( truncf(2.6) == 2 )
  assert ( nearbyint(2.5) == 2 )
  assert ( nearbyintf(2.5) == 2 )
  assert ( round(2.5) == 3 )
  assert ( roundf(2.5) == 3 )
  assert ( lround(2.5) == 3 )
  assert ( lroundf(2.5) == 3 )
  assert ( llround(2.5) == 3 )
  assert ( llroundf(2.5) == 3 )
  assert ( 1.9 - fmod(6.5, 2.3) <= float64_epsilon )
  assert ( 1.9 - fmodf(6.5, 2.3) <= float32_epsilon )
  assert ( -0.4 - drem(6.5, 2.3) <= float64_epsilon )
  assert ( -0.4 - dremf(6.5, 2.3) <= float32_epsilon )
  assert ( fmin(1.2, 2.3) == 1.2 )
  assert ( fminf(1.2'f32, 2.3'f32) - 1.2'f32 <= float32_epsilon )
  assert ( fmax(1.2, 2.3) == 2.3 )
  assert ( fmaxf(1.2, 2.3) - 2.3 <= float32_epsilon )
  assert ( fma(1, 2, 3'f64) == 5'f64 )
  assert ( fmaf(1, 2, 3) == 5 )
  block:
    var f : float64
    let x = 2.78'f64
    let i = modf (x, f)
    assert ( x == i + f )
  block:
    var f : float32
    let x = 2.78'f32
    let i = modff (x, f)
    assert ( x == i + f )
  block:
    var e : cint
    let x = 2E3'f64
    let v = frexp (x, e)
    assert ( x == ldexp (v, e) )
  block:
    var e : cint
    let x = 2E3'f32
    let v = frexpf (x, e)
    assert ( x == ldexpf (v, e) )
  block:
    var s, c : float64
    sincos (0, s, c)
    assert ( s == 0 and c == 1 )
  block:
    var s, c : float32
    sincosf (0, s, c)
    assert ( s == 0 and c == 1 )
  echo "cmath: all pass!"
  
  # Verifies random seed initialization.
  let seed = gettime(nil)
  randomize(seed)
  const SIZE = 10
  var buf : array[0..SIZE, int]
  # Fill the buffer with random values
  for i in 0..SIZE-1:
    buf[i] = random(high(int))
  # Check that the second random calls are the same for each position.
  randomize(seed)
  for i in 0..SIZE-1:
    assert buf[i] == random(high(int)), "non deterministic random seeding"
  echo "random values equal after reseeding"
