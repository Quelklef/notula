module Notula.Stdlib where

import Notula.Prelude

import Data.String.CodeUnits (slice)

stdlib :: String
stdlib = slice 1 (-1)
  """
# Notion null value
def null = [].at(0)

# Default a possibly-null value to something else
def x.orElse(y) = if(x == null, y, x)

# Apply an expression to a value, if it is not null
def x.mapNully(f) = if(x == null, x, let(it, x, f))

# Product of a list, via e^(ln a + ln b + ln c) = a * b * c
def list.prod = list.map(current.ln).sum.exp

# *-By variants
def list.sumBy(f) = list.map(f).sum()
def list.prodBy(f) = list.map(f).prod()
def list.minBy(f) = list.map(f).min()
def list.maxBy(f) = list.map(f).max()

# Produces the list [0, 1, 2, ..., n-1]
def rangeTo(n) = "x".repeat(n).split("").map(index)

# Round to a given precision
def x.toPrec(p) = let(scal, 10^p, round(x * scal) / scal)

# Rightwards-facing let()
def val.as(var, body) = let(var, val, body)

# Type checkers
def x.isNull = x == null
def x.isBool = [true, false].includes(x)
def x.isString = (x + "") == x
def x.isNumber = (x + "").toNumber() == x
"""
