module Notula.Stdlib where

import Notula.Prelude

import Data.String.CodeUnits (slice)

stdlib :: String
stdlib = slice 1 (-1)
  """
# Notion null value
null() = [].at(0) ;

# Default a possibly-null value to something else
x.orElse(y) = if(x == null(), y, x) ;

# Apply an expression to a value, if it is not null
x.mapNully(f) = if(x == null(), x, let(it, x, f)) ;

# Product of a list, via e^(ln a + ln b + ln c) = a * b * c
list.prod = list.map(current.ln).sum.exp ;

# -By variants
list.sumBy(f) = list.map(f).sum() ;
list.prodBy(f) = list.map(f).prod() ;
list.minBy(f) = list.map(f).min() ;
list.maxBy(f) = list.map(f).max() ;

# Produces the list [0, 1, 2, ..., n-1]
rangeTo(n) = "x".repeat(n).split("").map(index) ;

# Not working right now
# I think because it introduces a variable but isn't lets()?
val.as(var, body) = let(var, val, body) ;

# Type checkers
x.isNull = x == null() ;
x.isBool = [true, false].includes(x) ;
x.isString = (x + "") == x ;
x.isNumber = (x + "").toNumber() == x ;

# Parsers
stuff.getString(name) = stuff.match(name + "=\S*").at(0).split("=").at(1) ;
stuff.getBool(name) = stuff.getString(name) == "true" ;
stuff.getNumber(name) = stuff.getString(name).toNumber() ;
stuff.getDate(name) = stuff.getString(name).toNumber().fromTimestamp() ;
"""
