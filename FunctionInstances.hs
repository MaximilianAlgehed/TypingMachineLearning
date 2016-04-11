{-# LANGUAGE TypeSynonymInstances #-}
module FunctionInstances where
import Data.Function

instance Num b => Num (a -> b) where
    f + g       = \x -> f x + g x
    f * g       = \x -> f x * g x
    abs         = (.) abs
    signum      = (.) signum
    negate      = (.) negate
    fromInteger = const . fromInteger

instance Fractional b => Fractional (a -> b) where
    f / g        = \x -> f x / g x
    fromRational = const . fromRational

instance Floating b => Floating (a -> b) where
    pi     = const pi 
    exp    = (.) exp  
    log    = (.) log
    sin    = (.) sin
    cos    = (.) cos 
    tan    = (.) tan
    asin   = (.) asin
    acos   = (.) acos
    atan   = (.) atan
    sinh   = (.) sinh
    cosh   = (.) cosh
    tanh   = (.) tanh
    asinh  = (.) asinh
    acosh  = (.) acosh
    atanh  = (.) atanh
    sqrt   = (.) sqrt
