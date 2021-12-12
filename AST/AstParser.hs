module AstParser where

type Parser a = String -> [(a, String)]