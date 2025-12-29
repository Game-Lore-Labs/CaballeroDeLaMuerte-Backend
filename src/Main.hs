{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Infrastructure.Api

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "  RPG-BOT API Server (Clean Architecture)"
  putStrLn "=========================================="
  putStrLn "Servidor iniciado en: http://localhost:3000"
  putStrLn "=========================================="
  
  scotty 3000 api
