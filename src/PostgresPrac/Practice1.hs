{-# LANGUAGE OverloadedStrings #-}


module Practice1 where


import Database.PostgreSQL.Simple

hello :: IO Int
hello = do
  conn <- connectPostgreSQL ""
  [Only i] <- query_ conn "select 2 + 2"
  return i



  