{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.AttoLisp
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Control.Monad.State
import Lens.Micro
import Lens.Micro.Mtl
import Data.Maybe

import Data.Attoparsec.ByteString as Atto

import qualified Data.Text                  as T
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy.IO          as TL

type GState = (Count, IDS, NS, ES)
type Count  = Int
type IDS    = [(T.Text, Int)]
type NS     = [(Int, T.Text)]
type ES     = [(Int, Int, T.Text)]

type App = State GState
type App' = App (Maybe Int)

program1 :: B.ByteString
program1 = "(let (x 5) (a x) (b x) (baz (foo \"bar\" a b)))"

program2 :: B.ByteString
program2 = "(first 1000 (shuffle (outer-join-on-images (select-classes (animals cat dog) (project A)) (select-classes (classes person quadbike) (project B)))))"

main :: IO ()
main = do
  -- go program1
  -- go program2

  program <- B.getContents
  go program

go :: B.ByteString -> IO ()
go prog = do
  -- putStrLn ""
  -- BL.putStrLn "Program:"
  let g = Atto.parse lisp prog
  -- print g
  case g of
    Done _i r -> do
      putStrLn ""
      TL.putStrLn $ printDotGraph $ graphToDot quickParams $ graphpack r
    x -> do
      putStrLn "Error parsing expression"
      print x

graphpack :: Lisp -> Gr T.Text T.Text
graphpack l = mkGraph ns es where (_, _, ns, es) = execState (drafter l) (0,[],[],[])

drafter :: Lisp -> App'
drafter = \case
  (Symbol     t)              -> Just <$> respondSymbol t
  (String     t)              -> Just <$> respondSymbol t
  (Number     n)              -> Just <$> respondSymbol (T.pack (show n))
  (List      [])              -> return Nothing
  (List  (Symbol "let" : ts)) -> respondLet ts
  (List  (Symbol x     : ts)) -> respondCall x ts
  (List     [h])              -> drafter h
  (List  (h:ts))              -> respondList h ts
  (DotList ls d)              -> drafter (List (ls ++ [d]))

respondList :: Lisp -> [Lisp] -> App'
respondList h ts = do
  hi  <- drafter h
  tsi <- catMaybes <$> mapM drafter ts
  forOf_ _Just hi $ forM_ tsi . createEdgeTo
  return hi

respondCall :: T.Text -> [Lisp] -> App'
respondCall h ts = do
  hi  <- respondNewSymbol h
  tsi <- catMaybes <$> mapM drafter ts
  mapM_ (createEdgeTo hi) tsi
  return (Just hi)

respondLet :: [Lisp] -> App'
respondLet [] = return Nothing
respondLet [t] = drafter t
respondLet (h:ts) = drafter h >> respondLet ts

createEdgeTo :: Int -> Int -> App ()
createEdgeTo t f = do
  _4 %= prepend (f, t, " ")
  return ()

respondNewSymbol :: T.Text -> App Int
respondNewSymbol t = do
  i <- newSym t
  _3 %= prepend (i, t)
  return i

respondSymbol :: T.Text -> App Int
respondSymbol t = do
  i <- getSym t
  _3 %= prepend (i, t)
  return i

newSym :: T.Text -> App Int
newSym t = do
  i <- _1 <<%= succ
  _2 %= prepend (t, i)
  return i

getSym :: T.Text -> App Int
getSym t = zoom _2 (gets (lookup t)) >>= maybe (newSym t) return

prepend :: a -> [a] -> [a]
prepend = (:)
