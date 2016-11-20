{-# Language DeriveDataTypeable #-}
module Network.Notam (fetchNotam) where

import Network.Wreq
import Control.Lens hiding (universe, children)
import Text.HTML.DOM as DOM
import Text.XML
import Text.XML.Lens
import Data.Generics.Uniplate.Operations (universeBi)
import Data.Generics.Uniplate.Data()
import Data.Text (Text)
import Data.Data (Data)

fetchNotam :: IO [(Text, Text)]
fetchNotam = do
    r <- get "https://ais.fi/ais/bulletins/envfrm.htm"
    return . parseNotam . extractNotam . DOM.parseLBS $ r ^. responseBody

data Parse = Header Text | Content Text deriving (Data, Show)
isContent :: Parse -> Bool
isContent (Content _) = True
isContent _ = False

parseNotam :: [Parse] -> [(Text, Text)]
parseNotam = go []
    where
        go acc [] = acc
        go acc (Content _ : xs) = go acc xs
        go acc (Header h : xs) =
            let (content, next) = span isContent xs
            in go ((h, mconcat [c | Content c <- content]):acc) next
extractNotam :: Document -> [Parse]
extractNotam doc =
    let elements = doc ^.. root . deep (el "body") ./ id
    in map extract elements ^.. traverse . _Just
    where extract (Element "h3" _ c) = Header <$> (c ^? traverse . _Element ./ el "a" . attribute "name" . _Just)
          extract (Element "div" _ c) = Just (Content (mconcat [v | NodeContent v <- universeBi c]))
          extract _ = Nothing

