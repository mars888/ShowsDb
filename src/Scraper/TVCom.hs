{-# LANGUAGE NoMonomorphismRestriction #-}
module Scraper.TVCom where

import Text.HTML.Download
import Text.HTML.TagSoup
import Control.Monad

test = getEpisodes "http://www.tv.com/lost/show/24313" 5

getNumberOfSeasons baseUrl = do
    tags <- liftM parseTags (openURL fullUrl)
    let fTags = filter (~== "<li>") $ filter isTagOpen $ takeWhile (~/= "</ul>") $ dropWhile (~/= "<ul class=TAB_LINKS>") tags
    -- Remove 3, to account for "Special", "All", and "Top episodes" links.
    return $ (length fTags) - 3
    where fullUrl = baseUrl ++ "/episode.html"


getEpisodes baseUrl season = do
    tags <- liftM parseTags (openURL fullUrl)
    let eps = map getEpisode $ sections (~== "<div class=info>") tags
    return eps
    where fullUrl = baseUrl ++ "/episode.html?season=" ++ (show season)

getEpisode tags = do
    synop
    where meta  = grabTextBetweenTags "<div class=meta>" "</div>" tags
          title = grabTextBetweenTags "<h3>" "</h3>"              tags
          synop = grabTextBetweenTags "<p class=synopsis>" "</p>" tags


grabBetweenTags :: (TagRep a, TagRep b) => a -> b -> [Tag] -> [Tag]
grabBetweenTags start end tags = takeWhile (~/= end) . dropWhile (~/= start) $ tags

grabTextBetweenTags :: (TagRep a, TagRep b) => a -> b -> [Tag] -> String
grabTextBetweenTags start end tags = unwords . words . innerText $ (grabBetweenTags start end tags)
