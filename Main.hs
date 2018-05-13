{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Stephen Diehl 2013
-- License   :  MIT
-- Maintainer:  stephen.m.diehl@gmail.com
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------

module Main where

import Data.List
import System.FilePath.Posix
import Hakyll
import Hakyll.Web.Pandoc
import Text.Pandoc
import Data.Monoid (mappend)
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Pandoc.Options

--------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  `mappend` mathCtx
  `mappend` defaultContext

mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
  metadata <- getMetadata $ itemIdentifier item
  return $ case "mathjax" `lookupString` metadata of
            Just "on" -> "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
            _ -> ""

archiveCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Blog"
  `mappend` defaultContext

indexCtx posts =
  listField "posts" postCtx (return posts)
  `mappend` constField "title" "Home"
  `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

--------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)
  where idx = "index.html"
        cleanIndex url
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url

--------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
                            where p = toFilePath ident

static :: Rules ()
static = do
  match "fonts/*" $ do
    route idRoute
    compile $ copyFileCompiler
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  match "js/*" $ do
    route idRoute
    compile $ copyFileCompiler

pages :: Rules ()
pages = do
  match "pages/*" $ do
    route $ cleanRoute
    compile $ compiler
      >>= loadAndApplyTemplate "templates/page.html" postCtx
      >>= relativizeUrls
      >>= cleanIndexUrls 

posts :: Rules ()
posts = do
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  match "posts/*" $ do
    route $ cleanRoute
    compile $ compiler
      >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
      >>= relativizeUrls
      >>= cleanIndexUrls 

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route cleanRoute
    compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" title
                  `mappend` listField "posts" postCtx (return posts)
                  `mappend` defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/blog.html" ctx
            >>= relativizeUrls
            >>= cleanIndexUrls 

archive :: Rules ()
archive = do
  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/blog.html" (archiveCtx posts)
        >>= relativizeUrls
        >>= cleanIndexUrls 

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

favicon :: Rules ()
favicon = do
  match "favicon.ico" $ do
    route idRoute
    compile $ copyFileCompiler
--------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------

compiler :: Compiler (Item String)
compiler = pandocCompilerWith pandocROptions pandocWOptions

pandocROptions :: ReaderOptions
pandocROptions = defaultHakyllReaderOptions { readerExtensions = exts }
    where exts = foldl' (flip enableExtension) (readerExtensions defaultHakyllReaderOptions) [Ext_pipe_tables, Ext_backtick_code_blocks]

pandocWOptions :: WriterOptions
pandocWOptions = defaultHakyllWriterOptions{ writerHTMLMathMethod = MathJax "" }

cfg :: Configuration
cfg = defaultConfiguration

main :: IO ()
main = hakyllWith cfg $ do
  favicon
  static
  pages
  posts
  archive
  templates
