module Network.Wai.Application.Types where

data AppSpec = AppSpec {
    softwareName :: String
  , indexFile :: FilePath
  }
