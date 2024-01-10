module Ch13.MockFileSystem where

import Data.Map (Map)

newtype MockFileSystem = MockFileSystem
  { getMockFileSystem :: Map FilePath String
  }
