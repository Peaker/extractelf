{-# OPTIONS -Wall #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (forM_)
import Data.Elf (Elf(..), ElfSegment(..), ElfSection(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), (<>))
import Numeric (showHex)
import System.FilePath ((</>), (<.>))
import System.IO.Posix.MMap (unsafeMMapFile)
import qualified Data.ByteString as BS
import qualified Data.Elf as Elf
import qualified Options.Applicative as Opts
import qualified System.Directory as Dir

data Options = Options
  { _optFilenames :: FilePath
  , _outputDirName :: Maybe FilePath
  }

optsParser :: Opts.Parser Options
optsParser =
  Options
  <$> Opts.argument Opts.str (Opts.metavar "filename")
  <*> Opts.optional outputDirNameParser

outputDirNameParser :: Opts.Parser FilePath
outputDirNameParser =
  Opts.strOption $
     Opts.short 'o'
  <> Opts.long "output-prefix"
  <> Opts.metavar "output-directory"
  <> Opts.help "Output the ELF information files into this directory (defaults to <filename>.extracted)"

main :: IO ()
main = Opts.execParser opts >>= run
  where
    opts = Opts.info (Opts.helper <*> optsParser) mempty

-----------------

hex :: (Show a, Integral a) => a -> String
hex = (`showHex` "")

showSegment :: ElfSegment -> String
showSegment ElfSegment{..} = concat
  [ "VA: 0x", hex elfSegmentVirtAddr
  , ", PA: 0x", hex elfSegmentPhysAddr
  , ", Align: 0x", hex elfSegmentAlign
  , ", MemSize: 0x", hex elfSegmentMemSize
  , ", type:", show elfSegmentType
  , ", flags:", show elfSegmentFlags
  ]

sectionsDirName :: FilePath
sectionsDirName = "sections"

run :: Options -> IO ()
run (Options fileName mOutputDir) = do
  bs <- unsafeMMapFile fileName
  let outputDir = fromMaybe (fileName <.> "extracted") mOutputDir
  Dir.createDirectory outputDir
  Dir.createDirectory (outputDir </> sectionsDirName)
  let Elf{..} = Elf.parseElf bs
  writeFile (outputDir </> "hdr") . unlines . map unwords $
    [ [ "Class", show elfClass ]
    , [ "Data", show elfData ]
    , [ "Version", show elfVersion ]
    , [ "OSABI", show elfOSABI ]
    , [ "ABIVersion", show elfABIVersion ]
    , [ "Type", show elfType ]
    , [ "Machine", show elfMachine ]
    , [ "Entry", show elfEntry ]
    , [ "Segments:" ]
    ] ++
    map ((:[]) . ("  "++) . showSegment) elfSegments
  forM_ elfSections $ \ElfSection{..} -> do
    let
      sectionPrefix =
        outputDir </> sectionsDirName </>
        ("section" <.> elfSectionName)
    writeFile (sectionPrefix <.> "hdr") $
      unlines . map unwords $
      [ [ "Name", show elfSectionName ]
      , [ "  Type", show elfSectionType ]
      , [ "  Flgs", show elfSectionFlags ]
      , [ "  Addr", show elfSectionAddr ]
      , [ "  Size", show elfSectionSize ]
      , [ "  Link", show elfSectionLink ]
      , [ "  Info", show elfSectionInfo ]
      , [ "  Algn", show elfSectionAddrAlign ]
      , [ "  EnSz", show elfSectionEntSize ]
      ]
    BS.writeFile (sectionPrefix <.> "data") elfSectionData
