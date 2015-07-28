{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------
-- |
-- Module    : ghc-gc-tune
-- Copyright : (c) Don Stewart, 2010-2012
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons00@gmail.com>
-- Stability : provisional
-- Portability: Needs a few libraries from hackage.
--
--------------------------------------------------------------------
--
-- Graph the performance of a Haskell program as GC settings change.
--
-- Examples:
--
-- > ghc-gc-tune a.out
--

------------------------------------------------------------------------

import System.IO
import System.Exit
import System.Environment
import System.Directory
import System.Console.GetOpt
import Data.List
import Data.Char
import Data.Maybe
import Data.Int
import Data.Function
import Data.Ord
import Control.Monad
import Control.Concurrent
import Text.Printf
import System.Process hiding (readProcess)
import qualified Control.Exception as C

import System.FilePath

------------------------------------------------------------------------

{-

Goals:

 * Use criterion to time the computation
 * ADT for flags.
 * Collect 3D values. -H -A time
 * Render to gnuplot

 -}

------------------------------------------------------------------------
--
-- Command line parsing
--

data Options = Options
  { optHelp :: Bool
  , optType :: Maybe String
  , optAmin :: !Int64
  , optAmax :: !Int64
  , optHmin :: !Int64
  , optHmax :: !Int64
  , optMmax :: Maybe Int64
  , optTime :: Bool
  , optPeak :: Bool
  , optResi :: Bool
  , optConf :: Bool
  } deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
  { optHelp = False
  , optType = Nothing
  , optAmin = k 16
  , optAmax = m 512
  , optHmin = m 1
  , optHmax = g 1
  , optMmax = Nothing
  , optTime = True
  , optPeak = False
  , optResi = False
  , optConf = False
  }

options :: [OptDescr (Options -> Options)]
options =
    [   Option ['h'] ["help"]
            (NoArg (\opts -> opts { optHelp = True }))
            "Print this help message."

    ,    Option ['t'] ["type"]
            (ReqArg (\x opts -> opts { optType = Just x }) "TYPE")
            "Output type: pdf, png, svg, pngcairo, html."

    ,   Option [] ["Amin"]
            (ReqArg (\x opts -> opts { optAmin = read x }) "N")
            "Minimum allocation size to sample (default 16K)."

    ,   Option [] ["Amax"]
            (ReqArg (\x opts -> opts { optAmax = read x }) "N")
            "Maximum allocation size to sample (default 512M)."

    ,   Option [] ["Hmin"]
            (ReqArg (\x opts -> opts { optHmin = read x }) "N")
            "Minimum initial heap size to sample (default 1M)."

    ,   Option [] ["Hmax"]
            (ReqArg (\x opts -> opts { optHmax = read x }) "N")
            "Maximum initial heap size to sample (default 1G)."

    ,   Option [] ["Mmax"]
            (ReqArg (\x opts -> opts { optMmax = Just (read x) }) "N")
            "Maximal heap size for process (default unlimited)."

    ,   Option ['s'] ["Time"]
            (NoArg (\opts -> opts { optTime = True }))
            "Create graph of time for different GC settings (default True)."

    ,   Option ['p'] ["Peak"]
            (NoArg (\opts -> opts { optPeak = True }))
            "Create graph of peak memory allocation for different GC settings\
            \ (default False)."

    ,   Option ['r'] ["Residency"]
            (NoArg (\opts -> opts { optResi = True }))
            "Create graph of maximum resident memory for different GC settings\
            \ (default False)."

    ,   Option ['c'] ["Config"]
            (NoArg (\opts -> opts { optConf = True }))
            ("Create config file with personal default options.\n" ++ configMessage)

    ]

configMessage :: String
configMessage = unlines
    [   "The default options can be overridden in $HOME/.ghc-gc-tune.config"
    ,   "Sizes can be given in bytes or with suffix 'K', 'M', 'G' (lower case accepted)."
    ,   "Comment out lines with \"--\"."
    ]

parseOptions :: Options -> [String] -> IO (Options, [String])
parseOptions deflt argv =
    case getOpt RequireOrder options argv of
        (o, n, []) -> let o' = foldl (flip ($)) deflt o in
                        if optHelp o'
                            then do hPutStr stderr (usageInfo header options)
                                    exitWith ExitSuccess
                            else
                              if optConf o'
                                then do writeConfig o'
                                        exitWith ExitSuccess
                                else return (o', n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))

header :: String
header = "Usage: ghc-gc-tune [OPTS...] ./path/to/executable [PROG_ARGS...] [--RTS +RTS GHC_RTS_OPTIONS...]"

writeConfig :: Options -> IO ()
writeConfig o = do
    home <- getHomeDirectory
    writeFile (home </> ".ghc-gc-tune.config") $ defaults o

defaults :: Options -> String
defaults o = unlines
    [   "-- Default configuration for ghc-gc-tune"
    ,   "\n-- minimal and maximal sizes of allocation area"
    ,   "Allocation:"
    ,   "    Amin:   " ++ show (optAmin o)
    ,   "    Amax:   " ++ show (optAmax o)
    ,   "\n-- minimal and maximal sizes of initial heap"
    ,   "Heap:"
    ,   "    Hmin:   " ++ show (optHmin o)
    ,   "    Hmax:   " ++ show (optHmax o)
    ,   "\n-- maximal heap size"
    ,   case optMmax o of
            Nothing -> "--    Mmax:   1G"
            Just mm -> "    Mmax:   " ++ show mm
    ,   ""
    ,   "Time:       " ++ show (optTime o)
    ,   "Peak:       " ++ show (optPeak o)
    ,   "Residency:  " ++ show (optResi o)
    ]

------------------------------------------------------------------------

--
-- | A data type to represent the output of +RTS -t --machine-readable
--
-- <http://haskell.org/ghc/docs/6.12.1/html/users_guide/runtime-control.html#rts-options-gc>
--
-- * The peak memory the RTS has allocated from the OS.
-- * The maximum resident memory
-- * The amount of CPU time and elapsed wall clock time
--
data GCStats = GCStats
    {   numberOfGCs :: !Int64   -- ^ total number of GCs
    ,   peakMemory  :: !Int64   -- ^ peak memory allocated
    ,   maxResident :: !Int64   -- ^ maximum resident memory
    ,   mutatorTime :: !Double  -- ^ wall clock mutator time
    ,   gcTime      :: !Double  -- ^ wall clock GC time
    ,   totalTime   :: !Double  -- ^ sum of init, exit, gc, mutator
    }
    deriving Show

parse :: String -> GCStats
parse s = case maybeRead s of
    Nothing     -> error $ "Can't parse GC stats: " ++ show s
    Just (assocs :: [(String,String)]) ->

        let mgc = do
                numberOfGCs <- fmap read $ lookup "num_GCs" assocs
                peakMemory  <- fmap read $ lookup "peak_megabytes_allocated" assocs
                maxResident <- fmap read $ lookup "max_bytes_used" assocs
                mutatorTime <- fmap read $ lookup "mutator_wall_seconds" assocs
                gcTime      <- fmap read $ lookup "GC_wall_seconds" assocs
                return GCStats { totalTime   = mutatorTime + gcTime , .. }
                                           -- ^ record pun.

        in case mgc of
            Nothing -> error $ "Missing fields in GC stats: " ++ show s
            Just gc -> gc
    where
        maybeRead z = case reads z of
            [(x, s')] | all isSpace s' -> Just x
            _                          -> Nothing

------------------------------------------------------------------------
-- Data type for controlling the GC.

data GCHooks =
    GCHooks {
        -- | Set the allocation area size used by the garbage collector. The
        -- allocation area (actually generation 0 step 0) is fixed and is
        -- never resized (unless you use -H, below).
          sizeA :: !Int64

        -- | This option provides a “suggested heap size” for the garbage
        -- collector. The garbage collector will use about this much memory
        -- until the program residency grows and the heap size needs to be
        -- expanded to retain reasonable performance.
        , sizeH :: !Int64

        -- | Set the maximal heap size to preven thrashing.
        , sizeM :: Maybe Int64
    }
    deriving Show

k,m,g :: Int64 -> Int64
k x =   x * 1024
m x = k x * 1024
g x = m x * 1024

bToMB :: Int64 -> Double
bToMB b = fromIntegral b / 1048576
-- bToMB b = case b `quotRem` 1048576 of
--             (q,r)
--                 | r < 524288 -> q
--                 | otherwise  -> q+1

--
-- static defaults for various A and H sizes. Need an algorithm to determine these
-- based on available memory, L2 cache size, and application's default maximum heap
--
series :: Int64 -> Int64 -> [Int64]
series n h = takeWhile (<= h) . dropWhile (< n) $ iterate (*2) 8192

tuningSpace :: Options -> [GCHooks]
tuningSpace opts = [GCHooks a h mm | a <- series minA maxA
                                   , h <- series minH maxH]
      where
        mm   = optMmax opts
        minA = optAmin opts
        maxA = case mm of
                 Nothing -> optAmax opts
                 Just c  -> min c (optAmax opts)
        minH = optHmin opts
        maxH = case mm of
                 Nothing -> optHmax opts
                 Just c  -> min c (optHmax opts)

------------------------------------------------------------------------

getDefaultOptions :: IO Options
getDefaultOptions =
    C.handle (\(_ :: C.SomeException) -> return defaultOptions)
    (do
    home <- getHomeDirectory
    let conf = home </> ".ghc-gc-tune.config"
    haveConfig <- doesFileExist conf
    if haveConfig
        then fmap (parseConfig defaultOptions . lines) $ readFile conf
        else return defaultOptions
    )

main :: IO ()
main = do
    dflt <- getDefaultOptions
    -- Parse command line
    (opts, args') <- getArgs >>= parseOptions dflt
    (exe,args)    <- case args' of
                      []     -> ioError (userError (usageInfo header options))
                      (x:xs) -> return (x,xs)

    -- Now traverse the space
    stats <- forM (tuningSpace opts) $ \hooks -> do
                 s <- runGHCProgram exe args hooks
                 case s of
                   Just y ->
                      printf "\t<<GCs %6d, peak %5d, resident %5.2fm, MUT %4.3fs, GC %4.3fs>>\n"
                                     (numberOfGCs y)
                                     (peakMemory y)
                                     (bToMB $ maxResident y)
                                     (mutatorTime y)
                                     (gcTime y)
                   Nothing -> return ()
                 return (hooks, s)

    when (null . catMaybes . map snd $ stats)  $ do
        error "All program runs failed, unable to collect data."

    when (optTime opts) (best exe opts "time" "Running time" "s" "seconds" totalTime stats)
    when (optPeak opts) (best exe opts "peak" "Peak memory" "MB" "MB" (fromIntegral . peakMemory) stats)
    when (optResi opts) (best exe opts "residency" "Resident memory" "MB" "MB" (bToMB . maxResident) stats)
    when (optTime opts && optResi opts)
        (best exe opts "integ" "Residency*Time" "MBs" "MBs" (\s -> totalTime s * bToMB (maxResident s)) stats)

-- stats-output separate for various outputs

best :: String -> Options -> String -> String -> String -> String -> (GCStats -> Double) -> [(GCHooks, Maybe GCStats)] -> IO ()
best exe opts short title unit longunit field stats = do
    let best5 = take 5 $ sortBy (comparing thd3)
                    [(sizeA gs, sizeH gs, field r) | (gs, Just r) <- stats]
    putStrLn $ "Best settings for " ++ title ++ ":"
    forM best5 $ \(bestA, bestH, bestF) ->
        printf "%.2f%s:  +RTS -A%d -H%d\n" bestF unit bestA bestH

    -- TODO graph Z as time or space.
    -- x is A, y is H, z is total time
    let space = groupBy ((==) `on` fst3)
                 [ (sizeA gs, sizeH gs, field r) | (gs,Just r) <- stats ]


    C.bracket
        (openTempFile "/tmp" ("ghc-gc-tune-" ++ short ++ "-XXXX.dat"))
        (\(f,_) -> removeFile f)
        $ \(f,h) -> do

            -- generate the data file for gnuplot
            hPutStr h $
                 concatMap (\s -> case s of
                         []         -> "\n" -- blank line between Y lines
                         xs         -> unlines $
                                map (\(x,y,z) -> intercalate " " [show x, show y, show z]) xs
                 ) (intersperse [] space)

            hFlush h >> hClose h

            -- construct the gp script
            let script = plot3d f exe short title longunit (optType opts)

            -- get a handle to the gnuplot process
            (ih,_,eh,pid) <- C.handle
                (\(_::C.SomeException) -> error $ "Couldn't fork gnuplot.")
                (do mgnu <- findExecutable "gnuplot"
                    case mgnu of
                        Nothing      -> error "Cannot find gnuplot"
                        Just gnuplot -> runInteractiveCommand gnuplot)

            -- print script into gnuplot
            hPutStrLn ih script -- >> hClose ih -- write into gnuplot
            hFlush ih

            -- If interactive, tell them to use ^C^C to kill

            case optType opts of
                Nothing -> do
                        putStrLn "Rendering ... type q and ^C^C to exit interactive mode"

                        -- interactive keep it open.
                        hGetContents eh -- >>= putStr       -- dump any error output it produces
                        waitForProcess pid
                        return ()

                Just t  -> do
                        putStrLn $ "Output written to : " ++ outputFile exe short t
                        hClose ih
                        hGetContents eh -- >>= putStr       -- dump any error output it produces
                        waitForProcess pid
                        return ()


-- Work out the output file from the source name and the file type
outputFile :: FilePath -> String -> String -> FilePath
outputFile f what ty = "/tmp/" ++ takeFileName f ++ "-" ++ what ++"-gc-space." ++ realTy
    where
        realTy = case ty of
                   "pngcairo" -> "png"
                   _          -> ty

------------------------------------------------------------------------
-- 3d plot

plot3d :: FilePath -> FilePath -> String -> String -> String -> Maybe String -> String
plot3d datFile srcFile short what unit mty = script
  where script = unlines
           [ "set logscale xy"
           , "set title \"" ++ what ++ " of " ++ takeFileName srcFile ++
                             " as a function of GC -A and -H values\""
           , "set pm3d at b"
           , "set xlabel \"-A (allocation area)\""
           , "set ylabel \"-H (suggested heap)\""
           , "set zlabel \"" ++ short ++ " (" ++ unit ++")\" rotate by 90"
           , "unset key"
           , "set xtics (\"16k\"  16384 ,\"64k\"  65536 ,\"256k\" 262144 ,\"1M\"   1048576 ,\"4M\"   4194304 ,\"16M\"  16777216 , \"64M\" 67108864, \"256M\" 268435456 ,\"1G\"   1073741824, \"4G\" 4294967296, \"16G\" 17179869184, \"64G\" 68719476736)"
           , "set ytics (\"16k\"  16384 ,\"64k\"  65536 ,\"256k\" 262144 ,\"1M\"   1048576 ,\"4M\"   4194304 ,\"16M\"  16777216 , \"64M\" 67108864, \"256M\" 268435456 ,\"1G\"   1073741824, \"4G\" 4294967296, \"16G\" 17179869184, \"64G\" 68719476736)"
           , case mty of
                Nothing    -> "#" -- interactive

                Just "png" -> unlines
                   ["set terminal png enhanced font \",8\""
                   ,"set output \"" ++ outputFile srcFile short "png" ++ "\""]

                Just "pngcairo" -> unlines
                   ["set terminal pngcairo enhanced font \",8\""
                   ,"set output \"" ++ outputFile srcFile short "png" ++ "\""]

                Just "pdf" -> unlines
                   ["set terminal pdfcairo enhanced font \",8\""
                   ,"set output \"" ++ outputFile srcFile short "pdf" ++ "\""]

                Just "svg" -> unlines
                   ["set terminal svg dynamic enhanced font \",8\""
                   ,"set output \"" ++ outputFile srcFile short "svg" ++ "\""]

                Just "html" -> unlines
                   ["set terminal canvas enhanced font \",8\""
                   ,"set output \"" ++ outputFile srcFile short "html" ++ "\""]

                Just _     -> "#"

           , "splot \"" ++ datFile ++ "\" with lines"
           ]

------------------------------------------------------------------------

--
-- Run a GHC-compiled program with -t --machine-readable, passing
-- any user supplied args and RTS flags through as well.
--
-- The program needs to run to completion with a successful exit code.
-- If the user passes +RTS flags they'll need to add -RTS so we don't
-- clobber them. Perhaps filter for this.
--
runGHCProgram :: FilePath -> [String] -> GCHooks -> IO (Maybe GCStats)
runGHCProgram exe opts gcflags = do

    printf "%s %s\n" exe (intercalate " " $ tuningargs ++ opts)

    x <- readProcessStderr exe (rtsargs ++ tuningargs ++ opts) []
    case x of
         Left (err,str,std) -> do
            mapM_ putStrLn (lines str)
            mapM_ putStrLn (lines std)
            printf "Executable failed with error %s\n" (show err)
            return Nothing

                -- drop "test/binary-trees 16 +RTS -t --machine-readable \n "
         Right str     -> return $! Just $! parse str

  where
    rtsargs     = words "+RTS -t --machine-readable -RTS"

    tuningargs  = ["+RTS"
                  ,"-A" ++ show (sizeA gcflags)
                  ,"-H" ++ show (sizeH gcflags)
                  ] ++ case sizeM gcflags of
                        Nothing -> ["-RTS"]
                        Just mb -> ["-M" ++ show mb, "-RTS"]


--
-- Strict process reading (we only care about stderr)
--
readProcessStderr :: FilePath                              -- ^ command to run
            -> [String]                              -- ^ any arguments
            -> String                                -- ^ standard input
            -> IO (Either (ExitCode,String,String) String)  -- ^ either the stderr, or an exitcode and any output

readProcessStderr cmd args input = C.handle (return . handler) $ do
    (inh,outh,errh,pid) <- runInteractiveProcess cmd args Nothing Nothing

    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ (C.evaluate (length output) >> putMVar outMVar ())

    errput  <- hGetContents errh
    errMVar <- newEmptyMVar
    _ <- forkIO $ (C.evaluate (length errput) >> putMVar errMVar ())

    when (not (null input)) $ hPutStr inh input
    takeMVar outMVar
    takeMVar errMVar
    ex     <- C.catch (waitForProcess pid) (\(_::C.SomeException) -> return ExitSuccess)
    hClose outh
    hClose inh
    hClose errh

    return $ case ex of
        ExitSuccess   -> Right errput
        ExitFailure _ -> Left (ex, errput, output)

  where
    handler (e :: C.SomeException) = Left (ExitFailure 1, show e, "")


{-
-- Safe wrapper for getEnv
getEnvMaybe :: String -> IO (Maybe String)
getEnvMaybe name = handle (const $ return Nothing) (Just `fmap` getEnv name)
-}

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

thd3 :: (a,b,c) -> c
thd3 (_,_,z) = z


------------------------------------------------------------------------------
--                           parsing config file                            --
------------------------------------------------------------------------------

parseBound :: String -> [(Int64,String)]
parseBound str =
    case reads str of
      [(num,rst)] ->
        case lex rst of
          [(sz,tl)] ->
            case sz of
              ""    -> [(num,tl)]
              "k"   -> [(k num, tl)]
              "K"   -> [(k num, tl)]
              "m"   -> [(m num, tl)]
              "M"   -> [(m num, tl)]
              "g"   -> [(g num, tl)]
              "G"   -> [(g num, tl)]
              '-':'-':_ -> [(num,"")]
              _     -> []
          _ -> []
      _ -> []

parseConfig :: Options -> [String] -> Options
parseConfig op []   = op
parseConfig op (ln:lns) =
    case lex ln of
        [("Allocation",_)]  -> stanzaA op lns
        [("Heap",_)]        -> stanzaH op lns
        [("Time",tl)]       -> case lex tl of
                                [(_,ops)] -> case reads ops of
                                    [(b,_)] -> parseConfig (op{ optTime = b }) lns
                                    _       -> parseConfig op lns
                                _       -> parseConfig op lns
        [("Peak",tl)]       -> case lex tl of
                                [(_,ops)] -> case reads ops of
                                    [(b,_)] -> parseConfig (op{ optPeak = b }) lns
                                    _       -> parseConfig op lns
                                _       -> parseConfig op lns
        [("Residency",tl)]  -> case lex tl of
                                [(_,ops)] -> case reads ops of
                                    [(b,_)] -> parseConfig (op{ optResi = b }) lns
                                    _       -> parseConfig op lns
                                _       -> parseConfig op lns
        _                   -> parseConfig op lns

stanzaA :: Options -> [String] -> Options
stanzaA op [] = op
stanzaA op inp@(ln:lns) =
    case lex ln of
        [("Amin",tl)]   -> case lex tl of
                            [(_,nm)] -> case parseBound nm of
                                [(num,_)] -> stanzaA (op{ optAmin = num }) lns
                                _         -> parseConfig op lns
                            _         -> parseConfig op lns
        [("Amax",tl)]   -> case lex tl of
                            [(_,nm)] -> case parseBound nm of
                                [(num,_)] -> stanzaA (op{ optAmax = num }) lns
                                _         -> parseConfig op lns
                            _         -> parseConfig op lns
        _               -> parseConfig op inp

stanzaH :: Options -> [String] -> Options
stanzaH op [] = op
stanzaH op inp@(ln:lns) =
    case lex ln of
        [("Hmin",tl)]   -> case lex tl of
                            [(_,nm)] -> case parseBound nm of
                                [(num,_)] -> stanzaH (op{ optHmin = num }) lns
                                _         -> parseConfig op lns
                            _         -> parseConfig op lns
        [("Hmax",tl)]   -> case lex tl of
                            [(_,nm)] -> case parseBound nm of
                                [(num,_)] -> stanzaH (op{ optHmax = num }) lns
                                _         -> parseConfig op lns
                            _         -> parseConfig op lns
        [("Mmax",tl)]   -> case lex tl of
                            [(_,nm)] -> case parseBound nm of
                                [(num,_)] -> stanzaH (op{ optMmax = Just num }) lns
                                _         -> parseConfig op lns
                            _         -> parseConfig op lns
        _               -> parseConfig op inp
