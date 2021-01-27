{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Control.Foldl as L
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.System
import qualified Data.Vector.Unboxed as U
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import GHC.RTS.Events as E
import GHC.RTS.Events.Incremental as E
import System.Environment (getArgs)

pausesByThread :: [Event] -> Map ThreadId (U.Vector (Timestamp, Bool))
pausesByThread = L.fold (L.handles (to sel . _Just) $ L.groupBy fst (L.premap snd L.vector))
  where
    sel Event {evSpec = RunThread tid, ..} = Just (tid, (evTime, True))
    sel Event {evSpec = StopThread tid _, ..} = Just (tid, (evTime, False))
    sel _ = Nothing

{-
check ((_, True):(_, False):xs) = check xs
check [] = Right "OK"
check x = Left (length x)
> check $ U.toList evs3
Right "OK"

begins = U.map fst $ U.filter (not . snd) evs3
ends = U.map fst . U.filter snd $ U.drop 1 evs3
evsz = U.zipWith (-) ends begins

> U.take 10 evsz
[580000,300,668100,548800,639400,701800,943200,1086300,1078600,846900]

[(99.99, 1_505_684_200),(99.9,609_451_600),(99,4_243_700)]
-}

readEvs :: FilePath -> IO [Event]
readEvs f = do
  lbs <- LBS.readFile f
  print $ LBS.length lbs
  case E.readEventLog lbs of
    Right (EventLog {dat = Data es}, _) -> return $ sortOn evTime es
    Left x -> do print x; return []

convertEvs :: FilePath -> IO ()
convertEvs f = do
  ses <- readEvs f
  print $ length ses
  LBS.writeFile (f ++ ".json") $ bs ses

main :: IO ()
main = do
  [a] <- getArgs
  convertEvs a

data Phase = B | E | X | Il | C | M | Bl | Nl | El | Sl | Tl | Fl
  deriving stock (Show, Generic)

instance ToJSON Phase where
  toJSON = genericToJSON opts
    where
      opts = defaultOptions {constructorTagModifier = tmod}
      tmod [f, 'l'] = [Char.toLower f]
      tmod x = x

data ChromeEvent = ChromeEvent
  { name :: Text,
    cat :: Text, -- "category,list",
    ph :: Phase,
    ts :: Scientific,
    pid :: Word,
    tid :: Word
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Scope = G | P | T
  deriving stock (Show, Generic)

instance ToJSON Scope where
  toJSON = genericToJSON opts
    where
      opts =
        defaultOptions
          { constructorTagModifier = map Char.toLower
          }

data ChromeInstantEvent = ChromeInstantEvent
  { name :: Text,
    cat :: Text, -- "category,list",
    ph :: Phase,
    ts :: Scientific,
    pid :: Word,
    tid :: Word,
    s :: Scope
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data ChromeEventArgs a = ChromeEventArgs
  { name :: Text,
    cat :: Text, -- "category,list",
    ph :: Phase,
    ts :: Scientific,
    pid :: Word,
    tid :: Word,
    args :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype ChromeName = ChromeName {name :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

newtype ChromeCounter = ChromeCounter {counter :: Int}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Sparks = Sparks
  { sparksCreated,
    sparksDud,
    sparksOverflowed,
    sparksConverted,
    sparksFizzled,
    sparksGCd,
    sparksRemaining ::
      Word64
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

bs :: [Event] -> LBS.ByteString
bs xs =
  B.toLazyByteString
    ( "{\"metadata\" : "
        <> (B.lazyByteString . encode . getMetadata $ xs)
        <> ",\n\"traceEvents\":[\n"
        <> mconcat (intersperse ",\n" . concat $ prelude : map processEvent xs)
        <> "]}"
    )

prelude :: [B.Builder]
prelude =
  [ meta "thread_name" "ConcGC" concGCProc gcThread 0,
    meta "thread_name" "all threads" allProc 0 0
  ]

rec ph ts pid tid =
  B.lazyByteString . encode $
    ChromeEvent
      { name = T.pack $ show tid, -- sformat int tid,
        cat = "thrs",
        ph = ph,
        ts = fromFloatDigits (fromIntegral ts / 1000),
        pid = fromIntegral pid,
        tid = fromIntegral tid
      }

gc n ph ts pid =
  B.lazyByteString . encode $
    ChromeEvent
      { name = n,
        cat = "gc",
        ph = ph,
        ts = fromFloatDigits (fromIntegral ts / 1000),
        pid = fromIntegral pid,
        tid = 0
      }

instant name s ts pid tid =
  B.lazyByteString . encode $
    ChromeInstantEvent
      { cat = "thrs",
        ph = Il,
        ts = fromFloatDigits (fromIntegral ts / 1000),
        pid = fromIntegral pid,
        tid = fromIntegral tid,
        ..
      }

meta name anm pid tid ts =
  B.lazyByteString . encode $
    ChromeEventArgs
      { name = name,
        cat = name,
        ph = M,
        ts = fromFloatDigits (fromIntegral ts / 1000),
        pid = pid,
        tid = tid,
        args = ChromeName {name = anm}
      }

ctr name ts anv =
  B.lazyByteString . encode $
    ChromeEventArgs
      { name = name,
        cat = name,
        ph = C,
        ts = fromFloatDigits (fromIntegral ts / 1000),
        pid = countersProc,
        tid = 0,
        args = ChromeCounter {counter = anv}
      }

gcThread = 0

concGCProc = 128

allProc = 512

countersProc = 256

userEventProc = 255

hash :: Text -> Int
hash = sum . map Char.ord . T.unpack

processEvent :: Event -> [B.Builder]
processEvent Event {evCap = Nothing, ..} = case evSpec of
  (CapCreate i) -> [meta "thread_name" "!GC" (fromIntegral i) gcThread evTime]
  ConcMarkBegin -> [gc "Mark" B evTime concGCProc]
  (ConcMarkEnd _w) -> [gc "Mark" E evTime concGCProc]
  ConcSyncBegin -> [gc "Sync" B evTime concGCProc]
  ConcSyncEnd -> [gc "Sync" E evTime concGCProc]
  ConcSweepBegin -> [gc "Sweep" B evTime concGCProc]
  ConcSweepEnd -> [gc "Sweep" E evTime concGCProc]
  -- TODO
  (UserMessage t) -> [instant t G evTime 0 0]
  (UserMarker t) -> [instant t G evTime 0 0]
  -- (RtsIdentifier _w t) -> [meta "rts" (LBS.toStrict . B.toLazyByteString . B.stringUtf8 . show $ t) 256 0]
  -- (ProgramArgs _w l_t) -> [meta "args" (LBS.toStrict . B.toLazyByteString . B.stringUtf8 . show $ l_t) 256 0]
  -- (Version l_c) -> [meta "version" (LBS.toStrict . B.toLazyByteString . B.stringUtf8 $ l_c) 256 0]
  _x -> [] -- traceShow x []
processEvent Event {evCap = Just cap, ..} = case evSpec of
  --    (EventBlock w i w3) -> _
  --    (UnknownEvent w) -> _
  --    (Startup i) -> _
  --    Shutdown -> _
  -- thread scheduling
  (CreateThread _w) -> []
  (RunThread tid) -> [rec B evTime cap tid, rec B evTime 512 0]
  (StopThread tid _t) -> [rec E evTime cap tid, rec E evTime 512 0]
  --    (ThreadRunnable w) -> _
  --    (MigrateThread w i) -> _
  --    (WakeupThread w i) -> _
  (ThreadLabel tid t) -> [meta "thread_name" t (fromIntegral cap) (fromIntegral tid) evTime]
  (CreateSparkThread _w) -> []
  SparkCounters
    { sparksCreated = 0,
      sparksDud = 0,
      sparksOverflowed = 0,
      sparksConverted = 0,
      sparksFizzled = 0,
      sparksGCd = 0,
      sparksRemaining = 0
    } -> []
  SparkCounters {..} ->
    let sprk n c =
          [ B.lazyByteString . encode $
              ChromeEventArgs
                { name = n,
                  cat = "sparks",
                  ph = C,
                  ts = fromFloatDigits (fromIntegral evTime / 1000),
                  pid = fromIntegral cap,
                  tid = maxBound,
                  args = ChromeCounter $ fromIntegral c
                },
            meta "thread_name" "sparks" (fromIntegral cap) maxBound evTime
          ]
     in concat
          [ sprk "sparks created" sparksCreated,
            sprk "sparks dud" sparksDud,
            sprk "sparks overflowed" sparksOverflowed,
            sprk "sparks converted" sparksConverted,
            sprk "sparks fizzled" sparksFizzled,
            sprk "sparks GCd" sparksGCd,
            sprk "sparks remaining" sparksRemaining
          ]
  --    SparkCounters {..} -> [B.lazyByteString . encode $ ChromeEventArgs {
  --         name = "sparks",
  --         cat = "sparks",
  --         ph = C,
  --         ts = fromFloatDigits (fromIntegral evTime / 1000),
  --         pid = fromIntegral cap,
  --         tid = maxBound,
  --         args = Sparks {..}
  --     },meta "thread_name" "sparks" (fromIntegral cap) maxBound evTime]
  --    (SparkCounters w w2 w3 w4 w5 w6 w7) -> _
  SparkCreate -> []
  SparkDud -> []
  SparkOverflow -> []
  SparkRun -> []
  (SparkSteal _i) -> []
  SparkFizzle -> []
  SparkGC -> []
  --    (TaskCreate w i k) -> _
  --    (TaskMigrate w i i3) -> _
  --    (TaskDelete w) -> _
  --    -- garbage collection
  RequestSeqGC -> []
  RequestParGC -> []
  StartGC -> [gc "GC" B evTime cap]
  GCWork -> []
  GCIdle -> []
  GCDone -> []
  EndGC -> [gc "GC" E evTime cap]
  GlobalSyncGC -> []
  GCStatsGHC{} -> []
  (HeapAllocated _w a) -> [ctr "heap allocated" evTime (fromIntegral a)]
  (HeapSize _w sz) -> [ctr "heap size" evTime (fromIntegral sz)]
  (HeapLive _w sz) -> [ctr "heap live" evTime (fromIntegral sz)]
  --    (HeapInfoGHC w i w3 w4 w5 w6) -> _
  --    (CapCreate i) -> error (show i) -- [meta "thread_name" "GC" i 0]
  --    (CapDelete i) -> _
  --    (CapDisable i) -> _
  --    (CapEnable i) -> _
  --    (CapsetCreate w c) -> _
  --    (CapsetDelete w) -> _
  --    (CapsetAssignCap w i) -> _
  --    (CapsetRemoveCap w i) -> _
  --    (RtsIdentifier w t) -> _
  --    (ProgramArgs w l_t) -> _
  --    (ProgramEnv w l_t) -> _
  --    (OsProcessPid w w2) -> _
  --    (OsProcessParentPid w w2) -> _
  --    (WallClockTime w w2 w3) -> _
  --    (Message t) -> _
  (UserMessage t)
    | ("START" : n : _) <- T.words t ->
      [ B.lazyByteString . encode $
          ChromeEvent
            { name = n,
              cat = "user",
              ph = B,
              ts = fromFloatDigits (fromIntegral evTime / 1000),
              pid = userEventProc,
              tid = fromIntegral $ hash n
            },
        meta "thread_name" n userEventProc (fromIntegral $ hash n) evTime
      ]
    | ("STOP" : n : _) <- T.words t ->
      [ B.lazyByteString . encode $
          ChromeEvent
            { name = n,
              cat = "user",
              ph = E,
              ts = fromFloatDigits (fromIntegral evTime / 1000),
              pid = userEventProc,
              tid = fromIntegral $ hash n
            }
      ]
    | otherwise -> [instant t P evTime cap 0]
  (UserMarker t) -> [instant t P evTime cap 0]
  --    (Version l_c) -> _
  --    (ProgramInvocation l_c) -> _
  --    (CreateMachine w w2) -> _
  --    (KillMachine w) -> _
  --    (CreateProcess w) -> _
  --    (KillProcess w) -> _
  --    (AssignThreadToProcess w w2) -> _
  --    EdenStartReceive -> _
  --    EdenEndReceive -> _
  --    (SendMessage m w w3 w4 w5 w6) -> _
  --    (ReceiveMessage m w w3 w4 w5 w6 w7) -> _
  --    (SendReceiveLocalMessage m w w3 w4 w5) -> _
  --    (InternString l_c w) -> _
  --    (MerStartParConjunction w w2) -> _
  --    (MerEndParConjunction w) -> _
  --    (MerEndParConjunct w) -> _
  --    (MerCreateSpark w w2) -> _
  --    (MerFutureCreate w w2) -> _
  --    (MerFutureWaitNosuspend w) -> _
  --    (MerFutureWaitSuspended w) -> _
  --    (MerFutureSignal w) -> _
  --    MerLookingForGlobalThread -> _
  --    MerWorkStealing -> _
  --    MerLookingForLocalSpark -> _
  --    (MerReleaseThread w) -> _
  --    MerCapSleeping -> _
  --    MerCallingMain -> _
  --    (PerfName w t) -> _
  --    (PerfCounter w k w3) -> _
  --    (PerfTracepoint w k) -> _
  --    (HeapProfBegin w w2 h t t5 t6 t7 t8 t9 t10) -> _
  --    (HeapProfCostCentre w t t3 t4 h) -> _
  --    (HeapProfSampleBegin w) -> _
  --    (HeapProfSampleEnd w) -> _
  --    (HeapBioProfSampleBegin w w2) -> _
  --    (HeapProfSampleCostCentre w w2 w3 vw) -> _
  --    (HeapProfSampleString w w2 t) -> _
  --    (ProfSampleCostCentre w w2 w3 vw) -> _
  --    (ProfBegin w) -> _
  --    (UserBinaryMessage b) -> _
  --    ConcMarkBegin -> [gc "Mark" B evTime cap]
  --    (ConcMarkEnd w) -> [gc "Mark" E evTime cap]
  --    ConcSyncBegin -> [gc "Sync" B evTime cap]
  --    ConcSyncEnd -> [gc "Sync" E evTime cap]
  --    ConcSweepBegin -> [gc "Sweep" B evTime cap]
  --    ConcSweepEnd -> [gc "Sweep" E evTime cap]
  --    (ConcUpdRemSetFlush i) -> _
  --    (NonmovingHeapCensus w w2 w3 w4) -> _
  --    (TickyCounterDef w w2 t t4) -> _
  --    (TickyCounterSample w w2 w3 w4) -> _
  _x -> [] -- traceShow x []

data ChromeMeta = ChromeMeta
  { dateTime :: Maybe UTCTime,
    rtsIdent :: Maybe Text,
    progArgs :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

getMetadata :: [Event] -> ChromeMeta
getMetadata = foldl' step (ChromeMeta Nothing Nothing Nothing)
  where
    step ChromeMeta {..} (evSpec -> WallClockTime {..}) =
      ChromeMeta {dateTime = Just . systemToUTCTime $ MkSystemTime (fromIntegral sec) nsec, ..}
    step ChromeMeta {..} (evSpec -> RtsIdentifier {..}) = ChromeMeta {rtsIdent = Just rtsident, ..}
    step ChromeMeta {..} (evSpec -> ProgramArgs {..}) = ChromeMeta {progArgs = Just $ T.unwords args, ..}
    step met _ = met
