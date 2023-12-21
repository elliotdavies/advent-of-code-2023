module AOC2023.Day20
  ( part1,
    part2,
  )
where

import AOC2023.Lib (Solution, fromParser)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.Parsec (char, letter, many1, newline, oneOf, optionMaybe, optional, parse, spaces, string)
import Text.Parsec.String (Parser)

data Pulse = H | L
  deriving (Eq, Show)

type Id = String

data ModuleType
  = FlipFlop Bool
  | Conj (M.Map Id Pulse)
  | Broadcaster
  | Other
  deriving (Show)

-- N.B. Only seems to work if `broadcaster` is first item  in file
parseModule :: Parser (Id, (ModuleType, [Id]))
parseModule = do
  symbol <- optionMaybe $ oneOf "%&"
  mId <- many1 letter
  spaces
  string "->"
  spaces
  outputIds <- many1 $ do
    oId <- many1 letter
    optional $ char ','
    optional spaces
    pure oId
  optional newline

  let mType = case symbol of
        Just '%' -> FlipFlop False
        Just '&' -> Conj M.empty
        Just c -> error $ "Bad symbol: " ++ [c]
        Nothing -> if mId == "broadcaster" then Broadcaster else Other

  pure (mId, (mType, outputIds))

type ModuleState = M.Map Id (ModuleType, [Id])

buildState :: [(Id, (ModuleType, [Id]))] -> ModuleState
buildState defs = fillConjs $ M.fromList defs
  where
    fillConjs :: ModuleState -> ModuleState
    fillConjs =
       M.mapWithKey
            ( \mId (ty, os) -> case ty of
                Conj _ -> (Conj (initialise mId), os)
                t -> (t, os)
            )

    initialise :: Id -> M.Map Id Pulse
    initialise mId = M.fromList $ map (,L) $ S.toList $ revMap M.! mId

    revMap = foldr buildRevMap M.empty defs

    buildRevMap (inId, (_, outIds)) rm =
      foldr (\oId acc -> M.insertWith S.union oId (S.singleton inId) acc) rm outIds


-- updated state plus record of all (from, pulse, to) sent
type Result = (ModuleState, [(Id, Pulse, Id)])

pulse :: (Id, Pulse, Id) -> ModuleState -> Result
pulse (fromId, pIn, toId) ms
  | M.notMember toId ms = (ms, [])
  | otherwise = (M.insert toId (mType', os) ms, psOut)
  where
    psOut = case pOut of
      Just p' -> map (toId, p',) os
      Nothing -> []

    (mType', pOut) =
      case fst (ms M.! toId) of
        FlipFlop b ->
          case pIn of
            H -> (FlipFlop b, Nothing)
            L -> (FlipFlop (not b), Just (if b then L else H))
        Conj mem ->
          let mem' = M.insert fromId pIn mem
           in (Conj mem', Just (if all (== H) (M.elems mem') then L else H))
        Broadcaster -> (Broadcaster, Just pIn)
        Other -> (Other, Nothing)

    os = snd (ms M.! toId)

pushButton :: ModuleState -> Result
pushButton initMs = go [("button", L, "broadcaster")] (initMs, [])
  where
    go :: [(Id, Pulse, Id)] -> Result -> Result
    go [] acc = acc
    go (p : ps) (ms, acc) =
      let (ms', ps') = pulse p ms
       in go (ps ++ ps') (ms', acc ++ [p])

pushButtonN :: Int -> ModuleState -> Result
pushButtonN initN initMs = go initN (initMs, [])
  where
    go 0 acc = acc
    go n (ms, ps) =
      let (ms', ps') = pushButton ms in go (n -1) (ms', ps ++ ps')

part1 :: Solution
part1 = fromParser go . parse (many1 parseModule) ""
  where
    go :: [(Id, (ModuleType, [Id]))] -> Int
    go ms =
      let (_, ps) = pushButtonN 1000 (buildState ms)
          ls = length $ filter ((== L) . \(_,p,_) -> p) ps
          hs = length $ filter ((== H) . \(_,p,_) -> p) ps
       in ls * hs

part2 :: Solution
part2 = fromParser go . parse (many1 parseModule) ""
  where
    go ms = loopUntilRx (buildState ms) 0

    loopUntilRx ms n =
      let (ms', ps) = pushButton ms
       in if matches ps then n+1 else loopUntilRx ms' (n+1)

    matches = any (\(_,p,to) -> p == L && to == "rx")
