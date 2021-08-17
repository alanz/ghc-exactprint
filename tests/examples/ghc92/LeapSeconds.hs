-- | Interpretation of leap second files.
module Data.Time.Clock.LeapSeconds
(
    LeapSecondMap,
    LeapSecondList(..),
    parseNISTLeapSecondList,
    leapSecondListToMap,
) where
{
    import Data.Maybe;
    import Text.Read;
    import Data.Time;
    import Data.Time.Clock.TAI;


    -- | A list of leap-second transitions, etc.
    ;
    data LeapSecondList = MkLeapSecondList
    {
        lslVersion :: Day, -- when the list was created
        lslExpiration :: Day, -- when the list expires
        lslTransitions :: [(Day,Int)] -- transitions: TAI - UTC starting at this day
    } deriving Eq;

    -- | 1900-01-01
    ;
    ntpEpochDay :: Day;
    ntpEpochDay = ModifiedJulianDay 15020;

    ntpDateStringToDay :: String -> Maybe Day;
    ntpDateStringToDay s = do
    {
        n <- readMaybe s;
        return $ addDays (div n 86400) ntpEpochDay;
    };

    single :: [a] -> Maybe a;
    single [a] = return a;
    single _ = Nothing;

    separate :: Char -> String -> [String];
    separate sep s = let
    {
        (val,rest) = break ((==) sep) s;
        vals = case rest of
        {
            [] -> [];
            _:s' -> separate sep s';
        };
    } in val : vals;

    -- | Parse the text of a NIST leap-second file. This file can be found at <ftp://time.nist.gov/pub/leap-seconds.list>,
    -- and on UNIX systems, at @\/usr\/share\/zoneinfo\/leap-seconds.list@.
    ;
    parseNISTLeapSecondList :: String -> Maybe LeapSecondList;
    parseNISTLeapSecondList text = do
    {
        let
        {
            readLine ('#':'$':s) | Just version <- ntpDateStringToDay s = return (Just version,Nothing,Nothing);
            readLine ('#':'@':s) | Just expiration <- ntpDateStringToDay s = return (Nothing,Just expiration,Nothing);
            readLine ('#':_) = return (Nothing,Nothing,Nothing);
            readLine "" = return (Nothing,Nothing,Nothing);
            readLine s = case separate '\t' s of
            {
                (tstr:ostr:_) | Just t <- ntpDateStringToDay tstr, Just offset <- readMaybe ostr -> return $ (Nothing,Nothing,Just (t,offset));
                _ -> Nothing;
            };
        };
        mstrs <- traverse readLine $ lines text;
        version <- single $ catMaybes $ fmap (\(x,_,_) -> x) mstrs;
        expiration <- single $ catMaybes $ fmap (\(_,x,_) -> x) mstrs;
        let
        {
            transitions = catMaybes $ fmap (\(_,_,x) -> x) mstrs;
        };
        return $ MkLeapSecondList version expiration transitions;
    };

    -- | Obtain a map that can be used to convert between TAI and UTC (see "Data.Time.Clock.TAI")
    ;
    leapSecondListToMap :: LeapSecondList -> LeapSecondMap;
    leapSecondListToMap lsl day | day >= lslExpiration lsl = Nothing;
    leapSecondListToMap lsl day = let
    {
        findInList :: [(Day,Int)] -> Maybe Int -> Maybe Int;
        findInList [] mi = mi;
        findInList ((d,_):_) mi | day < d = mi;
        findInList ((_,i):rest) _ = findInList rest (Just i);
    } in findInList (lslTransitions lsl) Nothing;
}

