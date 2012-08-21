module Main where

main :: IO ()
main = do
    putStrLn "\\documentclass{article}"
    putStrLn "\\usepackage[utf8]{inputenc}"
    putStrLn "\\usepackage[ngerman]{babel}"
    putStrLn "\\usepackage{geometry}"
    putStrLn "\\usepackage{longtable}"
    putStrLn ""
    putStrLn "\\pagestyle{empty}"
    putStrLn ""
    putStrLn "\\begin{document}"
    putStrLn "\\begin{longtable}{|llllrlrlr|}"

    -- Alle zur Verfügung stehenden Räume für AG I
    let diamonds = [ "740", "431",  "435", "T35", "241", "430", "531", "242"
                   , "244", "1241", "432", "434", "T37", "T33", "T27" ]

    let lapsN   = 100
        lagN    = 60
        labN    = 20
        masterN = 40
        bamaN   = 110
        bawimaN = 120

    -- Generieren der Aufkleber für den Studiengang LAPS
    mapM_ putStrLn $ makeGeneric "LAPS" lapsN
        [ (show 241, "Katie"), (show 1241, "Neele") ] diamonds

    -- Generieren der Aufkleber für den Studiengang LAG
    mapM_ putStrLn $ makeGeneric "LAG" lagN
        [ (show 432, "Clara") ] diamonds

    -- Generieren der Aufkleber für den Studiengang LAB
    mapM_ putStrLn $ makeGeneric "LAB" labN
        [ ("T33", "Alex B.") ] diamonds

    -- Generieren der Aufkleber für den Studiengang Master
    mapM_ putStrLn $ makeGeneric "Master" masterN
        [ ("T27", "Prof. Dr. Iske") ] diamonds

    -- Generieren der Aufkleber für den Studiengang BaMa
    mapM_ putStrLn $ makeGeneric "BaMa" bamaN
        [ (show 740, "Steffen"), (show 431, "Kathrin"), (show 435, "Svenja")
        , ("T35",    "Arne Scheel"),    (show 434, "Antonia") ] diamonds

    -- Generieren der Aufkleber für den Studiengang BaWiMa
    mapM_ putStrLn $ makeGeneric "BaWiMa" bawimaN
        [ ("430", "Dennis"), ("531", "Arne Schulz"), ("242", "Marcel")
        , ("244", "Ines"), ("T37", "Alina") ] diamonds

    putStrLn "\\hline"
    putStrLn "\\end{longtable}"
    putStrLn "\\end{document}"

makeGeneric :: String             -- Name des Studiengangs
            -> Int                -- Anzahl der Aufkleber
            -> [(String, String)] -- Räume und Name des Tutors für das
                                  -- Kennenlernen
            -> [String]           -- Räume für die Arbeitsgruppe 1
            -> [String]
makeGeneric stud n clubs diamonds = map (uncurryN (makeGeneric' stud))
    $ zipN [1..n] (drop 0 $ cycle clubs)
                  (drop 1 $ cycle diamonds)
  where uncurryN f (n, clubs, diamonds) = f n clubs diamonds
        zipN []     _      _      = []
        zipN _      []     _      = []
        zipN _      _      []     = []
        zipN (a:as) (b:bs) (c:cs) = (a, b, c):zipN as bs cs

makeGeneric' :: String           -- Name des Studiengangs
             -> Int              -- Nummer des Aufklebers
             -> (String, String) -- Raum und Name des Tutors für das
                                 -- Kennenlernen
             -> String           -- Raum für die Arbeitsgruppe 1
             -> String
makeGeneric' stud n (clubs, tutor) diamonds = "\\hline " ++ stud
            ++ "&\\#" ++ show n
            ++ "&"    ++ tutor
            ++ "&$\\clubsuit$    Raum&"  ++ clubs
            ++ "&$\\diamondsuit$ AG&"    ++ diamonds
            ++ (if stud `elem` ["LAG", "LAB", "LAPS"] then
                   "&$\\heartsuit$ Präs.&241"
                 else
                   "&$\\spadesuit$ Raum &TBA")
            ++ "\\\\"

