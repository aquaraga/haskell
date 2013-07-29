gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = (map ('0':) $ gray $ pred n) ++ (map ('1':) $ reverse $ gray $ pred n)