import System.Random

diff_select m n = do 
   gen <- getStdGen
   return $ take m $ randomRs (1, n) gen