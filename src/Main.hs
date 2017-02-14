{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Static
import Label
import Network
import Layers
import Runners
import Conduits

import Data.Serialize
import Data.Singletons.TypeLits
import qualified Data.Vector.Unboxed as U
import Data.Word
import Data.Proxy
import Control.Monad
import Data.Conduit.Cereal
import Data.ByteString
import Conduit
import Data.Conduit.Binary as CBS

newtype MX n = MX (SArray U (ZZ ::. n ::. 1 ::. 28 ::. 28))

instance KnownNat n => Serialize (MX n) where
  get = do let n = fromInteger$ natVal (Proxy :: Proxy n)
           bs :: [Word8] <- replicateM (n*28*28) get
           let doubles = (/255) . fromIntegral <$> bs
           return (MX$ sFromUnboxed (U.fromList doubles))
  put = undefined

newtype MY n = MY (SArray U (ZZ ::. n ::. 10))

instance KnownNat n => Serialize (MY n) where
  get = do let n = fromInteger$ natVal (Proxy :: Proxy n)
           vs <- replicateM n$ do
             b :: Word8 <- get
             return . toVector $ (Label (fromIntegral b) :: Label 10)
           return . MY . sFromUnboxed . U.concat $ vs
  put = undefined

labelSource :: KnownNat n => Source (ResourceT IO) (MY n)
labelSource = sourceFileBS "./data/train-labels.idx1-ubyte" .| labelC
  where labelC = do CBS.take 8
                    conduitGet2 get

imageSource :: KnownNat n => Source (ResourceT IO) (MX n)
imageSource = sourceFileBS "./data/train-images.idx3-ubyte" .| imageC
  where imageC = do CBS.take 16
                    conduitGet2 get

type BatchSize = 10
type MNIST = Network (ZZ ::. BatchSize ::. 1 ::. 28 ::. 28)
                     '[ Convolution 5 1 13 13 16 16
                      , Pool
                      , ReLU
                      , Flatten
                      , FC 320 100
                      , ReLU
                      , FC 100 10
                      , MultiSoftMax '[10] ]

main = do Just (net :: MNIST) <-
            runConduitRes $ combineXY imageSource labelSource
                         .| mapC (\ (MX x, MY y) -> (x, y))
                         .| trainC (LearningParameters 1e-2 0.4 1e-3)
                         .| takeC 500
                         .| lastC
          undefined

