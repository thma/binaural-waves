module Sound where
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)
import Conversion


type Sound = [Int32]

type StereoSound = [Sound]

sound :: Double  -- | Frequency
      -> Int     -- | Samples per second
      -> Double  -- | Lenght of sound in seconds
      -> Int32   -- | Volume, (maxBound :: Int32) for highest, 0 for lowest
      -> Sound
sound freq sampleRate len volume = take (round $ len * fromIntegral sampleRate) $ 
                         map (round . (* fromIntegral volume)) $
                         map sin [0.0, (freq * 2 * pi / (fromIntegral sampleRate))..]

toSound:: (Double -> Double) -> Double -> Int -> Double -> Int32 -> Sound
toSound fun freq sampleRate len volume =
      take (round $ len * fromIntegral sampleRate) $   
      map (round . (* fromIntegral volume)) $     
      map fun [0.0, (freq * 2 * pi / (fromIntegral sampleRate))..]
               

samples :: [[Int32]]
samples = map (:[]) $ sound 440 sampleRate 3 (volumeDBFS 12)

samples1 :: [[Int32]]
samples1 = map (:[]) $ toSound cos 440 sampleRate 3 (volumeDBFS 12)

joinedSamples :: [[Int32]]
joinedSamples = map (:[]) $ zipWith (+) (sound 440 sampleRate 3 (volumeDBFS 12)) (toSound cos 880 sampleRate 3 (volumeDBFS 24))

samples2 :: [[Int32]] -- play two tones at once
samples2 = map (:[]) $ zipWith (+) (sound 440 sampleRate 3 (volumeDBFS 6)) (sound 880 sampleRate 3 (volumeDBFS 12))



toStereo :: Sound -> Sound -> StereoSound
toStereo = zipWith (\x y -> [x, y])

binaural :: Double -> Int -> Double -> Double -> Double -> StereoSound
binaural freq sampleRate len dbfs beat =
      let l = sound (freq - 0.5 * beat) sampleRate len (volumeDBFS dbfs)
          r = sound (freq + 0.5 * beat) sampleRate len (volumeDBFS dbfs)
      in  toStereo l r  

binaural2 :: Double -> Int -> Double -> Double -> Double -> StereoSound
binaural2 freq sampleRate len dbfs beat =
  let l = sound (freq - 0.5 * beat) sampleRate len (volumeDBFS dbfs)
      r = sound (freq + 0.5 * beat) sampleRate len (volumeDBFS dbfs)
  in  toStereo l r            

stereoSignal :: StereoSound
stereoSignal = binaural 440 sampleRate 3 (-12) 5


sampleRate = 44100
bitrate = 32

header =
  WAVEHeader {
    waveNumChannels   = 2,
    waveFrameRate     = 44100,
    waveBitsPerSample = 32,
    waveFrames        = Nothing
  }

waveData = WAVE header stereoSignal


makeWavFile :: WAVE -> IO ()
makeWavFile wav = putWAVEFile "temp.wav" wav


-- | convert a dBFS value (e.g. -14 dB) into the corresponding Int32 sample volume  
volumeDBFS :: Double -> Int32
volumeDBFS dbValue = round $ (fromInteger $ convert (maxBound :: Int32)) * (dBToFactor dbValue)

-- | converts a dB full scale value into a factor
dBToFactor :: Double -> Double
dBToFactor dbValue = 1 / (oneDB ** (abs dbValue))
  -- -6dB has half the amplitude as 0dB. Thus 1dB == 6th root of 2
  where 
    oneDB = nthRoot 6 2
    nthRoot :: Double -> Double -> Double 
    nthRoot n x = x ** (1 / n)

main1 = makeWavFile waveData