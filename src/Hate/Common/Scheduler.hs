{-# LANGUAGE RankNTypes #-}

module Hate.Common.Scheduler where

import Hate.Common.Types
import qualified Graphics.UI.GLFW as G (getTime)

import Data.MultiMap
import Data.Maybe (catMaybes)
import Control.Monad.State

type Action a = forall us. Hate us a

type Time = Double
type EventAction us = Hate us ()

data ScheduledEvent us = OneTimeEvent Time (EventAction us) | RepetitiveEvent Time Time (EventAction us)
instance Show (ScheduledEvent us) where
    show (OneTimeEvent t _) = "OneTimeEvent (t = " ++ show t ++ ")"
    show (RepetitiveEvent t i _) = "RepetitiveEvent (t = " ++ show t ++ ", interval = " ++ show i ++ ")"

fire :: ScheduledEvent us -> Hate us ()
fire (OneTimeEvent _ a) = a
fire (RepetitiveEvent _ _ a) = a

type Scheduler us = MultiMap Time (ScheduledEvent us)
instance (Show a, Show b) => Show (MultiMap a b) where
    show x = show . toMap $ x

every :: Time -> Hate us () -> Hate us (ScheduledEvent us)
every interval action = do
    t <- UnsafeHate $ gets lastUpdateTime
    return $ RepetitiveEvent t interval action

after :: Time -> Hate us () -> Hate us (ScheduledEvent us)
after t a = return $ OneTimeEvent t a

logv str v = UnsafeHate . liftIO $ putStrLn (str ++ " " ++ show v)

process :: Scheduler us -> Hate us (Scheduler us)
process sched = do
    currTime <- UnsafeHate $ gets lastUpdateTime
    --UnsafeHate . liftIO $ putStrLn ("currenttime " ++ show currTime)

    let maybeEvents = findMinWithValues sched
    case maybeEvents of 
        Just (t, nearestEvents) ->
            if currTime > t then do
                mapM_ fire nearestEvents

                let sched' = delete t sched
                let updatedEvents = catMaybes . Prelude.map (updateEvent currTime) $ nearestEvents

                let sched'' = Prelude.foldr (flip schedule) sched' updatedEvents

                return sched''
            else return sched -- no events to fire yet
        Nothing -> return sched -- no events scheduled at all

emptyScheduler :: Scheduler us
emptyScheduler = empty

schedule :: Scheduler us -> ScheduledEvent us -> Scheduler us
schedule sched evt = case evt of
    OneTimeEvent time _ -> insert time evt sched
    RepetitiveEvent time _ _ -> insert time evt sched

updateEvent :: Time -> ScheduledEvent us -> Maybe (ScheduledEvent us)
updateEvent _ (OneTimeEvent _ _) = Nothing
updateEvent t (RepetitiveEvent lastFire interval act) = Just $ RepetitiveEvent (lastFire+interval) interval act