{-# LANGUAGE RankNTypes #-}

{-|
Module      : Hate.Common.Scheduler
Description : Event scheduler for Hate programs
License     : MIT
Maintainer  : bananu7@o2.pl
Stability   : experimental

Scheduler allows you to use FRP-ish style of delaying and
scheduling actions.

-}

module Hate.Common.Scheduler 
    ( every
    , after
    , process
    , emptyScheduler
    , schedule
    , Scheduler
    )
where

import Hate.Common.Types
import qualified Graphics.UI.GLFW as G (getTime)

import Data.MultiMap
import Data.Maybe (catMaybes)
import Control.Monad.State

type Time = Double
type EventAction us = Hate us ()

data ScheduledEvent us = OneTimeEvent Time (EventAction us) | RepetitiveEvent Time Time (EventAction us)
instance Show (ScheduledEvent us) where
    show (OneTimeEvent t _) = "OneTimeEvent (t = " ++ show t ++ ")"
    show (RepetitiveEvent t i _) = "RepetitiveEvent (t = " ++ show t ++ ", interval = " ++ show i ++ ")"

fire :: ScheduledEvent us -> Hate us ()
fire (OneTimeEvent _ a) = a
fire (RepetitiveEvent _ _ a) = a

-- |This represents a control structure for your events.
-- You can use multiple schedulers and update them basing on the
-- state of your application.
type Scheduler us = MultiMap Time (ScheduledEvent us)
instance (Show a, Show b) => Show (MultiMap a b) where
    show x = show . toMap $ x

-- |Schedules an action to run every @t@ seconds.
every :: Time -> Hate us () -> Hate us (ScheduledEvent us)
every interval action = do
    t <- UnsafeHate $ gets lastUpdateTime
    return $ RepetitiveEvent t interval action

-- |Schedules an action to run once, after @t@ seconds.
after :: Time -> Hate us () -> Hate us (ScheduledEvent us)
after t a = do
    currentTime <- UnsafeHate $ gets lastUpdateTime
    return $ OneTimeEvent (t + currentTime) a

-- |This is a main way to update the scheduler and run appropriate events.
-- Typically you'll want to be running it every update.
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

                process sched'' -- recursive call to inspect the next batch
            else return sched -- no events to fire yet
        Nothing -> return sched -- no events scheduled at all

-- |This is provided in case the underlying implementation should change.
emptyScheduler :: Scheduler us
emptyScheduler = empty

-- |Adds a new event to scheduler and returns an updated one.
schedule :: Scheduler us -> ScheduledEvent us -> Scheduler us
schedule sched evt = case evt of
    OneTimeEvent time _ -> insert time evt sched
    RepetitiveEvent time _ _ -> insert time evt sched


updateEvent :: Time -> ScheduledEvent us -> Maybe (ScheduledEvent us)
updateEvent _ (OneTimeEvent _ _) = Nothing
updateEvent _ (RepetitiveEvent lastFire interval act) = Just $ RepetitiveEvent (lastFire+interval) interval act