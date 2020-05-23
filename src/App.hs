{-# LANGUAGE OverloadedStrings #-}

module App where

import Data.Aeson (decode)

import           Control.Monad.IO.Class
import System.Process
import           Brick.AttrMap                  ( AttrMap
                                                , attrMap
                                                )
import qualified Brick.AttrMap                 as A
import           Brick.Main                     ( App(..)
                                                , defaultMain
                                                , neverShowCursor
                                                )
import qualified Brick.Main                    as M
import           Brick.Types                    ( BrickEvent(..)
                                                , EventM
                                                , Next
                                                , Widget
                                                )
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.Core            as C
import qualified Brick.Widgets.Edit            as E
import           Control.Monad                  ( void )
import           Data.List                     as List
import qualified Data.Text                     as T
import           Graphics.Vty                   ( Event(EvKey)
                                                , Key(KChar, KEnter, KEsc)
                                                )
import qualified Graphics.Vty                  as V
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)


import Story (Story(..))
import qualified Story
import RemoteData (RemoteData(..))
import UI
import           Util                           ( nextIndex
                                                , prevIndex
                                                )

-- MODEL

data Name
  = Info
  | TextBox
  deriving (Show, Ord, Eq)

data Model = Model
  { stories :: RemoteData String [Story]
  , selectedStoryId :: Maybe T.Text
  , message :: Maybe String
  }


initialState :: IO Model
initialState = do
  stories <- loadStories
  pure Model { selectedStoryId = Nothing, stories = stories, message = Nothing }


getJSON :: String -> IO B.ByteString
getJSON = simpleHttp

decodeStory :: B.ByteString -> Maybe [Story]
decodeStory = decode

loadStories :: IO (RemoteData String [Story])
loadStories =
  let
    lobstersUrl = "https://lobste.rs/hottest.json"

    decodeToSuccess json =
      case (decodeStory json) of
        Nothing -> RemoteData.Success []
        Just stories -> RemoteData.Success stories
  in
    fmap decodeToSuccess (getJSON lobstersUrl)


-- UPDATE
storiesIndexToId :: [Story] -> Int -> T.Text
storiesIndexToId stories idx = sId $ stories !! idx

previousStoryId :: Maybe T.Text -> [Story] -> Maybe T.Text
previousStoryId selectedStoryId stories =
  fmap (storiesIndexToId stories) $ prevIndex stories $ Story.indexOfStoryId
    selectedStoryId
    stories

nextStoryId :: Maybe T.Text -> [Story] -> Maybe T.Text
nextStoryId selectedStoryId stories =
  fmap (storiesIndexToId stories) $ nextIndex stories $ Story.indexOfStoryId
    selectedStoryId
    stories

selectNextStory :: Model -> Model
selectNextStory model = 
  let loadedStories = case (stories model) of
        Success ss -> ss
        _ -> []

      newSelectedStoryId = nextStoryId (selectedStoryId model) loadedStories
  in  model { selectedStoryId = newSelectedStoryId }

selectPreviousStory :: Model -> Model
selectPreviousStory model =
  let loadedStories = case (stories model) of
        Success ss -> ss
        _ -> []

      newSelectedStoryId =
        previousStoryId (selectedStoryId model) loadedStories
  in  model { selectedStoryId = newSelectedStoryId }


openStory :: Model -> IO Model
openStory model =
  let
    storyById id =
      case (stories model) of 
      Success stories' -> Story.storyById stories' id
      _ -> Nothing

    story =
      selectedStoryId model >>= storyById

    open url =
      createProcess (proc "open" [T.unpack url])
  in
    case story of
      Nothing -> pure (model { message = Just "Can't find story" })
      Just s -> do
        fmap (\_ -> model ) (open (url s))


update :: Model -> BrickEvent Name e -> EventM Name (Next Model)
update model evt = case evt of
  (VtyEvent (EvKey (KChar 'R') [])) ->
    liftIO initialState >>= M.continue
  (VtyEvent (EvKey (KChar 'k') [])) -> M.continue $ selectPreviousStory model
  (VtyEvent (EvKey (KChar 'j') [])) -> M.continue $ selectNextStory model
  (VtyEvent (EvKey (KChar 'O') [])) -> liftIO (openStory model) >>= M.continue 
  (VtyEvent (EvKey (KChar 'Q') [])) -> M.halt model
  _ -> M.continue model


-- VIEW

  {--
let w = withAttr specificAttr $ str "foobar"
    generalAttr = attrName "general"
    specificAttr = attrName "general" <> attrName "specific"
    myMap = attrMap defAttr [ (generalAttr, bg blue)
                            , (specificAttr, fg white)
                            ]--}

formatStory :: Story -> String
formatStory story = 
  let
    upvotes' = "⯅ " ++ show (upvotes story)
    title' = T.unpack (title story)
  in
    upvotes' ++ " " ++ title'


isStorySelected :: Maybe T.Text -> Story -> Bool
isStorySelected Nothing           _    = False
isStorySelected (Just selectedId) story = sId story == selectedId

storyRow :: Maybe T.Text -> Story -> Widget Name
storyRow selectedStoryId story =
  let 
    selected = if isStorySelected selectedStoryId story then "█ " else "  "
    title' = T.unpack (title story)
    upvotes' = show (upvotes story)
    row = C.vBox [
      C.hBox [C.str " ⯅ ", C.str " ", C.str title'],
      C.hBox [C.str upvotes', C.str " ", C.str "author"] ]

        {--

      row = C.vBox
        [ C.vLimit 1
        $  C.str
        $  selected
        ++ formatStory story
        ]
        --}
  in  row

workspace :: Model -> Widget Name
workspace model = 
  let 
    stories' = case stories model of
      NotAsked -> [C.str "NotAsked"]
      Loading -> [C.str "Loading"]
      Failure _ -> [C.str "FAILED..."]
      Success ss -> List.map (storyRow (selectedStoryId model)) ss
  in
    C.vBox stories'


cmdline :: Model -> Widget Name
cmdline model =
  let
    cmd = case (message model) of
      Nothing -> "Awaiting command"
      Just m -> m
  in
  C.withBorderStyle BS.unicodeRounded . B.border $ C.vLimit 1 $
          C.vBox [C.str cmd]


render :: Model -> [Widget Name]
render model = 
  let 
    ui = C.joinBorders $ C.vBox [workspace model, cmdline model]
  in 
    [ui]


-- APP


app :: App Model e Name
app = App
  { appDraw         = render
  , appHandleEvent  = update
  , appAttrMap      = const (A.attrMap V.defAttr [])
  , appStartEvent   = return
  , appChooseCursor = neverShowCursor
  }


run :: IO ()
run = void $ initialState >>= M.defaultMain app
