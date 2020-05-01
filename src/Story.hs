{-# LANGUAGE OverloadedStrings #-}

module Story where

import Data.List as List
import Data.Text (Text)
import Data.Aeson

data Url = Url String

data StoryId = StoryId String

data Source = Lobsters

data Story = Story { sId :: Text
                   , title :: Text
                   , storyUrl :: Text
                   , url :: Text
                   , description :: Text
                   } deriving (Show, Eq)

instance FromJSON Story where
  parseJSON (Object v) =
     Story <$> v .: "short_id"
           <*> v .: "title"
           <*> v .: "short_id_url"
           <*> v .: "url"
           <*> v .: "description"

indexOfStoryId :: Maybe Text -> [Story] -> Maybe Int
indexOfStoryId _       [] = Nothing
indexOfStoryId Nothing _  = Nothing
indexOfStoryId (Just id) stories =
  let
    finder s = sId s == id
  in
    List.find finder stories >>= (`elemIndex` stories)

storyById :: [Story] -> Text -> Maybe Story
storyById stories storyId =
  let
    finder s =
      sId s == storyId
  in
    List.find finder stories


{--
data LobstersStory = LobstersStory {
  short_id: "vcx5vu",
  short_id_url: "https://lobste.rs/s/vcx5vu",
  created_at: "2020-04-27T09:33:22.000-05:00",
  title: "What are you working on this week?",
  url: String,
  score: 14,
  upvotes: 14,
  downvotes: 0,
  comment_count: 31,
  description: "<p>What are you doing this week? Feel free to share!</p>\n<p>Keep in mind it’s OK to do nothing at all, too.</p>\n",
  comments_url: "https://lobste.rs/s/vcx5vu/what_are_you_working_on_this_week",
  submitter_user: {
    username: "hyperpape",
    created_at: "2016-06-30T19:22:53.000-05:00",
    is_admin: false,
    about: "Into goofy hacks, optimization, compilers, and also everything else under the sun. I sometimes post my notes at https://justinblank.com.",
    is_moderator: false,
    karma: 782,
    avatar_url: "/avatars/hyperpape-100.png",
    invited_by_user: "telemachus",
    github_username: "hyperpape"
  },
  tags: [String]
}
--}
