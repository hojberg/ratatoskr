module Screen where

import RemoteData (RemoteData)
import Story

data Screen
  = Summary (RemoteData String [Story])
  | Details
