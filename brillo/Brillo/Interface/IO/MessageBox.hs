-- | Native message boxes, notification popups, and system beeps.
module Brillo.Interface.IO.MessageBox (
  -- * Functions
  messageBox,
  notifyPopup,
  beep,

  -- * Message box options
  IconType (..),
  MessageBox,
  OK (..),
  OKCancel (..),
  YesNo (..),
  YesNoCancel (..),
)
where

import Brillo.Internals.TinyFileDialogs (
  IconType (..),
  MessageBox,
  OK (..),
  OKCancel (..),
  YesNo (..),
  YesNoCancel (..),
  beep,
  messageBox,
  notifyPopup,
 )

