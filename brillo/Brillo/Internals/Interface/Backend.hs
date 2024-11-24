{-# LANGUAGE CPP #-}

-- Import window managed backend specific modules.
module Brillo.Internals.Interface.Backend (
  module Brillo.Internals.Interface.Backend.Types,
  module Brillo.Internals.Interface.Backend.GLFW,
  defaultBackendState,
)
where

import Brillo.Internals.Interface.Backend.GLFW
import Brillo.Internals.Interface.Backend.Types


defaultBackendState :: GLFWState
defaultBackendState = initBackendState
