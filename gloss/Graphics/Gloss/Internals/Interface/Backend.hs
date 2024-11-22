{-# LANGUAGE CPP #-}

-- Import window managed backend specific modules.
module Graphics.Gloss.Internals.Interface.Backend (
    module Graphics.Gloss.Internals.Interface.Backend.Types,
    module Graphics.Gloss.Internals.Interface.Backend.GLFW,
    defaultBackendState,
)
where

import Graphics.Gloss.Internals.Interface.Backend.GLFW
import Graphics.Gloss.Internals.Interface.Backend.Types

defaultBackendState :: GLFWState
defaultBackendState = initBackendState
