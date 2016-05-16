module Hate.Graphics.Backend.Common.Pipeline where

import qualified Graphics.Rendering.OpenGL as GL

-- |Pipeline object is a complete package needed to render something on the screen.
data Pipeline = Pipeline {
    vertexShader   :: GL.Shader,
    fragmentShader :: GL.Shader,
    program        :: GL.Program
    }
