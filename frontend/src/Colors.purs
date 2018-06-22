module Colors where

import MaterialUI.MuiThemeProvider (ColorPalette)


palette :: {primary :: ColorPalette, secondary :: ColorPalette}
palette =
  { primary:
    { light: "#ffe97d"
    , main: "#ffb74d"
    , dark: "#c88719"
    , contrastText: "#000000"
    }
  , secondary:
    { light: "#e4e65e"
    , main: "#afb42b"
    , dark: "#7c8500"
    , contrastText: "#000000"
    }
  }
