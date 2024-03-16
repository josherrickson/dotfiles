-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- Simpler tab bar, hide if one tab
config.use_fancy_tab_bar = false
config.hide_tab_bar_if_only_one_tab = true

-- Use native fullscreen
config.native_macos_fullscreen_mode = true

-- Remove padding
config.window_padding = {
  left = 0,
  right = 0,
  top = 0,
  bottom = 0,
}

-- config.keys = {
--    -- Disable font size increase with Alt +
--      {
--     key = '-',
--     mods = 'CTRLSHIFT',
--     action = wezterm.action.DisableDefaultAssignment,
--   },
-- }
-- For example, changing the color scheme:
-- config.color_scheme = 'Solarized (light) (terminal.sexy)'

config.font = wezterm.font('Comic Mono', { weight= 'Bold'})
config.font_size = 15

-- light mode/Dark mode
function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Light'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Solarized (dark) (terminal.sexy)'
  else
    return 'Solarized (light) (terminal.sexy)'
  end
end

config.color_scheme = scheme_for_appearance(get_appearance())


return config
