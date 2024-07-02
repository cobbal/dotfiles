local modal = hs.hotkey.modal.new('ctrl', '\\', 'throwing window')

local throw = function (screen, xFrac, yFrac, wFrac, hFrac)
   xFrac = xFrac or 0.0
   yFrac = yFrac or 0.0
   wFrac = wFrac or 1.0
   hFrac = hFrac or 1.0
   modal:exit()
   local win = hs.window.focusedWindow()
   screen = screen or win:screen()
   local f = screen:frame()
   win:setFrame(
      hs.geometry.rect(
         f.x + f.w * xFrac,
         f.y + f.h * yFrac,
         f.w * wFrac,
         f.h * hFrac
      )
   )
end

local throwTo = function (index)
   local screens = hs.screen.allScreens()
   table.sort(screens, function (a, b) return a:frame().x < b:frame().x end)
   screen = screens[index]
   if not screen then return end
   throw(screen)
end

modal:bind({}, "escape", function () modal:exit() end)
modal:bind({}, "r", function () hs.reload() end)

modal:bind({}, "1", function () throwTo(1) end)
modal:bind({}, "2", function () throwTo(2) end)
modal:bind({}, "3", function () throwTo(3) end)

modal:bind({}, "\\", function () throw() end)
modal:bind({"ctrl"}, "\\", function () throw() end)
modal:bind({}, "[", function () throw(nil, 0.0, 0.0, 0.5, 1.0) end)
modal:bind({}, "]", function () throw(nil, 0.5, 0.0, 0.5, 1.0) end)

modal:bind({}, "j", function () throw(nil, 0.0, 0.5, 1.0, 0.5) end)
modal:bind({}, "k", function () throw(nil, 0.0, 0.0, 1.0, 0.5) end)
modal:bind({}, "h", function () throw(nil, 0.0, 0.0, 0.5, 1.0) end)
modal:bind({}, "l", function () throw(nil, 0.5, 0.0, 0.5, 1.0) end)