local loader = require("loader")

local pretty
do
  local inputColor, outputColor, errorColor = 11, 12, 8

  local syntaxTheme = {
    keyword = 13,        -- purple
    specialKeyword = 12, -- light blue
    func = 12,           -- light blue
    string = 8,          -- red
    stringEscape = 10,   -- yellow
    primitive = 9,       -- orange
    comment = 6,         -- dark gray
    catch = 16           -- everything else is white
  }
  
  local keywords = {
    [ "and" ] = true, [ "break" ] = true, [ "do" ] = true, [ "else" ] = true,
    [ "elseif" ] = true, [ "end" ] = true, [ "false" ] = true, [ "for" ] = true,
    [ "function" ] = true, [ "if" ] = true, [ "in" ] = true, [ "local" ] = true,
    [ "nil" ] = true, [ "not" ] = true, [ "or" ] = true, [ "repeat" ] = true, [ "return" ] = true,
    [ "then" ] = true, [ "true" ] = true, [ "until" ] = true, [ "while" ] = true,
  }
  
  local function prettySort(a, b)
    local ta, tb = type(a), type(b)
  
    if ta == "string" then return tb ~= "string" or a < b
    elseif tb == "string" then return false end
  
    if ta == "number" then return tb ~= "number" or a < b end
  
    return false
  end
  
  local debugInfo = type(debug) == "table" and type(debug.getinfo) == "function" and debug.getinfo
  local function getFunctionArgs(func)
    if debugInfo then
      local args = {}
      local hook = debug.gethook()
  
      local argHook = function()
        local info = debugInfo(3)
        if info.name ~= "pcall" then return end
  
        for i = 1, math.huge do
          local name = debug.getlocal(2, i)
  
          if name == "(*temporary)" or not name then
            debug.sethook(hook)
            return error()
          end
  
          args[#args + 1] = name
        end
      end
  
      debug.sethook(argHook, "c")
      pcall(func)
  
      return args
    end
  end
  
  local function prettyFunction(fn)
    if debugInfo then
      local info = debugInfo(fn, "S")
      if info.short_src and info.linedefined and info.linedefined >= 1 then
        local args
        if info.what == "Lua" then
          args = getFunctionArgs(fn)
        end
  
        if args then
          return "function<" .. info.short_src .. ":" .. info.linedefined .. ">(" .. table.concat(args, ", ") .. ")"
        else
          return "function<" .. info.short_src .. ":" .. info.linedefined .. ">"
        end
      end
    end
  
    return tostring(fn)
  end
  
  local function prettySize(obj, tracking, limit)
    local objType = type(obj)
    if objType == "string" then return #string.format("%q", obj):gsub("\\\n", "\\n")
    elseif objType == "function" then return #prettyFunction(obj)
    elseif objType ~= "table" or tracking[obj] then return #tostring(obj) end
  
    local count = 2
    tracking[obj] = true
    for k, v in pairs(obj) do
      count = count + prettySize(k, tracking, limit) + prettySize(v, tracking, limit)
      if count >= limit then break end
    end
    tracking[obj] = nil
    return count
  end
  
  local function prettyImpl(obj, tracking, width, height, indent, tupleLength)
    local objType = type(obj)
    if objType == "string" then
      local formatted = string.format("%q", obj):gsub("\\\n", "\\n")
  
      local limit = math.max(8, math.floor(width * height * 0.8))
      if #formatted > limit then
        shell.write(formatted:sub(1, limit-3), syntaxTheme.string)
        shell.write("...", syntaxTheme.string)
      else
        shell.write(formatted, syntaxTheme.string)
      end
  
      return
    elseif objType == "number" then
      return shell.write(tostring(obj), syntaxTheme.primitive)
    elseif objType == "boolean" then
      return shell.write(tostring(obj), syntaxTheme.primitive)
    elseif objType == "function" then
      return shell.write(prettyFunction(obj), 7)
    elseif objType ~= "table" or tracking[obj] then
      return shell.write(tostring(obj), 7)
    elseif (getmetatable(obj) or {}).__tostring then
      return shell.write(tostring(obj), 16)
    end
  
    local open, close = "{", "}"
    if tupleLength then open, close = "(", ")" end
  
    if (tupleLength == nil or tupleLength == 0) and next(obj) == nil then
      return shell.write(open .. close, 16)
    elseif width <= 7 then
      shell.write(open, 16) shell.write(" ... ", 6) shell.write(close, 16)
      return
    end
  
    local shouldNewline = false
    local length = tupleLength or #obj
  
    local size, children, keys, kn = 2, 0, {}, 0
    for k, v in pairs(obj) do
      if type(k) == "number" and k >= 1 and k <= length and k % 1 == 0 then
        local vs = prettySize(v, tracking, width)
        size = size + vs + 2
        children = children + 1
      else
        kn = kn + 1
        keys[kn] = k
  
        local vs, ks = prettySize(v, tracking, width), prettySize(k, tracking, width)
        size = size + vs + ks + 2
        children = children + 2
      end
  
      if size >= width * 0.6 then shouldNewline = true end
    end
  
    if shouldNewline and height <= 1 then
      shell.write(open, 16) shell.write(" ... ", 6) shell.write(close, 16)
      return
    end
  
    table.sort(keys, prettySort)
  
    local nextNewline, subIndent, childWidth, childHeight
    if shouldNewline then
      nextNewline, subIndent = ",\n", indent .. " "
  
      height = height - 2
      childWidth, childHeight = width - 2, math.ceil(height / children)
  
      if children > height then children = height - 2 end
    else
      nextNewline, subIndent = ", ", ""
  
      width = width - 2
      childWidth, childHeight = math.ceil(width / children), 1
    end
  
    shell.write(open .. (shouldNewline and "\n" or " "), 16)
  
    tracking[obj] = true
    local seen = {}
    local first = true
    for k = 1, length do
      if not first then shell.write(nextNewline, 16) else first = false end
      shell.write(subIndent, 16)
  
      seen[k] = true
      prettyImpl(obj[k], tracking, childWidth, childHeight, subIndent)
  
      children = children - 1
      if children < 0 then
        if not first then shell.write(nextNewline, 16) else first = false end
        shell.write(subIndent .. "...", 6)
        break
      end
    end
  
    for i = 1, kn do
      local k, v = keys[i], obj[keys[i]]
      if not seen[k] then
        if not first then shell.write(nextNewline, 16) else first = false end
        shell.write(subIndent, 16)
  
        if type(k) == "string" and not keywords[k] and k:match("^[%a_][%a%d_]*$") then
          shell.write(k .. " = ", 16)
          prettyImpl(v, tracking, childWidth, childHeight, subIndent)
        else
          shell.write("[", 16)
          prettyImpl(k, tracking, childWidth, childHeight, subIndent)
          shell.write("] = ", 16)
          prettyImpl(v, tracking, childWidth, childHeight, subIndent)
        end
  
        children = children - 1
        if children < 0 then
          if not first then shell.write(nextNewline) end
          shell.write(subIndent .. "...", 6)
          break
        end
      end
    end
    tracking[obj] = nil
  
    shell.write((shouldNewline and "\n" .. indent or " ") .. (tupleLength and ")" or "}"), 16)
  end
  
  function pretty(t, n)
    local width, height = gpu.width / (gpu.font.data.w + 1), gpu.height / (gpu.font.data.h + 1)
    return prettyImpl(t, {}, width, 999999999, "", n)
  end
end

local args = {...}
if #args ~= 1 then
  print("Usage: lterm <file.wasm>", 8)
  return
end

local data
do
  local handle = fs.open(args[1], "rb")
  data = handle:read("*a")
  handle:close()
end

local function linearRead(memory, offset, len)
  local ptr = memory + offset
  local val = ""
  for i = 1, len do
    val = val .. string.char(ptr[i - 1])
  end

  return val
end

local instance = loader.load(data)

local width, height
instance:link("env", "setDisplayMode", function(a, b, c)
  debugTrace("DISPLAY: ", a, b, c)
  width, height = b, c
  return
end)

local bufferStack = {}
instance:link("env", "pushFromMemory", function(offset, length)
  bufferStack[#bufferStack + 1] = linearRead(instance.exports.memory, offset, length)
end)

instance:link("env", "print", function()
  shell.write(bufferStack[#bufferStack])
  bufferStack[#bufferStack] = nil
end)

instance:link("env", "log", function()
  debugTrace(bufferStack[#bufferStack])
  bufferStack[#bufferStack] = nil
end)

local requestCounter = 0
instance:link("env", "readImage", function(fn)
  debugTrace(bufferStack[#bufferStack])
  debugTrace(fn)
  requestCounter = requestCounter + 1
  return requestCounter
end)

local inputType = 1
instance:link("env", "focusInput", function(type)
  debugTrace("Input type: " .. type)
  inputType = type
end)

instance:link("env", "shutdown", function()
  print()
  os.exit()
end)

local stepInterval = -1
instance:link("env", "setStepInterval", function(ms)
  stepInterval = ms/1000
end)

instance:link("env", "getNativeDisplayWidth", function()
  return gpu.width
end)

instance:link("env", "getNativeDisplayHeight", function()
  return gpu.height
end)

instance:link("env", "displayMemory", function(offset, length)
  local disp = linearRead(instance.exports.memory, offset, length)

  for i = 1, width do
    for j = 1, height do
      local ind = (j*width + i) * 4 + 1
      local char = disp:sub(ind, ind):byte() or 0
      local val = 3 * char / 255
      gpu.drawPixel(i, j, math.floor(val))
    end
  end
end)

instance:link("env", "startTone", function() end)
instance:link("env", "stopTone", function() end)


instance:link("env", "getGameAxisX", function()
  return 0 -- TODO
end)

instance:link("env", "getGameAxisY", function()
  return 0 -- TODO
end)

instance:link("env", "getGameButtonA", function()
  return 0 -- TODO
end)

instance:link("env", "getGameButtonB", function()
  return 0 -- TODO
end)

instance:link("env", "getGameButtonX", function()
  return 0 -- TODO
end)

instance:link("env", "getGameButtonY", function()
  return 0 -- TODO
end)


if instance.startFunc then
  instance.startFunc()
end
if instance.exports.init then
  instance.exports.init()
end

if instance.exports.step or instance.exports.display then
  local running = true

  local function event(e, ...)
    if e == "key" then
      local k = ...
      if k == "escape" then
        if instance.exports["break"] then
          instance.exports["break"]()
        end

        running = false
      end
    end
  end

  local function update(dt)
    if instance.exports.step then
      for i = 1, 10 do
        instance.exports.step(os.clock())
      end
    end
  end

  local function draw()
    if instance.exports.display then
      instance.exports.display(os.clock())
    end

    gpu.swap()
  end

  local eq = {}
  local last = os.clock()
  while running do
    while true do
      local a = {coroutine.yield()}
      if not a[1] then break end
      table.insert(eq, a)
    end

    while #eq > 0 do
      event(unpack(table.remove(eq, 1)))
    end

    update(os.clock() - last)
    last = os.clock()

    draw()
  end
end

print()
