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

local instance = loader.load(data)
instance:link("env", "setDisplayMode", function(a, b, c)
  debugTrace("DISPLAY: ", a, b, c)
  return
end)

local bufferStack = {}
instance:link("env", "pushFromMemory", function(offset, length)
  bufferStack[#bufferStack + 1] = instance.exports.memory:linearRead(offset, length)
end)

instance:link("env", "print", function()
  print(bufferStack[#bufferStack])
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

if instance.startFunc then
  instance.startFunc()
end
instance.exports.init()
