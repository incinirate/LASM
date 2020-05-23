local loader = require("loader")
fs = fs or io
ffi = ffi or require("ffi")

local args = {...}
if #args ~= 1 then
  print("Usage: russet <file.wasm>", 8)
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
instance:link("./gol.js", "__wbg_printext_562a3fa68a631acc", function(offset, length)
  local str = linearRead(instance.exports.memory, offset, length)
  print(str)
end)



if instance.startFunc then
  instance.startFunc()
end

if instance.exports.init then
  instance.exports.init()
end

instance.exports.greet(1, 1)

-- print(instance.exports.main())
-- instance.exports.set(0, 10) print(instance.exports.getA() .. ", " .. instance.exports.getB())
-- instance.exports.set(1, 20) print(instance.exports.getA() .. ", " .. instance.exports.getB())
-- instance.exports.set(2, 30) print(instance.exports.getA() .. ", " .. instance.exports.getB())
-- instance.exports.set(0, 42) print(instance.exports.getA() .. ", " .. instance.exports.getB())
-- instance.exports.set(1, 42) print(instance.exports.getA() .. ", " .. instance.exports.getB())

--[[

set(0, 10) sets B = 10 and leaves A as it is
set(1, 20) sets A = 20 and leaves B as it is
set(2, 30) does nothing
set(0, 42) sets B = 42 and leaves A as it is
set(1, 42) sets A = 42 and then also B = 42


]]
