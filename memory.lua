local memory = {}
local pageSize = 2^16

function memory.new(pages)
  local t = {}

  t.size = pages
  t.realSize = pages * pageSize
  t.data = ffi.new("uint8_t[" .. t.realSize .. "]")
  t.data4 = ffi.cast("uint32_t*", t.data)

  setmetatable(t, {__index = memory})
  return t
end

function memory:expand(pages)
  local oldSize = self.size
  self.size = pages
  self.realSize = pages * pageSize
  local newData = ffi.new("uint8_t[" .. self.realSize .. "]")
  local newData4 = ffi.cast("uint32_t*", self.data)
  ffi.copy(newData, self.data, oldSize)
  self.data = newData
  self.data4 = newData4
end

function memory:read(addr, size)
  if addr < 0 or addr > self.realSize - size then
    error("Attempted read outside of allocated memory pages", 2)
  end

  local box = 0
  if size > 4 then
    box = ffi.new("uint64_t")
  end

  for i = size, 1, -1 do
    box = bit.lshift(box, 8) + self.data[addr + i - 1]
  end

  return box
end

function memory:store(addr, value, size)
  if addr < 0 or addr > self.realSize - size then
    error("Attempted store outside of allocated memory pages", 2)
  end

  if size == 4 then
    self.data4[addr / 4] = value
  else
    for i = 1, size do
      self.data[addr + i - 1] = bit.band(value, 0xFF)
      value = bit.rshift(value, 8)
    end
  end
end

function memory:readFloat(addr, size)
  if addr < 0 or addr > self.realSize - size then
    error("Attempted read outside of allocated memory pages", 2)
  end

  if ffi then
    local floatArr = self.data + addr

    local floatPtr
    if size == 4 then
      floatPtr = ffi.cast("float*", floatArr)
    elseif size == 8 then
      floatPtr = ffi.cast("double*", floatArr)
    else
      error("Invalid floating point type '" .. size .. "'", 0)
    end

    return floatPtr[0]
  else
    error("Floating point immedization has not been implemented on non-ffi systems yet", 0)
  end
end

function memory:storeFloat(addr, value, size)
  if addr < 0 or addr > self.realSize - size then
    error("Attempted store outside of allocated memory pages", 2)
  end

  if ffi then
    local floatPtr
    if size == 4 then
      floatPtr = ffi.cast("float*", self.data + addr)
    elseif size == 8 then
      floatPtr = ffi.cast("double*", self.data + addr)
    else
      error("Invalid floating point type '" .. bytes .. "'", 0)
    end

    floatPtr[0] = value
  else
    error("Floating point store has not been implemented on non-ffi systems yet", 0)
  end
end

function memory:linearStore(addr, bytes)
  local size = #bytes
  if addr < 0 or addr > self.realSize - size then
    error("Attempted store outside of allocated memory pages", 2)
  end

  for i = 1, size do
    self.data[addr + i - 1] = ffi.cast("uint8_t", bytes:sub(i, i):byte())
  end
end

function memory:linearRead(addr, size)
  if addr < 0 or addr > self.realSize - size then
    error("Attempted read outside of allocated memory pages", 2)
  end

  local outStr = ""
  for i = 1, size do
    outStr = outStr .. string.char(self.data[addr + i - 1])
  end

  return outStr
end

return memory
