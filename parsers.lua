local parsers = {}

function parsers.parseLEBu(stream, nBytes)
  local result, byte = 0
  local bitCnt = nBytes * 7
  for shift = 0, bitCnt, 7 do
    byte, stream = stream:sub(1, 1):byte(), stream:sub(2)
    result = bit.bor(result, bit.lshift(bit.band(byte, 0x7F), shift))
    if bit.band(byte, 0x80) == 0 then
      break
    end
  end

  return result, stream
end

function parsers.parseLEBs(stream, nBytes)
  local result, byte = 0
  local bitCnt = nBytes * 7
  local bitMask = 0
  for shift = 0, bitCnt, 7 do
    bitMask = bit.lshift(bitMask, 7) + 0x7F
    byte, stream = stream:sub(1, 1):byte(), stream:sub(2)
    result = bit.bor(result, bit.lshift(bit.band(byte, 0x7F), shift))
    if bit.band(byte, 0x80) == 0 then
      -- Perform signing
      if bit.band(bit.rshift(result, shift), 0x40) ~= 0 then
        result = -bit.band(bit.bnot(result) + 1, bitMask)
      end

      break
    end
  end

  return result, stream
end

function parsers.parseVLString(stream)
  local stringLen
  stringLen, stream = parsers.parseLEBu(stream, 4)

  return stream:sub(1, stringLen), stream:sub(stringLen + 1)
end

function parsers.parseFloat(stream, bytes)
  if ffi then
    local floatArr = ffi.new("uint8_t[" .. bytes .. "]")

    local byteStr
    byteStr, stream = stream:sub(1, bytes), stream:sub(bytes + 1)

    for i = 1, #byteStr do
      floatArr[i - 1] = byteStr:byte(i)
    end

    local floatPtr
    if bytes == 4 then
      floatPtr = ffi.cast("float*", floatArr)
    elseif bytes == 8 then
      floatPtr = ffi.cast("double*", floatArr)
    else
      error("Invalid floating point type '" .. bytes .. "'", 0)
    end

    return floatPtr[0], stream
  else
    error("Floating point immedization has not been implemented on non-ffi systems yet", 0)
  end
end

return parsers
