local opcodes = require("opcodes")
local parsers = require("parsers")

local magic = "\x00\x61\x73\x6D\x01\x00\x00\x00"
local decoder = {}

-- Valid types for signatures n stuff
local types = {
  [0x7f] = 1, -- i32
  [0x7e] = 2, -- i64
  [0x7d] = 3, -- f32
  [0x7c] = 4, -- f64
  [0x70] = 5, -- anyfunc
  [0x60] = 6, -- func
  [0x40] = 7  -- psuedo 'empty_block' type
}

local function nibble(stream)
  return stream:sub(1, 1), stream:sub(2)
end

local kinds = {
  Function = 0,
  Table = 1,
  Memory = 2,
  Global = 3
}

local typeMap = {
  NONE = 0,
  I32  = 1,
  I64  = 2,
  F32  = 3,
  F64  = 4,
  BLOC = 7,
  VUI1 = 10,
  VUI3 = 11,
  VUI6 = 12,
  VSI3 = 13,
  VSI6 = 14,
  VF32 = 15,
  VF64 = 16,
  BRTB = 17,
  CALI = 18,
  MEMI = 19
}

local function decodeImmediate(type, stream)
  local result
  if type == typeMap.VUI1 then
    result, stream = parsers.parseLEBu(stream, 1)
  elseif type == typeMap.VUI3 then
    result, stream = parsers.parseLEBu(stream, 4)
  elseif type == typeMap.VSI3 then
    result, stream = parsers.parseLEBs(stream, 4)
  elseif type == typeMap.VSI6 then
    result, stream = parsers.parseLEBs(stream, 8)
  elseif type == typeMap.VF64 then
    result, stream = parsers.parseFloat(stream, 8)
  elseif type == typeMap.VF32 then
    result, stream = parsers.parseFloat(stream, 4)
  elseif type == typeMap.BLOC then
    result, stream = parsers.parseLEBs(stream, 1)
  elseif type == typeMap.MEMI then
    -- We don't (currently) care about the alignment or the offset
    _, stream = parsers.parseLEBu(stream, 1)
    _, stream = parsers.parseLEBu(stream, 1)
  else
    error("Unsupported immediate type required: '" .. type .. "'", 0)
  end

  return result, stream
end

local function decodeFunctionBody(stream)
  local body = {}

  while #stream > 1 do
    local opcode
    opcode, stream = nibble(stream)
    opcode = opcode:byte()

    local opcodeDef = opcodes.codes[opcode]
    if not opcodeDef then
      error(("Unsupported opcode: '%x'"):format(opcode), 0)
    end

    local instr = {
      name = opcodeDef.textName,
      enum = opcodeDef.enumName,
      proto = opcodeDef
    }

    local immediate = opcodeDef.immediate

    if immediate ~= typeMap.NONE then
      instr.imVal, stream = decodeImmediate(immediate, stream)
    end

    body[#body + 1] = instr
  end

  if stream:byte() ~= opcodes.enum.End.opcode then
    error("Function declaration did not end with 'End' opcode", 0)
  end

  return body
end

local function parseInitializerExpr(stream)
  local opcode
  opcode, stream = nibble(stream)
  opcode = opcode:byte()

  
  local def = opcodes.codes[opcode]
  local initType = def.immediate
  local initVal
  initVal, stream = decodeImmediate(initType, stream)

  local endByte
  endByte, stream = nibble(stream)
  if endByte:byte() ~= opcodes.enum.End.opcode then
    error("getGlobal initializer expression NYI", 0)
  end

  return initVal, stream
end

local sections = {
  [1] = function(stream) -- Types section
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local typeDeclarations = {}
    
    for i = 1, count do
      local typeKind
      typeKind, stream = nibble(stream)
      typeKind = typeKind:byte()

      if types[typeKind] == 6 then -- Function Type
        local funcType = {
          params = {},
          returns = {}
        }

        -- Parse in the parameter types
        local paramCount
        paramCount, stream = nibble(stream)
        paramCount = parsers.parseLEBu(paramCount, 4)

        for j = 1, paramCount do
          local paramType
          paramType, stream = nibble(stream)
          paramType = paramType:byte()
          funcType.params[j] = types[paramType]
        end

        -- Parse in the return types
        local returnCount
        returnCount, stream = nibble(stream)
        returnCount = parsers.parseLEBu(returnCount, 4)

        for j = 1, returnCount do
          local returnType
          returnType, stream = nibble(stream)
          returnType = returnType:byte()
          funcType.returns[j] = types[returnType]
        end

        typeDeclarations[i - 1] = funcType
      else
        error("Unsupported type declaration '" .. typeKind .. "'", 0)
      end
    end

    return typeDeclarations
  end,
  [2] = function(stream) -- Imports Section
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local imports = {}

    for i = 1, count do
      local import = {}

      import.module, stream = parsers.parseVLString(stream)
      import.field, stream = parsers.parseVLString(stream)
      import.kind, stream = nibble(stream)
      import.kind = import.kind:byte()

      if import.kind == kinds.Function then
        import.typeIndex, stream = parsers.parseLEBu(stream, 4)
      else
        error("Unsupported import kind '" .. import.kind .. "'", 0)
      end

      imports[i - 1] = import
    end

    return imports
  end,
  [3] = function(stream) -- Function Declarations Dection
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local typeIndexes = {}
    
    for i = 1, count do
      local type
      type, stream = parsers.parseLEBu(stream, 4)
      typeIndexes[i - 1] = type
    end

    return typeIndexes
  end,
  [4] = function(stream) -- Table Section
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local tables = {}

    for i = 1, count do
      local type
      type, stream = nibble(stream)
      type = type:byte()

      local limits, flag = {}
      flag, stream = parsers.parseLEBu(stream, 1)
      limits.initial = parsers.parseLEBu(stream, 4)
      if flag == 1 then
        limits.maximum = parsers.parseLEBu(stream, 4)
      end

      tables[i - 1] = {type = type, limits = limits}
    end

    return tables
  end,
  [5] = function(stream) -- Memory Section
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local memories = {}

    for i = 1, count do
      local limits, flag = {}
      flag, stream = parsers.parseLEBu(stream, 1)
      limits.initial = parsers.parseLEBu(stream, 4)
      if flag == 1 then
        limits.maximum = parsers.parseLEBu(stream, 4)
      end

      memories[i - 1] = {limits = limits}
    end

    return memories
  end,
  [6] = function(stream) -- Global Declarations
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local globals = {}

    for i = 1, count do
      local type
      type, stream = nibble(stream)
      type = type:byte()

      local mutability
      mutability, stream = parsers.parseLEBu(stream, 1)
      
      local initVal
      initVal, stream = parseInitializerExpr(stream)
      globals[i - 1] = {
        type = type,
        mutability = mutability,
        value = initVal
      }
    end

    return globals
  end,
  [7] = function(stream) -- Exports Section
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local exports = {}
    
    for i = 1, count do
      local name
      name, stream = parsers.parseVLString(stream)

      local kind
      kind, stream = nibble(stream)
      kind = kind:byte()
      
      local index
      index, stream = parsers.parseLEBu(stream, 4)

      exports[name] = {index = index, kind = kind}
    end

    return exports
  end,
  [8] = function(stream) -- Start Function
    local index
    index, stream = parsers.parseLEBu(stream, 4)

    return index
  end,
  [9] = function(stream) -- Table Elements Section
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local tables = {}
    for i = 1, count do
      local tableIndex
      tableIndex, stream = parsers.parseLEBu(stream, 4)

      local offset
      offset, stream = parseInitializerExpr(stream)

      local elCount
      elCount, stream = parsers.parseLEBu(stream, 4)

      tables[tableIndex] = tables[tableIndex] or {}
      for j = 1, elCount do
        tables[tableIndex][offset + j - 1], stream = parsers.parseLEBu(stream, 4)
      end
    end

    return tables
  end,
  [10] = function(stream) -- Function Bodies
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local bodies = {}

    for i = 1, count do
      local func = {}

      local bodySize
      bodySize, stream = parsers.parseLEBu(stream, 4)

      local workingStream
      workingStream, stream = stream:sub(1, bodySize), stream:sub(bodySize + 1)

      do -- Decode Body
        local localCount
        localCount, workingStream = parsers.parseLEBu(workingStream, 4)

        -- Capture locals
        func.locals = {}
        for j = 1, localCount do
          local typeCount
          typeCount, workingStream = parsers.parseLEBu(workingStream, 4)

          local type
          type, workingStream = nibble(workingStream)
          type = type:byte()

          for k = 1, typeCount do
            func.locals[#func.locals + 1] = type
          end
        end

        -- Decode instructions
        func.instructions = decodeFunctionBody(workingStream)
      end

      bodies[i - 1] = func
    end

    return bodies
  end,
  [11] = function(stream) -- Data Section
    local count
    count, stream = parsers.parseLEBu(stream, 4)

    local segments = {}

    for i = 1, count do
      local memoryIndex
      memoryIndex, stream = parsers.parseLEBu(stream, 4)

      local offsetVal
      offsetVal, stream = parseInitializerExpr(stream)

      local data
      data, stream = parsers.parseVLString(stream)

      segments[i] = {index = memoryIndex, addr = offsetVal, data = data}
    end

    return segments
  end
}

function decoder.decode(stream)
  if stream:sub(1, #magic) ~= magic then
    error("Not a valid wasm 1.0 binary", 0)
  end
  -- Discard magic identifier
  stream = stream:sub(#magic + 1)
  
  -- Parse each section
  local sectionData = {[0] = {}}
  while #stream > 0 do
    local sectionID, sectionLength
    sectionID, stream = nibble(stream)
    sectionID = sectionID:byte()

    sectionLength, stream = parsers.parseLEBu(stream, 4)

    if sectionID == 0 then
      local sectionName
      sectionName = parsers.parseVLString(stream)

      local sectionStream
      sectionStream, stream = stream:sub(1, sectionLength), stream:sub(sectionLength + 1)
      sectionData[0][#sectionData[0] + 1] = {
        name = sectionName,
        data = sectionStream
      }
    else
      -- Well defined section
      if sections[sectionID] then
        local sectionStream
        sectionStream, stream = stream:sub(1, sectionLength), stream:sub(sectionLength + 1)
        sectionData[sectionID] = sections[sectionID](sectionStream, sectionData)
      else
        print("Invalid section id '" .. sectionID .. "'.. skipping..")
        local sectionStream
        sectionStream, stream = stream:sub(1, sectionLength), stream:sub(sectionLength + 1)
      end
    end
  end

  return sectionData
end

return decoder