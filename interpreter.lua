local memoryManager = require("memory")

local intepreter = {}
local instructions

function intepreter.newInstance(sectionData)
  local t = {}

  t.sectionData = sectionData

  -- TODO 'imported' functions DONE?
  -- TODO setup memory DONE?
  -- TODO setup globals DONE?
  -- TODO setup table
  
  t.importTable = {}

  t.tables = {}
  t.globals = {}
  t.memories = {}
  t.functions = {}
  t.functionImportCount = 0
  local importCount = 0
  if sectionData[2] then
    -- TODO other imports
    for k, v in pairs(sectionData[2]) do
      importCount = importCount + 1
      t.functionImportCount = t.functionImportCount + 1
      t.importTable[v.module .. "::" .. v.field] = {t.functions, k}
      t.functions[k] = function()
        error("Unlinked function '" .. v.module .. "::" .. v.field .. "'")
      end
    end
  end

  if sectionData[5] then
    -- Setup memory
    for k, v in pairs(sectionData[5]) do
      t.memories[k] = memoryManager.new(v.limits.initial)
    end
  end

  if sectionData[6] then
    -- Setup globals
    for k, v in pairs(sectionData[6]) do
      t.globals[k] = {
        type = v.type,
        mutable = v.mutability,
        value = v.value
      }
    end
  end

  if sectionData[9] then
    -- Setup tables
    for k, v in pairs(sectionData[9]) do
      t.tables[k] = v
    end
  end

  if sectionData[11] then
    for i = 1, #sectionData[11] do
      local segment = sectionData[11][i]

      t.memories[segment.index]:linearStore(segment.addr, segment.data)
    end
  end

  for k, v in pairs(sectionData[10]) do
    t.functions[importCount + k] = v
  end

  setmetatable(t, {__index = intepreter})
  return t
end

function intepreter:indexMemory(index)
  return self.memories[index]
end

function intepreter:link(module, field, value)
  local meta = self.importTable[module .. "::" .. field]
  if meta then
    meta[1][meta[2]] = value
  end
end

local function printStack(stack, prefix)
  local str = ""
  for i = 1, #stack do
    str = str .. tostring(stack[i]) .. " "
  end

  return prefix .. str
end

local lastChk = {}
local amtExecuted = 0
local byFunction = {}

function intepreter:call(funcIndex, ...)
  if self.functions[funcIndex] then
    byFunction[funcIndex] = byFunction[funcIndex] or 0
    local fn = self.functions[funcIndex]
    if type(fn) == "function" then
      return fn(...)
    else
      local instrSet = fn.instructions
      local stack = {...}
      for i = 1, #fn.locals do
        stack[#stack + 1] = 0
      end

      stack.blocks = {}

      local iPtr = 1
      while true do
        if iPtr > #instrSet then
          break
        end

        local normalControl = true

        local instr = instrSet[iPtr]
        if instructions[instr.enum] then
          amtExecuted = amtExecuted + 1
          byFunction[funcIndex] = byFunction[funcIndex] + 1

          local nPtr = instructions[instr.enum](stack, instr, instrSet, self, iPtr, funcIndex)
          if type(stack[#stack]) == "boolean" then
            stack[#stack] = stack[#stack] and 1 or 0
          end

          if nPtr then
            iPtr = nPtr
            normalControl = false
          end
        else
          error("Unsupported instruction '" .. instr.enum .. "' > " .. funcIndex .. " " .. iPtr, 0)
        end

        if normalControl then
          iPtr = iPtr + 1
        end
      end

      return stack[#stack]
    end
  else
    error("Undefined function '" .. funcIndex .. "'", 0)
  end
end

local function pop(stack)
  local val = stack[#stack]
  stack[#stack] = nil

  return val
end

local blockInstrs = {Block = true, Loop = true, If = true}
-- ONLY call this with the pointer pointing TO a block
local function skipBlock(iPtr, instrSet, brkOnElse)
  local stack = 1
  while stack > 0 do
    iPtr = iPtr + 1
    local instr = instrSet[iPtr]
    if blockInstrs[instr.enum] then
      stack = stack + 1
    elseif instr.enum == "End" then
      stack = stack - 1
    elseif instr.enum == "Else" then
      if stack == 1 and brkOnElse then
        return iPtr
      end
    end
  end

  return iPtr
end

local function num(val)
  local t = type(val) 
  if t == "number" then
    return val
  elseif t == "boolean" then
    return val == true and 1 or 0
  else
    return tonumber(val)
  end
end

instructions = {
-- INT32
  I32Const = function(stack, instr)
    stack[#stack + 1] = instr.imVal
  end,

  I32Add = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a + b
  end,
  I32Sub = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a - b
  end,
  I32Mul = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a * b
  end,
  I32DivU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = math.floor(math.abs(a / b))
  end,
  I32And = function(stack)
    local b = num(pop(stack))
    local a = num(pop(stack))
    stack[#stack + 1] = bit.band(a, b)
  end,
  I32Or = function(stack)
    local b = num(pop(stack))
    local a = num(pop(stack))
    stack[#stack + 1] = bit.bor(a, b)
  end,

  I32GtU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a > b
  end,
  I32GeU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a >= b
  end,
  I32LtU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a < b
  end,
  I32LeU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a <= b
  end,
  I32Eq = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a == b
  end,
  I32Ne = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a ~= b
  end,
  I32Eqz = function(stack)
    local a = pop(stack)
    stack[#stack + 1] = a == 0
  end,

-- Special FLOAT OPS
  I32TruncSF32 = function(stack)
    local a = pop(stack)
    stack[#stack + 1] = math.floor(a)
  end,
  I32TruncUF64 = function(stack)
    local a = pop(stack)
    stack[#stack + 1] = math.floor(a)
  end,
  I32TruncSF64 = function(stack)
    local a = pop(stack)
    stack[#stack + 1] = math.floor(a)
  end,
  F64ConvertUI32 = function(stack)
    local a = pop(stack)
    stack[#stack + 1] = math.abs(a)
  end,
  F64Div = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    stack[#stack + 1] = a / b
  end,

  GetGlobal = function(stack, instr, _, instance)
    local index = instr.imVal
    stack[#stack + 1] = instance.globals[index].value
  end,
  SetGlobal = function(stack, instr, _, instance)
    local index = instr.imVal
    local newValue = pop(stack)
    local global = instance.globals[index]
    if global.mutable then
      global.value = newValue
    else
      error("Global '" .. index .. "' is immutable", 0)
    end
  end,
  GetLocal = function(stack, instr)
    stack[#stack + 1] = stack[instr.imVal + 1]
  end,
  SetLocal = function(stack, instr)
    local val = pop(stack)
    stack[instr.imVal + 1] = val
  end,
  TeeLocal = function(stack, instr)
    stack[instr.imVal + 1] = stack[#stack]
  end,

  Call = function(stack, instr, _, instance)
    local realIndex = instr.imVal - instance.functionImportCount
    local fnKind
    
    if realIndex >= 0 then
      fnKind = instance.sectionData[3][realIndex]
    else
      -- Imported function
        -- TODO rework this as we allow more import types because it WILL break
      fnKind = instance.sectionData[2][instr.imVal].typeIndex
    end

    local sig = instance.sectionData[1][fnKind]

    local params = {}
    for i = 1, #sig.params do
      params[#sig.params - i + 1] = pop(stack)
    end

    local retCount = #sig.returns
    local ret = {instance:call(instr.imVal, unpack(params))}

    for i = 1, retCount do
      stack[#stack + 1] = ret[i]
    end
  end,

  -- INT MEMORY OPS
  I64Store = function(stack, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    instance.memories[0]:store(addr, value, 8)
  end,
  I64Load = function(stack, _, _, instance)
    local addr = pop(stack)
    local val = instance.memories[0].read(instance.memories[0], addr, 8)

    stack[#stack + 1] = val
  end,
  I32Store = function(stack, _, _, instance, iPtr, fn)
    local value = pop(stack)
    local addr = pop(stack)
    instance.memories[0]:store(addr, value, 4)
  end,
  I32Store8 = function(stack, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    instance.memories[0]:store(addr, value, 1)
  end,
  I32Store16 = function(stack, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    instance.memories[0]:store(addr, value, 2)
  end,
  I32Load = function(stack, _, _, instance)
    local addr = pop(stack)
    local val = instance.memories[0].read(instance.memories[0], addr, 4)

    stack[#stack + 1] = val
  end,
  I32Load8U = function(stack, _, _, instance)
    local addr = pop(stack)
    local val = instance.memories[0].read(instance.memories[0], addr, 1)
    -- Sign extend is broken
    -- if bit.band(val, 0x8) ~= 0 then
    --   -- val = -val
    -- end

    stack[#stack + 1] = val
  end,
  -- FLOAT MEMORY OPS
  F64Store = function(stack, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    instance.memories[0]:storeFloat(addr, value, 8)
  end,
  F64Load = function(stack, _, _, instance)
    local addr = pop(stack)
    stack[#stack + 1] = instance.memories[0]:readFloat(addr, 8)
  end,
  MemorySize = function(stack, _, _, instance)
    stack[#stack + 1] = instance.memories[0].size
  end,
  MemoryGrow = function(stack, _, _, instance)
    local delta = pop(stack)
    local osize = instance.memories[0].size

    instance.memories[0]:expand(delta)

    stack[#stack + 1] = osize
  end,

  Block = function(stack, _, _, _, iPtr)
    stack.blocks[#stack.blocks + 1] = iPtr
  end,
  Loop = function(stack, _, _, _, iPtr)
    stack.blocks[#stack.blocks + 1] = iPtr
  end,
  If = function(stack, _, instrSet, _, iPtr)
    local cond = pop(stack)
    if cond ~= 0 then
      stack.blocks[#stack.blocks + 1] = iPtr
    else
      return skipBlock(iPtr, instrSet, true) + 1
    end
  end,
  Br = function(stack, instr, instrSet)
    if instr.imVal == 0 then
      return (stack.blocks[#stack.blocks] or math.huge) + 1
    else
      local ptr
      for i = 1, instr.imVal do
        ptr = pop(stack.blocks)
      end

      return skipBlock(ptr, instrSet) + 1
    end
  end,
  BrIf = function(stack, instr, instrSet)
    local cond = pop(stack)
    if cond ~= 0 then
      return instructions.Br(stack, instr, instrSet)
    end
  end,
  Else = function(stack, _, instrSet)
    -- We hit Else's when we're done with the truthy part of an If
    local ptr = pop(stack.blocks)
    return skipBlock(ptr, instrSet) + 1
  end,
  End = function(stack)
    pop(stack.blocks)
  end,
  Return = function()
    return math.huge
  end,

  Drop = function(stack)
    pop(stack)
  end,
  Nop = function() end
}

do
  local i = instructions
  i.I64Const = i.I32Const
  i.I32LtS = i.I32LtU
  i.I32LeS = i.I32LeU
  i.I32GtS = i.I32GtU
  i.I32GeS = i.I32GeU

  i.F64Const = i.I32Const
  
  i.F64Eq = i.I32Eq
  i.F64Ne = i.I32Ne
  i.F64Gt = i.I32GtS
  i.F64Lt = i.I32LtS

  i.F64Mul = i.I32Mul
  i.F64Add = i.I32Add
  i.F64Sub = i.I32Sub
end

return intepreter
