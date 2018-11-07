local memoryManager = require("memory")

local compiler = {}
local instructions

local kinds = {
  Function = 0,
  Table = 1,
  Memory = 2,
  Global = 3
}

local pageSize = 2^16
local function makeMemory(name, size)
  return ([[
local %s = ffi.new("uint8_t[%d]")
local %sSize = %d
]]):format(name, size*pageSize, name, size)
end

local function constMemoryStore(memoryName, addr, segment)
  local src = ""
  for i = 1, #segment do
    src = src .. ([[%s[%d] = %d
]]):format(memoryName, addr + i - 1, segment:sub(i, i):byte())
  end

  return src
end

local function mangleImport(ns, field)
  return ns .. "__" .. field
end

local nameDebug = false
nameCounter = 0
nameAB = ("A"):byte()
function makeName()
  if nameDebug then
    nameCounter = nameCounter + 1
    return "var" .. nameCounter
  else
    local build = ""
    local thisCount = nameCounter
    repeat
      build = string.char(nameAB + (thisCount % 26)) .. build
      thisCount = math.floor(thisCount / 26)
    until thisCount == 0

    nameCounter = nameCounter + 1
    return build
  end
end

local prefabs = {
  unlinked = [[
for k, v in pairs(imports) do
  imports[k] = function()
    return error("Unlinked function: '" .. k .. "'")
  end
end
]],
  ifTrue = [[
local function checkCondition(cond)
  return cond == true or (cond ~= false and cond ~= 0)
end
]],
  memory = [[
local function storeMem(mem, memSize, addr, val, bytes)
  if addr < 0 or addr > memSize*(2^16) then
    error("Attempt to store outside bounds", 2)
  end
  
  if bytes == 8 then
    ffi.cast("uint" .. bytes .. "_t*", mem + addr)[0] = val
  else
    ffi.cast("int" .. bytes .. "_t*", mem + addr)[0] = val
  end
end
local function storeFloat(mem, memSize, addr, val, bytes)
  val = val or 0
  if addr < 0 or addr > memSize*(2^16) then
    error("Attempt to store outside bounds", 2)
  end
  
  if bytes == 8 then
    ffi.cast("double*", mem + addr)[0] = val
  else
    ffi.cast("float*", mem + addr)[0] = val
  end
end
local function readMem(mem, memSize, addr, bytes)
  if addr < 0 or addr > memSize*(2^16) then
    error("Attempt to read outside bounds " .. addr, 2)
  end
  if bytes == 8 then
    return ffi.cast("uint" .. bytes .. "_t*", mem + addr)[0]
  else
    return ffi.cast("int" .. bytes .. "_t*", mem + addr)[0]
  end
end
]],
cache = [[
local bit = require("bit")
]]
}

local function tee(stack, offset)
  offset = offset or 0
  return stack[#stack - offset]
end

local function pop(stack)
  local val = stack[#stack]
  stack[#stack] = nil
  return val
end

local function push(stack, val)
  stack[#stack + 1] = val
end

local function jumpInstr(loopQ)
  if loopQ then
    return "Start"
  else
    return "Finish"
  end
end

local generators
generators = {
  GetLocal = function(stack, instr, argList, fnLocals)
    local index = instr.imVal

    if index < #argList then
      stack[#stack + 1] = argList[index + 1]
    else
      stack[#stack + 1] = fnLocals[index - #argList + 1]
    end
  end,
  SetLocal = function(stack, instr, argList, fnLocals)
    local index = instr.imVal

    if index < #argList then
      return ("  %s = %s\n"):format(argList[index + 1], pop(stack))
    else
      return ("  %s = %s\n"):format(fnLocals[index - #argList + 1], pop(stack))
    end
  end,
  TeeLocal = function(stack, instr, argList, fnLocals)
    local index = instr.imVal

    if index < #argList then
      return ("  %s = %s\n"):format(argList[index + 1], tee(stack))
    else
      return ("  %s = %s\n"):format(fnLocals[index - #argList + 1], tee(stack))
    end
  end,

  GetGlobal = function(stack, instr, _, _, _, instance)
    push(stack, instance.globals[instr.imVal])
  end,
  SetGlobal = function(stack, instr, _, _, _, instance)
    return ("  %s = %s\n"):format(instance.globals[instr.imVal], pop(stack))
  end,

  Call = function(stack, instr, argList, fnLocals, blockStack, instance)
    -- TODO: ability to call imported functions
    local realIndex = instr.imVal - instance.functionImportCount
    local fnKind, fnName
    
    if realIndex >= 0 then
      local fn = instance.functions[instr.imVal]
      fnKind = instance.sectionData[3][realIndex]
      fnName = fn.name
    else
      -- Imported function
        -- TODO rework this as we allow more import types because it WILL break
      fnKind = instance.sectionData[2][instr.imVal].typeIndex
      local import = instance.sectionData[2][instr.imVal]
      fnName = ("imports.%s"):format(mangleImport(import.module, import.field))
    end

    
    local sig = instance.sectionData[1][fnKind]
    local passingArguments = {}
    for i = 1, #sig.params do
      passingArguments[#sig.params - i + 1] = pop(stack)
    end

    local results = {}
    for i = 1, #sig.returns do
      results[#results + 1] = makeName()
      push(stack, results[#results])
    end
    
    if #results > 0 then
      return ("  local %s = %s(%s)\n"):format(table.concat(results, ", "), fnName, table.concat(passingArguments, ", "))
    else
      return ("  %s(%s)\n"):format(fnName, table.concat(passingArguments, ", "))
    end
  end,

  I32Add = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(%s + %s)"):format(a, b))
  end,
  I32Sub = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(%s - %s)"):format(a, b))
  end,
  I32Mul = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(%s * %s)"):format(a, b))
  end,
  I32DivU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(math.abs(math.floor(%s / %s)))"):format(a, b))
  end,
  I32DivS = function(stack)
    -- This is wrong
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(math.floor(%s / %s))"):format(a, b))
  end,
  I32RemU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(math.floor(%s %% %s))"):format(a, b))
  end,
  I32Shl = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(bit.lshift(%s, %s))"):format(a, b))
  end,
  I32ShrU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(bit.rshift(%s, %s))"):format(a, b))
  end,
  I32ShrS = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(bit.arshift(%s, %s))"):format(a, b))
  end,
  I32And = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(bit.band(%s, %s))"):format(a, b))
  end,
  I32Or = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(bit.bor(%s, %s))"):format(a, b))
  end,
  I32Rotr = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(bit.ror(%s, %s))"):format(a, b))
  end,
  I32Rotl = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(bit.rol(%s, %s))"):format(a, b))
  end,
  I32Const = function(stack, instr)
    push(stack, tostring(instr.imVal))
  end,

  I32Ne = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("((%s ~= %s) and 1 or 0)"):format(a, b))
  end,
  I32Eq = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("((%s == %s) and 1 or 0)"):format(a, b))
  end,
  I32Eqz = function(stack)
    local a = pop(stack)
    push(stack, ("((%s == 0) and 1 or 0)"):format(a))
  end,
  I32GtU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("((%s > %s) and 1 or 0)"):format(a, b))
  end,
  I32GeU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("((%s >= %s) and 1 or 0)"):format(a, b))
  end,
  I32LtU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("((%s < %s) and 1 or 0)"):format(a, b))
  end,
  I32LeU = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("((%s <= %s) and 1 or 0)"):format(a, b))
  end,

  F64Div = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("(%s / %s)"):format(a, b))
  end,

  F64ConvertUI32 = function(stack)
    local a = pop(stack)
    push(stack, ("math.abs(%s)"):format(a))
  end,
  I32TruncUF64 = function(stack)
    local a = pop(stack)
    push(stack, ("math.floor(math.abs(%s))"):format(a))
  end,
  I32TruncSF64 = function(stack)
    local a = pop(stack)
    push(stack, ("math.floor(%s)"):format(a))
  end,
  F64Trunc = function(stack)
    local a = pop(stack)
    push(stack, ("math.floor(%s)"):format(a))
  end,
  F64Abs = function(stack)
    local a = pop(stack)
    push(stack, ("math.abs(%s)"):format(a))
  end,
  F64Floor = function(stack)
    local a = pop(stack)
    push(stack, ("math.floor(%s)"):format(a))
  end,
  F64Ceil = function(stack)
    local a = pop(stack)
    push(stack, ("math.ceil(%s)"):format(a))
  end,
  F64Nearest = function(stack)
    -- TODO: actually round
    local a = pop(stack)
    push(stack, ("math.floor(%s)"):format(a))
  end,
  F64Sqrt = function(stack)
    local a = pop(stack)
    push(stack, ("math.sqrt(%s)"):format(a))
  end,
  F64Min = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("math.min(%s, %s)"):format(a, b))
  end,
  F64Max = function(stack)
    local b = pop(stack)
    local a = pop(stack)
    push(stack, ("math.max(%s, %s)"):format(a, b))
  end,

  Block = function(stack, instr, argList, fnLocals, blockStack, instance, fn, customDo, loopq)
    loopq = loopq or false
    customDo = customDo or "do"
    if instr.imVal == -0x40 then
      -- Block does not return anything
      local blockLabel = makeName()
      push(blockStack, {label = blockLabel, exit = function()
        -- We got popped by an 'End', but we have nothing to return
      end, loop = loopq})
      return ("  %s ::%sStart::\n"):format(customDo, blockLabel)
    else
      -- Block returns something
      local blockResult = makeName()
      local blockLabel = makeName()
      push(blockStack, {label = blockLabel, exit = function(actor, shouldAct, shouldPop)
        -- We got popped by an 'End'
        local res = shouldPop and pop(stack) or tee(stack)
        actor(("  %s = %s\n"):format(blockResult, res))
        if shouldAct then
          push(stack, blockResult)
        end
      end, loop = loopq})
      return ("  local %s\n  %s ::%sStart::\n"):format(blockResult, customDo, blockLabel)
    end
  end,
  Loop = function(stack, instr, a, b, blockStack)
    return generators.Block(stack, instr, a, b, blockStack, nil, nil, nil, true)
  end,
  If = function(stack, instr, a, b, blockStack, c, d)
    local cond = pop(stack)
    return generators.Block(stack, instr, a, b, blockStack, c, d, ("if checkCondition(%s) then"):format(cond))
  end,
  Else = function(stack, _, _, _, blockStack)
    local effect = ""
    tee(blockStack).exit(function(str)
      effect = str
    end, false, true)

    return effect .. "  else\n"
  end,
  End = function(stack, _, _, _, blockStack)
    local effect = ""
    local block = pop(blockStack)
    block.exit(function(str)
      effect = str
    end, true, true)

    return ("::%sFinish::\n  %s  end\n"):format(block.label, effect)
  end,
  BrIf = function(stack, instr, a, b, blockStack, c, fn)
    local cond = pop(stack)
    local effect = ""
    for i = 0, instr.imVal - 1 do
      tee(blockStack).exit(function(str)
        effect = effect .. (str or "")
      end, false, false)
    end

    if instr.imVal == #blockStack then
      local retInstr = generators.Return(stack, instr, a, b, blockStack, c, fn)
      return ("  if checkCondition(%s) then\n  %s    %s\n  end\n"):format(cond, effect, retInstr)
    else
      local block = tee(blockStack, instr.imVal)
      local breakLabel = block.label
      local breakInstr = jumpInstr(block.loop)

      return ("  if checkCondition(%s) then\n  %s    goto %s%s\n  end\n"):format(cond, effect, breakLabel, breakInstr)
    end
  end,
  Br = function(stack, instr, a, b, blockStack, c, fn)
    local effect = ""
    for i = 0, instr.imVal - 1 do
      tee(blockStack).exit(function(str)
        effect = effect .. (str or "")
      end, false, false)
    end

    if instr.imVal == #blockStack then
      return generators.Return(stack, instr, a, b, blockStack, c, fn)
    end

    local block = tee(blockStack, instr.imVal)
    local jumpLabel = block.label
    local jumpInstr = jumpInstr(block.loop)

    return ("%s goto %s%s\n"):format(effect, jumpLabel, jumpInstr)
  end,
  Drop = function(stack)
    pop(stack)
  end,

  I32Load = function(stack, _, _, _, _, instance)
    local addr = pop(stack)
    push(stack, ([[(readMem(%s, %sSize, %s, 32))]]):format(instance.memories[0], instance.memories[0], addr)) -- ffi.cast("uint32_t*", %s + %s)[0]
  end,
  I32Load8U = function(stack, _, _, _, _, instance)
    local addr = pop(stack)
    push(stack, ([[(readMem(%s, %sSize, %s, 8))]]):format(instance.memories[0], instance.memories[0], addr)) -- ffi.cast("uint8_t*", %s + %s)[0]
  end,
  I32Load16U = function(stack, _, _, _, _, instance)
    local addr = pop(stack)
    push(stack, ([[(readMem(%s, %sSize, %s, 16))]]):format(instance.memories[0], instance.memories[0], addr)) -- ffi.cast("uint16_t*", %s + %s)[0]
  end,
  I32Store = function(stack, _, _, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    return ([[  storeMem(%s, %sSize, %s, %s, 32)]] .. "\n"):format(instance.memories[0], instance.memories[0], addr, value, "\n") -- ffi.cast("uint32_t*", %s + %s)[0] = %s%s
  end,
  I32Store8 = function(stack, _, _, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    return ([[  storeMem(%s, %sSize, %s, %s, 8)]] .. "\n"):format(instance.memories[0], instance.memories[0], addr, value, "\n") -- ffi.cast("uint8_t*", %s + %s)[0] = %s%s
  end,
  I32Store16 = function(stack, _, _, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    return ([[  storeMem(%s, %sSize, %s, %s, 16)]] .. "\n"):format(instance.memories[0], instance.memories[0], addr, value, "\n") -- ffi.cast("uint16_t*", %s + %s)[0] = %s%s
  end,
  I64Load = function(stack, _, _, _, _, instance)
    local addr = pop(stack)
    push(stack, ([[(ffi.cast("uint64_t*", %s + %s)[0])]]):format(instance.memories[0], addr))
  end,
  I64Store = function(stack, _, _, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    return ([[  storeMem(%s, %sSize, %s, %s, 64)]] .. "\n"):format(instance.memories[0], instance.memories[0], addr, value, "\n") -- ffi.cast("uint64_t*", %s + %s)[0] = %s%s
  end,
  F64Load = function(stack, _, _, _, _, instance)
    local addr = pop(stack)
    push(stack, ([[(ffi.cast("double*", %s + %s)[0])]]):format(instance.memories[0], addr))
  end,
  F64Store = function(stack, _, _, _, _, instance)
    local value = pop(stack)
    local addr = pop(stack)
    return ([[  storeFloat(%s, %sSize, %s, %s, 8)]]):format(instance.memories[0], instance.memories[0], addr, value, "\n") -- ffi.cast("double*", %s + %s)[0] = %s%s
  end,
  MemorySize = function(stack, _, _, _, _, instance)
    push(stack, instance.memories[0] .. "Size")
  end,
  MemoryGrow = function(stack, _, _, _, _, instance)
    local temp = makeName()
    local delta = pop(stack)
    push(stack, 2)

    -- TODO: find a better way to do this
    local extraLogic = ""
    if instance.sectionData[7] then
      for k, v in pairs(instance.sectionData[7]) do
        if v.kind == kinds.Memory then
          -- FIXME: This won't work after wasm MVP, because multiple memories
          extraLogic = ([[
  exportTable.%s = %s
]]):format(k, instance.memories[0])
        end
      end
    end

    return ([[  local %s = ffi.new("uint8_t[" .. (%sSize + %d)*%d .. "]")
  ffi.copy(%s, %s, %sSize*%d)
  %s, %sSize = %s, (%sSize + %d)
]]):format(temp, instance.memories[0], delta, pageSize,
           temp, instance.memories[0], instance.memories[0], pageSize,
           instance.memories[0], instance.memories[0], temp, instance.memories[0], delta) .. extraLogic
  end,

  Return = function(stack, _, _, _, _, instance, fn)
    local fnKind = instance.sectionData[3][fn]
    local sig = instance.sectionData[1][fnKind]

    local results = {}
    for i = 1, #sig.returns do
      push(results, pop(stack))
    end

    return ("  if true then return %s end\n"):format(table.concat(results, ", "))
  end,
  Unreachable = function()
    return "  error(\"Unreachable code reached..\", 2)\n"
  end,
  Nop = function() end
}

do -- Redundant Generators
  local g = generators
  g.I64Const = g.I32Const
  g.F32Const = g.I32Const
  g.F64Const = g.I32Const

  g.I32LtS = g.I32LtU
  g.I32LeS = g.I32LeU
  g.I32GtS = g.I32GtU
  g.I32GeS = g.I32GeU

  g.F64Ne = g.I32Ne
  g.F64Eq = g.I32Eq
  g.F64Ge = g.I32GeU
  g.F64Gt = g.I32GtU
  g.F64Lt = g.I32LtU
  g.F64Le = g.I32LeU

  g.F64Add = g.I32Add
  g.F64Sub = g.I32Sub
  g.F64Mul = g.I32Mul

  g.I32TruncSF32 = g.I32TruncSF64

  g.F64ConvertSI32 = g.Nop
end

function compiler.newInstance(sectionData)
  local t = {}

  t.sectionData = sectionData

  -- TODO 'imported' functions DONE?
  -- TODO setup memory DONE?
  -- TODO setup globals DONE?
  -- TODO setup table
  
  print("STARTED COMPILATION")
  shell.draw()

  t.source = ""

  t.importTable = {}

  t.tables = {}
  t.globals = {}
  t.memories = {}
  t.functions = {}
  t.functionImportCount = 0
  local importCount = 0
  if sectionData[2] then
    -- TODO other imports
    t.source = t.source .. "local imports = {"
    for k, v in pairs(sectionData[2]) do
      importCount = importCount + 1
      t.functionImportCount = t.functionImportCount + 1

      t.source = t.source .. ("%s = 0,"):format(mangleImport(v.module, v.field))
    end
    t.source = t.source .. "}\n" .. prefabs.unlinked
  end

  t.source = t.source .. prefabs.cache
  t.source = t.source .. prefabs.ifTrue
  t.source = t.source .. prefabs.memory

  if sectionData[7] then
    -- Forward declare export section so that we can swap out entries at runtime
    t.source = t.source .. "local exportTable = {}"
  end

  if sectionData[5] then
    -- Setup memory
    for k, v in pairs(sectionData[5]) do
      local name = makeName()
      t.source = t.source .. makeMemory(name, v.limits.initial)
      t.memories[k] = name
    end
    
  end

  if sectionData[6] then
    -- Setup globals
    for k, v in pairs(sectionData[6]) do
      t.globals[k] = makeName()
      t.source = t.source .. ("local %s = %d\n"):format(t.globals[k], v.value)
    end
  end

  if sectionData[9] then
    -- Setup tables TODO:
    for k, v in pairs(sectionData[9]) do
      local name = makeName()
      t.tables[k] = name
      t.source = t.source .. ("local %s = { %s }\n"):format(name, table.concat(v, ", "))
    end
  end

  if sectionData[11] then
    for i = 1, #sectionData[11] do
      local segment = sectionData[11][i]

      t.source = t.source .. constMemoryStore(t.memories[segment.index], segment.addr, segment.data)
    end
  end

  if sectionData[10] then
    local names = {}
    for k, v in pairs(sectionData[10]) do
      names[#names + 1] = makeName()
      t.functions[importCount + k] = {name = names[#names], index = k, info = v}
    end

    if #names > 0 then
      t.source = t.source .. "local " .. table.concat(names, ", ") .. "\n"
    end
  end

  for k, v in pairs(sectionData[10]) do
    -- Generate function body
    local argList = {}

    local fnKind = t.sectionData[3][k]
    local sig = t.sectionData[1][fnKind]
    for i = 1, #sig.params do
      argList[#argList + 1] = makeName()
    end

    local fnName = t.functions[importCount + k].name
    t.source = t.source .. ("function %s(%s)\n"):format(fnName, table.concat(argList, ", "))

    -- Function stack, used only for generation, we can optimize away the stack using inlining
    local valueStack = {}
    local blockStack = {}

    -- Generate function locals
    local fnLocals = {}
    for i = 1, #v.locals do
      fnLocals[i] = makeName()
      t.source = t.source .. ("  local %s = 0\n"):format(fnLocals[i])
    end

    -- Generate opcode instructions
    for i, instr in ipairs(v.instructions) do
      if generators[instr.enum] then
        local out = generators[instr.enum](valueStack, instr, argList, fnLocals, blockStack, t, k)
        if out then
          t.source = t.source .. out
        end
      else
        debugTrace("Source:\n" .. t.source)
        error("No generator for '" .. instr.enum .. "'")
      end
    end

    if #valueStack > 0 then
      -- results
      t.source = t.source .. "  return " .. table.concat(valueStack, ", ") .. "\n"
    end

    t.source = t.source .. "end\n"
  end

  -- Exports
  if sectionData[7] then
    for k, v in pairs(sectionData[7]) do
      t.source = t.source .. "exportTable."
      if v.kind == kinds.Function then
        t.source = t.source .. ("%s = %s\n"):format(k, t.functions[v.index].name)
      elseif v.kind == kinds.Memory then
        t.source = t.source .. ("%s = %s\n"):format(k, t.memories[v.index])
      elseif v.kind == kinds.Table then
        t.source = t.source .. ("%s = %s\n"):format(k, t.tables[v.index])
      else
        error("Unsupported export: '" .. v.kind .. "'", 0)
      end
    end
  end

  t.source = t.source .. "return { "

  if sectionData[7] then
    t.source = t.source .. "exports = exportTable, "
  end

  -- Import Linking
  if sectionData[2] then
    -- TODO other imports
    t.source = t.source .. "importTable = imports, "
  end

  if sectionData[8] then
    t.source = t.source .. "start = "
      .. ("%s "):format(t.functions[sectionData[8]].name)
  end

  t.source = t.source .. "}\n"

  do
    local handle = fs.open("debug.out.lua", "w")
    handle:write(t.source)
    handle:close()
  end

  debugTrace(t.source)

  local success, er = load(t.source)
  if not success then
    error("DID NOT COMPILE: " .. er)
  end

  local chunk = success()
  t.chunk = chunk

  setmetatable(t, {__index = compiler})
  return t
end

function compiler:link(module, field, value)
  if self.chunk.importTable then
    local ref = self.chunk.importTable[mangleImport(module, field)]
    if ref then
      self.chunk.importTable[mangleImport(module, field)] = value
    end
  end
end

return compiler
