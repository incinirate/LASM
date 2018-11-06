local decoder = require("decoder")
local opcodes = require("opcodes")
local parsers = require("parsers")
local compiler = require("compiler")

local loader = {}

local kinds = {
  Function = 0,
  Table = 1,
  Memory = 2,
  Global = 3
}

function loader.load(wasm)
  local t = {}

  local sectionData = decoder.decode(wasm)
  local instance = compiler.newInstance(sectionData)

  local exports = {}

  -- Link exported functions
  for k, v in pairs(sectionData[7]) do
    if v.kind == kinds.Function then
      -- exports[k] = function(...)
      --   return instance:call(v.index, ...)
      -- end
    elseif v.kind == kinds.Memory then
      exports[k] = instance:indexMemory(v.index)
    elseif v.kind == kinds.Table then
      exports[k] = instance.tables[v.index]
    else
      error("Unsupported export: '" .. v.kind .. "'", 0)
    end
  end

  -- if sectionData[8] then
  --   t.startFunc = function(...)
  --     return instance:call(sectionData[8], ...)
  --   end
  -- end

  t.exports = exports
  t.instance = instance
  setmetatable(t, {__index = loader})
  return t
end

function loader:link(...)
  return self.instance:link(...)
end

return loader
