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

  t.startFunc = instance.chunk.start
  t.exports = instance.chunk.exports
  t.instance = instance
  setmetatable(t, {__index = loader})
  return t
end

function loader:link(...)
  return self.instance:link(...)
end

return loader
