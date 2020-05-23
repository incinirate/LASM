local assert = {}

local Stream = {}
function Stream.cl_init(...)
  local t = {}
  setmetatable(t, {__index = Stream})

  t:init(...)
  return t
end

function Stream:init(text)
  self.text = text
  self.cursorCol = 1
  self.cursorLine = 1
  self.cursorAbs = 1
end

function Stream:peek(n)
  n = n or 1

  return self.text:sub(self.cursorAbs, self.cursorAbs + n)
end

function Stream:eat(n)
  n = n or 1

  local str = self:peek(n)
  for i = 1, n do
    local c = str:sub(i, i)
    if c == "\n" then
      self.cursorLine = self.cursorLine + 1
      self.cursorCol = 1
    else
      self.cursorCol = self.cursorCol + 1
    end
  end

  self.cursorAbs = self.cursorAbs + n
  return str
end

function Stream:has(n)
  n = n or 1
  return #self:peek(n) == n
end

setmetatable(Stream, {__call = Stream.cl_init})

local function expectChar(stream, char)
  if stream:peek() ~= char then
    error(("Expected '%s' at line %d; col %d, but got '%s'")
      :format(char, stream.cursorLine, stream.cursorCol, stream:peek()))
  end

  return stream:eat()
end

local function skipWhitespace(stream)
  while true do
    if stream:peek():match("^%s$") then
      stream:eat()
    elseif stream:peek(2) == ";;" then
      -- Line comment
      while stream:eat() ~= "\n" do end
    else
      break
    end
  end
end

local function parseIdent(stream)
  local ident, i = {}, 0
  while stream:peek():match("^[%w.]$") do
    i = i + 1
    ident[i] = stream:eat()
  end

  return {type = "ident", value = table.concat(i)}
end

local parseSElem -- Forward decleration
local function parseSExpr(stream)
  expectChar(stream, "(")

  skipWhitespace(stream)
  local name = parseIdent(stream)

  local elems, i = {}, 0
  while true do
    skipWhitespace(stream)

    -- Exit condition
    local nextToken = stream:peek()
    if nextToken == ")" then break end

    i = i + 1
    elems[i] = parseSElem(stream)
  end

  expectChar(stream, ")")

  return {type = "sexpr", name = name, elems = elems}
end

-- Not local since we forward declared
function parseSElem(stream)
  local token = stream:peek()

  if token == "(" then
    return parseSExpr(stream)
  elseif token:match("^%w$") then
    return parseIdent(stream)
  else
    error(("Unexpected character '%s' at line %d; col %d"):format(token, stream.cursorLine, stream.cursorCol))
  end
end

local function parseWAT(stream)
  local components, i = {}, 0

  while stream:has() do
    skipWhitespace(stream)

    i = i + 1
    components[i] = parseSExpr(stream)
  end

  return components
end

function assert.loadAssertions(wat)
  local tree = parseWAT(Stream(wat))

  return tree
end

function assert.runTest(wat, wasm)

end

assert.util = {}
function assert.util.readFile(filename)
  local handle = fs.open(filename, "r")
  local data = handle:read("*a")
  handle:close()

  return data
end

function assert.util.readTest(testname)
  return assert.util.readFile(("tests/%s.wat"):format(testname))
end

return assert