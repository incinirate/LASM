local bit64 = {}

local bit32 = bit or bit32 or require("bit32")

local b32lshift = bit32.lshift or bit32.blshift
local b32rshift = bit32.blogic_rshift or bit32.rshift
local b32arshift = bit32.arshift or bit32.rshift or bit32.brshift
local b32not, b32and, b32or, b32xor = bit32.bnot, bit32.band, bit32.bor, bit32.bxor

local low32Mask  = 0xFFFFFFFF
local high16Mask = 0xFFFF0000
local low16Mask  = 0x0000FFFF
local carryBit = 0x100000000
local high32Bit = 0x80000000

local constZero = {0, 0}
local constOne = {1, 0}
local constNegOne = {0xFFFFFFFF, 0xFFFFFFFF}

local floor = math.floor
local ceil = math.ceil
local min, max, abs = math.min, math.max, math.abs

local function xp(n, ...)
    local ags = {...}
    n = n or 8
    print((("%%0%dX "):format(n)):rep(#ags):format(...))
end


function bit64.newInt(v, hv)
    v, hv = v or 0, hv or 0
    assert(v >= 0 and hv >= 0, "newInt cannot be called with negative components, use :arInverse()")

    local t = {v, hv}
    return setmetatable(t, bit64)
end

function bit64.copy(n)
    return bit64.newInt(n[1], n[2])
end
bit64.clone = bit64.copy -- Alias

-- Pure functions
function bit64:plus(b)
    local sv, shv = self[1], self[2]
    local ov, ohv = b[1], b[2]

    local nv, nhv = sv + ov, shv + ohv
    if nv > low32Mask then
        -- Possible values are 0x1
        nv = nv - carryBit
        nhv = nhv + 1
    end

    -- Overflow behavior: ignore high bit
    if nhv > low32Mask then
        nhv = nhv - carryBit
    end

    return nv, nhv
end

function bit64:minus(b)
    return self:plus({bit64.unaryMinus(b)})
end

local function multiply32(a, b)
    local a_l, a_h = b32and(a, low16Mask), b32rshift(b32and(a, high16Mask), 16)
    local b_l, b_h = b32and(b, low16Mask), b32rshift(b32and(b, high16Mask), 16)

    local r_3h = bit64.newInt(0, a_h*b_h)
    local r_hc = a_l*b_h + a_h*b_l
    local r_hcV = b32and(low16Mask, r_hc)
    local r_hcU = b32lshift(r_hcV, 16)
    local r_hcO = b32rshift((r_hc - r_hcV) / 2, 15)
    local r_h = bit64.newInt(r_hcU, r_hcO)
    local r_l = bit64.newInt(a_l*b_l, 0)

    return r_3h:add(r_h):plus(r_l)
end

function bit64:times(b)
    local sv, shv = self[1], self[2]
    local ov, ohv = b[1], b[2]

    local b0 = bit64.newInt(multiply32(sv, ov))
    local b1 = bit64.newInt(multiply32(shv, ov))
    local b2 = bit64.newInt(multiply32(sv, ohv))
    local b3 = bit64.newInt(0, (b1:plus(b2)))
    return b0:plus(b3)
end

-- Division algorithm shamelessly stolen from
-- https://github.com/llvm-mirror/compiler-rt/blob/master/lib/builtins/udivmoddi4.c
local n_uword_bits = 32
local n_udword_bits = 64
function bit64:dividedByU(d)
    local n = self
    local q = bit64.newInt()
    local r = bit64.newInt()

    local nlow, nhigh = n[1], n[2]
    local dlow, dhigh = d[1], d[2]

    local sr = 0

    -- Special cases, X is unknown, K != 0
    if nhigh == 0 then
        if dhigh == 0 then
            -- 0 X
            -- ---
            -- 0 X
            if dlow == 0 then
                error("Integer divide by zero")
            end

            return floor(nlow / dlow), 0, nlow % dlow, 0
        end

        -- 0 X
        -- ---
        -- K X
        return 0, 0, nlow, 0
    end

    -- nhigh != 0
    if dlow == 0 then
        if dhigh == 0 then
            -- K X
            -- ---
            -- 0 0
            error("Integer divide by zero")
        end

        -- dhigh != 0
        if nlow == 0 then
            -- K 0
            -- ---
            -- K 0
            return floor(nhigh / dhigh), 0, 0, nhigh % dhigh
        end

        -- K K
        -- ---
        -- K 0
        if b32and(dhigh, dhigh - 1) == 0 then -- d power of 2
            return b32rshift(nhigh, bit64.countTrailingZeros({dhigh})), 0, nlow, b32and(nhigh, dhigh - 1)
        end

        sr = bit64.countLeadingZeros({dhigh}) - bit64.countLeadingZeros({nhigh})
        if sr < 0 then
            return 0, 0, nlow, nhigh
        end

        sr = sr + 1

        q[1] = 0
        q[2] = b32lshift(nlow, n_uword_bits - sr)

        r[2] = b32rshift(nhigh, sr)
        r[1] = b32or(b32lshift(nhigh, n_uword_bits - sr), b32rshift(nlow, sr))
    else -- dlow != 0
        if dhigh == 0 then
            -- K X
            -- ---
            -- 0 K
            if b32and(dlow, dlow - 1) == 0 then -- d power of 2
                if dlow == 1 then
                    return nlow, nhigh, b32and(nlow, dlow - 1), 0
                end

                sr = bit64.countTrailingZeros({dlow})
                return b32or(b32lshift(nhigh, n_uword_bits - sr), b32rshift(nlow, sr)), b32rshift(nhigh, sr),
                       b32and(nlow, dlow - 1), 0
            end

            sr = 1 + n_uword_bits + bit64.countLeadingZeros({dlow}) - bit64.countLeadingZeros({nhigh})

            if sr == n_uword_bits then
                q[1] = 0
                q[2] = nlow
                r[2] = 0
                r[1] = nhigh
            elseif sr < n_uword_bits then
                q[1] = 0
                q[2] = b32lshift(nlow, n_uword_bits - sr)
                r[2] = b32rshift(nhigh, sr)
                r[1] = b32or(b32lshift(nhigh, n_uword_bits - sr), b32rshift(nlow, sr))
            else
                q[1] = b32lshift(nlow, n_udword_bits - sr)
                q[2] = b32or(b32lshift(nhigh, n_udword_bits - sr),
                             b32rshift(nlow, sr - n_uword_bits))
                r[2] = 0
                r[1] = b32rshift(nhigh, sr - n_uword_bits)
            end
        else
            -- K X
            -- ---
            -- K K
            sr = bit64.countLeadingZeros({dhigh}) - bit64.countLeadingZeros({nhigh})
            if sr < 0 then
                return 0, 0, nlow, nhigh
            end

            sr = sr + 1

            q[1] = 0
            if sr == n_uword_bits then
                q[2] = nlow
                r[2] = 0
                r[1] = nhigh
            else
                q[2] = b32lshift(nlow, n_uword_bits - sr)
                r[2] = b32rshift(nhigh, sr)
                r[1] = b32or(b32lshift(nhigh, n_uword_bits - sr), b32rshift(nlow, sr))
            end
        end
    end

    -- Not a special case
    -- q and r are initialized with:
    -- q = n << (n_udword_bits - sr);
    -- r = n >> sr;
    -- 1 <= sr <= n_udword_bits - 1
    local carry = 0
    while sr > 0 do
        r[2] = b32or(b32lshift(r[2], 1), b32rshift(r[1], n_uword_bits - 1))
        r[1] = b32or(b32lshift(r[1], 1), b32rshift(q[2], n_uword_bits - 1))
        q[2] = b32or(b32lshift(q[2], 1), b32rshift(q[1], n_uword_bits - 1))
        q[1] = b32or(b32lshift(q[1], 1), carry)

        local t = bit64.newInt(bit64.minus(d, r)):sub(constOne):shr_s({n_udword_bits - 1})
        carry = b32and(t[1], 1)
        r:sub({bit64.band(d, t)})

        sr = sr - 1
    end

    q:shl({1})
    q[1] = b32or(q[1], carry)

    return q[1], q[2], r[1], r[2]
end

function bit64:dividedByS(o)
    local s_a = bit64.copy(bit64.isNegative(self) and constNegOne or constZero)
    local s_b = bit64.copy(bit64.isNegative(o)    and constNegOne or constZero)
    local a = bit64.newInt(bit64.bxor(self, s_a)):sub(s_a)
    local b = bit64.newInt(bit64.bxor(o   , s_b)):sub(s_b)

    s_a:bxored(s_b) -- s_a now holds the sign of the quotient

    local ir = bit64.newInt(a:dividedByU(b))
    return ir:bxored(s_a):minus(s_a)
end

function bit64:modU(b)
    local _, _, vl, vh = bit64.dividedByU(self, b)
    return vl, vh
end

function bit64:modS(b)
    local d = {bit64.dividedByS(self, b)}
    return self:minus({b:times(d)})
end

function bit64:raiseTo(exp)
    local base = self
    local res = bit64.copy(constOne)
    exp = bit64.copy(exp)

    while true do
        if exp:band(constOne) ~= 0 then
            res:mult(base)
        end

        exp:shr_u(constOne)
        if exp:eqz() == 1 then
            break
        end

        base:mult(base)
    end

    return res[1], res[2]
end

function bit64:lshift(c)
    local sl, sh = self[1], self[2]
    if sl == 0 and sh == 0 then
        return 0, 0
    end

    -- Mod c by 64
    c = b32and(c[1], 0x3F)
    if c == 0 then return sl, sh end

    local lowHiMask = b32arshift(high32Bit, min(31, c - 1))
    local remLowHi = b32rshift(b32and(sl, lowHiMask), n_uword_bits - c)

    return b32lshift(sl, c), b32lshift(sh, c) + remLowHi
end

function bit64:rshift(c)
    local sl, sh = self[1], self[2]
    if sl == 0 and sh == 0 then
        return 0, 0
    end

    -- Mod c by 64
    c = b32and(c[1], 0x3F)
    if c == 0 then return sl, sh end

    local hiLowMask = b32rshift(b32arshift(high32Bit, min(31, c - 1)), n_uword_bits - c)
    local remHiLow = b32lshift(b32and(sh, hiLowMask), n_uword_bits - c)

    return b32rshift(sl, c) + remHiLow, b32rshift(sh, c)
end

function bit64:arshift(c)
    local sl, sh = self[1], self[2]
    if sl == 0 and sh == 0 then
        return 0, 0
    end

    -- Mod c by 64
    c = b32and(c[1], 0x3F)
    if c == 0 then return sl, sh end

    local hiLowMask = b32rshift(b32arshift(high32Bit, min(31, c - 1)), n_uword_bits - c)
    local remHiLow = b32lshift(b32and(sh, hiLowMask), n_uword_bits - c)

    local hiRes = b32arshift(sh, min(31, c))
    local loRes = b32rshift(sl, c) + remHiLow
    if hiRes == low32Mask then
        loRes = b32or(loRes, b32arshift(high32Bit, c - 33))
    end

    return loRes, hiRes
end

function bit64:band(b)
    local sl, sh = self[1], self[2] or 0
    return b32and(sl, b[1]), b32and(sh, b[2] or 0)
end

function bit64:bor(b)
    local sl, sh = self[1], self[2] or 0
    return b32or(sl, b[1]), b32or(sh, b[2] or 0)
end

function bit64:bxor(b)
    local sl, sh = self[1], self[2] or 0
    return b32xor(sl, b[1]), b32xor(sh, b[2] or 0)
end

function bit64:countLeadingZeros()
    local x, hi32 = self[1], self[2] or 0
    if x == 0 and hi32 == 0 then return 64 end

	local n = 0
	if hi32 == 0 then
		n = n + 32
	else
		x = hi32
    end

	if b32and(x, 0xFFFF0000) == 0 then
		n = n + 16
		x = b32lshift(x, 16)
	end
	if b32and(x, 0xFF000000) == 0 then
		n = n + 8
		x = b32lshift(x, 8)
	end
	if b32and(x, 0xF0000000) == 0 then
		n = n + 4
		x = b32lshift(x, 4)
	end
	if b32and(x, 0xC0000000) == 0 then
		n = n + 2
		x = b32lshift(x, 2)
	end
	if b32and(x, 0x80000000) == 0 then
		n = n + 1
	end
	return n
end

function bit64:countTrailingZeros()
    local x, hi32 = self[1], self[2]
    if x == 0 and hi32 == 0 then return 64 end

	local n = 0
	if x == 0 then
        n = n + 32
        x = hi32
    end

	if b32and(x, 0x0000FFFF) == 0 then
		n = n + 16
		x = b32rshift ( x , 16 )
	end
	if b32and(x, 0x000000FF) == 0 then
		n = n + 8
		x = b32rshift ( x, 8)
	end
	if b32and(x, 0x0000000F) == 0 then
		n = n + 4
		x = b32rshift ( x, 4)
	end
	if b32and(x, 0x00000003) == 0 then
		n = n + 2
		x = b32rshift ( x, 2)
	end
	if b32and(x, 0x00000001) == 0 then
		n = n + 1
	end
	return n
end

local function count32SetBits(n)
    local i, c = 1, 0
    while i <= high32Bit do
        if b32and(n, i) ~= 0 then
            c = c + 1
        end
        i = i * 2
    end

    return c
end

function bit64:countSetBits()
    local sl, sh = self[1], self[2]
    return count32SetBits(sl) + count32SetBits(sh)
end

function bit64:sign()
    local sl, sh = self[1], self[2]
    if sl == 0 and sh == 0 then
        return 0
    end

    if b32and(sh, high32Bit) == 0 then
        return 1
    else
        return -1
    end
end

function bit64:unaryMinus()
    -- 2's complement
    -- invert all bits and then add one
    local lo = b32not(self[1]) % carryBit
    local hi = b32not(self[2] or 0) % carryBit
    return bit64.plus({lo, hi}, constOne)
end

function bit64:isPositive()
    return bit64.sign(self) == 1
end

function bit64:isNegative()
    return bit64.sign(self) == -1
end

function bit64:equals(b)
    return self[1] == b[1] and self[2] == b[2]
end

function bit64:eqz()
    return (self[1] == 0 and self[2] == 0) and 1 or 0
end

function bit64:eq(b)
    return (self[1] == b[1] and self[2] == b[2]) and 1 or 0
end

function bit64:ne(b)
    return (self[1] ~= b[1] or self[2] ~= b[2]) and 1 or 0
end


-- Mutating functions
function bit64:add(b)
    self[1], self[2] = self:plus(b)
    return self
end

function bit64:sub(b)
    self[1], self[2] = self:minus(b)
    return self
end

function bit64:mult(b)
    self[1], self[2] = self:times(b)
    return self
end

function bit64:div_u(b)
    self[1], self[2] = self:dividedByU(b)
    return self
end

function bit64:div_s(b)
    self[1], self[2] = self:dividedByS(b)
    return self
end

function bit64:rem_u(b)
    self[1], self[2] = self:modU(b)
    return self
end

function bit64:rem_s(b)
    self[1], self[2] = self:modS(b)
    return self
end

function bit64:shl(c)
    self[1], self[2] = self:lshift(c)
    return self
end

function bit64:shr_u(c)
    self[1], self[2] = self:rshift(c)
    return self
end

function bit64:shr_s(c)
    self[1], self[2] = self:arshift(c)
    return self
end

function bit64:ctz(c)
    self[1], self[2] = self:countTrailingZeros(c), 0
    return self
end

function bit64:clz(c)
    self[1], self[2] = self:countLeadingZeros(c), 0
    return self
end

function bit64:popcnt()
    self[1], self[2] = self:countSetBits(), 0
    return self
end

function bit64:banded(c)
    self[1], self[2] = self:band(c)
    return self
end

function bit64:bored(c)
    self[1], self[2] = self:bor(c)
    return self
end

function bit64:bxored(c)
    self[1], self[2] = self:bxor(c)
    return self
end

function bit64:arInverse()
    self[1], self[2] = self:unaryMinus()
    return self
end

-- Metamethods
function bit64:__tostring()
    return ("i64<%08X,%08X>"):format(self[2], self[1])
end

local function repack(fn)
    return function(...)
        return bit64.newInt(fn(...))
    end
end

bit64.__index = bit64
bit64.__call = function(t, ...) return bit64.newInt(...) end
bit64.__unm = repack(bit64.unaryMinus)
bit64.__add = repack(bit64.plus)
bit64.__sub = repack(bit64.minus)
bit64.__mul = repack(bit64.times)
bit64.__div = repack(bit64.dividedByS)
bit64.__idiv = repack(bit64.dividedByS)
bit64.__mod = repack(bit64.modS)
bit64.__pow = repack(bit64.raiseTo)

-- Helper Constants
bit64.zero = function() return bit64.copy(constZero) end
bit64.one = function() return bit64.copy(constOne) end
bit64.negOne = function() return bit64.copy(constNegOne) end

setmetatable(bit64, bit64)
return bit64
