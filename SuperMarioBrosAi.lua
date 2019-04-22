-- The following section is from lua-users.org.
-- SHA-256 code in Lua 5.2; based on the pseudo-code from
-- Wikipedia (http://en.wikipedia.org/wiki/SHA-2)


local band, rrotate, bxor, rshift, bnot =
  bit32.band, bit32.rrotate, bit32.bxor, bit32.rshift, bit32.bnot

local string, setmetatable, assert = string, setmetatable, assert

_ENV = nil

-- Initialize table of round constants
-- (first 32 bits of the fractional parts of the cube roots of the first
-- 64 primes 2..311):
local k = {
   0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
   0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
   0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
   0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
   0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
   0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
   0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
   0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
   0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
   0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
   0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
   0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
   0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
   0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
   0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
   0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
}


-- transform a string of bytes in a string of hexadecimal digits
local function str2hexa (s)
  local h = string.gsub(s, ".", function(c)
              return string.format("%02x", string.byte(c))
            end)
  return h
end


-- transform number 'l' in a big-endian sequence of 'n' bytes
-- (coded as a string)
local function num2s (l, n)
  local s = ""
  for i = 1, n do
    local rem = l % 256
    s = string.char(rem) .. s
    l = (l - rem) / 256
  end
  return s
end

-- transform the big-endian sequence of four bytes starting at
-- index 'i' in 's' into a number
local function s232num (s, i)
  local n = 0
  for i = i, i + 3 do
    n = n*256 + string.byte(s, i)
  end
  return n
end


-- append the bit '1' to the message
-- append k bits '0', where k is the minimum number >= 0 such that the
-- resulting message length (in bits) is congruent to 448 (mod 512)
-- append length of message (before pre-processing), in bits, as 64-bit
-- big-endian integer
local function preproc (msg, len)
  local extra = -(len + 1 + 8) % 64
  len = num2s(8 * len, 8)    -- original len in bits, coded
  msg = msg .. "\128" .. string.rep("\0", extra) .. len
  assert(#msg % 64 == 0)
  return msg
end


local function initH224 (H)
  -- (second 32 bits of the fractional parts of the square roots of the
  -- 9th through 16th primes 23..53)
  H[1] = 0xc1059ed8
  H[2] = 0x367cd507
  H[3] = 0x3070dd17
  H[4] = 0xf70e5939
  H[5] = 0xffc00b31
  H[6] = 0x68581511
  H[7] = 0x64f98fa7
  H[8] = 0xbefa4fa4
  return H
end


local function initH256 (H)
  -- (first 32 bits of the fractional parts of the square roots of the
  -- first 8 primes 2..19):
  H[1] = 0x6a09e667
  H[2] = 0xbb67ae85
  H[3] = 0x3c6ef372
  H[4] = 0xa54ff53a
  H[5] = 0x510e527f
  H[6] = 0x9b05688c
  H[7] = 0x1f83d9ab
  H[8] = 0x5be0cd19
  return H
end


local function digestblock (msg, i, H)

    -- break chunk into sixteen 32-bit big-endian words w[1..16]
    local w = {}
    for j = 1, 16 do
      w[j] = s232num(msg, i + (j - 1)*4)
    end

    -- Extend the sixteen 32-bit words into sixty-four 32-bit words:
    for j = 17, 64 do
      local v = w[j - 15]
      local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
      v = w[j - 2]
      local s1 = bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
      w[j] = w[j - 16] + s0 + w[j - 7] + s1
    end

    -- Initialize hash value for this chunk:
    local a, b, c, d, e, f, g, h =
        H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]

    -- Main loop:
    for i = 1, 64 do
      local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
      local maj = bxor(band(a, b), band(a, c), band(b, c))
      local t2 = s0 + maj
      local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
      local ch = bxor (band(e, f), band(bnot(e), g))
      local t1 = h + s1 + ch + k[i] + w[i]

      h = g
      g = f
      f = e
      e = d + t1
      d = c
      c = b
      b = a
      a = t1 + t2
    end

    -- Add (mod 2^32) this chunk's hash to result so far:
    H[1] = band(H[1] + a)
    H[2] = band(H[2] + b)
    H[3] = band(H[3] + c)
    H[4] = band(H[4] + d)
    H[5] = band(H[5] + e)
    H[6] = band(H[6] + f)
    H[7] = band(H[7] + g)
    H[8] = band(H[8] + h)

end


local function finalresult224 (H)
  -- Produce the final hash value (big-endian):
  return
    str2hexa(num2s(H[1], 4)..num2s(H[2], 4)..num2s(H[3], 4)..num2s(H[4], 4)..
             num2s(H[5], 4)..num2s(H[6], 4)..num2s(H[7], 4))
end


local function finalresult256 (H)
  -- Produce the final hash value (big-endian):
  return
    str2hexa(num2s(H[1], 4)..num2s(H[2], 4)..num2s(H[3], 4)..num2s(H[4], 4)..
             num2s(H[5], 4)..num2s(H[6], 4)..num2s(H[7], 4)..num2s(H[8], 4))
end


----------------------------------------------------------------------
local HH = {}    -- to reuse

local function hash224 (msg)
  msg = preproc(msg, #msg)
  local H = initH224(HH)

  -- Process the message in successive 512-bit (64 bytes) chunks:
  for i = 1, #msg, 64 do
    digestblock(msg, i, H)
  end

  return finalresult224(H)
end


local function hash256 (msg)
  msg = preproc(msg, #msg)
  local H = initH256(HH)

  -- Process the message in successive 512-bit (64 bytes) chunks:
  for i = 1, #msg, 64 do
    digestblock(msg, i, H)
  end

  return finalresult256(H)
end
----------------------------------------------------------------------
local mt = {}

local function new256 ()
  local o = {H = initH256({}), msg = "", len = 0}
  setmetatable(o, mt)
  return o
end

mt.__index = mt

function mt:add (m)
  self.msg = self.msg .. m
  self.len = self.len + #m
  local t = 0
  while #self.msg - t >= 64 do
    digestblock(self.msg, t + 1, self.H)
    t = t + 64 
  end
  self.msg = self.msg:sub(t + 1, -1)
end


function mt:close ()
  self.msg = preproc(self.msg, self.len)
  self:add("")
  return finalresult256(self.H)
end
----------------------------------------------------------------------

return {
  hash224 = hash224,
  hash256 = hash256,
  new256 = new256,
}
--[[
A Genetic Algorithm implementation to play Super Mario Bros.(NES).
Written in Lua, runs on FCEUX 2.2.3.


Chromosome is encoded as follows: 
A 32-bit binary string.
--]]




--MEMORY ADDRESSES USED IN THE RAM ON FCEUX.
local score_hundred= 0x0373;               --The Score(Can be used for fitness)
local score_tens=0x0374;
local score_unit =0x0375;

local lives_addr=0x000D;                   --Number of lives remaining(Debugging Only!)
local ball_pos_y_addr=0x0037;              --Y Pos of the ball(Used for debugging only!)
local ball_pos_x_addr=0x0038;              --X Pos of the ball(Used for debugging only!)
local no_of_blocks_addr=0x000F;            --Number of blocks remaining
local pad_addr=0x011C;                     --Current X Pos of the Paddle. Used to check if paddle has gone out of bounds.(Explained Later)
local death=0x0081;                        --The variable which tells if the player has died or not.




--Variables specific to Genetic Algorithms
local no_controls=15;                       --The total number of moves the paddle can make(Length of the chromosome)
local population_size=200;                  --Size of the population.
local cr_rate=0.2;                          --The amount of top performers selected from the population(Multiply by 100 to get percentage)
local mut_rate = 1;                         --The Mutation rate. Keep it as low as possible.
local frame_gap=20;                         --The number of frames each input is run for.
local max_score=50000;                      --The Maximum possible score for a level(Arbitrary)
local steps=0;                              --The number of frames that the game has played for (Can be used for fitness).
local max_steps=6000;                       --The maximum number of frames the game can play for.

local control_gap=5;                        --NEW! The amount of genes to add to the chromosome every generation.
local tbl = { {up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = true
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 1,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 1,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 1,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 1,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 1,
			        	B       = 0,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 1,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 1,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 1,
			        	A       = 0,
			        	B       = 0,
			        	start   = true,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 1,
			        	start   = true,
			        	select  = false
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = true,
			        	select  = true
			        	},
			        	{up      = 0,
	        			down    = 0,
	        			left    = 0,
	        			right   = 0,
			        	A       = 0,
			        	B       = 1,
			        	start   = true,
			        	select  = true
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	},
			        	{up      = 1,
	        			down    = 0,
	        			left    = 1,
	        			right   = 0,
			        	A       = 1,
			        	B       = 0,
			        	start   = true,
			        	select  = false
			        	},
			        	}


--Create a random chromosome of size=sz. Eg: 11000110111110
function create_member(sz)
	r='';
	for i=1,sz do
		k=math.random(0,1);
		r=r..k;
	end
	return r;
end


--The Fitness formula, you can use Number of Blocks, Score, Number of Frames elapsed to create a formula to get fitness.(Number of Blocks for now).
function fitness(n_b,m_b,stps,m_s,sc,m_s)
	return (100-(n_b/m_b)*100);
end


--Generate the initial population of size=size and chromosome size=n_controls.
function gen_population(size,n_controls)
	local ret={};
	for i=1,size do
		cand={}		
		for j=1,n_controls do
			if cand[1]==nil then
				cand[1]='';
			end
			k=math.random(0,1)
			cand[1]=cand[1]..k
		end
		cand[2]=0
		ret[i]=cand
	end
	return ret
end



--This is where crossover happens
function crossover(population,rate)
	--Select Top percentile players from population based on the Rate.
	local topp=math.floor(rate*(#population));

	--The commented section adds a feature to control 'control_gap' variable if the generations increase over a limit.
	--[[
	if gen_count%30==0 then
		control_gap=control_gap-1;
	end
	--]]
	
	--Store the top performers in a new table.
	top={}
	for i=1,topp do
		table.insert(top,population[i])
	end


	--Add new controls to the top performers(Increase chromosome size here)
	for i=1,topp do
		population[i][1]=top[i][1]..create_member(control_gap);
		population[i][2]=0;
	end

	--The rest of the new population is then obtained by crossing over one random top performer with the starting bits of another top performer.
	for i=topp+1,#population-10 do
		local p1=math.random(1,topp);
		local p2=math.random(1,topp);
		if math.random(0,10)>5 then
			population[i][1]=top[p1][1]..string.sub(top[p2][1],1,control_gap);
		else
			population[i][1]=top[p2][1]..string.sub(top[p1][1],1,control_gap);
		end
		population[i][2]=0;
	end
	--Increase number of controls( Chromosome Size)
	no_controls=no_controls+control_gap;

	--Make last ten members of population Random, This helps to find a variety of beginning positions in the starting generations and prevents convergence to local optima.
	for i=#population-9,#population do
		population[i][1]=create_member(no_controls);
		population[i][2]=0;		
	end
end


--The Mutation function, has a chance of mutating each bit based on mutation rate.
function mutation(population,mut_rate)
	local a=0;
	local b=1;
    for i=1, #population do
        for j=1, #(population[i][1]) do
            if math.random(1, 120) <= mut_rate then
            	if string.sub(population[i][1],j,j)=='1' then
                population[i][1] = string.sub(population[i][1],1,j-1)..a..string.sub(population[i][1],j+1);
            else
            	population[i][1] = string.sub(population[i][1],1,j-1)..b..string.sub(population[i][1],j+1);
            end
            end
        end
    end
end

--Read initial values from memory when the script is run after game starts.
local ball_pos_y=memory.readbyte(ball_pos_y_addr);                       
local no_blocks=memory.readbyte(no_of_blocks_addr);
local pad_pos=memory.readbyte(pad_addr);
local ball_pos_x=memory.readbyte(ball_pos_x_addr);
local max_blocks=memory.readbyte(no_of_blocks_addr);
local is_dead=memory.readbyte(death);

--Temporary variables for debugging and use in the game
local score=0;
local diff;
local gen_count=0;
local winner=0;
local winner_inp='';
local avg;
local best_f=0;
local cand_num;
local count=0;
local ti;

--Seeding improves randomness
math.randomseed(os.time());
--Create a save state when script is started, this is always loaded again when a new player plays the game.
ss=savestate.create();
savestate.save(ss);
--Generate the initial population.
pop=gen_population(population_size,no_controls)

--The Loop where learning takes place(or finding optimal solution!)
while true do

	--Break if winner is found!
	if winner==1 then
		break;
	end
	--Count the generations
	gen_count=gen_count+1;
	--Initialize average fitness each generation.
	avg=0;

	--A loop for each member of the population.
	for i=1,population_size do

		--Break if a winner is found
		if winner==1 then
			break;
		end
		
		--Load Save state for a new player to play.
		savestate.load(ss)

		--Take the input candidate from the population table. Table format { ('input',fitness),('input2',fitness2).....}
		local cand=pop[i][1]
		
		--A variable to count how many frames have passed and reset it when a certain amount has passed (see below)
		count=0;
		--Initialize score and number of steps
		score=0;
		steps=0;
		--Temporary variables
		local j=1;
		ti=1;

		--Loop to run on the input string. Iterates through each bit.
		while ti<=no_controls do
			--Increase steps
			steps=steps+1;
			--Read memory and update variables
			ball_pos_y=memory.readbyte(ball_pos_y_addr);
			no_blocks=memory.readbyte(no_of_blocks_addr);
			pad_pos=memory.readbyte(pad_addr);
			ball_pos_x=memory.readbyte(ball_pos_x_addr);
			is_dead=memory.readbyte(death);

			--Used for debugging only!
			diff=pad_pos-ball_pos_x;
			
			--Checks if it is game over or player is dead, then writes its fitness and calculates average, and remembers if it the best fitness.
			if is_dead==0 then
				pop[i][2]=fitness(no_blocks,max_blocks,steps,max_steps,score,max_score);
				avg=avg+pop[i][2];
				if pop[i][2]>best_f then
					best_f=pop[i][2];
				end
				ti=1;
				break;
			end

			--Checks to see if pad is out of bounds left or right, this is due to a powerup which opens a portal skipping the level.
			if pad_pos>=180 or pad_pos<=10 then
				pop[i][2]=fitness(no_blocks,max_blocks,steps,max_steps,score,max_score);
				avg=avg+pop[i][2];
				if pop[i][2]>best_f then
					best_f=pop[i][2];
				end
				winner_inp=cand;
				winner=1;
				ti=1;
				break;
			end
			

			--Winning condition, if there are no blocks, the cadidate is the winner.
			if no_blocks<=0 then
				winner_inp=cand;
				winner=1;
				ti=1;
				break;
			end

			--If Ball goes below the paddle, then the player has lost.
			if ball_pos_y>=230 then
				pop[i][2]=fitness(no_blocks,max_blocks,steps,max_steps,score,max_score);
				avg=avg+pop[i][2];
				if pop[i][2]>best_f then
					best_f=pop[i][2];
				end
				ti=1;
				break;
			end

			--This is used to make sure a button is held down(Left or Right) for 'frame_gap' amount of frames for smooth movement.(Important)
			if count<frame_gap then
				for k=1,frame_gap-count do
					--Print information onto the game surface.
					gui.text(0, 9, "Generation:"..gen_count);
					gui.text(0,39,"Candidate:"..i)
					gui.text(0,19,"BestFit:"..best_f);
					gui.text(0,29,"Blocks:"..no_blocks);
					gui.text(0,49,"Control:"..ti);
					--Table of what buttons to hold down/press.
					
			        	
					tbl={
	        			up      = 0,
	        			down    = 0,
	        			left    = lrv,
	        			right   = not lrv,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	};
			        --set controls on the joypad.
	        		joypad.set(1,tbl);
	        		--calculate score
	        		score=memory.readbyte(score_hundred)*100+memory.readbyte(score_tens)*10+memory.readbyte(score_unit);
	        		--Advance one frame
					emu.frameadvance();
					--increment count
					count=count+1
				end
			--In order to issue a new control.
			else
				count=0
				
				if string.sub(cand,ti,ti)=='1' then
					lrv = true;
				else
					lrv = false;
				end
				--Table of controls.	
		    	tbl={
		        	up      = 0,
		        	down    = 0,
		        	left    = lrv,
		        	right   = not lrv,
		        	A       = 0,
		        	B       = 0,
		        	start   = false,
		        	select  = false
		        	};
		        -- set buttons on joypad
		        joypad.set(1,tbl);
		        --Print information on game Surface
				gui.text(0, 9, "Generation:"..gen_count);
				gui.text(0,39,"Candidate:"..i)
				gui.text(0,19,"BestFit:"..best_f);
				gui.text(0,29,"Blocks:"..no_blocks);
				gui.text(0,49,"Control:"..ti);
				score=memory.readbyte(score_hundred)*100+memory.readbyte(score_tens)*10+memory.readbyte(score_unit);
				emu.frameadvance();
				-- Look at next control bit
				ti=ti+1;
			end
		end
		--In the beggining if the game ends prematurely due to lack of control bits, then fitness is calculated again.
		if ti>=no_controls then

			ball_pos_y=memory.readbyte(ball_pos_y_addr);
			no_blocks=memory.readbyte(no_of_blocks_addr);
			pad_pos=memory.readbyte(pad_addr);
			ball_pos_x=memory.readbyte(ball_pos_x_addr);
			is_dead=memory.readbyte(death);
			
			pop[i][2]=fitness(no_blocks,max_blocks,steps,max_steps,score,max_score);
				avg=avg+pop[i][2];
				if pop[i][2]>best_f then
					best_f=pop[i][2];
				end
		end
	end
	--Sort the population with best fitness being the first
	table.sort(pop,
        function(a, b)
            if a[2] > b[2] then
                return true;
            else
                return false;
            end
        end);
	--Crossover
	crossover(pop,cr_rate);
	--Average Population calculation
	avg=avg/population_size;
	--Mutation
	mutation(pop,mut_rate);
end

--Loop runs when winner is found, the winner repeatedly plays the level.
while true do
		--Exactly as the above loop but the member playing is the winner only.
		savestate.load(ss)
		local cand=winner_inp;
		count=0;
		score=0;
		steps=0;
		local j=1;
		ti=1;
		while ti<=no_controls do

			steps=steps+1;
			--READING MEMORY
			ball_pos_y=memory.readbyte(ball_pos_y_addr);
			no_blocks=memory.readbyte(no_of_blocks_addr);
			pad_pos=memory.readbyte(pad_addr);
			ball_pos_x=memory.readbyte(ball_pos_x_addr);
			is_dead=memory.readbyte(death);
			diff=pad_pos-ball_pos_x;
			
			
			if is_dead==0 then
				break;
			end
			if pad_pos>=180 or pad_pos<=10 then
				break;
			end
			
			if no_blocks<=0 then
				break;
			end
			if ball_pos_y>=230 then
				break;
			end
			if count<frame_gap then
				for k=1,frame_gap-count do
					tbl={
			        	up      = 0,
			        	down    = 0,
			        	left    = lrv,
			        	right   = not lrv,
			        	A       = 1,
			        	B       = 1,
			        	start   = false,
			        	select  = false
			        	};
	        		joypad.set(1,tbl);
	        		score=memory.readbyte(score_hundred)*100+memory.readbyte(score_tens)*10+memory.readbyte(score_unit);
					emu.frameadvance();
					count=count+1
				end
			else
				count=0;
				if string.sub(cand,ti,ti)=='1' then
					lrv = true;
				else
					lrv = false;
				end
		    	tbl={
		        	up      = 0,
		        	down    = 0,
		        	left    = lrv,
		        	right   = not lrv,
		        	A       = 0,
		        	B       = 0,
		        	start   = false,
		        	select  = false
		        	};
		        joypad.set(1,tbl);
				score=memory.readbyte(score_hundred)*100+memory.readbyte(score_tens)*10+memory.readbyte(score_unit);
				emu.frameadvance();
				ti=ti+1;
			end
		end
end
