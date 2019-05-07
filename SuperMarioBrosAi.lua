--[[---------------
LuaBit v0.4
-------------------
a bitwise operation lib for lua.

http://luaforge.net/projects/bit/

How to use:
-------------------
 bit.bnot(n) -- bitwise not (~n)
 bit.band(m, n) -- bitwise and (m & n)
 bit.bor(m, n) -- bitwise or (m | n)
 bit.bxor(m, n) -- bitwise xor (m ^ n)
 bit.brshift(n, bits) -- right shift (n >> bits)
 bit.blshift(n, bits) -- left shift (n << bits)
 bit.blogic_rshift(n, bits) -- logic right shift(zero fill >>>)

Please note that bit.brshift and bit.blshift only support number within
32 bits.

2 utility functions are provided too:
 bit.tobits(n) -- convert n into a bit table(which is a 1/0 sequence)
               -- high bits first
 bit.tonumb(bit_tbl) -- convert a bit table into a number 
-------------------

Under the MIT license.

copyright(c) 2006~2007 hanzhao (abrash_han@hotmail.com)
--]]---------------

--do

------------------------
-- bit lib implementions

local function check_int(n)
 -- checking not float
 if(n - math.floor(n) > 0) then
  error("trying to use bitwise operation on non-integer!")
 end
end

local function tbl_to_number(tbl)
 local n = #tbl

 local rslt = 0
 local power = 1
 for i = 1, n do
  rslt = rslt + tbl[i]*power
  power = power*2
 end

 return rslt
end

local function expand(tbl_m, tbl_n)
 local big = {}
 local small = {}
 if(#tbl_m > #tbl_n) then
  big = tbl_m
  small = tbl_n
 else
  big = tbl_n
  small = tbl_m
 end
 -- expand small
 for i = #small + 1, #big do
  small[i] = 0
 end

end

local to_bits = function () end

local function bit_not(n)
 local tbl = to_bits(n)
 local size = math.max(#tbl, 32)
 for i = 1, size do
  if(tbl[i] == 1) then
   tbl[i] = 0
  else
   tbl[i] = 1
  end
 end
 return tbl_to_number(tbl)
end


to_bits = function (n)
 check_int(n)
 if(n < 0) then
  -- negative
  return to_bits(bit_not(math.abs(n)) + 1)
 end
 -- to bits table
 local tbl = {}
 local cnt = 1
 while (n > 0) do
  local last = math.mod(n,2)
  if(last == 1) then
   tbl[cnt] = 1
  else
   tbl[cnt] = 0
  end
  n = (n-last)/2
  cnt = cnt + 1
 end

 return tbl
end


local function bit_or(m, n)
 local tbl_m = to_bits(m)
 local tbl_n = to_bits(n)
 expand(tbl_m, tbl_n)

 local tbl = {}
 local rslt = math.max(#tbl_m, #tbl_n)
 for i = 1, rslt do
  if(tbl_m[i]== 0 and tbl_n[i] == 0) then
   tbl[i] = 0
  else
   tbl[i] = 1
  end
 end

 return tbl_to_number(tbl)
end

local function bit_and(m, n)
 local tbl_m = to_bits(m)
 local tbl_n = to_bits(n)
 expand(tbl_m, tbl_n) 

 local tbl = {}
 local rslt = math.max(#tbl_m, #tbl_n)
 for i = 1, rslt do
  if(tbl_m[i]== 0 or tbl_n[i] == 0) then
   tbl[i] = 0
  else
   tbl[i] = 1
  end
 end

 return tbl_to_number(tbl)
end

local function bit_xor(m, n)
 local tbl_m = to_bits(m)
 local tbl_n = to_bits(n)
 expand(tbl_m, tbl_n) 

 local tbl = {}
 local rslt = math.max(#tbl_m, #tbl_n)
 for i = 1, rslt do
  if(tbl_m[i] ~= tbl_n[i]) then
   tbl[i] = 1
  else
   tbl[i] = 0
  end
 end

 --table.foreach(tbl, print)

 return tbl_to_number(tbl)
end

local function bit_rshift(n, bits)
 check_int(n)

 local high_bit = 0
 if(n < 0) then
  -- negative
  n = bit_not(math.abs(n)) + 1
  high_bit = 2147483648 -- 0x80000000
 end

 for i=1, bits do
  n = n/2
  n = bit_or(math.floor(n), high_bit)
 end
 return math.floor(n)
end

-- logic rightshift assures zero filling shift
local function bit_logic_rshift(n, bits)
 check_int(n)
 if(n < 0) then
  -- negative
  n = bit_not(math.abs(n)) + 1
 end
 for i=1, bits do
  n = n/2
 end
 return math.floor(n)
end

local function bit_lshift(n, bits)
 check_int(n)

 if(n < 0) then
  -- negative
  n = bit_not(math.abs(n)) + 1
 end

 for i=1, bits do
  n = n*2
 end
 return bit_and(n, 4294967295) -- 0xFFFFFFFF
end

local function bit_xor2(m, n)
 local rhs = bit_or(bit_not(m), bit_not(n))
 local lhs = bit_or(m, n)
 local rslt = bit_and(lhs, rhs)
 return rslt
end

-- An MD5 mplementation in Lua, requires bitlib (hacked to use LuaBit from above, ugh)
-- 10/02/2001 jcw@equi4.com

local md5={ff=tonumber('ffffffff',16),consts={}}

string.gsub([[ d76aa478 e8c7b756 242070db c1bdceee
    f57c0faf 4787c62a a8304613 fd469501
    698098d8 8b44f7af ffff5bb1 895cd7be
    6b901122 fd987193 a679438e 49b40821
    f61e2562 c040b340 265e5a51 e9b6c7aa
    d62f105d 02441453 d8a1e681 e7d3fbc8
    21e1cde6 c33707d6 f4d50d87 455a14ed
    a9e3e905 fcefa3f8 676f02d9 8d2a4c8a
    fffa3942 8771f681 6d9d6122 fde5380c
    a4beea44 4bdecfa9 f6bb4b60 bebfbc70
    289b7ec6 eaa127fa d4ef3085 04881d05
    d9d4d039 e6db99e5 1fa27cf8 c4ac5665
    f4292244 432aff97 ab9423a7 fc93a039
    655b59c3 8f0ccc92 ffeff47d 85845dd1
    6fa87e4f fe2ce6e0 a3014314 4e0811a1
    f7537e82 bd3af235 2ad7d2bb eb86d391
    67452301 efcdab89 98badcfe 10325476 ]],"(%w+)", function (s) table.insert(md5.consts, tonumber(s,16)) end)
    --67452301 efcdab89 98badcfe 10325476 ]],"(%w+)", function (s) tinsert(md5.consts,tonumber(s,16)) end)

function md5.transform(A,B,C,D,X)
  local f=function (x,y,z) return bit_or(bit_and(x,y),bit_and(-x-1,z)) end
  local g=function (x,y,z) return bit_or(bit_and(x,z),bit_and(y,-z-1)) end
  local h=function (x,y,z) return bit_xor(x,bit_xor(y,z)) end
  local i=function (x,y,z) return bit_xor(y,bit_or(x,-z-1)) end
  local z=function (f,a,b,c,d,x,s,ac)
        a=bit_and(a+f(b,c,d)+x+ac,md5.ff)
        -- be *very* careful that left shift does not cause rounding!
        return bit_or(bit_lshift(bit_and(a,bit_rshift(md5.ff,s)),s),bit_rshift(a,32-s))+b
      end
  local a,b,c,d=A,B,C,D
  local t=md5.consts

  a=z(f,a,b,c,d,X[ 0], 7,t[ 1])
  d=z(f,d,a,b,c,X[ 1],12,t[ 2])
  c=z(f,c,d,a,b,X[ 2],17,t[ 3])
  b=z(f,b,c,d,a,X[ 3],22,t[ 4])
  a=z(f,a,b,c,d,X[ 4], 7,t[ 5])
  d=z(f,d,a,b,c,X[ 5],12,t[ 6])
  c=z(f,c,d,a,b,X[ 6],17,t[ 7])
  b=z(f,b,c,d,a,X[ 7],22,t[ 8])
  a=z(f,a,b,c,d,X[ 8], 7,t[ 9])
  d=z(f,d,a,b,c,X[ 9],12,t[10])
  c=z(f,c,d,a,b,X[10],17,t[11])
  b=z(f,b,c,d,a,X[11],22,t[12])
  a=z(f,a,b,c,d,X[12], 7,t[13])
  d=z(f,d,a,b,c,X[13],12,t[14])
  c=z(f,c,d,a,b,X[14],17,t[15])
  b=z(f,b,c,d,a,X[15],22,t[16])

  a=z(g,a,b,c,d,X[ 1], 5,t[17])
  d=z(g,d,a,b,c,X[ 6], 9,t[18])
  c=z(g,c,d,a,b,X[11],14,t[19])
  b=z(g,b,c,d,a,X[ 0],20,t[20])
  a=z(g,a,b,c,d,X[ 5], 5,t[21])
  d=z(g,d,a,b,c,X[10], 9,t[22])
  c=z(g,c,d,a,b,X[15],14,t[23])
  b=z(g,b,c,d,a,X[ 4],20,t[24])
  a=z(g,a,b,c,d,X[ 9], 5,t[25])
  d=z(g,d,a,b,c,X[14], 9,t[26])
  c=z(g,c,d,a,b,X[ 3],14,t[27])
  b=z(g,b,c,d,a,X[ 8],20,t[28])
  a=z(g,a,b,c,d,X[13], 5,t[29])
  d=z(g,d,a,b,c,X[ 2], 9,t[30])
  c=z(g,c,d,a,b,X[ 7],14,t[31])
  b=z(g,b,c,d,a,X[12],20,t[32])

  a=z(h,a,b,c,d,X[ 5], 4,t[33])
  d=z(h,d,a,b,c,X[ 8],11,t[34])
  c=z(h,c,d,a,b,X[11],16,t[35])
  b=z(h,b,c,d,a,X[14],23,t[36])
  a=z(h,a,b,c,d,X[ 1], 4,t[37])
  d=z(h,d,a,b,c,X[ 4],11,t[38])
  c=z(h,c,d,a,b,X[ 7],16,t[39])
  b=z(h,b,c,d,a,X[10],23,t[40])
  a=z(h,a,b,c,d,X[13], 4,t[41])
  d=z(h,d,a,b,c,X[ 0],11,t[42])
  c=z(h,c,d,a,b,X[ 3],16,t[43])
  b=z(h,b,c,d,a,X[ 6],23,t[44])
  a=z(h,a,b,c,d,X[ 9], 4,t[45])
  d=z(h,d,a,b,c,X[12],11,t[46])
  c=z(h,c,d,a,b,X[15],16,t[47])
  b=z(h,b,c,d,a,X[ 2],23,t[48])

  a=z(i,a,b,c,d,X[ 0], 6,t[49])
  d=z(i,d,a,b,c,X[ 7],10,t[50])
  c=z(i,c,d,a,b,X[14],15,t[51])
  b=z(i,b,c,d,a,X[ 5],21,t[52])
  a=z(i,a,b,c,d,X[12], 6,t[53])
  d=z(i,d,a,b,c,X[ 3],10,t[54])
  c=z(i,c,d,a,b,X[10],15,t[55])
  b=z(i,b,c,d,a,X[ 1],21,t[56])
  a=z(i,a,b,c,d,X[ 8], 6,t[57])
  d=z(i,d,a,b,c,X[15],10,t[58])
  c=z(i,c,d,a,b,X[ 6],15,t[59])
  b=z(i,b,c,d,a,X[13],21,t[60])
  a=z(i,a,b,c,d,X[ 4], 6,t[61])
  d=z(i,d,a,b,c,X[11],10,t[62])
  c=z(i,c,d,a,b,X[ 2],15,t[63])
  b=z(i,b,c,d,a,X[ 9],21,t[64])

  return A+a,B+b,C+c,D+d
end

-- convert little-endian 32-bit int to a 4-char string
local function leIstr(i)
  local f=function (s) return string.char(bit_and(bit_rshift(i,s),255)) end
  return f(0)..f(8)..f(16)..f(24)
end

  -- convert raw string to big-endian int
  local function beInt(s)
    local v=0
    for i=1,string.len(s) do v=v*256+string.byte(s,i) end
    return v
  end
  -- convert raw string to little-endian int
  local function leInt(s)
    local v=0
    for i=string.len(s),1,-1 do v=v*256+string.byte(s,i) end
    return v
  end
  -- cut up a string in little-endian ints of given size
  local function leStrCuts(s,...)
    local o,r=1,{}
    for i=1,#arg do
      table.insert(r,leInt(string.sub(s,o,o+arg[i]-1)))
      o=o+arg[i]
    end
    return r
  end

function md5.Calc(s)
  local msgLen=string.len(s)
  local padLen=56- msgLen % 64
  if msgLen % 64 > 56 then padLen=padLen+64 end
  if padLen==0 then padLen=64 end
  s=s..string.char(128)..string.rep(string.char(0),padLen-1)
  s=s..leIstr(8*msgLen)..leIstr(0)
  assert(string.len(s) % 64 ==0)
  local t=md5.consts
  local a,b,c,d=t[65],t[66],t[67],t[68]
  for i=1,string.len(s),64 do
    local X=leStrCuts(string.sub(s,i,i+63),4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)
    assert(#X==16)
    X[0]=table.remove(X,1) -- zero based!
    a,b,c,d=md5.transform(a,b,c,d,X)
  end
  local swap=function (w) return beInt(leIstr(w)) end
  return string.format("%08x%08x%08x%08x",swap(a),swap(b),swap(c),swap(d))
end



--MEMORY ADDRESSES USED IN THE RAM ON FCEUX.
local world_ad = 0x075f;
local level_ad = 0x0760;
local player_horiz_pos_ad = 0x006d;
local player_x_pos_ad = 0x071d;
local lives_ad = 0x075a;
local player_float_status_ad = 0x001d;
local player_state_ad = 0x000e;


--Variables specific to Genetic Algorithms
local chrom_size = 64;                      --Length of the chromosome
local population_size=200;                  --Size of the population.
local cr_rate=0.2;                          --The amount of top performers selected from the population(Multiply by 100 to get percentage)
local mut_rate = 2;                         --The Mutation rate. Keep it as low as possible.
local frame_gap=20;                         --The number of frames each input is run for.
local max_score=50000;                      --The Maximum possible score for a level(Arbitrary)
local steps=0;                              --The number of frames that the game has played for (Can be used for fitness).
local max_steps=6000;                       --The maximum number of frames the game can play for.

local control_gap=5;
local inputs = {};

for i=1,2 do								--Create power set of possible inputs
	if i==1 then
		up_val = false;
	else up_val = true;
	end
	for j=1,2 do
		if j==1 then
			down_val = false;
		else down_val = true;
		end
		for k=1,2 do
			if k==1 then
				left_val = false;
			else left_val = true;
			end
			for l=1,2 do
				if l==1 then
					right_val = false;
				else right_val = true;
				end
				for m=1,2 do
					if m==1 then
						A_val = false;
					else A_val = true;
					end
					for n=1,2 do
						if n==1 then
							B_val = false;
						else B_val = true;
						end
								table.insert(inputs, {up = up_val,
													down = down_val,
													left = left_val,
													right = right_val,
													A = A_val,
													B = B_val,
													start = 0,
													select = 0
													})
								
							end
						end
					end
				end
			end
		end



--Create a random chromosome of size=sz. Eg: 11000110111110
function create_member(sz)
	r='';
	for i=1,sz do
		k=math.random(0,63);
		if k<10 then r = r..0
		r=r..k;
	end
	return r;
end


--The Fitness formula, 
function fitness(world, level, horiz_pos, x_pos)
	return (world)*27000000 + (level)*90000 + 300*horiz_pos + x_pos;
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
			k=math.random(0,63)
			if k<10 then cand[1]=cand[1]..0
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
	
	--[[if gen_count%30==0 then
		control_gap=control_gap-1;
	end]]
	
	--Store the top performers in a new table.
	top={}
	for i=1,topp do
		table.insert(top,population[i])
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
	--Increase chromosome size
	chrom_size=chrom_size+control_gap;
	

	--Make last ten members of population Random, This helps to find a variety of beginning positions in the starting generations and prevents convergence to local optima.
	for i=#population-9,#population do
		population[i][1]=create_member(chrom_size);
		population[i][2]=0;		
	end
end


--The Mutation function, has a chance of mutating each bit based on mutation rate.
function mutation(population,mut_rate)
	local a=0;
	local b=1;
    for i=1, #population do
        for j=1, #(population[i][1]) do
			
			local s = tonumber(population[i][1], 2);
			for k=1, #s do
				if math.random(1, 100) <= mut_rate then
					if string.sub(s,k,k)=='1' then
						s = string.sub(s,1,k-1)..a..string.sub(s,k+1);
					else
						s = string.sub(s,1,k-1)..b..string.sub(s,k+1);
					end
				end
			end
			s = tonumber(s, 64);
			population[i][1] = string.sub(population[i][1],1,j-1)..s..string.sub(population[i][1],j+1);
        end
    end
end

--Read initial values from memory when the script is run after game starts.
world = memory.readbyte(world_ad);
level = memory.readbyte(level_ad);
player_horiz_pos = memory.readbyte(player_horiz_pos_ad);
player_x_pos = memory.readbyte(player_x_pos_ad);
lives = memory.readbyte(lives_ad);
player_float_status = memory.readbyte(player_float_status_ad);
player_state = memory.readbyte(player_state_ad);

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

--emu.speedmode("maximum");
game_start = {up = false,
down = false,
left = false,
right = false,
A = false,
B = false,
start = true,
select = false
}
joypad.set(1, game_start);
emu.frameadvance();

--Create a save state when script is started, this is always loaded again when a new player plays the game.
ss=savestate.create();
savestate.save(ss);
--Generate the initial population.
pop=gen_population(population_size,chrom_size)


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
	print("Generation: "..gen_count);
	--A loop for each member of the population.
	for i=1,population_size do
		local screen = 0;
		print("Candidate: "..i);
		
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
		
		--TODO: Change loop. We are not iterating through an input string.
		--Loop to run on the input string. Iterates through each bit.
		while ti<=chrom_size do
			--Increase steps
			steps=steps+1;
			--Read memory and update variables
			world = memory.readbyte(world_ad);
			level = memory.readbyte(level_ad);
			player_horiz_pos = memory.readbyte(player_horiz_pos_ad);
			player_x_pos = memory.readbyte(player_x_pos_ad);
			lives = memory.readbyte(lives_ad);
			player_float_status = memory.readbyte(player_float_status_ad);
			player_state = memory.readbyte(player_state_ad);

			--Used for debugging only!
			
			--Checks if it is game over or player is dead, then writes its fitness and calculates average, and remembers if it the best fitness.
			if lives==0x01 then
				pop[i][2]=fitness(world, level, screen, player_x_pos);
				avg=avg+pop[i][2];
				if pop[i][2]>best_f then
					best_f=pop[i][2];
				end
				ti=1;
				break;
			end
			
			--Check if player has reached new screen
			if player_x_pos==0x00 and player_horiz_pos==screen then
				screen = screen+1;
			end
			
			
			--Winning condition, if the player is sliding down the flagpole, it is the winner
			  if player_float_status==0x03 then
				winner_inp=cand;
				winner=1;
				ti=1;
				break;
			end

			local state = game_state();
			local seed = cand..state;
			

			--This is used to make sure a button is held down(Left or Right) for 'frame_gap' amount of frames for smooth movement.(Important)
			if count<frame_gap then
				for k=1,frame_gap-count do
					--Print information onto the game surface.
					gui.text(0, 9, "Generation:"..gen_count);
					gui.text(0,19,"BestFit:"..best_f);
					gui.text(0,29,"Candidate:"..i)
					
					gui.text(0,49,"Current Fitness:"..fitness(world, level, screen, player_x_pos));
					gui.text(0,59,"Candidate's Best Fitness: "..best_cand_fitness);
					--gui.text(0,29,"Blocks:"..no_blocks);
					--gui.text(0,49,"Control:"..ti);
					math.random() math.random() math.random()
					tbl_index = math.random(1, 64);
					tbl = inputs[tbl_index];
					--print("Input: "..tbl_index);
			        --Table of what buttons to hold down/press.
					gui.text(0, 79, "Up: "..tostring(tbl["up"]));	
					gui.text(0, 89, "Down: "..tostring(tbl["down"]));
					gui.text(0, 99, "Left: "..tostring(tbl["left"]));
					gui.text(0, 109, "Right: "..tostring(tbl["right"]));
					gui.text(0, 119, "A: "..tostring(tbl["A"]));
					gui.text(0, 129, "B: "..tostring(tbl["B"]));
					--gui.text(0, 149, "Chromosome: "..tostring(cand));
					--gui.text(0, 139, "tbl_index"..tbl_index);
					--gui.text(0, 149, "seed"..seed);
					--gui.text(0, 159, "steps"..steps);
			        --set controls on the joypad.
	        		joypad.set(1,tbl);
	        		--Advance one frame
					emu.frameadvance();
					--increment count
					count=count+1
				end
			--In order to issue a new control.
			else
				count=0
				--Table of controls.
				math.random() math.random() math.random()
				tbl_index = math.random(1, 64);
		    	tbl = inputs[tbl_index];
		        -- set buttons on joypad
		        joypad.set(1,tbl);
		        --Print information on game Surface
				gui.text(0, 9, "Generation:"..gen_count);
				gui.text(0,19,"BestFit:"..best_f);
				gui.text(0,29,"Candidate:"..i)
					
				gui.text(0,49,"Current Fitness:"..fitness(world, level, screen, player_x_pos));
				gui.text(0,59,"Candidate's Best Fitness: "..best_cand_fitness);
					--gui.text(0,49,"Control:"..ti);
				--print("Input: "..tbl_index);
				 --Table of what buttons to hold down/press.
					gui.text(0, 79, "Up: "..tostring(tbl["up"]));	
					gui.text(0, 89, "Down: "..tostring(tbl["down"]));
					gui.text(0, 99, "Left: "..tostring(tbl["left"]));
					gui.text(0, 109, "Right: "..tostring(tbl["right"]));
					gui.text(0, 119, "A: "..tostring(tbl["A"]));
					gui.text(0, 129, "B: "..tostring(tbl["B"]));
					--gui.text(0, 149, "Chromosome: "..tostring(cand));
					--gui.text(0, 139, "tbl_index"..tbl_index);
					--gui.text(0, 149, "seed"..seed);
					--gui.text(0, 159, "steps"..steps);
				emu.frameadvance();
				-- Look at next control bit
				ti=ti+1;
			end
		end
		print("Candidate "..i..": "..cand.." Fitness: "..pop[i][2]);
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
		while lives ~= 0X01 do

			steps=steps+1;
			--READING MEMORY
			world = memory.readbyte(world_ad);
			level = memory.readbyte(level_ad);
			player_horiz_pos = memory.readbyte(player_horiz_pos_ad);
			player_x_pos = memory.readbyte(player_x_pos_ad);
			player_y_pos = memory.readbyte(player_y_ad);
			powerup_state = memory.readbyte(powerup_state_ad);
			walk_animation = memory.readbyte(walk_animation_ad);
			speed = memory.readbyte(speed_ad);
			swimming = memory.readbyte(swimming_ad);
			pause_status = memory.readbyte(pause_status_ad);
			gravity = memory.readbyte(gravity_ad);
			coins = memory.readbyte(coins_ad);
			coins2 = memory.readbyte(coins2_ad);
			fireball_counter = memory.readbyte(fireball_counter_ad);
			collion = memory.readbyte(collision_ad);
			collion2 = memory.readbyte(collision2_ad);
			vert_velocity = memory.readbyte(vert_velocity_ad);
			player_x_force = memory.readbyte(player_x_force_ad);
			lives = memory.readbyte(lives_ad);
			player_float_status = memory.readbyte(player_float_status_ad);

			enemy_drawn1 = memory.readbyte(enemy_drawn1_ad);
			enemy_drawn2 = memory.readbyte(enemy_drawn2_ad);
			enemy_drawn3 = memory.readbyte(enemy_drawn3_ad);
			enemy_drawn4 = memory.readbyte(enemy_drawn4_ad);
			enemy_drawn5 = memory.readbyte(enemy_drawn5_ad);
			enemy_type1 = memory.readbyte(enemy_type1_ad);
			enemy_type2 = memory.readbyte(enemy_type2_ad);
			enemy_type3 = memory.readbyte(enemy_type3_ad);
			enemy_type4 = memory.readbyte(enemy_type4_ad);
			enemy_type5 = memory.readbyte(enemy_type5_ad);

			fireball_relative_x = memory.readbyte(fireball_relative_x_ad);
			fireball_relative_y = memory.readbyte(fireball_relative_y_ad);

			enemy_x_pos1 = memory.readbyte(enemy_x_pos1_ad);
			enemy_x_pos2 = memory.readbyte(enemy_x_pos2_ad);
			enemy_x_pos3 = memory.readbyte(enemy_x_pos3_ad);
			enemy_x_pos4 = memory.readbyte(enemy_x_pos4_ad);
			enemy_x_pos5 = memory.readbyte(enemy_x_pos5_ad);
			enemy_y_pos = memory.readbyte(enemy_y_pos_ad);
			enemy_y_pos2 = memory.readbyte(enemy_y_pos2_ad);
			enemy_y_pos3 = memory.readbyte(enemy_y_pos3_ad);
			enemy_y_pos4 = memory.readbyte(enemy_y_pos4_ad);
			enemy_y_pos5 = memory.readbyte(enemy_y_pos5_ad);

			powerup_drawn = memory.readbyte(powerup_drawn_ad);
			powerup_x = memory.readbyte(powerup_x_ad);
			powerup_y = memory.readbyte(powerup_y_ad);

			fireball_hitbox1 = memory.readbyte(fireball_hitbox1_ad);
			fireball_hitbox2 = memory.readbyte(fireball_hitbox2_ad);
			fireball_hitbox3 = memory.readbyte(fireball_hitbox3_ad);
			fireball_hitbox4 = memory.readbyte(fireball_hitbox4_ad);
			fireball_hitbox5 = memory.readbyte(fireball_hitbox5_ad);
			fireball_hitbox6 = memory.readbyte(fireball_hitbox6_ad);
			fireball_hitbox7 = memory.readbyte(fireball_hitbox7_ad);
			fireball_hitbox8 = memory.readbyte(fireball_hitbox8_ad);

			hammer_hitbox1 = memory.readbyte(hammer_hitbox1_ad);
			hammer_hitbox2 = memory.readbyte(hammer_hitbox2_ad);
			hammer_hitbox3 = memory.readbyte(hammer_hitbox3_ad);
			hammer_hitbox4 = memory.readbyte(hammer_hitbox4_ad);
			hammer_hitbox5 = memory.readbyte(hammer_hitbox5_ad);
			hammer_hitbox6 = memory.readbyte(hammer_hitbox6_ad);
			hammer_hitbox7 = memory.readbyte(hammer_hitbox7_ad)
			hammer_hitbox8 = memory.readbyte(hammer_hitbox8_ad);
			hammer_hitbox9 = memory.readbyte(hammer_hitbox9_ad);
			hammer_hitbox10 = memory.readbyte(hammer_hitbox10_ad);
			hammer_hitbox11 = memory.readbyte(hammer_hitbox11_ad);
			hammer_hitbox12 = memory.readbyte(hammer_hitbox12_ad);
			hammer_hitbox13 = memory.readbyte(hammer_hitbox13_ad);
			hammer_hitbox14 = memory.readbyte(hammer_hitbox14_ad);
			hammer_hitbox15 = memory.readbyte(hammer_hitbox15_ad);
			hammer_hitbox16 = memory.readbyte(hammer_hitbox16_ad);

			coin_hitbox1 = memory.readbyte(coin_hitbox1_ad);
			coin_hitbox2 = memory.readbyte(coin_hitbox2_ad);
			coin_hitbox3 = memory.readbyte(coin_hitbox3_ad);
			coin_hitbox4 = memory.readbyte(coin_hitbox4_ad);
			coin_hitbox5 = memory.readbyte(coin_hitbox5_ad);
			coin_hitbox6 = memory.readbyte(coin_hitbox6_ad);
			coin_hitbox7 = memory.readbyte(coin_hitbox7_ad);
			coin_hitbox8 = memory.readbyte(coin_hitbox8_ad);
			coin_hitbox9 = memory.readbyte(coin_hitbox9_ad);
			coin_hitbox10 = memory.readbyte(coin_hitbox10_ad);
			coin_hitbox11 = memory.readbyte(coin_hitbox11_ad);
			coin_hitbox12 = memory.readbyte(coin_hitbox12_ad);
			
			
			if lives==0x01 then
				break;
			end
			if player_float_status==0x03 then
				break;
			end
			if count<frame_gap then
				for k=1,frame_gap-count do
					local state = game_state();
					
					math.random() math.random() math.random()
					tbl_index = math.random(1, 64);
					tbl = inputs[tbl_index];
	        		joypad.set(1,tbl);
					emu.frameadvance();
					count=count+1
				end
			else
				count=0;
		    	local state = game_state();
				
				math.random() math.random() math.random()
				tbl_index = math.random(1, 64);
		    	tbl = inputs[tbl_index];
		        joypad.set(1,tbl);emu.frameadvance();
				ti=ti+1;
			end
		end
end
