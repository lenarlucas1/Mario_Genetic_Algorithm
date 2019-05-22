--MEMORY ADDRESSES USED IN THE RAM ON FCEUX.
local world_ad = 0x075f;
local level_ad = 0x0760;
local player_horiz_pos_ad = 0x006d;
local player_x_pos_ad = 0x0086;
local lives_ad = 0x075a;
local player_float_status_ad = 0x001d;
local player_state_ad = 0x000e;
local beat_game_ad = 0x07fc;


--Variables specific to Genetic Algorithms
local chrom_size = 1000;                      --Length of the chromosome
local population_size=200;                  --Size of the population.
local cr_rate=0.2;                          --The amount of top performers selected from the population(Multiply by 100 to get percentage)
local mut_rate = 1;                         --The Mutation rate. Keep it as low as possible.
local frame_gap=20;                         --The number of frames each input is run for.
local max_score=50000;                      --The Maximum possible score for a level(Arbitrary)
local steps=0;                              --The number of frames that the game has played for (Can be used for fitness).
local max_steps=6000;                       --The maximum number of frames the game can play for.

local control_gap=1000;



--Create a random chromosome of size=sz. Eg: 11000110111110
function create_member(sz)
	r='';
	for i=1,sz do
		k=math.random(0,1);
		r=r..k;
	end
	return r;
end


--The Fitness formula, 
function fitness(world, level, horiz_pos, player_x_pos)
	return world*2560 + (level)*256 + horiz_pos + player_x_pos/256;
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
			k=math.random(0,1);
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
	local topp=math.floor(cr_rate*(#population));
	--Skip the top performers, only mutate the remaining individuals
    for i=topp+1, #population do
        for j=1, #(population[i][1]), 8 do
			local gene = string.sub(population[i][1], j, j+7)
			for k=1, #gene do
				if math.random(1, 100) <= mut_rate then
					if string.sub(gene,k,k)=='1' then
						gene = string.sub(gene, 1, k-1)..a..string.sub(gene,k+1);
					else
						gene = string.sub(gene, 1, k-1)..b..string.sub(gene,k+1);
					end
				end
			end
			population[i][1] = string.sub(population[i][1],1,j-1)..gene..string.sub(population[i][1],j+8);
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
beat_game = memory.readbyte(beat_game_ad);
player_state = memory.readbyte(player_state_ad);
fit = fitness(world, level, player_horiz_pos, player_x_pos);
--Temporary variables for debugging and use in the game
local score=0;
local diff;
local gen_count=0;
local winner=0;
local winner_inp='';
local avg;
local best_f=0;
local best_gen = 0;
local best_species = 0;
local cand_num;
local count=0;
local ti;

--Seeding improves randomness
math.randomseed(os.time());

emu.speedmode("maximum");
game_start = {up = false,
down = false,
left = false,
right = false,
A = false,
B = false,
start = true,
select = false
}
frame = 0;
joypad.set(1, game_start);

--Advance past title screen
while player_state ~= 8 or frame < 10 do
	emu.frameadvance();
	player_state = memory.readbyte(player_state_ad);
	frame=frame+1;
end

--emu.frameadvance();
--print("frame is ",frame);
--Create a save state when script is started, this is always loaded again when a new player plays the game.
ss=savestate.create();
savestate.save(ss);
--Generate the initial population.
pop=gen_population(population_size,chrom_size)

--The Loop where learning takes place(or finding optimal solution!)
while true do
	--file = io.open("data.txt", "a");
	--Break if winner is found!
	if winner==1 then
		print("Winner has been found.");
				print("Winner is species "..i..": "..winner_inp);
				--[[for i=1,population_size do
					file:write("Species ",i);
					file:write("Fitness: ",pop[i][2],"\n");
				end]]
		break;
	end
	--Count the generations
	gen_count=gen_count+1;
	--Initialize average fitness each generation.
	avg=0;
	--print("Generation: "..gen_count);
	--A loop for each member of the population.
	for i=1,population_size do
		
		--Break if a winner is found
		if winner==1 then
			print("Winner has been found.");
				print("Winner is species "..i..": "..winner_inp);
				--[[for i=1,population_size do
					file:write("Species ",i);
					file:write("Fitness: ",pop[i][2],"\n");
				end]]
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
		
		screen = 0;
		level_tracker = 0;
		
		--Loop to run on the input string. Iterates through each gene.
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
			beat_game = memory.readbyte(beat_game_ad);
			
			if level_tracker~= level then
				level_tracker = level;
				screen = 0;
			end
			
			
			--Used for debugging only!
			
			--Checks if it is game over or player is dead, then writes its fitness and calculates average, and remembers if it the best fitness.
			if player_state == 0x0b or lives==0x01 then
				--[[if lives==0x01 then
					print("Pit");
					print("Pit fitness is",fit);
				end]]
				--print("Dead");
				pop[i][2]=fit;
				avg=avg+pop[i][2];
				if pop[i][2]>best_f then
					best_f=pop[i][2];
					best_gen = gen_count;
					best_species = i;
				end
				ti=1;
				break;
			end
			
			
			--screen = player_horiz_pos;
			
			
			fit = fitness(world, level, player_horiz_pos, player_x_pos);
			
			
			--Winning condition, player has beat the game
			  if beat_game ~= 0 then
				winner_inp=cand;
				winner=1;
				ti=1;
				print("Winner has been found.");
				print("Winner is species "..i..": "..winner_inp);
				--[[for i=1,population_size do
					file:write("Species ",i);
					file:write("Fitness: ",pop[i][2],"\n");
				end]]
				break;
			end

			

			
				count=0
				--Table of controls.
				gene = string.sub(cand,ti,ti+7);
				u = string.sub(gene,1,1);
				d = string.sub(gene,2,2);
				l = string.sub(gene,3,3);
				r = string.sub(gene,4,4);
				a = string.sub(gene,5,5);
				b = string.sub(gene,6,6);
				str = string.sub(gene,7,7);
				sel = string.sub(gene,8,8);
				
				if u=='1' then
					up_val = true;
				else up_val = false;
				end
				
				if d=='1' then
					down_val = true;
				else down_val = false;
				end
				
				if l=='1' then
					left_val = true;
				else left_val = false;
				end
				
				if r=='1' then
					right_val = true;
				else right_val = false;
				end
				
				if a=='1' then
					A_val = true;
				else A_val = false;
				end
				
				if b=='1' then
					B_val = true;
				else B_val = false;
				end
				
				if str=='1' then
					start_val = true;
				else start_val = false;
				end
				
				if sel=='1' then
					select_val = true;
				else select_val = false;
				end
				
				tbl={
					up = up_val,
					down = down_val,
					left = left_val,
					right = right_val,
					A = A_val,
					B = B_val,
					start = start_val,
					select = select_val
					};
		        -- set buttons on joypad
		        joypad.set(1,tbl);
		        --Print information on game Surface
				gui.text(0, 9, "Generation:"..gen_count);
					gui.text(0,39,"BestFit:"..best_f.." (Generation "..best_gen.." Species "..best_species..")");
					--gui.text(0,29,"Average Fitness:"..avg/(i-1));
					if i>1 then
					gui.text(0,219,"Last fitness:"..pop[i-1][2]);
					end
					gui.text(0,19,"Species:"..i)
						
					gui.text(0,29,"Current Fitness:"..fit);
					
					--gui.text(0,69,"Control:"..ti);
				emu.frameadvance();
				frame=frame+1;
				-- Look at next gene
				ti=ti+8;
		end
		--In the beggining if the game ends prematurely due to lack of control bits, then fitness is calculated again.
		if ti>=chrom_size then
			--print("Ended prematurely");
			--print(ti, chrom_size, frame);
			world = memory.readbyte(world_ad);
			level = memory.readbyte(level_ad);
			player_horiz_pos = memory.readbyte(player_horiz_pos_ad);
			player_x_pos = memory.readbyte(player_x_pos_ad);
			
			pop[i][2] = fitness(world, level, player_horiz_pos, player_x_pos);
			--print("Fitness is "..pop[i][2]);
				avg=avg+pop[i][2];
				if pop[i][2]>best_f then
					best_f=pop[i][2];
					best_gen = gen_count;
					best_species = i;
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
	--[[for i=1,population_size do
		file:write("Species ",i,"\nFitness: ",pop[i][2],"\n\n");
	end]]
	--print("Best fitness: "..pop[1][2]);
	best = pop[1][2];
	--Crossover
	crossover(pop,cr_rate);
	--Average Population calculation
	avg=avg/population_size;
	print(best, "\t", avg);
	--Mutation
	mutation(pop,mut_rate);
	--io.close(file);
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
		while ti<=chrom_size do

			steps=steps+1;
			--READING MEMORY
			world = memory.readbyte(world_ad);
			level = memory.readbyte(level_ad);
			player_horiz_pos = memory.readbyte(player_horiz_pos_ad);
			player_x_pos = memory.readbyte(player_x_pos_ad);
			lives = memory.readbyte(lives_ad);
			player_float_status = memory.readbyte(player_float_status_ad);
			
			
			if lives==0x01 then
				break;
			end
			
			if beat_game~=0 then
				break;
			end
			
			
				count=0;
		    	gene = string.sub(cand,ti,ti+7);
				u = string.sub(gene,1,1);
				d = string.sub(gene,2,2);
				l = string.sub(gene,3,3);
				r = string.sub(gene,4,4);
				a = string.sub(gene,5,5);
				b = string.sub(gene,6,6);
				str = string.sub(gene,7,7);
				sel = string.sub(gene,8,8);
				
				if u=='1' then
					up_val = true;
				else up_val = false;
				end
				
				if d=='1' then
					down_val = true;
				else down_val = false;
				end
				
				if l=='1' then
					left_val = true;
				else left_val = false;
				end
				
				if r=='1' then
					right_val = true;
				else right_val = false;
				end
				
				if a=='1' then
					A_val = true;
				else A_val = false;
				end
				
				if b=='1' then
					B_val = true;
				else B_val = false;
				end
				
				if str=='1' then
					start_val = true;
				else start_val = false;
				end
				
				if sel=='1' then
					select_val = true;
				else select_val = false;
				end
				
				tbl={
					up = up_val,
					down = down_val,
					left = left_val,
					right = right_val,
					A = A_val,
					B = B_val,
					start = start_val,
					select = select_val
					};
		        joypad.set(1,tbl);emu.frameadvance();
				ti=ti+8;
		end
end
