-- title:  game title
-- author: game developer
-- desc:   short description
-- script: lua


WIDTH = 240
HEIGHT = 136
W2 = 120
H2 = 68
PI = 3.1415
sin = math.sin
cos = math.cos
max = math.max
min = math.min
abs = math.abs
sqrt = math.sqrt
sqrt2 = sqrt(2)
ceil = math.ceil
floor = math.floor
random = math.random
sysmouse = mouse 
systime = time

time = {

	t = 0,
	dt = 0,
	last_t = 0,
	count = 0,
	fps = 60,

	update = function(self)
		local t = systime()
		self.dt = t - self.t
		self.t = t
		if t - self.last_t > 1000 then
			self.last_t = t
			self.fps = self.count
			self.count = 0
		else
			self.count = self.count + 1 
		end
	end,

	draw = function(self)
		print(self.fps, 228, 1, 4, true)
	end

}

debugger = {

	task = {},

	update = function(self)
		self.task = {}
	end,

	draw = function(self)
		for i = 1,#self.task do
			print(self.task[i], 0, 7*(i-1), 1)
		end
	end,

	trace = function(a)
		debugger.task[#debugger.task+1] = a
	end,

}

mathFun = {

	between = function(x, a, b)
		return x >= a and x < b
	end,

	clamp = function(x, a, b)
		if mathFun.between(x, a, b) then return x end
		if x < a then return a end
		if x >= b then return b end
	end,

	bool2int = function(a)
		return a and 1 or 0
	end,

	mod = function(x, a)
		return x - floor(x/a)*a
	end,

	sign = function(x)
	    return x > 0 and 1 or -1
	end,

	maprange = function(x, a1, b1, a2, b2)
		return (x - a1)/(b1 - a1)*(b2 - a2) + a2
	end,

	isnan = function(x)
		return x ~= x
	end,

	round = function(x)
		return math.floor(x + 0.5)
	end,

	lerp = function(x, a, b)
		return (1-x)*a + x*b
	end,

	isin = function(a, list)
		local flag = false
		for i = 1,#list do
			if list[i] == a then 
				flag = true 
				break
			end
		end
		return flag
	end,

	randlist = function(list, n)
		if n ==  nil then n = #list end
		local i = random(1, n)
		return list[i]
	end,

	randprob = function(list, prob)
		local a = random()
		local cumsum = 0
		for i = 1,#list do
			if a <= prob[i] then return a end
			cumsum = cumsum + prob[i+1]
		end
	end,

	sum = function(list)
		local tot = 0 
		for i = 1,#list do
			tot = tot + list[i]
		end
		return tot
	end,

}

Vec = {

	new = function(x, y, z)
		local v = {x, y, z}
		setmetatable(v, Vec.mt)
		return v
	end,

	mt = {

		__add = function (u, v)
			return Vec.new(u[1]+v[1], u[2]+v[2], u[3]+v[3])
		end,

		__sub = function(u, v)
			return Vec.new(u[1]-v[1], u[2]-v[2], u[3]-v[3])
		end,

		__mul = function(k, v)
			if type(k) == "table" then
				return Vec.new(k[1]*v[1], k[2]*v[2], k[3]*v[3])
			else
				return Vec.new(k*v[1], k*v[2], k*v[3])
			end
		end,

		__div = function(v, k)
			if type(k) == "table" then
				return Vec.new(v[1]/k[1], v[2]/k[2], v[3]/k[3])
			else
				return Vec.new(v[1]/k, v[2]/k, v[3]/k)
			end
		end,

		__pow = function(v, k)
			return Vec.new(v[1]^k, v[2]^k, v[3]^k)
		end,

		__eq = function(u, v)
			return u[1] == v[1] and u[2] == v[2] and u[3] == v[3]
		end,

		__tostring = function(v)
			return "(" .. v[1] .. "," .. v[2] .. "," .. v[3] .. ")"
		end,

		__concat = function(s, v)
			return s .. "(" .. v[1] .. "," .. v[2] .. "," .. v[3] .. ")"
		end,

		__index = {

			normalise = function(self)
				local r = Vec.norm(self)
				self[1] = self[1] / r
				self[2] = self[2] / r
				self[3] = self[3] / r
				return self
			end,

			proj = function(self, camera)
				local x = Vec.dot(self, camera.x)
				local y = Vec.dot(self, camera.y)
				return x+W2, -y+H2 - camera.y2
			end,

			rotate = function(self, ax, ay, az)
				local cx = cos(ax)
				local sx = sin(ax)
				local cy = cos(ay)
				local sy = sin(ay)
				local cz = cos(az)
				local sz = sin(az)
				local x1 = self[1]
				local y1 = self[2] * cx - self[3] * sx
				local z1 = self[2] * sx + self[3] * cx
				local x2 = x1 * cy + z1 * sy
				local y2 = y1
				local z2 = -x1 * sy + z1 * cy
				self[1] = x2 * cz - y2 * sz
				self[2] = x2 * sz + y2 * cz
				self[3] = z2
				return self
			end,

			draw = function(self, col)
				if col == nil then col = 4 end
				local x, y = self:proj(camera)
				circb(x, y, 2, col)
			end,
		}
	},

	norm = function(v)
		return sqrt(v[1]*v[1] + v[2]*v[2] + v[3]*v[3])
	end,

	dot = function(u, v)
		return u[1]*v[1] + u[2]*v[2] + u[3]*v[3]
	end,

	cross = function(u, v)
		return Vec.new(u[2]*v[3]-v[2]*u[3], v[1]*u[3]-u[1]*v[3], u[1]*v[2]-v[1]*u[2])
	end,

	avg = function(u)
		return (u[1] + u[2] + u[3]) / 3
	end,

	dist = function(u, v)
		return Vec.norm(u - v)
	end,

	mid = function(u, v)
		return (u + v) / 2
	end,

	line = function(x0, y0, u, v, camera, col)
		local x1, y1 = u:proj(camera)
		local x2, y2 = v:proj(camera)
		line(x1, y1, x2, y2, col)
	end,

}

Mat = {

	new = function(A)
		setmetatable(A, Mat.mt)
		return A
	end,

	newZeros = function(n, m, c)
		local A = {}
		if c == nil then c = 0 end
		for i = 1,n do
			A[i] = {}
			for j = 1,m do
				A[i][j] = c
			end
		end
		setmetatable(A, Mat.mt)
		return A 
	end,

	mt = {

		__add = function(A, B)
			local n, m = A:size()
			C = {}
			if getmetatable(B) == Mat.mt then 
				for i = 1,n do
					C[i] = {}
					for j = 1,m do
						C[i][j] = A[i][j] + B[i][j]
					end
				end
			else
				for i = 1,n do
					C[i] = {}
					for j = 1,m do
						C[i][j] = A[i][j] + B
					end
				end
			end
			return Mat.new(C)
		end,

		__sub = function(A, B)
			local n, m = A:size()
			C = {}
			if getmetatable(B) == Mat.mt then 
				for i = 1,n do
					C[i] = {}
					for j = 1,m do
						C[i][j] = A[i][j] - B[i][j]
					end
				end
			else
				for i = 1,n do
					C[i] = {}
					for j = 1,m do
						C[i][j] = A[i][j] - B
					end
				end
			end
			return Mat.new(C)
		end,

		__mul = function(k, A)
			local n, m = A:size()
			B = {}
			if getmetatable(k) == Mat.mt then
				for i = 1,n do
					B[i] = {}
					for j = 1,m do
						B[i][j] = k[i][j] * A[i][j]
					end
				end
			else
				for i = 1,n do
					B[i] = {}
					for j = 1,m do
						B[i][j] = k * A[i][j]
					end
				end
			end
			return Mat.new(B)
		end,

		__div = function(A, k)
			local n, m = A:size()
			B = {}
			for i = 1,n do
				B[i] = {}
				for j = 1,m do
					B[i][j] = A[i][j] / k
				end
			end
			return Mat.new(B)
		end,

		__tostring = function(A)
			local text = ""
			local n, m = A:size()
			for i = 1,n do
				for j = 1,m do
					if j < m then
						text = text .. A[i][j]
					else
						text = text .. A[i][j] .. "\n"
					end
				end
			end
			return text
		end,

		__index = {

			size = function(self)
				local n = #self
				local m = 0
				if n ~= 0 then
					m = #self[1]
				end
				return n, m
			end,

		}
	}

}

Face = {

	new = function(verts, uvs, col, is_map)
		local f = {verts=verts, uvs=uvs, col=col, is_map=is_map}
		setmetatable(f, Face.mt)
		return f
	end,

	mt = {

		__index = {

			drawCol = function(self)
				local x1, y1 = self.verts[1]:proj(camera)
				local x2, y2 = self.verts[2]:proj(camera)
				local x3, y3 = self.verts[3]:proj(camera)
				local x4, y4 = self.verts[4]:proj(camera)
				if Face.isccw(x1, y1, x2, y2, x3, y3) then return end
				tri(x1, y1, x2, y2, x3, y3, self.col)
				tri(x2, y2, x3, y3, x4, y4, self.col)
				Face.border(x1, y1, x2, y2, x3, y3, x4, y4, 8)
			end,

			drawTex = function(self, bcol)
				local x1, y1 = self.verts[1]:proj(camera)
				local x2, y2 = self.verts[2]:proj(camera)
				local x3, y3 = self.verts[3]:proj(camera)
				local x4, y4 = self.verts[4]:proj(camera)
				if Face.isccw(x1, y1, x2, y2, x3, y3) then return end
				tri(x1, y1, x2, y2, x3, y3, self.col)
				tri(x2, y2, x3, y3, x4, y4, self.col)
				if self.uvs[1] ~= nil then
					local u1, v1, u2, v2 = table.unpack(self.uvs)
					textri(x1, y1, x2, y2, x3, y3, u1, v1, u2, v1, u1, v2, false, 0)
					textri(x2, y2, x3, y3, x4, y4, u2, v1, u1, v2, u2, v2, false, 0)
				end
				if bcol == nil then
					Face.border(x1, y1, x2, y2, x3, y3, x4, y4, 8)
				elseif bcol ~= 0 then
					Face.border(x1, y1, x2, y2, x3, y3, x4, y4, bcol)
				end
			end,

			drawOutline = function(self)
				local x1, y1 = self.verts[1]:proj(camera)
				local x2, y2 = self.verts[2]:proj(camera)
				local x3, y3 = self.verts[3]:proj(camera)
				local x4, y4 = self.verts[4]:proj(camera)
				if Face.isccw(x1, y1, x2, y2, x3, y3) then return end
				Face.border(x1, y1, x2, y2, x3, y3, x4, y4, self.col)
			end,

			flip = function(self)
				local u1, u2, u3, u4 = table.unpack(self.verts)
				self.verts = {u2, u1, u4, u3}
				return self
			end,

			draw = function(self, col, bcol)
				local x1, y1 = self.verts[1]:proj(camera)
				local x2, y2 = self.verts[2]:proj(camera)
				local x3, y3 = self.verts[3]:proj(camera)
				local x4, y4 = self.verts[4]:proj(camera)
				if Face.isccw(x1, y1, x2, y2, x3, y3) then return end
				if col == nil then col = self.col end
				tri(x1, y1, x2, y2, x3, y3, col)
				tri(x2, y2, x3, y3, x4, y4, col)
				if bcol == nil then
					Face.border(x1, y1, x2, y2, x3, y3, x4, y4, 8)
				elseif bcol ~= 0 then
					Face.border(x1, y1, x2, y2, x3, y3, x4, y4, bcol)
				end
			end,

		}
	},

	isccw = function(x1, y1, x2, y2, x3, y3)
		local a1 = x2 - x1
		local a2 = y2 - y1
		local b1 = x3 - x1
		local b2 = y3 - y1
		if (a1*b2 - b1*a2) < 0 then return true else return false end
		return flag
	end,

	border = function(x1, y1, x2, y2, x3, y3, x4, y4, col)
		line(x1, y1, x2, y2, col)
		line(x2, y2, x4, y4, col)
		line(x4, y4, x3, y3, col)
		line(x3, y3, x1, y1, col)
	end,

	zsort = function(u, v)
		return Vec.dot(camera.o, u) < Vec.dot(camera.o, u)
	end,

}

camera = {

	o = Vec.new(0, 0, 1),
	x = Vec.new(1, 0, 0),
	y = Vec.new(0, 1, 0),

	target_ax = -PI/6,
	target_ay = PI/4,
	lerp_fac = 0.1,
	pos = 0,
	ax = 0,
	ay = 0,
	target_y2 = 0,
	y2 = 0,
	offset_y2 = 0,

	tick_x = 190,
	tick_y = H2-3,

	load = function(self)

	end,

	update = function(self)
		-- if key(23) then self.ax = self.ax + 0.03 end
		-- if key(19) then self.ax = self.ax - 0.03 end
		if key(1) then self.target_ay = self.target_ay + 0.03 end
		if key(4) then self.target_ay = self.target_ay - 0.03 end
		self.ax = mathFun.lerp(self.lerp_fac, self.ax, self.target_ax)
		self.ay = mathFun.lerp(self.lerp_fac, self.ay, self.target_ay)
		self.o = Vec.new(0, 0, 1):rotate(self.ax, self.ay, 0)
		self.x = Vec.new(1, 0, 0):rotate(self.ax, self.ay, 0)
		self.y = Vec.new(0, 1, 0):rotate(self.ax, self.ay, 0)
		-- vertical motion
		if key(23) then self.offset_y2 = self.offset_y2 + 1 end
		if key(19) then self.offset_y2 = self.offset_y2 - 1 end
		-- self.target_y2 = player.i * terrain.h*cos(self.ax) + self.offset_y2
		self.target_y2 = -player.y*cos(self.ax) + self.offset_y2
		self.y2 = mathFun.lerp(self.lerp_fac, self.y2, self.target_y2)
	end,

	rotateRightAngle = function(self, n)
		self.target_ay = self.target_ay + n*PI/2
		self.pos = self.pos + n
	end,

	drawTicks = function(self)
		local c = cos(self.ax)
		for i = 0,terrain.n,5 do
			print(-i, self.tick_x, self.tick_y+(i-0.5)*terrain.h*c-self.y2, 13)
		end
	end,

}

Voxel = {

	render_radius = 13,

	new = function(f1, f2, f3, f4, f5, o, r)
		if r == nil then r = Voxel.render_radius end
		local v = {f1=f1, f2=f2, f3=f3, f4=f4, f5=f5, o=o, r=r}
		setmetatable(v, Voxel.mt)
		return v
	end,

	mt = {

		__index = {

			draw = function(self)
				if self.f1 ~= nil then self.f1:draw() end
				if self.f2 ~= nil then self.f2:draw() end
				if self.f3 ~= nil then self.f3:draw() end
				if self.f4 ~= nil then self.f4:draw() end
				if self.f5 ~= nil then self.f5:draw() end
			end,

			drawTex = function(self)
				if self.f1 ~= nil then self.f1:drawTex() end
				if self.f2 ~= nil then self.f2:drawTex() end
				if self.f3 ~= nil then self.f3:drawTex() end
				if self.f4 ~= nil then self.f4:drawTex() end
				if self.f5 ~= nil then self.f5:drawTex() end
			end,

			isonscreen = function(self)
				local x, y = self.o:proj(camera)
				return y+self.r > 0 and y-self.r < HEIGHT
			end,

		}
	},

	type1 = function()
		
	end

}

terrain = {

	n = 11, 	-- height
	m = 5,		-- depth
	w = 12*sqrt2,		-- voxel width (12)
	h = 28/sqrt(3),		-- voxel height (14)

	mesh = {},
	faces = {},
	data = {{}},
	voxels = {{{}}},
	order = {},
	deadend = {},
	vents = {{}},

	mesh_id = {
		{1,1}, {1,2}, {1,3}, {1,4},
		{1,5}, {2,5}, {3,5}, {4,5},
		{5,5}, {5,4}, {5,3}, {5,2},
		{5,1}, {4,1}, {3,1}, {2,1}
	},
	color_id = {[0]=0, [1]=3, [2]=5, [3]=13, [4]=0},
	tile_id = {[0]=0, [1]=2, [2]=4, [3]=6, [4]=8},
	data_id = {
		{1, 2, 3, 4, 5},
		{16, nil, nil, nil, 6},
		{15, nil, nil, nil, 7},
		{14, nil, nil, nil, 8},
		{13, 12, 11, 10, 9},
	},

	load = function(self)
		self:loadMesh()
		local deadend
		self.data, self.deadend = maze.generate(self.n, 16)
		self:loadVoxels()
		self.vents = maze.getVents(self.data)
	end,

	loadMesh = function(self)
		local mesh = {}
		for i = 1,self.m+1 do
			mesh[i] = {}
			for j = 1,self.m+1 do
				mesh[i][j] = Vec.new(i-1, 0, j-1) 
			end
		end
		setmetatable(mesh, Mat.mt)
		mesh = self.w * mesh - self.m/2 * self.w * Vec.new(1, 0, 1)
		self.mesh = mesh
		self.l = self.w * (self.m - 1)
	end,

	loadVoxels = function(self)
		local voxels = {}
		local n = self.n 
		local m = self.m
		for i = 1,n do
			voxels[i] = {}
			for j = 1,m do
				voxels[i][j] = {}
				for k = 1,m do
					voxels[i][j][k] = self:getVoxel(i, j, k)
				end
			end
		end
		-- tree voxel
		local u1 = self.mesh[3][3]
		local u2 = self.mesh[4][3]
		local u3 = self.mesh[3][4]
		local u4 = self.mesh[4][4]
		local f5 = Face.new({u1, u2, u3, u4}, {}, 5)
		voxels[1][3][3] = Voxel.new(nil, nil, nil, nil, f5, (u1+u3)/2)
		self.voxels = voxels
		-- create list for sorting
		local order = {}
		for j = 1,m do
			for k = 1,m do
				table.insert(order, {terrain.mesh[j][k], j, k})
			end
		end
		self.order = order
	end,

	getVoxel = function(self, i, j, k)
		-- coords
		local dy = Vec.new(0, -self.h, 0)
		local y = (i-1)*dy
		local u1 = self.mesh[j][k] + y
		local u2 = self.mesh[j+1][k] + y
		local u3 = self.mesh[j][k+1] + y
		local u4 = self.mesh[j+1][k+1] + y
		local v1 = u1 + dy
		local v2 = u2 + dy
		local v3 = u3 + dy
		local v4 = u4 + dy
		local o = (u1+v4)/2
		local data_id = self.data_id[j][k]
		if data_id ~= nil then
			local type = self.data[i][data_id]
			local voxel = voxeltype[type](u1, u2, u3, u4, v1, v2, v3, v4)
			if mathFun.isin(data_id, {2, 3, 4}) then voxel.f3 = nil
			elseif mathFun.isin(data_id, {6, 7, 8}) then voxel.f4 = nil
			elseif mathFun.isin(data_id, {10, 11, 12}) then voxel.f1 = nil
			elseif mathFun.isin(data_id, {14, 15, 16}) then voxel.f2 = nil
			end
			if i > 1 and self.data[i-1][data_id] ~= 0 then voxel.f5 = nil end
			return voxel
		elseif j == 3 and k == 3 then 
			-- center column
			return nil
		else
			-- local type = mathFun.randlist({2,3})
			local type = 3
			local col = self.color_id[type]
			-- local tile = self.tile_id[type]
			-- local uvs = {tile*8, 16, (tile+2)*8, 32}
			local uvs = {}
			local f1, f2, f3, f4, f5
			if j == 2 then f1 = Face.new({u1, u3, v1, v3}, uvs, col) end
			if j == 4 then f3 = Face.new({u4, u2, v4, v2}, uvs, col) end
			if k == 2 then f4 = Face.new({u2, u1, v2, v1}, uvs, col) end
			if k == 4 then f2 = Face.new({u3, u4, v3, v4}, uvs, col) end
			-- tile = 2
			-- uvs = {tile*8, 0, (tile+2)*8, 16}
			if i == 1 then f5 = Face.new({u1, u2, u3, u4}, uvs, 3) end
			return Voxel.new(f1, f2, f3, f4, f5, o)
		end
	end,

	sortOrder = function(u, v)
		return Vec.dot(camera.o, u[1]) < Vec.dot(camera.o, v[1])
	end,

}

voxeltype = {

	-- empty
	[0] = function(u1, u2, u3, u4, v1, v2, v3, v4)
		local o = (u1+v4)/2
		return Voxel.new(nil, nil, nil, nil, nil, o) 
	end,

	-- dirt 
	[1] = function(u1, u2, u3, u4, v1, v2, v3, v4)
		local f1 = Face.new({u1, u3, v1, v3}, {}, 5)
		local f2 = Face.new({u3, u4, v3, v4}, {}, 5)
		local f3 = Face.new({u4, u2, v4, v2}, {}, 5)
		local f4 = Face.new({u2, u1, v2, v1}, {}, 5)
		local f5 = Face.new({u1, u2, u3, u4}, {}, 5)
		local o = (u1+v4)/2
		return Voxel.new(f1, f2, f3, f4, f5, o)
	end,

	-- dirt with grass
	[2] = function(u1, u2, u3, u4, v1, v2, v3, v4)
		local f1 = Face.new({u1, u3, v1, v3}, {}, 5)
		local f2 = Face.new({u3, u4, v3, v4}, {}, 5)
		local f3 = Face.new({u4, u2, v4, v2}, {}, 5)
		local f4 = Face.new({u2, u1, v2, v1}, {}, 5)
		local f5 = Face.new({u1, u2, u3, u4}, {}, 3)
		local o = (u1+v4)/2
		return Voxel.new(f1, f2, f3, f4, f5, o)
	end,

	-- rock
	[3] = function(u1, u2, u3, u4, v1, v2, v3, v4)
		local f1 = Face.new({u1, u3, v1, v3}, {}, 13)
		local f2 = Face.new({u3, u4, v3, v4}, {}, 13)
		local f3 = Face.new({u4, u2, v4, v2}, {}, 13)
		local f4 = Face.new({u2, u1, v2, v1}, {}, 13)
		local f5 = Face.new({u1, u2, u3, u4}, {}, 13)
		local o = (u1+v4)/2
		return Voxel.new(f1, f2, f3, f4, f5, o)
	end,

	-- rock
	[4] = function(u1, u2, u3, u4, v1, v2, v3, v4)
		local f1 = Face.new({u1, u3, v1, v3}, {}, 0)
		local f2 = Face.new({u3, u4, v3, v4}, {}, 0)
		local f3 = Face.new({u4, u2, v4, v2}, {}, 0)
		local f4 = Face.new({u2, u1, v2, v1}, {}, 0)
		local f5 = Face.new({u1, u2, u3, u4}, {}, 0)
		local o = (u1+v4)/2
		return Voxel.new(f1, f2, f3, f4, f5, o)
	end,

}

Sprite = {

	new = function(spr, o, w, h)
		local s = {spr=spr, o=o, w=w, h=h}
		setmetatable(s, Sprite.mt)
		return s
	end,

	mt = {

		__index	= {

			draw = function(self)
				local x, y = self.o:proj(camera) 
				spr(self.spr, x-8*self.w/2, y-8*self.h, 0, 1, 0, 0, self.w, self.h)
			end

		}
	},

	ysort = function(u, v)
		return u[2] < v[2]
	end,

}

player = {
	
	dr = 0,
	o = Vec.new(0, 0, 0),
	speed_x = 0.7,
	is_left = false,
	range = terrain.w * (terrain.m - 1),
	r = 2 * terrain.w * (terrain.m - 1),
	i = 0,
	j = 9,

	njump = 1,
	jump_height = 18,
	jump_dist = 1.3*terrain.w,
	y = 0,
	speed_y = 0,
	gravity = -500,
	jump_vel = 0,
	h = 14,

	ncoin = 0,

	load = function(self)
		self.r = 2 * self.range
		self.o = Vec.new(self.range/2, 0, self.range/2)
		self.j = 2 * terrain.m - 1
		self.jump_vel = 4*self.jump_height*self.speed_x*60 / self.jump_dist
		self.maxgravity = -2*self.speed_x*60*self.jump_vel / self.jump_dist
		self.gravity = self.maxgravity
		self.maxvel = 1.2*self.jump_vel
	end,

	update = function(self)
		-- movement
		local coll = self:getCollision()
		local rwall, lwall = self:getWall()
		if btn(2) then 
			self.is_left = true 
			if not (coll[3] or coll[5]) then
				self.r = self.r - self.speed_x
				self.dr = self.dr - self.speed_x
			elseif self.r-self.speed_x >= lwall then
				self.r = self.r - self.speed_x
				self.dr = self.dr - self.speed_x
			end
		end
		if btn(3) then 
			self.is_left = false 
			if not (coll[4] or coll[6]) then
				self.r = self.r + self.speed_x
				self.dr = self.dr + self.speed_x
			elseif self.r + self.speed_x < rwall then
				self.r = self.r + self.speed_x
				self.dr = self.dr + self.speed_x
			end
		end
		self.r = mathFun.mod(self.r, 4*self.range)
		self.j = mathFun.mod(floor(self.r/terrain.w + 0.5), 16) + 1
		
		-- warp
		if self.y < -(terrain.n+8)*terrain.h then
			local dy = camera.y2 - camera.target_y2 
			self.y = 9*terrain.h 
			camera.target_y2 = -self.y*cos(camera.ax) + camera.offset_y2
			camera.y2 = camera.target_y2 + dy
			reload()
		end

		-- jump
		coll = self:getCollision()
		if keyp(48) and self.njump > 0 then
		-- if btnp(5) and self.njump > 0 then 
			self.speed_y = self.jump_vel
			self.njump = self.njump - 1
		end
		-- update gravity on vents
		self.gravity = self.maxgravity
		self.maxvel = 1.2*self.jump_vel
		if self.i > 0 then
			if terrain.vents[self.i][self.j] == 1 then
				self.gravity = self.maxgravity / 2 
				self.maxvel = 0.7*self.jump_vel
			end
		end
		self.speed_y = mathFun.clamp(self.speed_y + self.gravity*time.dt/1000, -self.maxvel, 1.2*self.jump_vel) 
		self.y = self.y + self.speed_y*time.dt/1000
		-- count jump
		if coll[1] then
			self.speed_y = min(self.speed_y, 0)
			self.y = min(-(self.i-1) * terrain.h, self.y)
		end
		if (self.y < -self.i * terrain.h and coll[2]) then
			self.njump = 1
		elseif self.i > 0 then
			if terrain.vents[self.i][self.j] == 1 then
				self.njump = 1
			end
		else
			self.njump = 0
		end
		-- collision
		if coll[2] then
			self.y = max(-self.i * terrain.h, self.y)
		end
		self.i = mathFun.clamp(ceil(-self.y/terrain.h), 0, terrain.n) 

		-- update position
		local dx = mathFun.clamp(-abs(self.r-2.5*self.range)+1.5*self.range, 0, self.range)
		local dz = mathFun.clamp(-abs(self.r-1.5*self.range)+1.5*self.range, 0, self.range)
		self.o = Vec.new(-self.range/2+dx, self.y, -self.range/2+dz)
		
		-- rotate camera
		if abs(self.dr) >= self.range then
			camera:rotateRightAngle(mathFun.sign(self.dr))
			self.dr = 0
		end

		-- collect coins
		if self.i > 0 then
			if field.data[self.i][self.j] ~= nil then
				if field.data[self.i][self.j].spr == 324 then 
					local a = (self.r/terrain.w + 0.5) % 1
					if mathFun.between(a, 0.3, 0.7) then
						field.data[self.i][self.j] = nil 
						self.ncoin = self.ncoin + 1
					end
				end
			end
		end

	end,

	draw = function(self)
		local x, y = self.o:proj(camera)
		if self.is_left then
			spr(256, x-8, y-16, 0, 1, 1, 0, 2, 2)
		else
			spr(256, x-8, y-16, 0, 1, 0, 0, 2, 2)
		end
	end,

	getCollision = function(self)
		local coll = {false, false, false, false, false, false}
		-- up
		if self.i > 1 then
			coll[1] = terrain.data[self.i-1][self.j] ~= 0
		end
		-- down
		if self.i < terrain.n then
			coll[2] = terrain.data[self.i+1][self.j] ~= 0
		end
		-- layer 0
		if self.i == 0 then return coll end
		-- left and right
		if self.j == 1 then jm1 = 16 else jm1 = self.j-1 end
		if self.j == 16 then jp1 = 1 else jp1 = self.j+1 end
		coll[3] = terrain.data[self.i][jm1] ~= 0
		coll[4] = terrain.data[self.i][jp1] ~= 0
		if self.i > 1 and self.y + self.h > -(self.i-1) * terrain.h then
			coll[5] = terrain.data[self.i-1][jm1] ~= 0
			coll[6] = terrain.data[self.i-1][jp1] ~= 0
		end
		return coll
	end,

	getWall = function(self)
		local j = self.j-1
		local lwall, rwall
		if j ~= 0 then
			rwall = (j+0.5)*terrain.w 
			lwall = mathFun.mod(j-0.5, 16)*terrain.w
		else
			if self.r >= 15.5*terrain.w then
				lwall = 15.5*terrain.w
				rwall = 16.5*terrain.w
			else
				lwall = -0.5*terrain.w
				rwall = 0.5*terrain.w
			end
		end
		return rwall, lwall
	end,

	drawHUD = function(self)
		spr(260, 2, 2, 0)
		print(self.ncoin, 12, 4, 4)
	end

}

Color = {

	addr = 0x03FC0,

	new = function(r, g, b)
		local c = Vec.new(r, g, b)
		return c
	end,

	getcol = function(col)
		local addr = Color.addr + 3*col
		local r = peek(addr)
		local g = peek(addr+1)
		local b = peek(addr+3)
		return Vec.new(r, g, b)
	end,

	setcol = function(c, col)
		local addr = Color.addr + 3*col
		poke(addr, c[1])
		poke(addr+1, c[2])
		poke(addr+2, c[3])
	end,

}

background = {

	c1 = Color.new(0x13, 0x27, 0x43),
	c2 = Color.new(0x40, 0x70, 0x88),
	stars = {},

	scn = function(self, line)
		local k = line/HEIGHT
		Color.setcol(mathFun.lerp(k, self.c1, self.c2), 15)
	end,

	load = function(self)
		self.stars = {}
		for i = 1,15 do
			local x = random(0, WIDTH)
			local y = random(0, HEIGHT)
			table.insert(self.stars, {x, y, t=random(2000)})
		end
	end,

	draw = function(self)
		-- local t = time.t // 1000 % 2
		for i = 1,#self.stars do
			local x = mathFun.mod(self.stars[i][1] + camera.ay*100, WIDTH)
			-- pix(x, self.stars[i][2], 12)
			spr(261 + (self.stars[i].t+time.t)//2000 % 2, x, self.stars[i][2], 0)
		end
	end

}

tree = {

	trunk = nil, 
	leaves = {},

	load = function(self)
		self.leaves = {}
		for i = 2,4 do
			local z = i*10
			for j = 1,i^2 do
				local r = random()*z/2
				local t = 2*PI * random()
				local v = Vec.new(r*cos(t), -z+65, r*sin(t))
				table.insert(self.leaves, v)
			end
		end
		-- load trunk
		self.trunk = Sprite.new(288, Vec.new(0, -5, 0), 4, 4)
	end,

	draw = function(self)
		self.trunk:draw()
		for i = 1,#self.leaves do
			local x, y = self.leaves[i]:proj(camera)
			spr(292, x-8, y-8, 0, 1, 0, 0, 2, 2)
		end
	end,

}

vending = {

	voxel = nil,
	faces = {},
	ison = false,
	ncoin = 0,

	load = function(self)
		local j = 2 
		local k = 4
		local dy = Vec.new(0, -9/7*terrain.h, 0)
		local y = -1*dy
		local u1 = terrain.mesh[j][k] + y + Vec.new(0, 0, terrain.w/3)
		local u2 = terrain.mesh[j+1][k] + y + Vec.new(0, 0, terrain.w/3)
		local u3 = terrain.mesh[j][k+1] + y
		local u4 = terrain.mesh[j+1][k+1] + y
		local v1 = u1 + dy
		local v2 = u2 + dy
		local v3 = u3 + dy
		local v4 = u4 + dy
		local f1 = Face.new({u1, u3, v1, v3}, {12, 32, 20, 48}, 2)
		local f3 = Face.new({u4, u2, v4, v2}, {12, 32, 20, 48}, 2)
		local f4 = Face.new({u2, u1, v2, v1}, {19, 32, 32, 48}, 2)
		local f5 = Face.new({u1, u2, u3, u4}, {0, 24, 13, 32}, 2)
		local o = (u1+v4)/2
		self.voxel = Voxel.new(f1, nil, f3, f4, f5, o)
		local f21 = Face.new({u3, u4, v3, v4}, {0, 32, 13, 49}, 2)
		local f22 = Face.new({u3, u4, v3, v4}, {32, 32, 45, 49}, 2)
		self.faces = {f21, f22}
	end,

	update = function(self)
		self.ison = false 
		if player.j == 6 then 
			if player.i == 0 and player.y < 9/7*terrain.h then
				self.ison = true 
			end
		end 
		if btnp(4) then
			if self.ison then 
				if player.ncoin > 0 then 
					player.ncoin = player.ncoin - 1 
					field.ncoin = field.ncoin - 1
					self.ncoin = self.ncoin + 1
				end
			end
		end
	end,

	draw = function(self)
		self.voxel.f1:drawTex(2)
		self.voxel.f3:drawTex(2)
		self.voxel.f4:drawTex(2)
		self.voxel.f5:drawTex(2)
		if self.ison then
			self.faces[2]:drawTex(2)
		else
			self.faces[1]:drawTex(2)
		end
		local x, y = self.voxel.o:proj(camera)
		local offset = time.t // 1000 % 2
		spr(324, x-4, y-28+offset, 0)
		spr(263 + offset, x-4, y-20, 0)
	end,

}

field = {

	data = {{}},
	ncoin = 0,
	coin_pos = {},

	load = function(self)
		local n, m = terrain.data:size()
		self.ncoin = 0
		self.coin_pos = {}
		setmetatable(self.coin_pos, Mat.mt)
		local data = {}
		for i = 1,n do
			data[i] = {}
		end
		self.data = data
		setmetatable(self.data, Mat.mt)
		-- load vents 
		self:loadVents(terrain.vents)
		-- load coins 
		for _,v in ipairs(terrain.deadend) do
			local i, j = table.unpack(v)
			if terrain.data[i][j] == 0 and terrain.data[i+1][j] ~= 0 and self.data[i][j] == nil then
				local a, b = table.unpack(terrain.mesh_id[j])
				local o = terrain.mesh[a][b] + Vec.new(terrain.w/2, -i*terrain.h, terrain.w/2)
				self.data[i][j] = Sprite.new(324, o, 1, 1)
				table.insert(self.coin_pos, {i, j})
				self.ncoin = self.ncoin + 1
			end 
		end
		-- add more coins
		local r = 5 - self.ncoin
		local tempi = {2,4,6,8,10} 
		for k = 1,r do
			local i = table.remove(tempi, random(1, #tempi))
			local tempj = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16}
			while #tempj > 0 do
				local j = table.remove(tempj, random(1, #tempj))
				if terrain.data[i][j] == 0 and terrain.data[i+1][j] ~= 0 and self.data[i][j] == nil then
					local a, b = table.unpack(terrain.mesh_id[j])
					local o = terrain.mesh[a][b] + Vec.new(terrain.w/2, -i*terrain.h, terrain.w/2)
					self.data[i][j] = Sprite.new(324, o, 1, 1)
					table.insert(self.coin_pos, {i, j})
					self.ncoin = self.ncoin + 1
					break
				end
			end
		end
	end,

	zsort = function(u, v)
		return Vec.dot(camera.o, u.o) < Vec.dot(camera.o, v.o)
	end,

	loadVents = function(self, B)
		local n, m = B:size()
		for j = 1,m do
			local h = 0
			for i = 0,n-1 do
				if B[i+1][j] == 1 then
					h = h+1 
				elseif h > 0 then
					local a, b = table.unpack(terrain.mesh_id[j])
					local o = terrain.mesh[a][b] + Vec.new(terrain.w/2, -i*terrain.h, terrain.w/2)
					self.data[i][j] = Pslib.new(o, h)
					h = 0
				end
			end
		end
	end,

}

maze = {

	generate = function(n, m)
		local A = {}
		for i = 1,n do
			A[i] = {}
			for j = 1,m do
				-- A[i][j] = mathFun.randlist({2, 2, 2, 2, 3})
				A[i][j] = 2
			end
		end
		setmetatable(A, Mat.mt)
		-- Randomized depth-first search
		local stack = {{2*random(1, (n-1)/2), 2*random(1, m/2) - 1}}
		local deadend = {}
		local islast = true
		while #stack > 0 do
			local current = table.remove(stack, #stack)
			local neighbours = maze.getneighbours(A, current[1], current[2])
			if #neighbours > 0 then
				local new = maze.randbias(neighbours)
				if new[3] == 1 then A[current[1]][current[2]+1] = 0 
				elseif new[3] == 2 then 
					local jm1
					if current[2] == 1 then jm1 = 16 else jm1 = current[2]-1 end
					A[current[1]][jm1] = 0 
				elseif new[3] == 3 then A[current[1]-1][current[2]] = 0 
				elseif new[3] == 4 then A[current[1]+1][current[2]] = 0 
				end
				A[new[1]][new[2]] = 0
				table.insert(stack, current)
				table.insert(stack, new)
				islast = true
			else
				if islast then table.insert(deadend, current) end 
				islast = false
			end
		end
		-- beautify
		maze.beautify(A)
		-- start and end
		A[1][13] = 0
		A[n][5] = 0
		A[n-1][4] = 0
		A[n-1][5] = 0
		A[n-1][6] = 0
		A[n-2][4] = 0
		A[n-2][5] = 0
		A[n-2][6] = 0 
		return A, deadend
	end,

	getneighbours = function(A, i, j)
		local n, m = A:size()
		local neighbours = {}
		local jp2, jm2 
		if j == 15 then jp2 = 1 else jp2 = j+2 end
		if j == 1 then jm2 = 15 else jm2 = j-2 end
		if A[i][jp2] ~= 0 then 
			table.insert(neighbours, {i, jp2, 1})		-- right
		end
		if A[i][jm2] ~= 0 then 
			table.insert(neighbours, {i, jm2, 2})		-- left
		end
		if i ~= 2 then
			if A[i-2][j] ~= 0 then
				table.insert(neighbours, {i-2, j, 3}) 	-- up
			end
		end
		if i ~= n-1 then 
			if A[i+2][j] ~= 0 then
				table.insert(neighbours, {i+2, j, 4}) 	-- down
			end
		end
		return neighbours
	end,

	randbias = function(t)
		for i = 1,#t do
			if t[i][3] < 3 then
				table.insert(t, t[i])
				table.insert(t, t[i])
			end
		end
		return mathFun.randlist(t)
	end,

	beautify = function(A)
		local n, m = A:size()
		for i = 4,n,2 do
			for j = 1,m,2 do
				if A[i][j] == 0 then
					local im1 = i-1
					local ip1 = i+1
					local jm1 = j-1
					local jp1 = j+1
					if j == 1 then jm1 = 16 end
					local t = {0, 0, 0, 0}
					if A[im1][j] == 0 then t[1] = 1 end 	-- up
					if A[i][jm1] == 0 then t[2] = 1 end 	-- left
					if A[i][jp1] == 0 then t[3] = 1 end 	-- right
					if A[ip1][j] == 0 then t[4] = 1 end 	-- down
					if mathFun.sum(t) == 2 then
						if t[1] == 1 and t[2] == 1 then
							A[i][j] = A[im1][jm1]
							A[im1][jm1] = 0
						elseif t[1] == 1 and t[3] == 1 then
							A[i][j] = A[im1][jp1]
							A[im1][jp1] = 0
						end
					end
				end
			end
		end
	end,

	getVents = function(A)
		local n, m = A:size()
		local B = Mat.newZeros(n, m, 0)
		for i = 3,n-2 do
			for j = 1,m do
				local im1 = i-1
				if A[i][j] ~= 0 and A[im1][j] == 0 then
					local ip1 = i+1
					local jm1 = j-1
					local jp1 = j+1
					if j == 1 then jm1 = 16 end
					if j == 16 then jp1 = 1 end
					if A[im1][jp1] == 0 and A[i][jp1] == 0 and A[ip1][jp1] == 0 then
						local ii = i-1
						while ii < n do
							B[ii][jp1] = 1
							ii = ii + 1
							if A[ii][jp1] ~= 0 then break end
						end
					end
					if A[im1][jm1] == 0 and A[i][jm1] == 0 and A[ip1][jm1] == 0 then
						local ii = i-1 
						while ii < n do
							B[ii][jm1] = 1
							ii = ii + 1
							if A[ii][jm1] ~= 0 then break end
						end
					end
				end
			end
		end
		local ii = 1
		while ii < n do
			B[ii][13] = 1
			ii = ii + 1
			if A[ii][13] ~= 0 then break end
		end
		return B
	end,

}

boid = {

	nparticle = 20,
	r = terrain.m*terrain.w,
	w = 2*PI*terrain.m*terrain.w,
	h = terrain.n*terrain.h,
	objs = {},
	tile = {{}},
	tn = 20,
	timing = {0.05, 0.5, 0.8, 1},

	load = function(self)
		for i = 1,self.nparticle do
			local x = random()*self.w
			local y = random()*self.h
			local dx = random()*2 - 1
			local dy = -0.5
			local b = {x=x, y=y, dx=dx, dy=dy}
			local a = b.x/self.w * 2*PI
			b.o = Vec.new(self.r*cos(a), -b.y, self.r*sin(a))
			table.insert(self.objs, b)
		end
	end,

	update = function(self)
		-- init tile
		local d = self.w / self.tn
		local tile = {}
		for _,b in ipairs(self.objs) do
			-- update position
			b.x = mathFun.mod(b.x + b.dx, self.w)
			b.y = mathFun.mod(b.y + b.dy, self.h)
			-- convert 3d position
			local a = b.x/self.w * 2*PI
			b.o = Vec.new(self.r*cos(a), -b.y, self.r*sin(a))
			b.is_front = camera.o[1]*b.o[1] + camera.o[3]*b.o[3] > 0
			-- update tile
			-- b.i = b.x // d + 1 
			-- b.j = b.y // d + 1
			-- if tile[b.i] == nil then 
			-- 	tile[b.i] = {} 
			-- 	tile[b.i][b.j] = {}
			-- elseif tile[b.i][b.j] == nil then
			-- 	tile[b.i][b.j] = {}
			-- end
			-- table.insert(tile[b.i][b.j], b)
			-- self.tile = tile 
		end
	end,

	drawfront = function(self)
		for _,b in ipairs(self.objs) do
			if b.is_front then
				local x, y = b.o:proj(camera)
				if b.y < self.timing[1]*self.h then
					spr(355, x, y-4, 0)
				elseif b.y < self.timing[2]*self.h then
					spr(354, x, y-4, 0)
				elseif b.y < self.timing[3]*self.h then
					spr(353, x, y-4, 0)
				elseif b.y <= self.timing[4]*self.h then
					spr(352, x, y-4, 0)
				end
				-- spr(354+mathFun.bool2int(b.o[2]>-5), x, y-4, 0)
			end
		end 
	end,

	drawback = function(self)
		for _,b in ipairs(self.objs) do
			if not b.is_front then
				local x, y = b.o:proj(camera)
				if b.y < self.timing[1]*self.h then
					spr(355, x, y-4, 0)
				elseif b.y < self.timing[2]*self.h then
					spr(354, x, y-4, 0)
				elseif b.y < self.timing[3]*self.h then
					spr(353, x, y-4, 0)
				elseif b.y <= self.timing[4]*self.h then
					spr(352, x, y-4, 0)
				end
			end
		end 
	end,

}

Pslib = {

	objs = {},
	frames = {352,352,353,353,353,354,354,354,354,354,354,354,354,354,354,355},
	nextemittime = time.t,

	new = function(o, h)
		local ps = {
			particles = {},
			o = o,
			h = h*terrain.h,
			minvy = 0.2,
			maxvy = 0.8,
			next = time.t,
		}
		ps.maxlife = ps.h / ps.maxvy * 60
		ps.minlife = terrain.h / ps.maxvy * 60
		setmetatable(ps, Pslib.mt)
		table.insert(Pslib.objs, ps)
		return ps 
	end,

	mt = {

		__index = {

			draw = function(self)
				for i,p in ipairs(self.particles) do
					local x, y = p.o:proj(camera)
					local n = floor(p.fac*16)+1
					local f = Pslib.frames[n]
					spr(f, x-4, y-8, 0)
				end
			end,

			update = function(self)
				self:emittimmer()
				for k,p in pairs(self.particles) do
					p.o[2] = p.o[2] + p.vy
					p.fac = (time.t - p.start) / (p.death - p.start)
					-- remove
					if p.fac > 1 or p.o[2]-self.o[2] > self.h then 
						table.remove(self.particles, k)
					end
				end
			end,

			emittimmer = function(self)
				if self.next <= time.t then
					self:emit()
					self.next = self.next + 200
				end
			end,

			emit = function(self)
				local p = {}
				p.o = self.o + terrain.w * Vec.new(random()-0.5, 0, random()-0.5)
				p.vy = random()*(self.maxvy-self.minvy)+self.minvy
				p.fac = 0
				p.start = time.t
				p.death = time.t+random()*(self.maxlife-self.minlife)+self.minlife
				table.insert(self.particles, p)
			end,

			isonscreen = function(self)
				local x, y = self.o:proj(camera)
				return y+self.h < HEIGHT and y > 0
			end,

		}

	},

	update = function(self)
		for i,ps in ipairs(self.objs) do
			ps:update()
		end
	end,

}

lantern = {

	o = terrain.w*Vec.new(2, 2, -2),

	drawtop = function(self)
		local x, y = self.o:proj(camera)
		-- circ(x, y-8, 8+(time.t//1000%2), 4)
		-- spr(385, x-7, y-16, 0, 1, 0, 0, 2, 2)
		circ(x, y-6, 8+(time.t//1000%2), 4)
		spr(387, x-7, y-16, 0, 1, 0, 0, 2, 2)
		spr(368, x-3, y-40-16, 0, 1, 0, 0, 1, 5)
		spr(368, x-3, y-80-16, 0, 1, 0, 0, 1, 5)
		spr(368, x-3, y-120-16, 0, 1, 0, 0, 1, 5)
		spr(368, x-3, y-160-16, 0, 1, 0, 0, 1, 5)
		-- spr(368, x-3, y-200-16, 0, 1, 0, 0, 1, 5)
	end,

	drawbot = function(self)
		local x, y = self.o:proj(camera)
		spr(368, x-3, y+terrain.n*terrain.h, 0, 1, 0, 0, 1, 5)
		spr(368, x-3, y+terrain.n*terrain.h+40, 0, 1, 0, 0, 1, 5)
		spr(368, x-3, y+terrain.n*terrain.h+80, 0, 1, 0, 0, 1, 5)
		spr(368, x-3, y+terrain.n*terrain.h+120, 0, 1, 0, 0, 1, 5)
		spr(368, x-3, y+terrain.n*terrain.h+160, 0, 1, 0, 0, 1, 5)
	end

}


function SCN(line)

	background:scn(line)

end

function reload()
	if field.ncoin <= 0 then
		terrain.data = maze.generate(terrain.n, 16)
		terrain:loadVoxels()
		terrain.vents = maze.getVents(terrain.data)
		field:load()
	end
end

function draw()
	local n = terrain.n 
	local m = terrain.m
	table.sort(terrain.order, terrain.sortOrder)
	for _,v in ipairs(terrain.order) do
		local j = v[2]
		local k = v[3]
		local data_id = terrain.data_id[j][k]
		for i = n,1,-1 do
			if terrain.voxels[i][j][k] ~= nil then
				-- check if voxel is on screen
				if terrain.voxels[i][j][k]:isonscreen() then
					terrain.voxels[i][j][k]:drawTex()
				end
				-- draw sprite
				if data_id ~= nil then
					if field.data[i][data_id] ~= nil then
						field.data[i][data_id]:draw()
					end
				end
			end
			if i == player.i and data_id == player.j then
				if field.data[i][data_id] ~= nil then
					local temp = {field.data[i][data_id], player}
					table.sort(temp, field.zsort)
					temp[1]:draw()
					temp[2]:draw()
				else  
					player:draw()
				end
			end
		end
		-- layer 0
		if player.i == 0 and data_id == player.j then player:draw() end
		if j == 3 and k == 3 then tree:draw() end
		if j == 2 and k == 4 then vending:draw() end
		if j == 5 and k == 1 then lantern:drawtop() end
	end
end

function drawHUD()
	for i = 1,field.ncoin do
		spr(325, (i-1)*10+2, 2, 0)
	end
	for i = 1,player.ncoin do 
		spr(324, (i-1)*10+2, 2, 0)
	end 
end


background:load()
camera:load()
terrain:load()
player:load()
tree:load()
vending:load()
field:load()
-- boid:load()

music(1)


function TIC()

	debugger:update()
	time:update()
	player:update()
	camera:update()
	vending:update()
	-- boid:update()
	Pslib:update()
	
	cls(15)
	background:draw()
	-- boid:drawback()
	lantern:drawbot()
	draw()
	-- boid:drawfront()
	-- player:drawHUD()
		drawHUD()

	-- debug
	time:draw()
	camera:drawTicks()
	debugger:draw()

end


-- <TILES>
-- 002:3333333333333333333333333333333333333333333333333333333333333333
-- 003:3333333333333333333333333333333333333333333333333333333333333333
-- 004:5555555555555555555555555555555555555555555555555555555555555555
-- 005:5555555555555555555555555555555555555555555555555555555555555555
-- 006:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 007:ddddddddddddddddddddddddddeeddddddeedddddddddddddddddddddddddddd
-- 018:3333333333333333333333333333333333333333333333333333333333333333
-- 019:3333333333333333333223333322223333322333333333333333333333333333
-- 020:5555555555555555555555555555555555555555555555555555555555555555
-- 021:5555555555555555555555555555555555555555555555555555555555555555
-- 022:dddddddddddddddddddddddddddeeddddddeeddddddddddddddddddddddddddd
-- 023:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 034:3333333355335533555555555555555566666666666666665555555555555555
-- 035:3333333333353333335555555355555566666666666666665555555555555555
-- 036:5555555555555555555555555555555566666666666666665555555555555555
-- 037:5555555555555555555555555555555566666666666666665555555555555555
-- 038:ddddddddddddddddddddddddddddddddeeeeeeeeeeeeeeeedddddddddddddddd
-- 039:ddddddddddddddddddddddddddddddddeeeeeeeeeeeeeeeedddddddddddddddd
-- 040:dddddddddddddd99dddd9999ddd9999ddd999ddddd99ddddd999ddddd99ddddd
-- 041:dddddddd99dddddd9999ddddd9999dddd99999ddd99d99ddd99d999ddd9dd99d
-- 048:2222222222266666266666662666666626666666226666662226666622222222
-- 049:2222000022220000622200006662000066620000662200002222000022220000
-- 050:6666666666666666666666666666666655555555555555555555555555555555
-- 051:6666666666666666666666666666666655555555555555555555555555555555
-- 052:6666666666666666666666666666666655555555555555555555555555555555
-- 053:6666666666666666666666666666666655555555555555555555555555555555
-- 054:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddddd
-- 055:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddddd
-- 056:d99dddddd999dddddd99dddddd999dddddd9999dddd99999ddd99d99dddd9ddd
-- 057:dd9dd99ddddd999ddddd99ddddd999ddd9999ddd9999dddd99dddddddddddddd
-- 064:2336666623666666288888882888888838888888688888886888888868888888
-- 065:2322222262322236883322668823236688822666888226668886266688866666
-- 066:6232226666222666663266666662666666666666666666666666666655666666
-- 067:6666222266666322666665226666665266666663666666656666666566666666
-- 068:2336666623666666244444442444444434444444644444446444444464444444
-- 069:2322000062320000443300004423000044420000444200004446000044460000
-- 080:6666666666666666666555662665556626655566236666662326666622326666
-- 081:6666666666666666666666666666666666663666666326666662226666222226
-- 082:5566665566666655556666555553665555526655553226665522236662222232
-- 083:5566666655666666556666665566666355666632666626226266222222222222
-- 084:6666666666666666666555662665556626655566236666662326666622326666
-- 085:6666000066660000666600006666000066660000666300006662000066220000
-- </TILES>

-- <SPRITES>
-- 000:0000000000000044000004220000422200042222000422220004222100042226
-- 001:0000000044000000224000002224000022224000252240005512400055640000
-- 002:0000000000000044000004220000422200042222000422220004222100042226
-- 003:0000000044000000224000002224000022224000252240005512400055640000
-- 004:0044440004555740456665744566657445666574456665740455574000444400
-- 005:000000000000000000c000000000000000000000000000000000000000000000
-- 006:0000000000c000000ccc000000c0000000000000000000000000000000000000
-- 007:00000000000000000cccccd000cccd00000cd000000000000000000000000000
-- 008:0000000000000000000000000cccccd000cccd00000cd0000000000000000000
-- 016:0004222200042228000422280004222500042228000042250000044500000004
-- 017:5224000088240000882400005524000088240000254000004500000004000000
-- 018:0004222200042228000422280004222500042228000042250000044500000004
-- 019:5224000088240000882400005524000088240000254000004500000004000000
-- 020:0000000000044000004774000427774004777240477777740445544000455400
-- 021:0000000000000000000000000400040041404240041424140411214004112140
-- 032:0000000000000000000000000000000000000000000000000000010000000011
-- 033:0000000000000000000000010100000100100001001000010010001100011011
-- 034:0000000000000000000000000000000010001000100010002001001120011100
-- 035:0000000000000000000000000000000000010000101000000010000001000000
-- 036:0000000000000060000006000006666600006666006666600006660000060606
-- 037:0000000000000000000006006066600066666000076666000677600066667000
-- 048:0000000000000000000000000000001100000000000000000000000000000000
-- 049:1110101100011111000011111000011101111111000001110000001100000011
-- 050:2110000021000000220000012200011022011000220100002210000022100000
-- 051:1100110010110000100000000000000000000000000000000000000000000000
-- 052:0007070000077776007777770000770700077706000007000000000000000000
-- 053:6666600066660000766660000706000000006000000000000000000000000000
-- 065:0000001100000011000000110000001100000011000000110000011100000111
-- 066:2200000022000000220000001220000012200000122000001220000012200000
-- 068:0044440004555740456665744566657445666574456665740455574000444400
-- 069:0044440004000040400000044000000440000004400000040400004000444400
-- 081:0000011100001111000111110001111100001111000000110000000000000000
-- 082:1222000012220000112220001122200011120000110000000000000000000000
-- 084:0004400000477400047777400455664004556640045566400045640000044000
-- 085:0004400000400400040000400400004004000040040000400040040000044000
-- 096:000000000000000000000000000ba000000aa000000000000000000000000000
-- 097:0000000000000000000bb00000bc0a0000b00a00000aa0000000000000000000
-- 098:00000000000bb00000bc0b000bc000a00b0000a000b00a00000aa00000000000
-- 099:b00000000b00000000b00b000000b00000b000000b00b00000000b0000000000
-- 112:04eee40004eee40004e4e40004eee400004e400004eee40004eee40004e4e400
-- 113:000004e4000004ee0000004e000004ee000004ee000004e4000004ee0000004e
-- 114:e4000000e400000040000000e4000000e4000000e4000000e400000040000000
-- 115:000004e4000004ee0000004e000004ee000004ee000004e4000004ee0000004e
-- 116:e4000000e400000040000000e4000000e4000000e4000000e400000040000000
-- 128:04eee400004e400004eee40004eee40004e4e40004eee400004e400004eee400
-- 129:000004ee000046ee000467e70046777e00466777004605550046000000460000
-- 130:e4000000e6400000e76400007775400077554000550540000005400000054000
-- 131:000004ee000004ee000004ee0000047e00000477000004500000045000004500
-- 132:e4000000e4000000e40000007400000074000000540000005400000005400000
-- 144:04eee40004e4e40004eee400004e400004eee40004eee40004e4e40004eee400
-- 145:0046000000460000004600000046000000460000000460000000466600000444
-- 146:0005400000054000000540000005400000054000005400006640000044000000
-- 147:0004500000450000004500000045000000045000000045000000045500000044
-- 148:00540000d00540000d0540000005400000540000054000005400000040000000
-- 160:004e400004eee40004eee40004e4e40004eee400004e400004eee40004eee400
-- 176:04e4e40004eee400004e400004eee40004eee40004e4e40004eee400004e4000
-- </SPRITES>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:1309439883e893088308630883089308a308b308b308c308c308c308d308d308d308d308d308e308f308e308e308e308e308e309e309e309f308f308254000000000
-- 001:01f001f001f001e111d421c721a03180114011410132112401260117010001001100110111121120013411600177119021a021c001e001f001f001f0200000000000
-- 002:020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200307000000000
-- 003:02000200020002002200020042006200020082000200a200b2008200f200f200f200d200c200b200e200e200e200f200c200c200e200e200e200e200000000000000
-- </SFX>

-- <PATTERNS>
-- 000:f00038000030000030100030100030100020800038000020000020100020100020100020b0003c000000100030100030b9913c000030100030100030b0003c000030100030100030b9913c000030100030100030b5013c000000100030100030f00038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 001:400036000030000030000000000000000000000000000030f00036000000000000000000000000000000000000000000100030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 002:100010000000800036000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 003:400006000000000000000000000000000000000000000000f0000e000000000000000000000000000000800004000000f0000e10000010000010000010000010000070000e100000100000100000700008100000f0000e100000700004100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 004:400036000030000030000000f00012000030000030000030400012100010100030100030100030000000f00038000000f0003a100010000030000000100030000000000000000000f00016000000000000000000f00036000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 005:100010000000800036000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100030100030000000100030000000800038000000a00038000000b00038000000d00038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 006:f0003800000040003a000000d00038000000000000000000100030000000400038000000f0003800000040003a000000d00038000000000000100030100030000000100030000000100030000000100030000000100030000000100030000000a00038000000800038000000f00038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 007:900036000030000030100030400038100020900038000020000020100020100020100020b0003c000000100030100030b9913c000030100030100030100030000030100030100030b9913c000030100030100030b5013c000000100030100030f00038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 008:800036000030f00036100030100030100020100030000020000020100020100020100020100030000000100030100030199130000030100030100030100030000030100030100030199130000030100030100030150130000000100030100030f00038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 009:f0003800000040003a000000d00038000000000000000000100030000000400038000000f0003800000040003a000000d00038000000000000100030800038000000100030000000800038000000100030000000600038000000100030000000a00038000000800038000000f00038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 010:a00038000000b00038000000800038000000000000000000100030000000f20138000000f4013a000000f20138000000f9913a000000f70138000030f9913a000000f50138000000f7013a000000f50138000000f5013a000000f20138000000a00038000000800038000000f00038000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </PATTERNS>

-- <TRACKS>
-- 001:180301141601800701800a01900b01900b01000000000000000000000000000000000000000000000000000000000000ec0200
-- </TRACKS>

-- <PALETTE>
-- 000:21193c15788c0b99a500b9beffeeccffb0a3ff8d8bff6973386c862844793c95c665c2deeeeef294b0c265819d000000
-- </PALETTE>

