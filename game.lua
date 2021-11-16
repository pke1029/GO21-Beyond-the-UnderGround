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
	end

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
					textri(x1, y1, x2, y2, x3, y3, u1, v1, u2-1, v1, u1, v2-1, false, 0)
					textri(x2, y2, x3, y3, x4, y4, u2-1, v1, u1, v2-1, u2-1, v2-1, false, 0)
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
	lerp_fac = 0.12,
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
		self.target_y2 = -player.y*cos(self.ax)
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
				if self:isonscreen() then 
					if self.f1 ~= nil then self.f1:drawTex() end
					if self.f2 ~= nil then self.f2:drawTex() end
					if self.f3 ~= nil then self.f3:drawTex() end
					if self.f4 ~= nil then self.f4:drawTex() end
					if self.f5 ~= nil then self.f5:drawTex() end
				end
			end,

			isonscreen = function(self)
				local x, y = self.o:proj(camera)
				return y+self.r > 0 and y-self.r < HEIGHT
			end,

		}
	}

}

terrain = {

	n = 7, 	-- height
	m = 5,		-- depth
	w = 12*sqrt2,		-- voxel width (12)
	h = 28/sqrt(3),		-- voxel height (14)

	mesh = {},
	faces = {},
	data = {{}},
	voxels = {{{}}},
	order = {},

	mesh_id = {
		{1,1}, {1,2}, {1,3}, {1,4},
		{1,5}, {2,5}, {3,5}, {4,5},
		{5,5}, {5,4}, {5,3}, {5,2},
		{5,1}, {4,1}, {3,1}, {2,1}
	},
	color_id = {[0]=0, [1]=3, [2]=5, [3]=13},
	tile_id = {[0]=0, [1]=2, [2]=4, [3]=6},
	data_id = {
		{1, 2, 3, 4, 5},
		{16, nil, nil, nil, 6},
		{15, nil, nil, nil, 7},
		{14, nil, nil, nil, 8},
		{13, 12, 11, 10, 9},
	},

	load = function(self)
		self:loadMesh()
		-- math.randomseed(28)
		-- self:loadData()
		self.data = maze.generate(self.n, 16)
		self:loadVoxels()
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

	loadData = function(self)
		local n = self.n 
		local m = 4*self.m-4
		-- generate
		local data = {{}}
		-- for j = 1,m do
		-- 	data[1][j] = self.id[2] -- layer 0
		-- end
		for i = 1,n do
			data[i] = {}
			for j = 1,m do
				data[i][j] = mathFun.randlist({0, 2, 2, 3})
			end
		end
		setmetatable(data, Mat.mt)
		self.data = data
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
		-- generate faces
		local faces = {}
		local data_id = self.data_id[j][k]
		if data_id ~= nil then
			local type = self.data[i][data_id]
			if type ~= 0 then
				local col = self.color_id[type]
				local tile = self.tile_id[type]
				local uvs = {tile*8, 16, (tile+2)*8, 32}
				local f1 = Face.new({u1, u3, v1, v3}, uvs, col)
				local f2 = Face.new({u3, u4, v3, v4}, uvs, col)
				local f3 = Face.new({u4, u2, v4, v2}, uvs, col)
				local f4 = Face.new({u2, u1, v2, v1}, uvs, col)
				if mathFun.isin(data_id, {2, 3, 4}) then f3 = nil
				elseif mathFun.isin(data_id, {6, 7, 8}) then f4 = nil
				elseif mathFun.isin(data_id, {10, 11, 12}) then f1 = nil
				elseif mathFun.isin(data_id, {14, 15, 16}) then f2 = nil
				end
				-- top face
				local f5
				if i > 1 and self.data[i-1][data_id] == 0 then
					uvs = {tile*8, 0, (tile+2)*8, 16}
					f5 = Face.new({u1, u2, u3, u4}, uvs, col)
				elseif i == 1 then
					tile = 2
					uvs = {tile*8, 0, (tile+2)*8, 16}
					f5 = Face.new({u1, u2, u3, u4}, uvs, 3)
				end
				return Voxel.new(f1, f2, f3, f4, f5, o)
			else
				-- empty block
				return Voxel.new(nil, nil, nil, nil, nil, o) 
			end
		elseif j == 3 and k == 3 then 
			-- center column
			return nil
		else
			local type = mathFun.randlist({2,3})
			local col = self.color_id[type]
			local tile = self.tile_id[type]
			local uvs = {tile*8, 16, (tile+2)*8, 32}
			local f1, f2, f3, f4, f5
			if j == 2 then f1 = Face.new({u1, u3, v1, v3}, uvs, col) end
			if j == 4 then f3 = Face.new({u4, u2, v4, v2}, uvs, col) end
			if k == 2 then f4 = Face.new({u2, u1, v2, v1}, uvs, col) end
			if k == 4 then f2 = Face.new({u3, u4, v3, v4}, uvs, col) end
			tile = 2
			uvs = {tile*8, 0, (tile+2)*8, 16}
			if i == 1 then f5 = Face.new({u1, u2, u3, u4}, uvs, 3) end
			return Voxel.new(f1, f2, f3, f4, f5, o)
		end
	end,

	sortOrder = function(u, v)
		return Vec.dot(camera.o, u[1]) < Vec.dot(camera.o, v[1])
	end,

	reload = function(self)
		self.data = maze.generate(self.n, 16)
		self:loadVoxels()
	end

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

	jump_height = 18,
	jump_dist = terrain.w,
	y = 0,
	speed_y = 0,
	gravity = -500,
	jump_vel = 0,
	h = 14,

	ncoin = 10,

	load = function(self)
		self.r = 2 * self.range
		self.o = Vec.new(self.range/2, 0, self.range/2)
		self.j = 2 * terrain.m - 1
		self.jump_vel = 4*self.jump_height*self.speed_x*60 / self.jump_dist
		self.gravity = -2*self.speed_x*60*self.jump_vel / self.jump_dist
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
		
		-- jump
		coll = self:getCollision()
		if keyp(48) or btnp(5) then 
			self.speed_y = self.jump_vel
		end
		self.speed_y = mathFun.clamp(self.speed_y + self.gravity*time.dt/1000, -1.2*self.jump_vel, 1.2*self.jump_vel) 
		self.y = self.y + self.speed_y*time.dt/1000
		if coll[2] then
			self.y = max(-self.i * terrain.h, self.y)
		end
		if coll[1] then
			self.speed_y = min(self.speed_y, 0)
			self.y = min(-(self.i-1) * terrain.h, self.y)
		end
		if self.y < -(terrain.n+10)*terrain.h then
			local dy = camera.y2 - camera.target_y2 
			self.y = 10*terrain.h 
			camera.target_y2 = -self.y*cos(camera.ax)
			camera.y2 = camera.target_y2 + dy
			terrain:reload()
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
		local f1 = Face.new({u1, u3, v1, v3}, {12, 32, 20, 49}, 2)
		local f3 = Face.new({u4, u2, v4, v2}, {12, 32, 20, 49}, 2)
		local f4 = Face.new({u2, u1, v2, v1}, {19, 32, 32, 49}, 2)
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
	end,

}

loot = {

	data = {{}},

	load = function(self)
		local n, m = terrain.data:size()
		local data = {}
		for i = 1,n do
			data[i] = {}
		end
		for i = 2,n-1 do
			for j = 1,m do
				if terrain.data[i][j] == 0 and terrain.data[i+1][j] ~= 0 then
					local a, b = table.unpack(terrain.mesh_id[j])
					local o = terrain.mesh[a][b] + Vec.new(terrain.w/2, -i*terrain.h, terrain.w/2)
					data[i][j] = mathFun.randlist({nil, Sprite.new(260, o, 1, 1)})
				end
			end
		end
		self.data = data
	end,

	zsort = function(u, v)
		return Vec.dot(camera.o, u.o) < Vec.dot(camera.o, v.o)
	end

}

maze = {

	generate = function(n, m)
		local A = {}
		for i = 1,n do
			A[i] = {}
			for j = 1,m do
				A[i][j] = mathFun.randlist({2, 2, 2, 3})
			end
		end
		setmetatable(A, Mat.mt)
		-- Randomized depth-first search
		local stack = {{2*random(1, (n-1)/2), 2*random(1, m/2) - 1}}
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
			end
		end
		-- start and end
		A[1][13] = 0
		A[n][5] = 0 
		return A
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
		local temp = {}
		for i,v in ipairs(t) do
			if v[3] < 3 then 
				table.insert(temp, v)
				table.insert(temp, v)
				table.insert(temp, v)
			else
				table.insert(temp, v)
			end
		end
		return mathFun.randlist(temp)
	end,

	beautify = function(A)
		local n, m = A:size()
		for i = 1,n do
			for j = 1,m do

			end
		end
	end 

}


function SCN(line)

	background:scn(line)

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
					terrain.voxels[i][j][k]:draw()
					-- draw sprite
					if data_id ~= nil then
						if loot.data[i][data_id] ~= nil then
							loot.data[i][data_id]:draw()
						end
					end
				end
			end
			if i == player.i and data_id == player.j then
				if loot.data[i][data_id] ~= nil then
					local temp = {loot.data[i][data_id], player}
					table.sort(temp, loot.zsort)
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
	end
end


background:load()
camera:load()
terrain:load()
player:load()
tree:load()
vending:load()
loot:load()


function TIC()

	debugger:update()
	time:update()
	player:update()
	camera:update()
	vending:update()
	
	cls(15)
	background:draw()
	camera:drawTicks()
	draw()
	player:drawHUD()

	-- debug
	time:draw()
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
-- 048:2222222222266666266666662666666626666666226666662226666622222222
-- 049:2222000022220000622200006662000066620000662200002222000022220000
-- 050:6666666666666666666666666666666655555555555555555555555555555555
-- 051:6666666666666666666666666666666655555555555555555555555555555555
-- 052:6666666666666666666666666666666655555555555555555555555555555555
-- 053:6666666666666666666666666666666655555555555555555555555555555555
-- 054:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddddd
-- 055:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddddd
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
-- 016:0004222200042229000422290004222500042229000042250000044500000004
-- 017:5224000099240000992400005524000099240000254000004500000004000000
-- 018:0004222200042229000422290004222500042229000042250000044500000004
-- 019:5224000099240000992400005524000099240000254000004500000004000000
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
-- 081:0000011100001111000111110001111100001111000000110000000000000000
-- 082:1222000012220000112220001122200011120000110000000000000000000000
-- </SPRITES>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000300000000000
-- 001:010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100300000000000
-- 002:020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200020002000200300000000000
-- </SFX>

-- <PALETTE>
-- 000:21193c15788c0b99a500b9beffeeccffb0a3ff8d8bff6973386c86285979bc8de2ffc2dedadeee94b0c265819d000000
-- </PALETTE>

