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

	draw = function(self)
		for i = 1,#self.task do
			print(self.task[i], 0, 7*(i-1), 1)
		end
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
				return x+W2, -y+H2 - camera.h 
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
				Face.quadb(x1, y1, x2, y2, x3, y3, x4, y4, 1)
			end,

			drawTex = function(self)
				local x1, y1 = self.verts[1]:proj(camera)
				local x2, y2 = self.verts[2]:proj(camera)
				local x3, y3 = self.verts[3]:proj(camera)
				local x4, y4 = self.verts[4]:proj(camera)
				-- trib(x1, y1, x2, y2, x3, y3, 12)
				-- trib(x2, y2, x3, y3, x4, y4, 12)
				if Face.isccw(x1, y1, x2, y2, x3, y3) then return end
				local u1, v1, u2, v2 = table.unpack(self.uvs)
				tri(x1, y1, x2, y2, x3, y3, self.col)
				tri(x2, y2, x3, y3, x4, y4, self.col)
				textri(x1, y1, x2, y2, x3, y3, u1, v1, u2-1, v1, u1, v2-1, true, 0)
				textri(x2, y2, x3, y3, x4, y4, u2-1, v1, u1, v2-1, u2-1, v2-1, true, 0)
				-- Face.quadb(x1, y1, x2, y2, x3, y3, x4, y4, 0)
			end,

			drawOutline = function(self)
				local x1, y1 = self.verts[1]:proj(camera)
				local x2, y2 = self.verts[2]:proj(camera)
				local x3, y3 = self.verts[3]:proj(camera)
				local x4, y4 = self.verts[4]:proj(camera)
				if Face.isccw(x1, y1, x2, y2, x3, y3) then return end
				Face.quadb(x1, y1, x2, y2, x3, y3, x4, y4, self.col)
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

	quadb = function(x1, y1, x2, y2, x3, y3, x4, y4, col)
		line(x1, y1, x2, y2, col)
		line(x2, y2, x4, y4, col)
		line(x4, y4, x3, y3, col)
		line(x3, y3, x1, y1, col)
	end,

	zsort = function(u, v)
		return Vec.dot(camera.o, u) < Vec.dot(camera.o, u)
	end

}

camera = {

	o = Vec.new(0, 0, 1),
	x = Vec.new(1, 0, 0),
	y = Vec.new(0, 1, 0),
	-- target_ax = -PI/6,
	target_ax = -0.52,
	target_ay = PI/4,
	lerp_fac = 0.12,
	pos = 0,
	ax = 0,
	ay = 0,
	target_h = 0,
	h = 0,

	load = function(self)

	end,

	update = function(self)
		-- if key(23) then self.ax = self.ax + 0.03 end
		-- if key(19) then self.ax = self.ax - 0.03 end
		-- if key(1) then self.ay = self.ay + 0.03 end
		-- if key(4) then self.ay = self.ay - 0.03 end
		self.ax = mathFun.lerp(self.lerp_fac, self.ax, self.target_ax)
		self.ay = mathFun.lerp(self.lerp_fac, self.ay, self.target_ay)
		self.o = Vec.new(0, 0, 1):rotate(self.ax, self.ay, 0)
		self.x = Vec.new(1, 0, 0):rotate(self.ax, self.ay, 0)
		self.y = Vec.new(0, 1, 0):rotate(self.ax, self.ay, 0)
		-- vertical motion
		if key(23) then self.target_h = self.target_h + 1 end
		if key(19) then self.target_h = self.target_h - 1 end
		self.h = mathFun.lerp(self.lerp_fac, self.h, self.target_h)
	end,

	rotateRightAngle = function(self, n)
		self.target_ay = self.target_ay + n*PI/2
		self.pos = self.pos + n
	end,

}

Voxel = {

	mesh_id = {
		{1,1}, {1,2}, {1,3}, {1,4},
		{1,5}, {2,5}, {3,5}, {4,5},
		{5,5}, {5,4}, {5,3}, {5,2},
		{5,1}, {4,1}, {3,1}, {2,1}
	},
	color_id = {[0]=0, [1]=3, [2]=5, [3]=13},

	new = function(i, j, type)
		if type == nil then type = 0 end
		local col = Voxel.color_id[type]
		local faces = {}
		if type ~= 0 then 
			local a, b = table.unpack(Voxel.mesh_id[j])
			local y = Vec.new(0, -(i-1) * terrain.w, 0)
			local u1 = terrain.mesh[a][b] + y
			local u2 = terrain.mesh[a+1][b] + y
			local u3 = terrain.mesh[a][b+1] + y
			local u4 = terrain.mesh[a+1][b+1] + y
			if i > 1 and terrain.data[i-1][j] == 0 then
				local f = Face.new({u1, u2, u3, u4}, {}, col)
				table.insert(faces, f)
			-- elseif i == 1 then
			-- 	local f = Face.new({u1, u2, u3, u4}, {}, col)
			-- 	table.insert(faces, f)
			end
			y = Vec.new(0, -terrain.w, 0)
			local v1 = u1 + y
			local v2 = u2 + y
			local v3 = u3 + y
			local v4 = u4 + y
			local f1 = Face.new({u1, u3, v1, v3}, {}, col)
			local f2 = Face.new({u3, u4, v3, v4}, {}, col)
			local f3 = Face.new({u4, u2, v4, v2}, {}, col)
			local f4 = Face.new({u2, u1, v2, v1}, {}, col)
			table.insert(faces, f1)
			table.insert(faces, f2)
			table.insert(faces, f3)
			table.insert(faces, f4)
		end
		local v = {i=i, j=j, type=type, faces=faces}
		setmetatable(v, Voxel.mt)
		return v
	end,

	mt = {

		__index = {

			draw = function(self)
				-- check if layer on frame
				for k = 1,#self.faces do
					self.faces[k]:drawCol()
				end
			end,

		}
	}

}

terrain = {

	n = 10, 	-- height
	m = 5,		-- depth
	w = 12*sqrt2,		-- voxel width
	h = 8.5,

	mesh = {},
	faces = {},
	data = {{}},
	voxels = {{}},
	order = {},
	tile_id = {[0]=0, [1]=2, [2]=4, [3]=6},

	l = 4*20,
	tick_x = 190,
	tick_y = H2-3,

	load = function(self)
		self:loadMesh()
		-- self:loadTiles()
		self:loadFaces()
		self:loadData()
		self:loadMap()
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

	loadMap = function(self)
		local n, m = self.data:size()
		for i = 1,n do
			for j = 1,m do
				local id = self.tile_id[self.data[i][j]]
				self:mset(i, j, id+32)
			end
		end
	end,

	-- loadTiles = function(self)
	-- 	self.tiles = {}
	-- 	for i = 1,self.m do
	-- 		for j = 1,self.m do
	-- 			local u1 = self.mesh[i][j]
	-- 			local u2 = self.mesh[i+1][j]
	-- 			local u3 = self.mesh[i][j+1]
	-- 			local u4 = self.mesh[i+1][j+1]
	-- 			local f = Face.new({u1, u2, u3, u4}, {}, 1)
	-- 			table.insert(self.tiles, f)
	-- 		end
	-- 	end
	-- end,

	-- drawTiles = function(self)
	-- 	for i,f in ipairs(self.tiles) do
	-- 		f:drawCol()
	-- 	end
	-- end,

	loadFaces = function(self)
		self.faces = {}
		local n = self.n 
		local m = self.m
		-- top
		local u1 = self.mesh[1][1]
		local u2 = self.mesh[m+1][1]
		local u3 = self.mesh[1][m+1]
		local u4 = self.mesh[m+1][m+1]
		local a = 16*m
		local f0 = Face.new({u1, u2, u3, u4}, {0, 0, a, a}, 3)
		table.insert(self.faces, f0)
		-- side
		local y = -n * terrain.w
		v1 = u1 + Vec.new(0, y, 0)
		v2 = u2 + Vec.new(0, y, 0)
		v3 = u3 + Vec.new(0, y, 0)
		v4 = u4 + Vec.new(0, y, 0)
		local x = 16*m
		local y = 16*n
		local f1 = Face.new({u1, u3, v1, v3}, {x, 0, 2*x, y}, 5)
		local f2 = Face.new({u3, u4, v3, v4}, {2*x-16, 0, 3*x-16, y}, 5)
		local f3 = Face.new({u4, u2, v4, v2}, {3*x-32, 0, 4*x-32, y}, 5)
		local f4 = Face.new({u2, u1, v2, v1}, {4*x-48, 0, 5*x-48, y}, 5)
		table.insert(self.faces, f1)
		table.insert(self.faces, f2)
		table.insert(self.faces, f3)
		table.insert(self.faces, f4)
	end,

	drawFaces = function(self)
		for i = 2,#self.faces do
			self.faces[i]:drawTex()
			-- self.faces[i]:drawCol()
		end
		-- top
		self.faces[1]:drawCol()
		self.faces[1]:drawTex()

		self:drawTicks()
	end,

	-- draw = function(self)
	-- 	self.faces[1]:drawCol()
	-- 	self.faces[1]:drawTex()
	-- end,

	mset = function(self, j, i, id)
		local x0 = 2*self.m
		local i = 2*(i-1)
		local j = 2*(j-1)
		mset(x0+i, j, id)
		mset(x0+i+1, j, id+1)
		mset(x0+i, j+1, id+16)
		mset(x0+i+1, j+1, id+17)
		if i == 0 then
			i = 8*(self.m-1)
			mset(x0+i, j, id)
			mset(x0+i+1, j, id+1)
			mset(x0+i, j+1, id+16)
			mset(x0+i+1, j+1, id+17)
		end
	end,

	loadVoxels = function(self)
		local voxels = {}
		local n, m = self.data:size()
		for i = 1,n do
			voxels[i] = {}
			for j = 1,m do
				voxels[i][j] = Voxel.new(i, j, self.data[i][j])
			end
		end
		self.voxels = voxels
		-- create list for sorting
		local order = {}
		for j = 1,m do
			local a, b = table.unpack(Voxel.mesh_id[j])
			table.insert(order, {j, terrain.mesh[a][b]})
		end
		self.order = order
	end,

	drawVoxels = function(self)
		local n, m = self.data:size()
		table.sort(self.order, self.sortOrder)
		for i = n,1,-1 do
			for k,v in ipairs(self.order) do
				local j = v[1]
				self.voxels[i][j]:draw()
			end
		end
	end,

	drawTicks = function(self)
		for i = 0,self.n-1,5 do
			print(-i, self.tick_x, self.tick_y+i*self.w-camera.h, 1)
		end
	end,

	sortOrder = function(u, v)
		return Vec.dot(camera.o, u[2]) < Vec.dot(camera.o, v[2])
	end

}

Sprite = {

	obj = {},

	new = function(spr, u, w, h)
		local s = {spr=spr, u=u, w=w, h=h}
		setmetatable(s, Sprite.mt)
		table.insert(Sprite.obj, s)
		return s
	end,

	mt = {

		__index	= {

			draw = function(self)
				local x, y = self.u:proj(camera) 
				spr(self.spr, x-8*self.w/2, y-8*self.h, 0, 1, 0, 0, self.w, self.h)
			end

		}
	},

	draw = function(self)
		-- table.sort(self.obj, self.ysort)
		for i,v in ipairs(self.obj) do
			v:draw()
		end
	end,

	ysort = function(u, v)
		return u[2] < v[2]
	end,

}

player = {

	r = 0,
	dr = 0,
	o = Vec.new(0, 0, 0),
	speed = 1,
	is_left = false,
	target_y = 0,

	load = function(self)
		
	end,

	update = function(self)
		-- movement
		local d = 0
		if btn(2) then 
			d = d - self.speed
			self.is_left = true 
		end
		if btn(3) then 
			d = d + self.speed
			self.is_left = false 
		end
		self.r = self.r + d
		self.r = mathFun.mod(self.r, 4*terrain.l)
		self.dr = self.dr + d

		-- warp position
		local dz = mathFun.clamp(-abs(self.r-1.5*terrain.l)+1.5*terrain.l, 0, terrain.l)
		local dx = mathFun.clamp(-abs(self.r-2.5*terrain.l)+1.5*terrain.l, 0, terrain.l)
		self.o = Vec.new(terrain.l/2-dx, 0, terrain.l/2-dz)
		
		-- rotate camera
		if abs(self.dr) >= terrain.l then
			camera:rotateRightAngle(mathFun.sign(self.dr))
			self.dr = 0
		end

		-- falling
		self.tilenumber = mathFun.mod(floor(self.r/terrain.w + 0.5), 16) + 1

	end,

	draw = function(self)
		local x, y = self.o:proj(camera)
		if self.is_left then
			spr(256, x-8, y-16, 0, 1, 1, 0, 2, 2)
		else
			spr(256, x-8, y-16, 0, 1, 0, 0, 2, 2)
		end
	end,

	getFloor = function(self)
		local n, m = terrain.data:size()
		for i = 1,n do
			if terrain.data[i][self.tilenumber] == nil then 
				self.target_y = (i-1)*terrain.w
				break 
			end
		end
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

	scn = function(self, line)
		local k = line/HEIGHT
		Color.setcol(mathFun.lerp(k, self.c1, self.c2), 15)
	end,

	load = function(self)
		self.stars = {}
		for i = 1,15 do
			local x = random(0, WIDTH)
			local y = random(0, HEIGHT)
			table.insert(self.stars, {x, y})
		end
	end,

	draw = function(self)
		-- local t = time.t // 1000 % 2
		for i = 1,#self.stars do
			local x = mathFun.mod(self.stars[i][1] + camera.ay*100, WIDTH)
			pix(x, self.stars[i][2], 12)
		end
	end

}

tree = {

	leaves = {},

	load = function(self)
		self.leaves = {}
		-- math.randomseed(5)
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
		Sprite.new(288, Vec.new(0, -5, 0), 4, 4)
	end,

	draw = function(self)
		for i = 1,#self.leaves do
			local x, y = self.leaves[i]:proj(camera)
			spr(292, x-8, y-8, 0, 1, 0, 0, 2, 2)
		end
	end,

}


function SCN(line)

	background:scn(line)

end


background:load()
camera:load()
terrain:load()
tree:load()

function TIC()

	time:update()
	camera:update()
	player:update()

	cls(15)
	background:draw()
	
	terrain:drawVoxels()
	terrain.faces[1]:drawCol()
	terrain.faces[1]:drawTex()
	-- terrain:drawFaces()

	Sprite:draw()
	tree:draw()
	player:draw()

	-- debug
	time:draw()
	debugger:draw()

end


-- <TILES>
-- 002:0000000003333333033333330333333303333333033333330333333303333333
-- 003:0000000033333330333333303333333033333330333333303333333033333330
-- 004:5555555555555555555555555555555555555555555555555555555555555555
-- 005:5555555555555555555555555555555555555555555555555555555555555555
-- 006:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 007:ddddddddddddddddddddddddddeeddddddeedddddddddddddddddddddddddddd
-- 008:aaaaaaaaa9aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
-- 009:aaaaaaaaaaaaaa9aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
-- 010:5555555555555555555555555555555555555555555555555555555555555555
-- 011:5555555555555555555555555555555555555555555555555555555555555555
-- 012:5555555555555555555555555555555555555555555555555555555555555555
-- 013:5555555555555555555555555555555555555555555555555555555555555555
-- 018:0333333303333333033333330333333303333333033333330333333300000000
-- 019:3333333033333330333223303322223033322330333333303333333000000000
-- 020:5555555555555555555555555555555555555555555555555555555555555555
-- 021:5555555555555555555555555555555555555555555555555555555555555555
-- 022:dddddddddddddddddddddddddddeeddddddeeddddddddddddddddddddddddddd
-- 023:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
-- 024:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa9aaaaaaaaaaaaaa
-- 025:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa9aaaaaaaaa
-- 026:5555555555555555555555555555555555555555555555555555555555555555
-- 027:5555555555555555555555555555555555555555555555555555555555555555
-- 028:5555555555555555555555555555555555555555555555555555555555555555
-- 029:5555555555555555555555555555555555555555555555555555555555555555
-- 034:3333333355335533555555555555555566666666666666665555555555555555
-- 035:3333333333353333335555555355555566666666666666665555555555555555
-- 036:5555555555555555555555555555555566666666666666665555555555555555
-- 037:5555555555555555555555555555555566666666666666665555555555555555
-- 038:ddddddddddddddddddddddddddddddddeeeeeeeeeeeeeeeedddddddddddddddd
-- 039:ddddddddddddddddddddddddddddddddeeeeeeeeeeeeeeeedddddddddddddddd
-- 040:aaaaaaaaa9aaaaaaaaaaaaaaaaa44444aaa44444aaa44aaaaaa44aaaaaaaaaa4
-- 041:aaaaaaaaaaaaaa9aaaaaaaaa44444aaa44444aaaaaa44aaaaaa44aaa44444aaa
-- 042:5555555555555555555555555555555766667775666755555577555555777555
-- 043:5555555555555555555555557755555577776666777776665777755557777755
-- 044:55555555555555555555555c555555cb66666cbb6666cbbb555cbbbb55cbbbbb
-- 045:5555555555555555c5555555cc555555cac66666bcac6666bcaac555bbcaac55
-- 050:6666666666666666666666666666666655555555555555555555555555555555
-- 051:6666666666666666666666666666666655555555555555555555555555555555
-- 052:6666666666666666666666666666666655555555555555555555555555555555
-- 053:6666666666666666666666666666666655555555555555555555555555555555
-- 054:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddddd
-- 055:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddddd
-- 056:aaaaaaa4aaaaaaa4aaaaaaa4aaaaaaaaaaaaaaa4aaaaaaa4a9aaaaaaaaaaaaaa
-- 057:44444aaa4aaaaaaa4aaaaaaaaaaaaaaa4aaaaaaa4aaaaaaaaaaaaa9aaaaaaaaa
-- 058:6677777566777777667777776667777655555775555555555555555555555555
-- 059:7777576655755766557757667577576677777555577755555555555555555555
-- 060:66cccbbb666caccb6666caac66666caa555555ca5555555c5555555555555555
-- 061:bbcccc66bcc9c666cc9c6666c9c66666cc555555c55555555555555555555555
-- </TILES>

-- <SPRITES>
-- 000:0000000000000044000004220000422200042222000422220004222100042226
-- 001:0000000044000000224000002224000022224000252240005512400055640000
-- 002:0000000000000007000000970000099700009999000099990000999500009995
-- 003:0000000000070000797700007777000099990000999900005559000055600000
-- 016:000422220004222f0004222f000422250004222f000042250000044500000004
-- 017:52240000ff240000ff24000055240000ff240000254000004500000004000000
-- 018:0000999900009998000099980000999500009998000099950000000500000000
-- 019:5990000088900000889000005590000088900000959000000500000000000000
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
-- 081:0000011100001111000011110001111100001111000000110000000000000000
-- 082:1222000012220000112220001122200011120000110000000000000000000000
-- </SPRITES>

-- <MAP>
-- 000:202020202020202020202232223222322232223222322232223222322232223222322232223222320000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 001:202020202020202020202333233323332333233323332333233323332333233323332333233323330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 002:2020312020312020202062720000a2b2627262726272a2b2627262726272a2b26272a2b2627262720000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 003:2020202020202020312063730000a3b3637363736373a3b3637363736373a3b36373a3b3637363730000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 004:202020204040202020206272627262726272a2b2627262726272a2b2627262726272627262726272c2d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 005:202031204040202020206373637363736373a3b3637363736373a3b3637363736373637363736373c3d3000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 006:20202020202020202020c2d20000c2d20000c2d20000c2d20000c2d20000c2d20000c2d20000c2d2c2d2000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 007:20312020203120202020c3d30000c3d30000c3d30000c3d30000c3d30000c3d30000c3d30000c3d3c3d3000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 008:202020202020203120200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- 009:202020202020202020200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </MAP>

-- <PALETTE>
-- 000:21193c15788c0b99a500b9beffeeccffb0a3ff8d8bff697329366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

