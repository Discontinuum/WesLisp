-- returns value, is defined (to tell if a variable is defined with nil value)
function find_in_env(env, symbname) 
	if symbname == "false" then
		return false, true
	end
	if symbname == "nil" then
		return nil, true
	end
	if not env then
		return nil, false
	end
	if env.values[symbname] then
		return env.values[symbname], true
	end
	if env.defined[symbname] then
		return nil, true
	end
	if env.parent then
		return find_in_env(env.parent, symbname)
	end
	return nil, false
end

function new_env(parent)
	return {values={}, defined={}, parent=parent}
end

--WARN: can override!
function add_to_env(env,name,val) 
	env.defined[name] = true
	env.values[name] = val
end
--FUNCTION OBJECT
--type = TFUN
--lua = lua-function if it's lua implemented
--sexpr = body to evaluate if weslisp function
--args = array with names of args
--variadic = name of variadic param or nil if not variadic
--env = environment where function was declared

-- f must accept an env (params will be bound in it)
-- f returns result (s-expression), error
function create_lua_function(f, env, args, variadic)
	local fn = {type = TFUN}
	fn.lua = f
	fn.args = args
	fn.variadic = variadic
	fn.env = env
	return fn
end

function create_lisp_function(sexpr, env, args, variadic)
	local fn = {type = TFUN}
	fn.sexpr = sexpr
	fn.args = args
	fn.variadic = variadic
	fn.env = env
	return fn
end

function apply_fun(f, args)
	local nenv = new_env(f.env)
	for i,v in ipairs(f.args) do
		add_to_env(nenv, v, args[i])
	end
	if f.variadic then
		local vrd = {type = TLIST}
		local diff = #args - #f.args
		while diff > 0 do
			table.insert(vrd, args[#args - diff + 1])
			diff = diff - 1
		end
		add_to_env(nenv, f.variadic, vrd)
	end
	if f.lua then
		return f.lua(nenv)
	else
		return eval_sexpr(f.sexpr, nenv)
	end
end

function evaluate_fn_spform(fullList, env)
	if #fullList < 2 then
		return nil, "insufficient amount of argument for fn special form"
	end
	local params = fullList[2]
	if not is_list(params) then
		return nil, "illegal argument list for fn form"
	end
	local args = {}
	local variadic
	for i,v in ipairs(params) do
		if not is_sym(v) then
			return nil, "not a symbol in arguments' list"
		end
		if v.name == "&" then
			if i == #params - 1 then
				-- TODO: is it checked to be a symbol?
				variadic = params[#params].name
				break
			else
				return nil, "& separator of variadic should be last-but-one in arguments' list"
			end
		else
			table.insert(args, v.name)
		end
	end
	local body = {type = TTOPLVL}
	for i,v in ipairs(fullList) do
		if i > 2 then
			table.insert(body, v)
		end
	end
	return create_lisp_function(body, env, args, variadic)
end

--TODO: finish
-- I decided to go for simplified syntax
-- (let (a val1 b val2 c val3))
function evaluate_let(fullList, env)
	if #fullList < 2 then
		return nil, "insufficient amount of argument for let/let* special form"
	end
	local bounds = fullList[2]
	if not is_list(bounds) then
		return nil, "not list as binding argument of "..fullList[1]
	end
	if #bounds % 2 ~= 0 then
		return nil, "uneven number of forms in binding argument of "..fullList[1]
	end
	
	local star = fullList[1] == "let*"
	local bound_vars = {}
	local bound_vars_names = {} -- in case of nil values
	local nenv = new_env(env)
	local function bind_var(var, val)
		if star then
			add_to_env(nenv, var, val)
		else
			bound_vars[var] = val
			table.insert(bound_vars_names, var)
		end
	end
	for i=1,#bounds,2 do
		local sym, ex = bounds[i], bounds[i+1]
		if not is_sym(sym) then -- don't evaluate, must be simply a symbol
			return nil, "not a symbol in binding argument of "..fullList[1]
		end
		local resex, errex = eval_sexpr(ex, nenv)
		if errex then
			return nil, errex .." when evaluating value of "..sym.name .. " in "..fullList[1]
		end
		bind_var(sym.name, resex)
	end
	if not star then -- we initialized all variables, now add them to the env. let* has already done it
		for i,name in ipairs(bound_vars_names) do
			add_to_env(nenv, name, bound_vars[name])
		end
	end
end

function evaluate_special_form(fs, fullList, env)
	if fs.name == "fn" then
		return evaluate_fn_spform(fullList, env)
	end
	if fs.name == "quote" then
		if #fullList ~= 2 then
			return nil, "wrong number of arguments for quote special form "..tostring(#fullList - 1) -- first element is "quote", others are arguments
		end
		return fullList[2]
	end
	if fs.name == "do" then
		local list = {type = TTOPLVL}
		for i, v in ipairs(fullList) do
			if i > 1 then
				table.insert(list, v)
			end
		end
		return eval_sexpr(list, env)
	end
	if fs.name == "if" then
		if #fullList < 3 then
			return nil, "too few arguments for if"
		end
		if #fullList > 4 then
			return nil, "too many arguments for if"
		end
		local test, err
		test, err = eval_sexpr(fullList[2], env)
		if err then
			return nil, err
		end
		if test then
			return eval_sexpr(fullList[3], env)
		end
		if #fullList == 4 then
			return eval_sexpr(fullList[4], env)
		end
		return nil, nil
	end
end


function eval_sexpr(sexpr, env)
	local t = type(sexpr)
	if t == "number" or t == "string" or t == "boolean" then
		return sexpr
	end
	if sexpr.type == TTOPLVL then
		local res, err
		for i,v in ipairs(sexpr) do
			res, err = eval_sexpr(v, env)
			if err then
				return nil, err
			end
		end
		return res
	end
	if sexpr.type == TSYM then
		local res, found = find_in_env(env, sexpr.name)
		if not found then
			return nil, "unbound variable "..sexpr.name
		else
			return res
		end
	end
	if sexpr.type == TLIST then
		if #sexpr == 0 then
			return sexpr
		end
		local fs, err = eval_sexpr(sexpr[1], env)
		if err then
			return nil, err
		end
		if type(fs) ~= "table" then
			return nil, type(fs) .. " can't be called as function"
		end
		if fs.type == TFUN then
			local actualArgs = #sexpr - 1
			if actualArgs < #fs.args then
				return nil, "insufficient amount of arguments"
			end
			if actualArgs > #fs.args and (not fs.variadic) then
				return nil, "superflous arguments"
			end
			local ind = 2
			local evaledArgs = {}
			while ind - 1 <= actualArgs do
				local a, err = eval_sexpr(sexpr[ind], env)
				if err then
					return nil, err
				end
				table.insert(evaledArgs, a)
				ind = ind + 1
			end
			local res, err = apply_fun(fs, evaledArgs)
			if err then 
				return nil, err
			end
			return res
		end
		if fs.type == TSPEC then
			local res, err = evaluate_special_form(fs, sexpr, env)
			if err then 
				return nil, err
			end
			return res
		end
		return nil, "illegal function call"
	end
end

ROOT_ENV = new_env()
add_to_env(ROOT_ENV, "type", create_lua_function(
function(env)
	local v = find_in_env(env, "val")
	if type(v) ~= "table" then
		return type(v)
	end
	return v.type
end, ROOT_ENV, {"val"}))

add_to_env(ROOT_ENV, "fn", {type = TSPEC, name = "fn"})
add_to_env(ROOT_ENV, "if", {type = TSPEC, name = "if"})
add_to_env(ROOT_ENV, "quote", {type = TSPEC, name = "quote"})
add_to_env(ROOT_ENV, "do", {type = TSPEC, name = "do"})
add_to_env(ROOT_ENV, "true", true)

add_to_env(ROOT_ENV, "+", create_lua_function(function(env)
	local s = 0
	local val = find_in_env(env, "val")
	for i,v in ipairs(val) do
		if type(v) ~= "number" then
			return nil, "tried to add non-number"
		end
		s = s + v
	end
	return s
end, ROOT_ENV, {}, "val"))
add_to_env(ROOT_ENV, "inc", create_lisp_function({{"+", type = TSYM}, 1, {"arg", type = TSYM}, type = TLIST}, ROOT_ENV, {"arg"}))
add_to_env(ROOT_ENV, "-", create_lua_function(function(env)
	local s = find_in_env(env, "hd")
	if type(s) ~= "number" then
		return nil, "tried to subtract from non-number"
	end
	local tail = find_in_env(env, "tl")
	if #tail == 0 then
		return -s
	end
	
	for i,v in ipairs(tail) do
		if type(v) ~= "number" then
			return nil, "tried to add non-number"
		end
		s = s - v
	end
	return s
end, ROOT_ENV, {"hd"}, "tl"))


add_to_env(ROOT_ENV, "list", create_lua_function(function(env)
	local r = {type = TLIST}
	local val = find_in_env(env, "val")
	for i,v in ipairs(val) do
		table.insert(r, v)
	end
	return r
end, ROOT_ENV, {}, "val"))
add_to_env(ROOT_ENV, "concat", create_lua_function(function(env)
	local r = {type = TLIST}
	local val = find_in_env(env, "val")
	for i,v in ipairs(val) do
		if not is_list(v) then
			return nil, "all arguments of concat must be lists"
		end
		for ii,vv in ipairs(v) do
			table.insert(r, vv)
		end
	end
	return r
end, ROOT_ENV, {}, "val"))
