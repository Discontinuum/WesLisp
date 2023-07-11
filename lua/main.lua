LPAREN = "lparen"
RPAREN = "rparen"
QUOTE = "quote"
--ATOM = 4
LBRACKET = "lbracket"
RBRACKET = "rbracket"
DOT = "dot"
SHARP = "sharp"
COMMA = "comma"
BACKTICK = "backtick"
AT = "at"
STRING="string"
NUMBER="number"
SYMBOL="symbol"

local isspaceornil = function(str)
	return (char == nil) and str:match("%s") ~= nil
end

local isspace = function(str)
	return (str ~= nil) and str:match("%s") ~= nil
end

local isdigit = function(str)
	return (str ~= nil) and str:match("%d") ~= nil
end

local simpleToken = {["'"] = QUOTE, ["("] = LPAREN, [")"] = RPAREN, 
	["["] = LBRACKET, ["]"] = RBRACKET, 
	-- ["."] = DOT, 
	["#"] = SHARP, [","] = COMMA, ["`"] = BACKTICK, ["@"] = AT}
	
local canStartSymbol = function(char)
	return (char ~= nil) and (not (isspace(char))) and (not (isdigit(char))) and (simpleToken[char] == nil) and (char ~= ";") and (char ~= '"')
end

local canStartNumber = function(char)
	return (char ~= nil) and ((isdigit(char)) or (char == "-"))
end

local canContSymbol = function(char)
	return (char ~= nil) and (not (isspace(char))) and (simpleToken[char] == nil) and (char ~= ";") and (char ~= '"')
end

function tokenize(input)
	local ind = 1
	local tokens = {}
	
	while ind <= #input do
		local sym = input[ind]
		if sym == ';' then
			while input[ind + 1] ~= '\n' and input[ind + 1] ~= nil do
				ind = ind + 1
			end
		elseif simpleToken[sym] ~= nil then
			table.insert(tokens, {type = simpleToken[sym]})
		elseif sym == '"' then
			local val = ""
			ind = ind + 1
			while input[ind] ~= '"' do
				if input[ind] == '\\' then
					ind = ind + 1 --TODO: this doesn't support \n, makes it just n
				end
				if input[ind] == nil then
					return tokens, "error, unexpected EOF during string token"
				end
				val = val .. input[ind]
				ind = ind + 1
			end
			table.insert(tokens, {type = STRING, value = val})
		else
			local probNum = canStartNumber(sym)
			local probSymb = canStartSymbol(sym)
			if probSymb and probNum then -- it's -, either hypen or minus sign
				if isdigit(input[ind + 1]) then
					probSymb = false
				else
					probNum = false
				end
			end
			if probNum then
				local val = sym
				while isdigit(input[ind + 1]) do
					ind = ind + 1
					val = val .. input[ind]
				end
				if input[ind + 1] == '.' then -- float number
					val = val .. '.'
					ind = ind + 1
					while isdigit(input[ind + 1]) do
						ind = ind + 1
						val = val .. input[ind]
					end
				end
				table.insert(tokens, {type = NUMBER, value = tonumber(val)})
			end
			if probSymb then
				local val = sym
				while canContSymbol(input[ind + 1]) do
					ind = ind + 1
					val = val .. input[ind]
				end
				table.insert(tokens, {type = SYMBOL, value = val})
			end
		end
		ind = ind + 1
	end
	return tokens
end

-- datatypes
TTOPLVL = "toplevel"
TLIST = "list"
TVEC = "vector"
--TSTR = 4
TSYM = "symbol"
TFUN = "function"
TSPEC = "specialform"
TMACRO = "macro"

function is_sym(sexpr)
	return (type(sexpr) == "table") and (sexpr.type == TSYM)
end

function is_list(sexpr)
	return (type(sexpr) == "table") and (sexpr.type == TLIST)
end

function read_tokens(tokens) 
	local ind = 1
	local function gettoken()
		return tokens[ind]
	end
	local function nexttoken()
		return tokens[ind + 1]
	end
	local function forward()
		ind = ind + 1
	end
	local function eof()
		return tokens[ind] == nil
	end
	local sexpr
	local function list()
		if gettoken().type ~= LPAREN then
			return nil, "list() called when no LPAREN ("
		end
		local l = {}
		l.type = TLIST
		forward()
		while (not eof()) and gettoken().type ~= RPAREN do
			local s, err = sexpr()
			if err then
				return l, err
			end
			table.insert(l,s)
			forward()
		end
		if eof() then
			return l, "unexpected eof during list parsing"
		end
		return l, nil
	end
	sexpr = function()
		if eof() then
			return nil, "uenxpected eof, expected s-expression"
		end
		local t = gettoken()
		if t.type == LPAREN then
			local list, err = list()
			if err then
				return nil, err
			else
				return list
			end
		end
		if t.type == NUMBER then
			return t.value
		end
		if t.type == SYMBOL then
			local sym = {type = TSYM, name = t.value}
			return sym
		end
		if t.type == STRING then
			return t.value
		end
		if t.type == QUOTE then
			forward()
			local qs = {type = TLIST}
			table.insert(qs, {type = TSYM, name = "quote"})
			local s, err = sexpr()
			if err then
				return nil, err
			else
				table.insert(qs,s)
				return qs
			end
		end
		return nil, "unexpected token "..t.type
	end
	local function sexprs() 
		local out = {}
		out.type = TTOPLVL
		while (not eof()) do
			local sexpr, err = sexpr()
			if err then
				return out, err
			else
				table.insert(out, sexpr)
			end
			forward()
		end
		return out
	end
	return sexprs() 
end

function read_string (str) 
	local tokens, err = tokenize(str)
	if err then
		return nil, err
	end
	return read_tokens(tokens)
end

function print_sexpr(expr)
	if type(expr) == "number" then
		return tostring(expr)
	end
	if type(expr) == "string" then
		local s = '"'
		local ind = 1
		while expr[ind] ~= nil do
			local c = expr[ind]
			if c == '\\' or c == '"' then
				s = s .. '\\'
			end
			s = s .. c
			ind = ind + 1
		end
		s = s .. '"'
		return s
	end
	if type(expr) == 'nil' then
		return "nil"
	end
	if type(expr) == "boolean" then
		if expr then
			return "true"
		else
			return "false"
		end
	end
	if expr.type == TTOPLVL then
		local s = ""
		local ind = 1
		while expr[ind] ~= nil do
			s = s .. print_sexpr(expr[ind])
			s = s .. "\n"
			ind = ind + 1
		end
		return s
	end
	if expr.type == TLIST then
		local s = "("
		local ind = 1
		while expr[ind] ~= nil do
			s = s .. print_sexpr(expr[ind])
			if expr[ind + 1] ~= nil then
				s = s .. " "
			end
			ind = ind + 1
		end
		s = s .. ")"
		return s
	end
	if expr.type == TVEC then
		local s = "["
		local ind = 1
		while expr[ind] ~= nil do
			s = s .. print_sexpr(expr[ind])
			if expr[ind + 1] ~= nil then
				s = s .. " "
			end
			ind = ind + 1
		end
		s = s .. "]"
		return s
	end
	if expr.type == TSYM then
		return expr.name
	end
	if expr.type == TFUN then
		return "#function#"
	end
	if expr.type == TMACRO then
		return "#macro#"
	end
	if expr.type == TSPEC then
		return "#specialform#"
	end
	return ""
end

wesnoth.dofile("~add-ons/WesLisp/lua/evaluator.lua")
scenario_env = new_env(ROOT_ENV)
function eval(sexpr) 
	return eval_sexpr(sexpr, scenario_env)
end
