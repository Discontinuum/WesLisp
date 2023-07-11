wesnoth.dofile("~add-ons/WesLisp/lua/parser.lua")
wesnoth.dofile("~add-ons/WesLisp/lua/evaluator.lua")
scenario_env = new_env(ROOT_ENV)
function eval(sexpr) 
	return eval_sexpr(sexpr, scenario_env)
end
