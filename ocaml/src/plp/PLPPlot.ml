let str_to_ieq =
"def to_ieq(poly, ring, variables):\n\
\tmonomials = [ring(1)] + [ring(x) for x in variables]\n\
\treturn [poly.monomial_coefficient(m) for m in monomials]\n\
"

let str_projection =
"#PROJECTION\n\
def standard_basis_vector(n, i):\n\
\tassert i>=0\n\
\tassert i<n\n\
\treturn vector(QQ, [0,]*i + [1] + [0,]*(n-i-1))\n\
\n\
def monomial_image(m,variables,nb_var):\n\
\ttry:\n\
\t\treturn standard_basis_vector(nb_var,variables.index(str(m)))\n\
\texcept ValueError:\n\
\t\treturn vector(QQ, nb_var)\n\
\n\
#Projète le polyèdre suivant la variable\n\
# variables doit contenir la liste des variables, y compris celle qui va être projetée\n\
#P doit avoir la même dimension que variables\n\
def proj(P,variable, variables, ring, nb_dim):\n\
\tmonomials = [ring(x) for x in variables]\n\
\tvertices = P.vertices()\n\
\ti = variables.index(variable)\n\
\tnew_variables = variables[0:i] + variables[i+1:]\n\
\tnb_var = len(variables) -1\n\
\tprojection = matrix(QQ, [monomial_image(m,new_variables,nb_var) for m in monomials])\n\
\tproj_vertices = [(v.vector()*projection)[0:len(new_variables)] for v in vertices]\n\
\treturn Polyhedron(vertices = proj_vertices)\n\
\n\
#On va projeter toutes les variables sauf variable pour obtenir l'intervalle de variable\n\
def get_itv_from_poly(P,variable,variables,ring, nb_dim):\n\
\ti = variables.index(variable)\n\
\tnew_variables = variables[0:i] + variables[i+1:] #on enlève variable de variables\n\
\tvariables_inter = variables\n\
\tfor v in new_variables:\t#on parcourt toutes les variables sauf variable\n\
\t\tP = proj(P,v,variables_inter,ring, nb_dim)\n\
\t\ti = variables_inter.index(v)\n\
\t\tvariables_inter = variables_inter[0:i] + variables_inter[i+1:]\n\
\tvertices = P.vertices_list()\n\
\tif len(vertices) == 0:\n\
\t\treturn []\n\
\tif len(vertices) == 1:\n\
\t\treturn [int(vertices[0][0]), int(vertices[0][0])]\n\
\treturn [int(min(vertices[0][0],vertices[1][0])),int(max(vertices[0][0],vertices[1][0]))]\n\
"

let str_plot_polyhedra =
"# polyhedra = polyhedron list\n\
def plot_polyhedra(polyhedra, nb_dim):\n\
\tif len(polyhedra) > 0:\n\
\t\tif nb_dim == 1:\n\
\t\t\tcouleur = color(polyhedra[0])\n\
\t\t\tto_plot = polyhedra[0].projection().render_line_1d()\n\
\t\t\tfor i in range(1,len(polyhedra)):\n\
\t\t\t\tcouleur = color(polyhedra[i])\n\
\t\t\t\tto_plot += polyhedra[i].projection().render_line_1d()\n\
\n\
\t\tif nb_dim == 2:\n\
\t\t\tcouleur = color(polyhedra[0])\n\
\t\t\tto_plot = polyhedra[0].projection().render_outline_2d(color = couleur)\n\
\t\t\tfor i in range(1,len(polyhedra)):\n\
\t\t\t\tcouleur = color(polyhedra[i])\n\
\t\t\t\tto_plot += polyhedra[i].projection().render_outline_2d(color = couleur)\n\
\n\
\t\tif nb_dim == 3:\n\
\t\t\tcouleur = color(polyhedra[0])\n\
\t\t\tto_plot = polyhedra[0].projection().render_wireframe_3d(color = couleur)\n\
\t\t\tfor i in range(1,len(polyhedra)):\n\
\t\t\t\tcouleur = color(polyhedra[i])\n\
\t\t\t\tto_plot += polyhedra[i].projection().render_wireframe_3d(color = couleur)\n\
\t\treturn to_plot\n\
"
let str_plot_polynomial =
"\n\
def plot_polynomial(f, ranges, parameters):\n\
\tvar(' '.join(parameters))\n\
\tif len(parameters) == 1:\n\
\t\tto_plot = plot(f,ranges[0][0],ranges[0][1])\n\
\n\
\tif len(parameters) == 2:\n\
\t\tto_plot = plot3d(f,(parameters[0], ranges[0][0],ranges[0][1]),(parameters[1], ranges[1][0],ranges[1][1]), color = 'green', opacity=0.2)\n\
\treturn to_plot\n\
"

let str_color =
"#Color handling\n\
colors = []\n\
color_bind = []\n\
\n\
def bind_color(x):\n\
\tif color_bind.count(x) == 0:\n\
\t\tcolor_bind.append(x)\n\
\n\
def def_colors():\n\
\timport random\n\
\trandom.seed(425)\n\
\tr = lambda: random.random()\n\
\tfor i in range(0,len(color_bind)):\n\
\t\tcolors.append((r(),r(),r()))\n\
\n\
def color(x):\n\
\treturn colors[color_bind.index(x)]\n\
"

let str_color_from_polyhedra =
"#polyhedra : polyhedron list\n\
def color_from_polyhedra(polyhedra):\n\
\tfor p in polyhedra:\n\
\t\tbind_color(p)\n\
\tdef_colors()\n\
"

let str_plot_regions =
"def plot_regions(regions, nb_dim):\n\
\tif len(regions) > 0:\n\
\t\tif nb_dim == 1:\n\
\t\t\tcouleur = color(regions[0][1])\n\
\t\t\tto_plot = regions[0][0].projection().render_line_1d()\n\
\t\t\tfor i in range(1,len(regions)):\n\
\t\t\t\tcouleur = color(regions[i][1])\n\
\t\t\t\tto_plot += regions[i][0].projection().render_line_1d()\n\
\n\
\t\tif nb_dim == 2:\n\
\t\t\tcouleur = color(regions[0][1])\n\
\t\t\tto_plot = regions[0][0].projection().render_fill_2d(color = couleur)\n\
\t\t\tfor i in range(1,len(regions)):\n\
\t\t\t\tcouleur = color(regions[i][1])\n\
\t\t\t\tto_plot += regions[i][0].projection().render_fill_2d(color = couleur)\n\
\n\
\t\tif nb_dim == 3:\n\
\t\t\tcouleur = color(regions[0][1])\n\
\t\t\tto_plot = regions[0][0].projection().render_solid_3d(color = couleur, alpha = 0.7)\n\
\t\t\tfor i in range(1,len(regions)):\n\
\t\t\t\tcouleur = color(regions[i][1])\n\
\t\t\t\tto_plot += regions[i][0].projection().render_solid_3d(color = couleur, alpha = 0.7)\n\
\t\treturn to_plot\n\
"

let str_def_regions =
"#arbre = liste de [region, solution]\n\
def regions_from_tree(arbre, ring, variables, nb_dim):\n\
\tregions = []\n\
\tlines = []\n\
\tif len(variables) == nb_dim:\n\
\t\tfor i in range(0,len(arbre)):\n\
\t\t\tineqs = [to_ieq(c,ring,variables) for c in arbre[i][0]]\n\
\t\t\tbind_color(arbre[i][1])\n\
\t\t\tregions = regions + [[Polyhedron(ieqs = ineqs), arbre[i][1]]]\n\
\t\t\tlines = lines + [arbre[i][1]]\n\
\tif len(variables) + 1 == nb_dim :\n\
\t\tfor i in range(0,len(arbre)):\n\
\t\t\tineqs = [to_ieq(c,ring,variables)+[0] for c in arbre[i][0]]\n\
\t\t\teqs = [to_ieq(-1*arbre[i][1],ring,variables) + [-1]]\n\
\t\t\tbind_color(arbre[i][1])\n\
\t\t\tregions = regions + [[Polyhedron(ieqs = ineqs, eqns = eqs), arbre[i][1]]]\n\
\t\t\tlines = lines + [arbre[i][1]]\n\
\tdef_colors()\n\
\treturn (regions,lines)\n\
"

let str_poly_from_regions =
"def poly_from_regions(regions):\n\
\treturn [x[0] for x in regions]\n\
"

let str_print_lines =
"def print_lines(lines, ranges, variables):\n\
\tif len(variables) == 2:\n\
\t\tvar(variables[0])\n\
\t\tnewring = PolynomialRing(QQ,[variables[0]],1)\n\
\t\tp = ring(lines[0])\n\
\t\tc = p.monomial_coefficient(ring(variables[1]))\n\
\t\tg = newring(str((p - c * ring(variables[1])) / (-1*c)))\n\
\t\tcouleur = color(p)\n\
\t\tto_plot = plot(g,ranges[0][0],ranges[0][1], color=couleur)\n\
\t\tfor i in range(1,len(lines)):\n\
\t\t\tp = ring(lines[i])\n\
\t\t\tc = p.monomial_coefficient(ring(variables[1]))\n\
\t\t\tg = newring(str((p - c * ring(variables[1])) / (-1*c)))\n\
\t\t\tcouleur = color(p)\n\
\t\t\tto_plot += plot(g,ranges[0][0],ranges[0][1], color=couleur)\n\
\t\tto_plot.show()\n\
"

let str_compute_solution =
"def compute_output_polyhedron(result, nb_dim, variables):\n\
\tsolutions = [-1*x[1] for x in result]\n\
\tif len(variables) == nb_dim:\n\
\t\tP = Polyhedron(ieqs = [to_ieq(c,ring,parameters) for c in solutions])\n\
\tif len(variables) + 1 == nb_dim:\n\
\t\tineqs = [to_ieq(c,ring,variables)+[0] for c in solutions]\n\
\t\teqs = [[0,0,0,1]]\n\
\t\tP = Polyhedron(ieqs = ineqs, eqns = eqs)\n\
\n\
\tif nb_dim == 2:\n\
\t\treturn P.projection().render_outline_2d(color = 'black')\n\
\tif nb_dim == 3:\n\
\t\treturn P.projection().render_wireframe_3d(color = 'black', thickness = 2)\n\
"

let str_plot =
"to_plot.show()"

module Cs = Cstr.Rat.Positive

module Plot (Minimization : Min.Type) = struct
	include PLPCore.PLP(Minimization)

	module Plot = struct
		module Poly = ParamCoeff.Poly

		let (monomial_to_string : Poly.Monomial.t -> string)
			= fun m -> let (vlist, c) = Poly.Monomial.data m in
			match Poly.MonomialBasis.to_list_expanded vlist with
			| [] -> Q.to_string c
			| _ -> if Q.equal c (Q.of_int 1)
			then Poly.MonomialBasis.to_string_param vlist "p"
			else if Q.lt c (Q.of_int 0)
				then String.concat "" ["(";Q.to_string c;")*"; Poly.MonomialBasis.to_string_param vlist "p"]
				else String.concat "" [Q.to_string c;"*"; Poly.MonomialBasis.to_string_param vlist "p"]

		let polynomial_to_string : Poly.t -> string
			= fun p ->
			Poly.data p
			|> List.map monomial_to_string
			|> String.concat " + "
			|> fun s -> if String.length s = 0 then "0" else s

		let (result_to_string : (Region.t * 'c Cons.t) list -> string)
		= fun solutions ->
		Misc.list_to_string
			(fun (reg,(c,_)) -> Printf.sprintf "[%s,%s]"
				(Misc.list_to_string
					(fun cstr ->
						let poly = Poly.ofCstr cstr.Cs.v (Scalar.Rat.neg cstr.Cs.c) in
						Printf.sprintf "ring(\"%s\")" (Poly.neg poly |> polynomial_to_string))
					(Region.get_cstrs reg)
					" , ")
				(let cstr = c in
				 let poly = Poly.ofCstr cstr.Cs.v (Scalar.Rat.neg cstr.Cs.c) in
				 Printf.sprintf "ring(\"%s\")" (polynomial_to_string poly))
			)
			solutions ","

		let (plot': string -> Poly.V.t list -> int -> (Region.t * 'c Cons.t) list -> unit)
			= fun file_name parameters nb_dim result ->
			let output_file = open_out file_name in
			let result_str = result_to_string result in
			let str = Printf.sprintf "%s\n\nparameters = %s\nring = %s\nnb_dim = %s\nresult = %s\n(P,lines) = %s\n%s\n%s"
				(Printf.sprintf "%s%s%s%s%s"
					str_to_ieq
					str_color
					str_def_regions
					str_plot_regions
					str_compute_solution)
				(Misc.list_to_string (fun x -> String.concat "" ["\"";Poly.V.to_string' "p" x;"\""]) parameters ",")
				("PolynomialRing(QQ,parameters,len(parameters))")
				(string_of_int(nb_dim))
				result_str
				(String.concat "\n" ["regions_from_tree(result, ring, parameters, nb_dim)";
				"to_plot = plot_regions(P, nb_dim)"])
				"to_plot += compute_output_polyhedron(result, nb_dim, parameters)"
				(str_plot)
				in Pervasives.output_string output_file str;
				close_out output_file;;

		let plot : (Region.t * 'c Cons.t) list -> unit
			= fun res ->
			let parameters = List.split res
				|> Pervasives.snd
				|> List.split
				|> Pervasives.fst
				|> Cs.getVars
				|> Cs.Vec.V.Set.elements
			in
			let n_params = List.length parameters in
			let dim = if n_params = 2 then 3 else n_params in
			plot' Config.sage_log parameters dim res
	end
end
