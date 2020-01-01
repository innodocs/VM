(*
**  absyn.sml
**  vm
**
**  Created by Ovidiu Podisor on 01/30/19.
**  Copyright © 2019 innodocs. All rights reserved.
*)

structure Absyn = struct 

	type id = string

	datatype binop = Plus | Minus | Times | Div

	datatype stm = CompoundStm of stm * stm
    	         | AssignStm of id * exp
        	     | PrintStm of exp list

    	 and exp = IdExp of id
   	             | NumExp of int
      	         | OpExp of exp * binop * exp
        	     | EseqExp of stm * exp
end
