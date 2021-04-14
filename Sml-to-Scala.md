# VM

<h2>Notes on Migrating from SML to Scala</h2>

* [Basic Types](#basic-types)
* [Variable Declarations](#variable-declarations)
* [Datatypes](#datatypes)  
* [Signatures and Structures](#signatures-and-structures)

<br/>
<h3>Basic Types</h3>

SML | Scala
----| -------------
__unit__    | __Unit__
__int__     | __Int__
__string__  | __String__
ty1 __*__ ty2 | __(__ _ty1_ __,__ _ty2_ __)__
_ty1_ __->__ _ty2_ | _ty1_ __=>__ _ty2_
_ty_ __option__ | __Option__[_ty_]
_ty_ __list__   | __List__[_ty_]
... |

<br/>
<h3>Variable Declarations</h3>

SML | Scala
----| -------------
__val__ _name_ __=__ _value_     | __val__ _name_ __=__ _value_
__val__ _name_ __= ref__ _value_ | __var__ _name_ __=__ _value_

<br/>
<h3>Dataypes</h3>

For nullary constructor datatypes:

SML | Scala
----| -------------
__datatype__ _ty-name_ __=__ | __sealed trait__ _ty-name_
nullary value constructors: |
_vconstructor-name_ | __case object__ _vconstructor-name_ __extends__ _ty-name_
value constructors w/o arguments: |
_vconstructor-name_ __of__ _ty_ | __case class__ _vconstructor-name_ __(__ _ty_ __) extends__ _ty-name_

For example, to convert the `ty` datatype declaration below

      datatype ty = ANY
      | ANYVAL
      | BOOL
      | INT
      | ANYREF
      | STRING
      | ARRAY of ty
      | RECORD of (Symbol.symbol * ty) list
      | NULL
      | NOTHING
      | META of ty ref

follow the steps

1. replace `datatype ty =` with `sealed trait Ty`
2. replace nullary value constructor `ANY` with `case object ANY extends Ty`
3. replace value constructor w/ arguments `ARRAY of ty` with
   `case class ARRAY(ty: Ty) extends Ty`
4. for `ref` types use a `var` constructor argument (vs `val` for
everything else), e.g. `META of ty ref` will be translated to
   `case class  META(var ty: Ty) extends Ty`
   
Here is the _Scala_ construct corresponding to the _SML_
`datatype` declaratio above:

      sealed trait Ty
      case object ANY    extends Ty
      case object ANYVAL extends Ty
      case object BOOL   extends Ty
      case object INT    extends Ty
      case object ANYREF extends Ty
      case object STRING extends Ty
      case class  ARRAY(ty: Ty) extends Ty
      case class  RECORD(lts: List[(String, Ty)]) extends Ty
      case object NULL extends Ty
      case object NOTHING extends Ty
      case class  META(var ty: Ty) extends Ty


<br/>
<h3>Signatures and Structures</h3>

SML | Scala
------- | -------------
__signature__ _name_ | __sealed trait__ _name_
__structure__ _name_ __:__ _sig-name_ | __object__ _name_ __extends__ _sig-name_
__= struct__ | __private object__ _nameImp_ __extends__ _sig-name_

For example for converting file `stringpool.sml` containing the following
signature/struture:

    signature STRING_POOL = sig
      type entry
      val id : entry -> int
      val str: entry -> string
      
      val clear : unit -> unit
      val size  : unit -> int
      val add   : string -> entry
      val lookup: string -> entry option
      val emit  : (int -> unit) -> unit
    end

    structure StringPool : STRING_POOL = struct
      <structure implementaton>
    end

follow the steps:

1. create a new file `stringpool.scala` (or copy `stringpool.sml` into `stringpool.scala`
   if you want to retain comments, headers, aso asf)
2. create a `sealed trait STRING_POOL` and copy all declarations from the _SML_ signature
   into it, like so:

        sealed trait STRING_POOL {
          type Entry
          val id : Entry => Int
          val str: Entry => String
          
          val clear : Unit => Unit
          val size  : Unit => Int
          val add   : String => Entry
          val lookup: String => Option[Entry]
          val emit  : (Int => Unit) => Unit
        }

3. create an `object StringPool` extending the `STRING_POOL` trait, and assign every
`val` in the object to the corresponding implementation from `StringPoolImp`:

        object StringPool extends STRING_POOL {
          type Entry = (String, Int)
          val id = StringPoolImp.id
          val str= StringPoolImp.str
          
          val clear = StringPoolImp.clear
          val size  = StringPoolImp.size
          val add   = StringPoolImp.add
          val lookup= StringPoolImp.lookup
          val emit  = StringPoolImp.emit
        }
    
4. create a private `StringPoolImp` object which will contain the actual implementation
   from the `StringPool` _SML_ structure:
   
        private object StringPoolImp extends STRING_POOL {
          <structure implementaton goes here>
        }


clausal function expression, pg 53
each component pat => exp is called a clause
entire assembly of rules is called a match

nullary type constructor

datatype suit = Spades | Hearts | Diamonds | Clubs
This declaration introduces a new type suit with four nullary value
constructors, Spades, Hearts, Diamonds, and Clubs

