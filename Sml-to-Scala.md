# VM

<h2>Notes on Migrating from SML to Scala</h2>

* [Basic Types](#basic-types)
* [Variable Definitions](#variable-definitions)
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
<h3>Variable Definitions</h3>

SML | Scala
----| -------------
__val__ _name_ __=__ _value_     | val _name_ __=__ _value_
__val__ _name_ __= ref__ _value_ | __var__ _name_ __=__ _value_


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

follow the following steps:

1. create a new file `stringpool.scala` (or copy `stringpool.sml` into `stringpool.scala`
   if you want to keep comments, headers, aso asf)
2. create a `sealed trait STRING_POOL` and copy all declarations from the sml signature
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
   from the `StringPool` SML structure:
   
        private object StringPoolImp extends STRING_POOL {
          <structure implementaton goes here>
        }