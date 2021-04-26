/**
 * symbol.scala
 * vm-comp
 *
 * Created by Ovidiu Podisor on 04/05/20.
 * Copyright © 2019-2021 innodocs. All rights reserved.
 */

import scala.collection.mutable.HashMap
import scala.collection.immutable.TreeMap

/*
structure Symbol :> SYMBOL = struct
*/
object Symbol {

  type Ty = (String, Int)

  private var nextsym = -1
  private val symbolTable = new HashMap[String, Symbol.Ty]

  def name(sym: Ty): String = sym match { case (s, n) => s }

  def symbol(name: String): Ty = {
    symbolTable.find(e => e match {case (s, _) => name == s}) match {
      case Some((_, e)) => e
      case None => {
        nextsym += 1
        val e: Symbol.Ty = (name, nextsym)
        symbolTable.addOne(name, e)
        e
      }
    }
  }

  case class Table[A](treeMap : TreeMap[Int, A] = TreeMap[Int,A]()) {

    def size: Int = treeMap.size

    def enter(sym: Ty, a: A): Table[A] =
      sym match { case (s, n) => Table(treeMap + (n -> a)) }

    def lookup(sym: Ty): Option[A] =
      sym match { case (s, n) => treeMap.get(n) }

    def list: Iterable[A] = {
      val toListFn: PartialFunction[(Int, A), A] = {
        case (k, v) => v
      }
      treeMap.collect(toListFn)
    }
  }
}
/* end */
