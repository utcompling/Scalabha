package opennlp.scalabha.ccg

import collection.mutable

sealed abstract class Value (name: String) { 
  override def toString = name 
}

case class Variable (name: String) extends Value(name)

case class Constant (name: String) extends Value(name)

case class Substitution (val submap: Map[Variable, Value]) {
  def add (newSub: Substitution) = Substitution(submap ++ newSub.submap)
  def add (entry: (Variable, Value)) = Substitution(submap ++ Map(entry))
}

object EmptySubstitution extends Substitution(Map[Variable, Value]())

object FeatConst {
  def apply (attr: String, value: String) = AttrValMap(Map(attr -> Constant(value)))
}

object FeatVar {
  def apply (attr: String, value: String) = AttrValMap(Map(attr -> Variable(value)))
}

case class AttrValMap (avmap: Map[String, Value]) {

  def ++ (that: AttrValMap) = AttrValMap(avmap ++ that.avmap)

  def applySubstitution (sub: Substitution) = {
    AttrValMap(avmap.mapValues { 
      value => value match {    
        case v: Variable => sub.submap.getOrElse(v, v)
        case _ => value
      }
    })
  }
  
  def unifies (that: AttrValMap, sub: Substitution): Option[Substitution] = that match {
    case EmptyAttrValMap => Some(sub)

    case _ => 
      val commonAttributes = avmap.keySet & that.avmap.keySet
      val newSubs = commonAttributes.flatMap (
        attr => (avmap(attr), that.avmap(attr)) match {
          case (x, y: Variable) => Some(y -> x)
          case (x: Variable, y) => Some(x -> y)
          case (Constant(x), Constant(y)) => if (x == y) None else return None
        }
      ).toMap
      Some(sub.add(Substitution(newSubs)))
  }

  def makeVarsUnique (uniqueIndex: Int, sub: Substitution) = {
    var newSub = sub
    var newIndex = uniqueIndex
    val newAvMap = mutable.Map[String, Value]()

    for ((key,value) <- avmap) {
      value match {
        case v: Variable => 
          val freshVar = newSub.submap.get(v) match {
            case Some(x) => x
            case _ => Variable(v.name + "_" + newIndex)
          }
          newSub = newSub.add(v -> freshVar)
          newIndex += 1
          newAvMap(key) = freshVar

        case _ => 
          newAvMap(key) = value
      }
    }

    (AttrValMap(newAvMap.toMap), newIndex, newSub)
  }

  override def toString = {
    if (avmap.size == 0) ""
    else "[" + avmap.keys.map(k => k+"="+avmap(k)).mkString(",") + "]"
  }

}


object EmptyAttrValMap extends AttrValMap(Map[String, Value]())


