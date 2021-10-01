opaque type Category[o] with Eq = (List[o], List[Morphism[o]])
opaque type Morphism[o] with Eq = (o, o)

/// In every category, every object has a unique id morphism.
law id: forall(cat: Category[o]) with Eq[o] . match cat {
  case Category(objects, morphisms) => List.forall(
    obj -> List.memberOf(Morphism(obj, obj), morphisms),
    objects
  )
}

law associativity: forall(m1: Morphism[o], m2: Morphism[o], m3: Morphism[o]) with Eq[o] .
  use Option.flatMap;
  (compose(m1, m2) |> flatMap(m12 -> compose(m12, m3))) ==
  (compose(m2, m3) |> flatMap(m23 -> compose(m1, m23)))
  

// Two morphisms are composable if the input of one is the output of the other
def composable(m1: Morphism[o], m2: Morphism[o]): Bool with Eq[o] = match (m1, m2) {
  case (Morphism(i1, _), Morphism(_, o2)) => i1 == o2
}

/// (m1 o m2)(x) = m1(m2(x))
def compose(m1: Morphism[o], m2: Morphism[o]): Option[Morphism[o]] with Eq[o] = match (m1, m2) {
  case (Morphism(i1, o1), Morphism(i2, o2)) if o2 == i1 => Some(Morphism(i2, o1))
  case _ => None
}

/// auxiliary definition
def isSet(l: List[a]): Bool = ???

/// auxiliary definition
def cartesianProduct(l1: List[a], l2: List[b]): List[(a, b)] = {
  List.flatMap(a -> List.map(b -> (a, b), l2), l1)
}

/// written cat(src, dst)
def matchingMorphisms(src: o, dst: o, cat: Category[o]): List[Morphism[o]] with Eq[o] = match cat {
  case Category(_, morphs) => morphs |> List.filter(Eq.eq(Morphism(src, dst)))
}

def locallySmall(c: Category[o]): Bool with Eq[o] = match c {
  case Category(objs, _) =>
    let pairs = cartesianProduct(objs, objs);
    pairs |> List.forall(
      match (src, dst) -> matchingMorphisms(src, dst, c) |> isSet
    )
}

def small(c: Category[o]): Bool with Eq[o] = match c {
  case Category(objs, _) =>
    locallySmall(c) and isSet(objs)
}

def opposite(c: Category[o]): Category[o] = match c {
  case Category(objs, morphs) =>
    let oppMorphs = morphs |> List.map(
      match Morphism(a, b) -> Morphism(b, a)
    );
    Category(objs, oppMorphs)
}

/// written (morph (down-arrow) c)
def slices(obj: o, c: Category[o]): Category[Morphism[o]] = match c {
  case Category(objs0, morphs0) =>
    let objs = morphs0;
    let morphs = ???
    Category(objs, morphs)
}

/// TODO: need an interpretation of the morphisms ?


