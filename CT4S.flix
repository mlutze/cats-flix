namespace CT4S {
  pub type alias Category[o, m] = {
    objects :: List[o], 
    homSets :: (o, o) -> List[m],
    id :: o -> m,
    compose :: m -> m -> m
  }

  law id: forall (c: Category[o, m]) with Eq[m] .
    use List.flatMap;
    {
      let* x = c.objects;
      let* y = c.objects;
      let* f = (c.homSets)(x, y);
      ((c.compose)(f, (c.id)(x)) == f and (c.compose)((c.id)(y), f) == f) :: Nil
    } |> List.forall(identity)

  law assoc: forall (c: Category[o, m]) with Eq[m] .
    use List.flatMap;
    {
      let* w = c.objects;
      let* x = c.objects;
      let* y = c.objects;
      let* z = c.objects;
      let* f = (c.homSets)(w, x);
      let* g = (c.homSets)(x, y);
      let* h = (c.homSets)(y, z);
      ((c.compose)((c.compose)(h, f), g) == (c.compose)(h, (c.compose)(f, g))) :: Nil
    } |> List.forall(identity)


  def catOfSets(): Category[a, a -> a] = {
    objects = ???, // all sets
    homSets = ???, // all functions
    id = _ -> identity,
    compose = f1 -> f2 -> f1 >> f2
  }

  def isIsomorphism(c: Category[o, m], f: m): Bool with Eq[m] = {
    use List.flatMap;
    {
      let* x = c.objects;
      let* y = c.objects;
      let* g = (c.homSets)(x, y);
      (g, x, y) :: Nil
    } |> List.exists(
      match (g, x, y) -> (c.compose)(g, f) == (c.id)(x) and (c.compose)(f, g) == (c.id)(y)
    )
  }

  // now taking from course notes
  def isMonomorphism(c: Category[o, m], f: m): Bool with Eq[m] = {
    use List.flatMap;
    {
      let* c0 = c.objects;
      /// let* c1 = c.objects;
      let* c2 = c.objects;
      let* u0 = (c.homSets)(c2, c0);
      let* u1 = (c.homSets)(c2, c0);
      (u0, u1) :: Nil
    } |> List.forall(
      match (u0, u1) -> ((c.compose)(u0, f) == (c.compose)(u1, f)) ==> (u0 == u1)
    )
  }

  pub type alias Funct[o1, m1, o2, m2] = {
    objs :: o1 -> o2,
    morphs :: m1 -> m2
  }

  law functIds: forall (c1: Category[o1, m1], c2: Category[o2, m2], f: Funct[o1, m1, o2, m2]) with Eq[m1], Eq[m2] . {
    use List.flatMap;
    c1.objects |> List.forall(
      obj -> (f.morphs)((c1.id)(obj)) == (c2.id)((f.objs)(obj))
    )
  }

  law functCompose: forall (c1: Category[o1, m1], c2: Category[o2, m2], f: Funct[o1, m1, o2, m2]) with Eq[m1], Eq[m2] . {
    use List.flatMap;
    {
      let* b = c1.objects;
      let* c = c1.objects;
      let* d = c1.objects;
      let* g = (c1.homSets)(b, c);
      let* h = (c1.homSets)(c, d);
      (g, h) :: Nil
    } |> List.forall(
      match (g, h) -> (f.morphs)((c1.compose)(h, g)) == (c2.compose)((f.morphs)(h), (f.morphs)(g))
    )
  }
  
}