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

  
}