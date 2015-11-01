Lemma inj_f : forall n m, ((fun s : nat -> nat => s n) = (fun s : nat -> nat => s m)) -> n = m.
Proof.
  intros.
  assert (forall f, (fun s : nat -> nat => s n) f = (fun s : nat -> nat => s m) f).
  

Lemma inj_f : forall n m, (forall f : nat -> nat, f n = f m) -> n = m.
Proof.
  intros.
  apply (H (fun n => n)).
  Defined.