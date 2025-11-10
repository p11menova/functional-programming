open Utils

let lcm_from_one_to_target_lazy n = Seq.ints 1 |> Seq.take n |> Seq.fold_left lcm 1
