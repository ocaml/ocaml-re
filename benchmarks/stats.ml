open Re_benchmarks

let () =
  Workload.select ~name:(fun (c : Workload.case) -> c.name) Suite.cases
  |> List.iter (fun (c : Workload.case) -> Workload.report c.name (c.make ()))
;;
