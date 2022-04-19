module Step = struct
  type 'a two_track = Success of 'a | Failure of string

  let bind switchFn twoTrackInput =
    match twoTrackInput with Success v -> switchFn v | Failure s -> Failure s

  let map singleTrackFn twoTrackInput =
    match twoTrackInput with
    | Success v -> Success (singleTrackFn v)
    | Failure s -> Failure s

  (* let map_with_bind singleTrackFn = bind (singleTrackFn >> Success) *)

  let tee deadEndFn oneTrackInput =
    deadEndFn oneTrackInput;
    oneTrackInput
end
