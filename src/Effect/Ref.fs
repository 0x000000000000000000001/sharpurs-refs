let _new s = fun _ -> box (ref s)

let newWithSelf f =
    fun _ ->
        let r = ref (box null)
        let f' = unbox<obj -> obj> f
        let s = unbox<obj> (f' (box r))
        r := s
        box r

let read r =
    fun _ ->
        let rRef = unbox<obj ref> r
        lock rRef (fun () -> !rRef)

let write s r =
    fun _ ->
        let rRef = unbox<obj ref> r
        lock rRef (fun () -> rRef := s)
        box null

let modifyImpl f r =
    fun _ ->
        let rRef = unbox<obj ref> r
        let f' = unbox<obj -> Map<string, obj>> f
        lock rRef (fun () ->
            let result = f' (!rRef)
            rRef := Map.find "state" result
            Map.find "value" result
        )
