let _new s = ref s

let newWithSelf f =
    let r = ref (box null)
    let s = (unbox (f (box r)))
    r := s
    box r

let read r = !(unbox<obj ref> r)

let write s r =
    let rRef = unbox<obj ref> r
    rRef := s

let modifyImpl f r =
    let rRef = unbox<obj ref> r
    let result = unbox<Map<string, obj>> ((unbox<obj -> obj> f) (!rRef))
    rRef := Map.find "state" result
    Map.find "value" result
