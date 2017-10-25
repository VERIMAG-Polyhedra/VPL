open BinInt
open BinNums
open Debugging
open DomainInterfaces
open Itv
open LinTerm
open NumC
open ZNone

module ZNItv =
 struct
  type itv = { low : ZN.t; up : ZN.t }

  (** val low : itv -> ZN.t **)

  let low x = x.low

  (** val up : itv -> ZN.t **)

  let up x = x.up

  type t = itv

  (** val top : itv **)

  let top =
    { low = None; up = None }

  (** val fromBndT : ZItv.bndT -> ZN.t **)

  let fromBndT = function
  | ZItv.Infty -> None
  | ZItv.Open _ ->
    (fun st mesg _ -> raise (CertcheckerConfig.CertCheckerFailure (st, (CoqPr.charListTr mesg))))
      INTERN
      ('Z'::('N'::('I'::('t'::('v'::('.'::('f'::('r'::('o'::('m'::('Z'::('I'::('t'::('v'::(' '::('r'::('e'::('q'::('u'::('i'::('r'::('e'::('s'::(' '::('n'::('o'::(' '::('o'::('p'::('e'::('n'::(' '::('b'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))))))))
      None
  | ZItv.Closed n -> Some n

  (** val fromZItv : ZItv.t -> itv **)

  let fromZItv i =
    { low = (fromBndT i.ZItv.lower); up = (fromBndT i.ZItv.upper) }

  (** val fromQItv : QItv.t -> itv **)

  let fromQItv i =
    fromZItv (ZItv.fromQItv i)

  (** val single : coq_Z -> itv **)

  let single z =
    let zn = Some z in { low = zn; up = zn }

  (** val select : mode -> ZN.t -> ZN.t -> itv **)

  let select m l u =
    match m with
    | BOTH -> { low = l; up = u }
    | UP -> { low = None; up = u }
    | LOW -> { low = l; up = None }

  (** val add : mode -> itv -> itv -> itv **)

  let add m i1 i2 =
    match m with
    | BOTH ->
      { low =
        (match i1.low with
         | Some z1 ->
           (match i2.low with
            | Some z2 -> Some (Z.add z1 z2)
            | None -> None)
         | None -> None); up =
        (match i1.up with
         | Some z1 ->
           (match i2.up with
            | Some z2 -> Some (Z.add z1 z2)
            | None -> None)
         | None -> None) }
    | UP ->
      { low = None; up =
        (match i1.up with
         | Some z1 ->
           (match i2.up with
            | Some z2 -> Some (Z.add z1 z2)
            | None -> None)
         | None -> None) }
    | LOW ->
      { low =
        (match i1.low with
         | Some z1 ->
           (match i2.low with
            | Some z2 -> Some (Z.add z1 z2)
            | None -> None)
         | None -> None); up = None }

  (** val opp : itv -> itv **)

  let opp i =
    { low = (match i.up with
             | Some z -> Some (Z.opp z)
             | None -> None); up =
      (match i.low with
       | Some z -> Some (Z.opp z)
       | None -> None) }

  (** val oppMode : mode -> mode **)

  let oppMode = function
  | BOTH -> BOTH
  | UP -> LOW
  | LOW -> UP

  (** val join : mode -> itv -> itv -> itv **)

  let join m i1 i2 =
    match m with
    | BOTH -> { low = (ZN.meet i1.low i2.low); up = (ZN.join i1.up i2.up) }
    | UP -> { low = None; up = (ZN.join i1.up i2.up) }
    | LOW -> { low = (ZN.meet i1.low i2.low); up = None }

  (** val mulZ : mode -> coq_Z -> itv -> itv **)

  let mulZ m a i =
    if coq_Z_isNat a
    then (match m with
          | BOTH ->
            { low =
              (if coq_Z_isZero a
               then Some Z0
               else (match i.low with
                     | Some z2 -> Some (Z.mul a z2)
                     | None -> None)); up =
              (if coq_Z_isZero a
               then Some Z0
               else (match i.up with
                     | Some z2 -> Some (Z.mul a z2)
                     | None -> None)) }
          | UP ->
            { low = None; up =
              (if coq_Z_isZero a
               then Some Z0
               else (match i.up with
                     | Some z2 -> Some (Z.mul a z2)
                     | None -> None)) }
          | LOW ->
            { low =
              (if coq_Z_isZero a
               then Some Z0
               else (match i.low with
                     | Some z2 -> Some (Z.mul a z2)
                     | None -> None)); up = None })
    else (match m with
          | BOTH ->
            { low =
              (match i.up with
               | Some z2 -> Some (Z.mul a z2)
               | None -> None); up =
              (match i.low with
               | Some z2 -> Some (Z.mul a z2)
               | None -> None) }
          | UP ->
            { low = None; up =
              (match i.low with
               | Some z2 -> Some (Z.mul a z2)
               | None -> None) }
          | LOW ->
            { low =
              (match i.up with
               | Some z2 -> Some (Z.mul a z2)
               | None -> None); up = None })

  (** val mulZZ : mode -> coq_Z -> coq_Z -> itv -> itv **)

  let mulZZ m a b i2 =
    join m
      (if coq_Z_isNat a
       then (match m with
             | BOTH ->
               { low =
                 (if coq_Z_isZero a
                  then Some Z0
                  else (match i2.low with
                        | Some z2 -> Some (Z.mul a z2)
                        | None -> None)); up =
                 (if coq_Z_isZero a
                  then Some Z0
                  else (match i2.up with
                        | Some z2 -> Some (Z.mul a z2)
                        | None -> None)) }
             | UP ->
               { low = None; up =
                 (if coq_Z_isZero a
                  then Some Z0
                  else (match i2.up with
                        | Some z2 -> Some (Z.mul a z2)
                        | None -> None)) }
             | LOW ->
               { low =
                 (if coq_Z_isZero a
                  then Some Z0
                  else (match i2.low with
                        | Some z2 -> Some (Z.mul a z2)
                        | None -> None)); up = None })
       else (match m with
             | BOTH ->
               { low =
                 (match i2.up with
                  | Some z2 -> Some (Z.mul a z2)
                  | None -> None); up =
                 (match i2.low with
                  | Some z2 -> Some (Z.mul a z2)
                  | None -> None) }
             | UP ->
               { low = None; up =
                 (match i2.low with
                  | Some z2 -> Some (Z.mul a z2)
                  | None -> None) }
             | LOW ->
               { low =
                 (match i2.up with
                  | Some z2 -> Some (Z.mul a z2)
                  | None -> None); up = None }))
      (if coq_Z_isNat b
       then (match m with
             | BOTH ->
               { low =
                 (if coq_Z_isZero b
                  then Some Z0
                  else (match i2.low with
                        | Some z2 -> Some (Z.mul b z2)
                        | None -> None)); up =
                 (if coq_Z_isZero b
                  then Some Z0
                  else (match i2.up with
                        | Some z2 -> Some (Z.mul b z2)
                        | None -> None)) }
             | UP ->
               { low = None; up =
                 (if coq_Z_isZero b
                  then Some Z0
                  else (match i2.up with
                        | Some z2 -> Some (Z.mul b z2)
                        | None -> None)) }
             | LOW ->
               { low =
                 (if coq_Z_isZero b
                  then Some Z0
                  else (match i2.low with
                        | Some z2 -> Some (Z.mul b z2)
                        | None -> None)); up = None })
       else (match m with
             | BOTH ->
               { low =
                 (match i2.up with
                  | Some z2 -> Some (Z.mul b z2)
                  | None -> None); up =
                 (match i2.low with
                  | Some z2 -> Some (Z.mul b z2)
                  | None -> None) }
             | UP ->
               { low = None; up =
                 (match i2.low with
                  | Some z2 -> Some (Z.mul b z2)
                  | None -> None) }
             | LOW ->
               { low =
                 (match i2.up with
                  | Some z2 -> Some (Z.mul b z2)
                  | None -> None); up = None }))

  (** val mulNN : itv -> itv **)

  let mulNN i =
    if match i.low with
       | Some z -> coq_Z_isZero z
       | None -> false
    then if match i.up with
            | Some z -> coq_Z_isZero z
            | None -> false
         then i
         else top
    else top

  (** val bndLow : coq_Z -> itv **)

  let bndLow z =
    { low = (Some z); up = None }

  (** val bndUp : coq_Z -> itv **)

  let bndUp z =
    { low = None; up = (Some z) }

  (** val isLOW : mode -> bool **)

  let isLOW = function
  | LOW -> true
  | _ -> false

  (** val isUP : mode -> bool **)

  let isUP = function
  | UP -> true
  | _ -> false

  (** val mulZNNZ : mode -> coq_Z -> coq_Z -> itv **)

  let mulZNNZ m b1 b2 =
    match m with
    | LOW -> top
    | _ ->
      if coq_Z_isNat b1
      then if coq_Z_isNegNat b2
           then { low = None; up = (Some (Z.mul b1 b2)) }
           else top
      else top

  (** val mulZNZN : mode -> coq_Z -> coq_Z -> itv **)

  let mulZNZN m b1 b2 =
    match m with
    | UP -> top
    | _ ->
      if coq_Z_isNat b1
      then if coq_Z_isNat b2
           then { low = (Some (Z.mul b1 b2)); up = None }
           else top
      else top

  (** val mulNZNZ : mode -> coq_Z -> coq_Z -> itv **)

  let mulNZNZ m b1 b2 =
    match m with
    | UP -> top
    | _ ->
      if coq_Z_isNegNat b1
      then if coq_Z_isNegNat b2
           then { low = (Some (Z.mul b1 b2)); up = None }
           else top
      else top

  (** val mul : mode -> itv -> itv -> itv **)

  let mul m i1 i2 =
    match i1.low with
    | Some l1 ->
      (match i1.up with
       | Some u1 ->
         join m
           (if coq_Z_isNat l1
            then (match m with
                  | BOTH ->
                    { low =
                      (if coq_Z_isZero l1
                       then Some Z0
                       else (match i2.low with
                             | Some z2 -> Some (Z.mul l1 z2)
                             | None -> None)); up =
                      (if coq_Z_isZero l1
                       then Some Z0
                       else (match i2.up with
                             | Some z2 -> Some (Z.mul l1 z2)
                             | None -> None)) }
                  | UP ->
                    { low = None; up =
                      (if coq_Z_isZero l1
                       then Some Z0
                       else (match i2.up with
                             | Some z2 -> Some (Z.mul l1 z2)
                             | None -> None)) }
                  | LOW ->
                    { low =
                      (if coq_Z_isZero l1
                       then Some Z0
                       else (match i2.low with
                             | Some z2 -> Some (Z.mul l1 z2)
                             | None -> None)); up = None })
            else (match m with
                  | BOTH ->
                    { low =
                      (match i2.up with
                       | Some z2 -> Some (Z.mul l1 z2)
                       | None -> None); up =
                      (match i2.low with
                       | Some z2 -> Some (Z.mul l1 z2)
                       | None -> None) }
                  | UP ->
                    { low = None; up =
                      (match i2.low with
                       | Some z2 -> Some (Z.mul l1 z2)
                       | None -> None) }
                  | LOW ->
                    { low =
                      (match i2.up with
                       | Some z2 -> Some (Z.mul l1 z2)
                       | None -> None); up = None }))
           (if coq_Z_isNat u1
            then (match m with
                  | BOTH ->
                    { low =
                      (if coq_Z_isZero u1
                       then Some Z0
                       else (match i2.low with
                             | Some z2 -> Some (Z.mul u1 z2)
                             | None -> None)); up =
                      (if coq_Z_isZero u1
                       then Some Z0
                       else (match i2.up with
                             | Some z2 -> Some (Z.mul u1 z2)
                             | None -> None)) }
                  | UP ->
                    { low = None; up =
                      (if coq_Z_isZero u1
                       then Some Z0
                       else (match i2.up with
                             | Some z2 -> Some (Z.mul u1 z2)
                             | None -> None)) }
                  | LOW ->
                    { low =
                      (if coq_Z_isZero u1
                       then Some Z0
                       else (match i2.low with
                             | Some z2 -> Some (Z.mul u1 z2)
                             | None -> None)); up = None })
            else (match m with
                  | BOTH ->
                    { low =
                      (match i2.up with
                       | Some z2 -> Some (Z.mul u1 z2)
                       | None -> None); up =
                      (match i2.low with
                       | Some z2 -> Some (Z.mul u1 z2)
                       | None -> None) }
                  | UP ->
                    { low = None; up =
                      (match i2.low with
                       | Some z2 -> Some (Z.mul u1 z2)
                       | None -> None) }
                  | LOW ->
                    { low =
                      (match i2.up with
                       | Some z2 -> Some (Z.mul u1 z2)
                       | None -> None); up = None }))
       | None ->
         (match i2.low with
          | Some l2 ->
            (match i2.up with
             | Some u2 ->
               join m
                 (if coq_Z_isNat l2
                  then (match m with
                        | BOTH ->
                          { low =
                            (if coq_Z_isZero l2
                             then Some Z0
                             else (match i1.low with
                                   | Some z2 -> Some (Z.mul l2 z2)
                                   | None -> None)); up =
                            (if coq_Z_isZero l2
                             then Some Z0
                             else (match i1.up with
                                   | Some z2 -> Some (Z.mul l2 z2)
                                   | None -> None)) }
                        | UP ->
                          { low = None; up =
                            (if coq_Z_isZero l2
                             then Some Z0
                             else (match i1.up with
                                   | Some z2 -> Some (Z.mul l2 z2)
                                   | None -> None)) }
                        | LOW ->
                          { low =
                            (if coq_Z_isZero l2
                             then Some Z0
                             else (match i1.low with
                                   | Some z2 -> Some (Z.mul l2 z2)
                                   | None -> None)); up = None })
                  else (match m with
                        | BOTH ->
                          { low =
                            (match i1.up with
                             | Some z2 -> Some (Z.mul l2 z2)
                             | None -> None); up =
                            (match i1.low with
                             | Some z2 -> Some (Z.mul l2 z2)
                             | None -> None) }
                        | UP ->
                          { low = None; up =
                            (match i1.low with
                             | Some z2 -> Some (Z.mul l2 z2)
                             | None -> None) }
                        | LOW ->
                          { low =
                            (match i1.up with
                             | Some z2 -> Some (Z.mul l2 z2)
                             | None -> None); up = None }))
                 (if coq_Z_isNat u2
                  then (match m with
                        | BOTH ->
                          { low =
                            (if coq_Z_isZero u2
                             then Some Z0
                             else (match i1.low with
                                   | Some z2 -> Some (Z.mul u2 z2)
                                   | None -> None)); up =
                            (if coq_Z_isZero u2
                             then Some Z0
                             else (match i1.up with
                                   | Some z2 -> Some (Z.mul u2 z2)
                                   | None -> None)) }
                        | UP ->
                          { low = None; up =
                            (if coq_Z_isZero u2
                             then Some Z0
                             else (match i1.up with
                                   | Some z2 -> Some (Z.mul u2 z2)
                                   | None -> None)) }
                        | LOW ->
                          { low =
                            (if coq_Z_isZero u2
                             then Some Z0
                             else (match i1.low with
                                   | Some z2 -> Some (Z.mul u2 z2)
                                   | None -> None)); up = None })
                  else (match m with
                        | BOTH ->
                          { low =
                            (match i1.up with
                             | Some z2 -> Some (Z.mul u2 z2)
                             | None -> None); up =
                            (match i1.low with
                             | Some z2 -> Some (Z.mul u2 z2)
                             | None -> None) }
                        | UP ->
                          { low = None; up =
                            (match i1.low with
                             | Some z2 -> Some (Z.mul u2 z2)
                             | None -> None) }
                        | LOW ->
                          { low =
                            (match i1.up with
                             | Some z2 -> Some (Z.mul u2 z2)
                             | None -> None); up = None }))
             | None ->
               (match m with
                | UP -> top
                | _ ->
                  if coq_Z_isNat l1
                  then if coq_Z_isNat l2
                       then { low = (Some (Z.mul l1 l2)); up = None }
                       else top
                  else top))
          | None ->
            (match i2.up with
             | Some u2 ->
               (match m with
                | LOW -> top
                | _ ->
                  if coq_Z_isNat l1
                  then if coq_Z_isNegNat u2
                       then { low = None; up = (Some (Z.mul l1 u2)) }
                       else top
                  else top)
             | None -> top)))
    | None ->
      (match i1.up with
       | Some u1 ->
         (match i2.low with
          | Some l2 ->
            (match i2.up with
             | Some u2 ->
               join m
                 (if coq_Z_isNat l2
                  then (match m with
                        | BOTH ->
                          { low =
                            (if coq_Z_isZero l2
                             then Some Z0
                             else (match i1.low with
                                   | Some z2 -> Some (Z.mul l2 z2)
                                   | None -> None)); up =
                            (if coq_Z_isZero l2
                             then Some Z0
                             else (match i1.up with
                                   | Some z2 -> Some (Z.mul l2 z2)
                                   | None -> None)) }
                        | UP ->
                          { low = None; up =
                            (if coq_Z_isZero l2
                             then Some Z0
                             else (match i1.up with
                                   | Some z2 -> Some (Z.mul l2 z2)
                                   | None -> None)) }
                        | LOW ->
                          { low =
                            (if coq_Z_isZero l2
                             then Some Z0
                             else (match i1.low with
                                   | Some z2 -> Some (Z.mul l2 z2)
                                   | None -> None)); up = None })
                  else (match m with
                        | BOTH ->
                          { low =
                            (match i1.up with
                             | Some z2 -> Some (Z.mul l2 z2)
                             | None -> None); up =
                            (match i1.low with
                             | Some z2 -> Some (Z.mul l2 z2)
                             | None -> None) }
                        | UP ->
                          { low = None; up =
                            (match i1.low with
                             | Some z2 -> Some (Z.mul l2 z2)
                             | None -> None) }
                        | LOW ->
                          { low =
                            (match i1.up with
                             | Some z2 -> Some (Z.mul l2 z2)
                             | None -> None); up = None }))
                 (if coq_Z_isNat u2
                  then (match m with
                        | BOTH ->
                          { low =
                            (if coq_Z_isZero u2
                             then Some Z0
                             else (match i1.low with
                                   | Some z2 -> Some (Z.mul u2 z2)
                                   | None -> None)); up =
                            (if coq_Z_isZero u2
                             then Some Z0
                             else (match i1.up with
                                   | Some z2 -> Some (Z.mul u2 z2)
                                   | None -> None)) }
                        | UP ->
                          { low = None; up =
                            (if coq_Z_isZero u2
                             then Some Z0
                             else (match i1.up with
                                   | Some z2 -> Some (Z.mul u2 z2)
                                   | None -> None)) }
                        | LOW ->
                          { low =
                            (if coq_Z_isZero u2
                             then Some Z0
                             else (match i1.low with
                                   | Some z2 -> Some (Z.mul u2 z2)
                                   | None -> None)); up = None })
                  else (match m with
                        | BOTH ->
                          { low =
                            (match i1.up with
                             | Some z2 -> Some (Z.mul u2 z2)
                             | None -> None); up =
                            (match i1.low with
                             | Some z2 -> Some (Z.mul u2 z2)
                             | None -> None) }
                        | UP ->
                          { low = None; up =
                            (match i1.low with
                             | Some z2 -> Some (Z.mul u2 z2)
                             | None -> None) }
                        | LOW ->
                          { low =
                            (match i1.up with
                             | Some z2 -> Some (Z.mul u2 z2)
                             | None -> None); up = None }))
             | None ->
               (match m with
                | LOW -> top
                | _ ->
                  if coq_Z_isNat l2
                  then if coq_Z_isNegNat u1
                       then { low = None; up = (Some (Z.mul l2 u1)) }
                       else top
                  else top))
          | None ->
            (match i2.up with
             | Some u2 ->
               (match m with
                | UP -> top
                | _ ->
                  if coq_Z_isNegNat u1
                  then if coq_Z_isNegNat u2
                       then { low = (Some (Z.mul u1 u2)); up = None }
                       else top
                  else top)
             | None -> top))
       | None ->
         if match i2.low with
            | Some z -> coq_Z_isZero z
            | None -> false
         then if match i2.up with
                 | Some z -> coq_Z_isZero z
                 | None -> false
              then i2
              else top
         else top)
 end

module NA =
 struct
  type t = ZAffTerm.t option

  (** val cte : ZN.t -> t **)

  let cte = function
  | Some z -> Some { ZAffTerm.lin = ZAffTerm.Lin.nil; ZAffTerm.cte = z }
  | None -> None

  (** val add : t -> t -> t **)

  let add aft1 aft2 =
    match aft1 with
    | Some aft3 ->
      (match aft2 with
       | Some aft4 -> Some (ZAffTerm.add aft3 aft4)
       | None -> None)
    | None -> None

  (** val opp : t -> t **)

  let opp = function
  | Some aft0 -> Some (ZAffTerm.opp aft0)
  | None -> None

  (** val mul : ZN.t -> ZAffTerm.t -> t **)

  let mul zn aft =
    if ZAffTerm.isZero aft
    then Some ZAffTerm.nil
    else (match zn with
          | Some z -> Some (ZAffTerm.mul z aft)
          | None -> None)

  (** val mulZ1 : ZNum.t -> t -> t **)

  let mulZ1 z = function
  | Some aft0 -> Some (ZAffTerm.mul z aft0)
  | None -> None

  (** val mulZ : ZNum.t -> t -> t **)

  let mulZ z = function
  | Some aft0 -> Some (ZAffTerm.mul z aft0)
  | None -> if coq_Z_isZero z then Some ZAffTerm.nil else None
 end

module NAItv =
 struct
  type itv = { low : NA.t; up : NA.t }

  (** val low : itv -> NA.t **)

  let low x = x.low

  (** val up : itv -> NA.t **)

  let up x = x.up

  (** val cte : ZNItv.t -> itv **)

  let cte i =
    { low = (NA.cte i.ZNItv.low); up = (NA.cte i.ZNItv.up) }

  (** val single : ZAffTerm.t -> itv **)

  let single aft =
    let bnd = Some aft in { low = bnd; up = bnd }

  (** val select : mode -> NA.t -> NA.t -> itv **)

  let select mo l u =
    match mo with
    | BOTH -> { low = l; up = u }
    | UP -> { low = None; up = u }
    | LOW -> { low = l; up = None }

  (** val add : mode -> itv -> itv -> itv **)

  let add mo i1 i2 =
    match mo with
    | BOTH -> { low = (NA.add i1.low i2.low); up = (NA.add i1.up i2.up) }
    | UP -> { low = None; up = (NA.add i1.up i2.up) }
    | LOW -> { low = (NA.add i1.low i2.low); up = None }

  (** val opp : mode -> itv -> itv **)

  let opp mo i =
    match mo with
    | BOTH -> { low = (NA.opp i.up); up = (NA.opp i.low) }
    | UP -> { low = None; up = (NA.opp i.low) }
    | LOW -> { low = (NA.opp i.up); up = None }

  (** val mulZ : mode -> coq_Z -> itv -> itv **)

  let mulZ mo c i =
    if coq_Z_isNat c
    then (match mo with
          | BOTH -> { low = (NA.mulZ c i.low); up = (NA.mulZ c i.up) }
          | UP -> { low = None; up = (NA.mulZ c i.up) }
          | LOW -> { low = (NA.mulZ c i.low); up = None })
    else (match mo with
          | BOTH -> { low = (NA.mulZ1 c i.up); up = (NA.mulZ1 c i.low) }
          | UP -> { low = None; up = (NA.mulZ1 c i.low) }
          | LOW -> { low = (NA.mulZ1 c i.up); up = None })

  (** val mulN : mode -> ZNItv.t -> ZAffTerm.t -> itv **)

  let mulN mo i aft =
    match mo with
    | BOTH -> { low = (NA.mul i.ZNItv.up aft); up = (NA.mul i.ZNItv.low aft) }
    | UP -> { low = None; up = (NA.mul i.ZNItv.low aft) }
    | LOW -> { low = (NA.mul i.ZNItv.up aft); up = None }

  (** val mulP1 : mode -> ZNItv.t -> ZAffTerm.t -> itv **)

  let mulP1 mo i aft =
    match mo with
    | BOTH -> { low = (NA.mul i.ZNItv.low aft); up = (NA.mul i.ZNItv.up aft) }
    | UP -> { low = None; up = (NA.mul i.ZNItv.up aft) }
    | LOW -> { low = (NA.mul i.ZNItv.low aft); up = None }
 end
