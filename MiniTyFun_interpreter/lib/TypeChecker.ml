module TypeChecker = struct

 open Ast

  let rec check_type (e: ty env) (t: term) : ty option =
    match t with
    | TNum _ -> Some(TInt)
    | TBool _ -> Some(TBool)
    | Var x -> (match e x with
      | UnBound -> None
      | t -> Some(t)
      )
    | Plus (t1, t2) | Times (t1, t2) -> (
      match (check_type e t1, check_type e t2) with
        | (Some(TInt), Some(TInt)) -> Some(TInt)
        | _ -> None
      )
    | Minus (t1, t2) -> (
      match (check_type e t1, check_type e t2) with
        | (Some(TInt), Some(TInt)) -> Some(TInt)
        | _ -> None
      )
    | And (t1, t2) -> (
      match (check_type e t1, check_type e t2) with
        | (Some(TBool), Some(TBool)) -> Some(TBool)
        | _ -> None
      )
    | Not t -> (
      match check_type e t with
        | Some(TBool) -> Some(TBool)
        | _ -> None
      )
    | Less (t1, t2) -> (
      match (check_type e t1, check_type e t2) with
        | (Some(TInt), Some(TInt)) -> Some(TBool)
        | _ -> None
      )
    | Let (x, t1, t2) -> (
      let et1 : ty option = (check_type e t1) in
      match et1 with
        | Some(v) -> check_type (bind e x v) t2
        | _ -> None
    )
    | IfThenElse (t1, t2, t3) -> (
      match (check_type e t1, check_type e t2, check_type e t3) with
        | (Some(TBool), Some(t2), Some(t3)) when t2 = t3 -> Some(t2)
        | _ -> None
      )
    | LetFun (f, x, ty, t1, t2) -> (
        match ty with
          | TRecFunctional(ty', ty'') -> (
            let e': ty env = bind e f ty in
            match (check_type (bind e' x ty') t1) with
              | Some(result_type) when result_type = ty'' -> check_type e' t2
              | _ -> None
            )
          | _ -> None
      )
    | Fun (x, ty, t) -> (
        match check_type (bind e x ty) t with
          | Some(ty'') -> Some(TFunctional(ty, ty''))
          | _ -> None
        )
    | FunApp (t1, t2) -> (
      match check_type e t1 with
        | Some(TFunctional(ty, ty1)) | Some TRecFunctional(ty, ty1) -> (
              match check_type e t2 with 
                | Some(ty') when ty = ty' -> Some(ty1)
                | _ -> None
        )
        | _ -> None
        )
  ;;


  let var_to_string (e: ty env) (x: ide) : string =
    match e x with
    | TInt -> x^":TInt"
    | TBool -> x^":TBool"
    | TFunctional (in_ty, out_ty) -> x^":Function " ^ (type_to_string (Some in_ty)) ^ " -> " ^ (type_to_string (Some out_ty)) ^ ")"
    | TRecFunctional (in_ty, out_ty) -> x^":RecFunction " ^ (type_to_string (Some in_ty)) ^ " -> " ^ (type_to_string (Some out_ty)) ^ ")"
    | UnBound -> x^":Variable not found"
  ;;

end