structure Absyn = struct
  datatype con = Con of { id: string,
                          typ: ty option }
       and tyvar = Tyvar of { id: string }
       and fixdir = Left of int | Right of int | Non
       and dec = Valdec of { patt: pat,
                             expr: exp,
                             recr: bool,
                             anddec: dec option,
                             typ: ty option }
               | Fvaldec of { id: string,
                              args: atpat list, 
                              typ: ty option,
                              body: exp,
                              anddec: dec option }
               | Tydec of { id: string,
                            params: tyvar list,
                            typ: ty,
                            anddec: dec option }
               | Datdec of { params: tyvar list,
                             id: string,
                             constrs: con list,
                             anddec: dec option }
               | Exdec of { id: string,
                            constr: con,
                            anddec: dec option }
               | Local of { locals: dec list,
                            body: dec list }
               | Open of { ids: string list }
               | Fixity of { dir: fixdir,
                             ids: string list }
end

